//! frontend/semantic_analyzer.rs
//!
//! Traverses the AST to perform semantic checks that go beyond name and type resolution.

use super::errors::FrontEndErrorKind;
use crate::ast::common::IdentifierResolution;
use crate::ast::visit::{walk_callable_decl, walk_stmt, walk_table_decl, Visitor};
use crate::ast::*;
use crate::util::{CompilerError, Span};

/// Analyzes the AST for semantic correctness.
pub fn analyze_semantics(program: &Program) -> Result<(), Vec<CompilerError>> {
    let mut analyzer = SemanticAnalyzer::new(program);
    analyzer.analyze()
}

/// A visitor that checks for semantic rules.
struct SemanticAnalyzer<'a> {
    program: &'a Program,
    errors: Vec<CompilerError>,
    current_callable_kind: Option<CallableKind>,
}

impl<'a> SemanticAnalyzer<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
            current_callable_kind: None,
        }
    }

    /// Runs the analyzer over the entire program.
    fn analyze(&mut self) -> Result<(), Vec<CompilerError>> {
        let _ = self.visit_program(self.program);
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    /// Adds an error to the list of semantic errors.
    fn add_error(&mut self, kind: FrontEndErrorKind, span: Span) {
        self.errors.push(CompilerError::new(kind, span));
    }

    /// Checks if an expression is a compile-time constant integer.
    fn is_const_integer(&self, expr_id: ExprId) -> bool {
        let expr = &self.program.expressions[expr_id];
        match expr {
            Expression::Literal { value, .. } => matches!(value, Literal::Integer(_)),
            Expression::Identifier {
                resolved_declarations,
                ..
            } => {
                resolved_declarations.iter().any(|res| {
                    if let IdentifierResolution::Const(const_id) = res {
                        // Further check if the const is of integer type
                        let const_decl = &self.program.const_decls[*const_id];
                        if let Some(ResolvedType::Primitive { type_id, .. }) =
                            &const_decl.resolved_type
                        {
                            // Assuming 'int' is a known primitive type name
                            self.program.type_decls[*type_id].name.name == "int"
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                })
            }
            _ => false,
        }
    }
}

// Implement the Visitor trait for the SemanticAnalyzer
impl<'ast> Visitor<'ast, (), ()> for SemanticAnalyzer<'ast> {
    /// Visit a callable declaration (function, transaction, etc.)
    fn visit_callable_decl(
        &mut self,
        prog: &'ast Program,
        id: FunctionId,
        decl: &'ast CallableDecl,
    ) -> Result<(), ()> {
        let previous_kind = self.current_callable_kind.take();
        self.current_callable_kind = Some(decl.kind);

        // Rule 1: Transactions must only contain `hop` or `hops_for` at the top level.
        if decl.kind == CallableKind::Transaction {
            if let Some(body_id) = decl.body {
                let body_block = &prog.blocks[body_id];
                for stmt_id in &body_block.statements {
                    let statement = &prog.statements[*stmt_id];
                    let is_allowed =
                        matches!(statement, Statement::Hop { .. } | Statement::HopsFor { .. });

                    if !is_allowed {
                        if let Some(span) = statement.span {
                            self.add_error(
                                FrontEndErrorKind::InvalidTransactionStatement {
                                    statement_type: statement.name().to_string(),
                                },
                                span,
                            );
                        }
                    }
                }
            }
        }

        walk_callable_decl(self, prog, id, decl)?;
        self.current_callable_kind = previous_kind;
        Ok(())
    }

    /// Visit a table declaration.
    fn visit_table_decl(
        &mut self,
        prog: &'ast Program,
        id: TableId,
        decl: &'ast TableDecl,
    ) -> Result<(), ()> {
        // Rule 2: Only one `node` is allowed in a table definition.
        if decl
            .elements
            .iter()
            .filter(|e| matches!(e, TableElement::Node(_)))
            .count()
            > 1
        {
            if let Some(span) = decl.span {
                self.add_error(
                    FrontEndErrorKind::MultipleTableNodes {
                        table_name: decl.name.name.clone(),
                    },
                    span,
                );
            }
        }
        walk_table_decl(self, prog, id, decl)
    }

    /// Visit a statement.
    fn visit_stmt(&mut self, prog: &'ast Program, id: StmtId) -> Result<(), ()> {
        let stmt = &prog.statements[id];

        // Rule 5: `hops_for` start and end must be constant integers.
        if let Statement::HopsFor {
            start, end, span, ..
        } = stmt
        {
            // Check start expression
            if !self.is_const_integer(*start) {
                if let Some(span) = prog.expressions[*start].span {
                    self.add_error(
                        FrontEndErrorKind::HopsForNonConstant {
                            context: "start expression".to_string(),
                        },
                        span,
                    );
                }
            }

            // Check end expression
            if !self.is_const_integer(*end) {
                if let Some(span) = prog.expressions[*end].span {
                    self.add_error(
                        FrontEndErrorKind::HopsForNonConstant {
                            context: "end expression".to_string(),
                        },
                        span,
                    );
                }
            }
        }
        walk_stmt(self, prog, id)
    }

    /// Visit an expression.
    fn visit_expr(&mut self, prog: &'ast Program, id: ExprId) -> Result<(), ()> {
        let expr = &prog.expressions[id];
        // Rule 4: Check for unresolved identifiers (should have been caught earlier).
        if let Expression::Identifier {
            name,
            resolved_declarations,
            span,
            ..
        } = expr
        {
            if resolved_declarations.is_empty() {
                if let Some(s) = span {
                    self.add_error(
                        FrontEndErrorKind::UnresolvedReference {
                            name: name.name.clone(),
                            category: "identifier".to_string(),
                        },
                        *s,
                    );
                }
            }
        }
        Ok(())
    }
}

// Helper to get a human-readable name for a Statement variant.
impl Statement {
    pub fn name(&self) -> &str {
        match self {
            Statement::VarDecl(_) => "Variable Declaration",
            Statement::If { .. } => "If Statement",
            Statement::For { .. } => "For Loop",
            Statement::Return { .. } => "Return Statement",
            Statement::Assert { .. } => "Assert Statement",
            Statement::Hop { .. } => "Hop Block",
            Statement::HopsFor { .. } => "Hops-For Loop",
            Statement::Expression { .. } => "Expression Statement",
            Statement::Block(_) => "Block Statement",
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            Statement::VarDecl(_) => None, // VarDecl span is in the arena
            Statement::If { span, .. } => *span,
            Statement::For { span, .. } => *span,
            Statement::Return { span, .. } => *span,
            Statement::Assert { span, .. } => *span,
            Statement::Hop { span, .. } => *span,
            Statement::HopsFor { span, .. } => *span,
            Statement::Expression { span, .. } => *span,
            Statement::Block(_) => None, // Block span is in the arena
        }
    }
}
