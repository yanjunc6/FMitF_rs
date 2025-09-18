//! frontend/semantic_analyzer.rs
//!
//! Traverses the AST to perform semantic checks that go beyond name and type resolution.

use super::errors::FrontEndErrorKind;
use crate::ast::common::IdentifierResolution;
use crate::ast::item::ConstId;
use crate::ast::visit::{walk_callable_decl, walk_const_decl, walk_stmt, walk_table_decl, Visitor};
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
    in_global_hop: bool,
}

impl<'a> SemanticAnalyzer<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
            current_callable_kind: None,
            in_global_hop: false,
        }
    }

    /// Runs the analyzer over the entire program.
    fn analyze(&mut self) -> Result<(), Vec<CompilerError>> {
        let _ = self.visit_program(self.program);
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Adds an error to the list of semantic errors.
    fn add_error(&mut self, kind: FrontEndErrorKind, span: Span) {
        self.errors.push(CompilerError::new(kind, span));
    }

    /// Gets the span from a statement.
    fn get_statement_span(&self, stmt: &Statement) -> Option<Span> {
        match stmt {
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

    /// Gets the human-readable name of a statement type.
    fn get_statement_type_name(&self, stmt: &Statement) -> &str {
        match stmt {
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

    /// Gets the span from an expression.
    fn get_expression_span(&self, expr: &Expression) -> Option<Span> {
        match expr {
            Expression::Literal { span, .. } => *span,
            Expression::Identifier { span, .. } => *span,
            Expression::Binary { span, .. } => *span,
            Expression::Unary { span, .. } => *span,
            Expression::Assignment { span, .. } => *span,
            Expression::Call { span, .. } => *span,
            Expression::MemberAccess { span, .. } => *span,
            Expression::TableRowAccess { span, .. } => *span,
            Expression::Grouped { span, .. } => *span,
            Expression::Lambda { span, .. } => *span,
        }
    }

    /// Checks if an expression is a compile-time constant integer.
    /// Only allows: 1) Literal integers, 2) Global const variables that are integers
    /// Operations like 2 + C are NOT allowed.
    fn is_const_integer(&self, expr_id: ExprId) -> (bool, Option<i64>) {
        let expr = &self.program.expressions[expr_id];
        match expr {
            Expression::Literal { value, .. } => {
                if let Literal::Integer(val_str) = value {
                    (true, val_str.parse().ok())
                } else {
                    (false, None)
                }
            }
            Expression::Identifier {
                resolved_declarations,
                ..
            } => {
                if let Some(IdentifierResolution::Const(const_id)) = resolved_declarations.get(0) {
                    let const_decl = &self.program.const_decls[*const_id];
                    if let Expression::Literal {
                        value: Literal::Integer(val_str),
                        ..
                    } = &self.program.expressions[const_decl.value]
                    {
                        return (true, val_str.parse().ok());
                    }
                }
                (false, None)
            }
            _ => (false, None),
        }
    }

    /// Checks if a function is marked with the @global decorator.
    fn is_global_function(&self, function_id: FunctionId) -> bool {
        let decl = &self.program.functions[function_id];
        decl.decorators
            .iter()
            .any(|decorator| decorator.name.name == "global")
    }

    /// Checks if a hop block has the @global decorator.
    fn is_global_hop(&self, decorators: &[Decorator]) -> bool {
        decorators
            .iter()
            .any(|decorator| decorator.name.name == "global")
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
                        if let Some(span) = self.get_statement_span(statement) {
                            self.add_error(
                                FrontEndErrorKind::InvalidTransactionStatement {
                                    statement_type: self
                                        .get_statement_type_name(statement)
                                        .to_string(),
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

        // Handle hop blocks and track @global decorator
        match stmt {
            Statement::Hop {
                decorators, body, ..
            } => {
                let was_in_global_hop = self.in_global_hop;
                self.in_global_hop = self.is_global_hop(decorators);

                // Visit the hop body
                self.visit_block(prog, *body)?;

                // Restore previous state
                self.in_global_hop = was_in_global_hop;
                return Ok(());
            }
            Statement::HopsFor {
                decorators,
                start,
                end,
                body,
                ..
            } => {
                // Rule 5: `hops_for` start and end must be constant integers.
                // Check start expression
                let (is_const_start, _) = self.is_const_integer(*start);
                if !is_const_start {
                    if let Some(span) = self.get_expression_span(&prog.expressions[*start]) {
                        self.add_error(
                            FrontEndErrorKind::HopsForNonConstant {
                                context: "start expression".to_string(),
                            },
                            span,
                        );
                    }
                }

                // Check end expression
                let (is_const_end, _) = self.is_const_integer(*end);
                if !is_const_end {
                    if let Some(span) = self.get_expression_span(&prog.expressions[*end]) {
                        self.add_error(
                            FrontEndErrorKind::HopsForNonConstant {
                                context: "end expression".to_string(),
                            },
                            span,
                        );
                    }
                }

                let was_in_global_hop = self.in_global_hop;
                self.in_global_hop = self.is_global_hop(decorators);

                // Visit the hops_for body
                self.visit_block(prog, *body)?;

                // Restore previous state
                self.in_global_hop = was_in_global_hop;
                return Ok(());
            }
            _ => {}
        }

        walk_stmt(self, prog, id)
    }

    /// Visit an expression.
    fn visit_expr(&mut self, prog: &'ast Program, id: ExprId) -> Result<(), ()> {
        let expr = &prog.expressions[id];

        // Check for function calls to @global functions
        if let Expression::Call { callee, span, .. } = expr {
            if let Expression::Identifier {
                name,
                resolved_declarations,
                ..
            } = &prog.expressions[*callee]
            {
                // Check if this is a call to a @global function
                for resolution in resolved_declarations {
                    if let IdentifierResolution::Function(function_id) = resolution {
                        if self.is_global_function(*function_id) && !self.in_global_hop {
                            if let Some(s) = span {
                                self.add_error(
                                    FrontEndErrorKind::GlobalFunctionInNonGlobalHop {
                                        function_name: name.name.clone(),
                                    },
                                    *s,
                                );
                            }
                        }
                    }
                }
            }
        }

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

        // Continue visiting child expressions
        Ok(())
    }

    /// Visit a const declaration to validate const constraints.
    fn visit_const_decl(
        &mut self,
        prog: &'ast Program,
        _id: ConstId,
        decl: &'ast crate::ast::item::ConstDecl,
    ) -> Result<(), ()> {
        // Rule: Const declarations must only contain literal values
        let value_expr = &prog.expressions[decl.value];

        if !matches!(value_expr, Expression::Literal { .. }) {
            if let Some(span) = self.get_expression_span(value_expr) {
                self.add_error(
                    FrontEndErrorKind::InvalidInput {
                        message: "Const declarations must only contain literal values. Operations on constants are not allowed.".to_string(),
                    },
                    span,
                );
            }
        }

        // Continue with default walking behavior
        walk_const_decl(self, prog, _id, decl)
    }
}
// node's function should return int
// no hop in function, only in transaction
// transaction's top level only hop and hops_for
// partition must return int
// transaction should not have <T> generic
