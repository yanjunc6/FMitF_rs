//! frontend/ast_builder.rs
//!
//! This module is responsible for walking the Pest parse tree and constructing
//! the Abstract Syntax Tree (AST) defined in `crate::ast`.

use crate::ast::{
    AstType, Block, CallableDecl, CallableKind, ConstDecl, Decorator, Expression, ForInit,
    FunctionId, GenericParam, GenericParamId, Identifier, Item, KeyValue, Literal, ParamId,
    Parameter, Program, Spanned, Statement, TableDecl, TableElement, TableField, TableId,
    TableNode, TypeDecl, TypeDeclId, VarDecl,
};
use crate::frontend::FrontEndErrorKind;
use crate::util::{CompilerError, Span};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

// ============================================================================
// --- Pest Parser Definition
// ============================================================================

#[derive(Parser)]
#[grammar = "frontend/grammar.pest"]
pub struct TransactParser;

// ============================================================================
// --- AST Builder
// ============================================================================

type Result<T> = std::result::Result<T, CompilerError>;

/// Consumes Pest `Pairs` and produces an `ast::Program`.
pub struct AstBuilder {
    program: Program,
    current_filename: &'static str,
}

impl AstBuilder {
    /// Creates a new `AstBuilder`.
    pub fn new(current_filename: &'static str) -> Self {
        AstBuilder {
            program: Program::default(),
            current_filename,
        }
    }

    /// Converts a pest::Span into our custom util::Span.
    fn to_span(&self, span: pest::Span) -> Span {
        // Leak the string to get a 'static lifetime
        Span {
            start: span.start(),
            end: span.end(),
            filename: self.current_filename,
        }
    }

    fn build_compiler_error(&self, kind: FrontEndErrorKind, span: pest::Span) -> CompilerError {
        CompilerError::new(kind, self.to_span(span))
    }

    /// Builds the full `ast::Program` from the parsed pairs.
    /// This is the main entry point for the builder.
    pub fn build(mut self, pairs: Pairs<Rule>) -> Result<Program> {
        let program_pair = pairs.peek().ok_or(self.build_compiler_error(
            FrontEndErrorKind::ParseError("missing program rules".to_string()),
            pairs.span(),
        ))?;
        if program_pair.as_rule() != Rule::program {
            return Err(
                self.build_compiler_error(FrontEndErrorKind::UnexpectedRule {
                    message: "expected: program, found: ".to_string()
                        + &program_pair.as_rule().to_string(),
                }),
            );
        }

        for pair in program_pair.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    let item = self.build_declaration(pair)?;
                    self.program.declarations.push(item);
                }
                Rule::EOI => (),
                _ => {
                    return Err(AstBuilderError::UnexpectedRule {
                        expected: vec![Rule::declaration, Rule::EOI],
                        found: pair.as_rule(),
                    })
                }
            }
        }
        Ok(self.program)
    }

    // ========================================================================
    // --- Declarations
    // ========================================================================

    fn build_declaration(&mut self, pair: Pair<Rule>) -> Result<Item> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or(AstBuilderError::MissingRule(Rule::declaration))?;
        match inner.as_rule() {
            Rule::callable_declaration => {
                self.build_callable_declaration(inner).map(Item::Callable)
            }
            Rule::type_declaration => self.build_type_declaration(inner).map(Item::Type),
            Rule::const_declaration => self.build_const_declaration(inner).map(Item::Const),
            Rule::table_declaration => self.build_table_declaration(inner).map(Item::Table),
            _ => Err(AstBuilderError::UnexpectedRule {
                expected: vec![
                    Rule::callable_declaration,
                    Rule::type_declaration,
                    Rule::const_declaration,
                    Rule::table_declaration,
                ],
                found: inner.as_rule(),
            }),
        }
    }

    fn build_callable_declaration(&mut self, pair: Pair<Rule>) -> Result<FunctionId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();

        let decorators = self.build_decorators(&mut inner)?;

        // The next item is a choice between `function`, `operator`, etc.
        let kind_pair = inner.next().unwrap();
        let (kind, name) = if kind_pair.as_rule() == Rule::operator_symbol {
            // It's an operator
            let name = Identifier {
                name: kind_pair.as_str().to_string(),
                span: Some(self.to_span(kind_pair.as_span())),
            };
            (CallableKind::Operator, name)
        } else {
            // It's function/partition/transaction
            let kind = match kind_pair.as_str() {
                "function" => CallableKind::Function,
                "partition" => CallableKind::Partition,
                "transaction" => CallableKind::Transaction,
                _ => unreachable!("Unexpected callable kind"),
            };
            let name = self.build_identifier(inner.next().unwrap())?;
            (kind, name)
        };

        let mut generic_params = vec![];
        let mut params = vec![];
        let mut return_type = None;
        let mut assumptions = vec![];
        let mut body = None;

        // Process remaining parts of the declaration
        for p in inner {
            match p.as_rule() {
                Rule::generic_param_list => generic_params = self.build_generic_param_list(p)?,
                Rule::parameter_list => params = self.build_parameter_list(p)?,
                Rule::type_rule => return_type = Some(self.build_type(p)?),
                Rule::assume_statement => {
                    let expr_id = self.build_expression(p.into_inner().next().unwrap())?;
                    assumptions.push(expr_id);
                }
                Rule::block => body = Some(self.build_block(p)?),
                Rule::EOI | Rule::COMMENT => {} // Ignore
                _ => {
                    return Err(AstBuilderError::UnexpectedRule {
                        expected: vec![
                            Rule::generic_param_list,
                            Rule::parameter_list,
                            Rule::type_rule,
                            Rule::assume_statement,
                            Rule::block,
                        ],
                        found: p.as_rule(),
                    })
                }
            }
        }

        let decl = CallableDecl {
            decorators,
            kind,
            name,
            generic_params,
            params,
            return_type,
            assumptions,
            body,
            resolved_return_type: None,
            resolved_function_type: None,
            span,
        };
        Ok(self.program.functions.alloc(decl))
    }

    fn build_type_declaration(&mut self, pair: Pair<Rule>) -> Result<TypeDeclId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();

        let decorators = self.build_decorators(&mut inner)?;
        let name = self.build_identifier(inner.next().unwrap())?;

        let generic_params = if let Some(p) = inner.next() {
            if p.as_rule() == Rule::generic_param_list {
                self.build_generic_param_list(p)?
            } else {
                vec![]
            }
        } else {
            vec![]
        };

        let decl = TypeDecl {
            decorators,
            name,
            generic_params,
            span,
        };
        Ok(self.program.type_decls.alloc(decl))
    }

    fn build_const_declaration(&mut self, pair: Pair<Rule>) -> Result<ConstId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();

        let name = self.build_identifier(inner.next().unwrap())?;
        let ty = self.build_type(inner.next().unwrap())?;
        let value = self.build_expression(inner.next().unwrap())?;

        let decl = ConstDecl {
            name,
            ty,
            value,
            resolved_type: None,
            span,
        };
        Ok(self.program.const_decls.alloc(decl))
    }

    fn build_table_declaration(&mut self, pair: Pair<Rule>) -> Result<TableId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();

        let name = self.build_identifier(inner.next().unwrap())?;
        let mut elements = vec![];

        for p in inner {
            if p.as_rule() == Rule::table_field {
                elements.push(self.build_table_element(p)?);
            }
        }

        let decl = TableDecl {
            name,
            elements,
            span,
        };
        Ok(self.program.table_decls.alloc(decl))
    }

    fn build_table_element(&mut self, pair: Pair<Rule>) -> Result<TableElement> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::field_declaration => {
                let mut p_inner = inner.into_inner();
                let first = p_inner.peek().unwrap();
                let is_primary = if first.as_rule() == Rule::primary {
                    p_inner.next(); // consume "primary"
                    true
                } else {
                    false
                };

                let name = self.build_identifier(p_inner.next().unwrap())?;
                let ty = self.build_type(p_inner.next().unwrap())?;

                let field = TableField {
                    is_primary,
                    name,
                    ty,
                    resolved_type: None,
                    span: Some(self.to_span(inner.as_span())),
                };
                Ok(TableElement::Field(field))
            }
            Rule::node_declaration => {
                let mut p_inner = inner.into_inner();
                let name = self.build_identifier(p_inner.next().unwrap())?;
                let args = if let Some(expr_list) = p_inner.next() {
                    self.build_expression_list(expr_list)?
                } else {
                    vec![]
                };

                let node = TableNode {
                    name,
                    args,
                    resolved_partition: None,
                    span: Some(self.to_span(inner.as_span())),
                };
                Ok(TableElement::Node(node))
            }
            Rule::invariant_declaration => {
                let expr = self.build_expression(inner.into_inner().next().unwrap())?;
                Ok(TableElement::Invariant(expr))
            }
            _ => Err(AstBuilderError::UnexpectedRule {
                expected: vec![
                    Rule::field_declaration,
                    Rule::node_declaration,
                    Rule::invariant_declaration,
                ],
                found: inner.as_rule(),
            }),
        }
    }

    // ========================================================================
    // --- Statements
    // ========================================================================

    fn build_statement(&mut self, pair: Pair<Rule>) -> Result<StmtId> {
        let inner = pair.into_inner().next().unwrap();
        let span = Some(self.to_span(inner.as_span()));
        let statement = match inner.as_rule() {
            Rule::if_statement => {
                let mut p = inner.into_inner();
                let condition = self.build_expression(p.next().unwrap())?;
                let then_block = self.build_block(p.next().unwrap())?;
                let else_block = p.next().map(|pair| self.build_block(pair)).transpose()?;
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                    span,
                }
            }
            Rule::for_statement => {
                let mut p = inner.into_inner();
                // for (init; cond; update) body
                let init = self.build_for_init(p.next().unwrap())?;
                let condition = p
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .map(|pair| self.build_expression(pair))
                    .transpose()?;
                let update = p
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .map(|pair| self.build_expression(pair))
                    .transpose()?;
                let body = self.build_block(p.next().unwrap())?;
                Statement::For {
                    init,
                    condition,
                    update,
                    body,
                    span,
                }
            }
            Rule::var_declaration => {
                let var_id = self.build_var_decl_from_pair(inner)?;
                Statement::VarDecl(var_id)
            }
            Rule::return_statement => {
                let value = inner
                    .into_inner()
                    .next()
                    .map(|pair| self.build_expression(pair))
                    .transpose()?;
                Statement::Return { value, span }
            }
            Rule::assert_statement => {
                let expr = self.build_expression(inner.into_inner().next().unwrap())?;
                Statement::Assert { expr, span }
            }
            Rule::hop_block => {
                let mut p = inner.into_inner();
                let decorators = self.build_decorators(&mut p)?;
                let body = self.build_block(p.next().unwrap())?;
                Statement::Hop {
                    decorators,
                    body,
                    span,
                }
            }
            Rule::hops_for_loop => {
                let mut p = inner.into_inner();
                let decorators = self.build_decorators(&mut p)?;
                let var_name = self.build_identifier(p.next().unwrap())?;
                let ty = self.build_type(p.next().unwrap())?;
                let start = self.build_expression(p.next().unwrap())?;
                let end = self.build_expression(p.next().unwrap())?;
                let body = self.build_block(p.next().unwrap())?;

                // Hots for implicitly creates a var declaration for the loop variable
                let var_decl = VarDecl {
                    name: var_name,
                    ty: Some(ty),
                    init: Some(start), // The loop variable is initialized with the start value.
                    resolved_type: None,
                    span: None, // Or span of the variable part
                };
                let var = self.program.var_decls.alloc(var_decl);

                Statement::HopsFor {
                    decorators,
                    var,
                    start,
                    end,
                    body,
                    span,
                }
            }
            Rule::expression_statement => {
                let expr = self.build_expression(inner.into_inner().next().unwrap())?;
                Statement::Expression { expr, span }
            }
            Rule::block => {
                let block_id = self.build_block(inner)?;
                Statement::Block(block_id)
            }
            _ => {
                return Err(AstBuilderError::UnexpectedRule {
                    expected: vec![
                        Rule::if_statement,
                        Rule::for_statement,
                        Rule::var_declaration,
                        /* ... other statements */
                    ],
                    found: inner.as_rule(),
                });
            }
        };
        Ok(self.program.statements.alloc(statement))
    }

    fn build_for_init(&mut self, pair: Pair<Rule>) -> Result<Option<ForInit>> {
        let Some(inner) = pair.into_inner().next() else {
            return Ok(None); // Empty init statement (e.g., `for(;;){}`)
        };
        match inner.as_rule() {
            Rule::var_declaration => {
                let var_id = self.build_var_decl_from_pair(inner)?;
                Ok(Some(ForInit::VarDecl(var_id)))
            }
            Rule::expression_statement => {
                let expr_id = self.build_expression(inner.into_inner().next().unwrap())?;
                Ok(Some(ForInit::Expression(expr_id)))
            }
            _ => Err(AstBuilderError::UnexpectedRule {
                expected: vec![Rule::var_declaration, Rule::expression_statement],
                found: inner.as_rule(),
            }),
        }
    }

    fn build_var_decl_from_pair(&mut self, pair: Pair<Rule>) -> Result<VarId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();

        let name = self.build_identifier(inner.next().unwrap())?;
        let mut ty = None;
        let mut init = None;

        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::type_rule {
                ty = Some(self.build_type(inner.next().unwrap())?);
            }
        }
        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::expression {
                init = Some(self.build_expression(inner.next().unwrap())?);
            }
        }

        let decl = VarDecl {
            name,
            ty,
            init,
            resolved_type: None,
            span,
        };
        Ok(self.program.var_decls.alloc(decl))
    }

    // ========================================================================
    // --- Expressions
    // ========================================================================

    fn build_expression(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        match pair.as_rule() {
            Rule::assignment
            | Rule::level10_expr
            | Rule::level9_expr
            | Rule::level8_expr
            | Rule::level7_expr
            | Rule::level6_expr
            | Rule::level5_expr
            | Rule::level4_expr
            | Rule::level3_expr => self.build_binary_or_unary_expr(pair),
            Rule::prefix => self.build_binary_or_unary_expr(pair),
            Rule::postfix => self.build_postfix_expr(pair),
            Rule::primary => self.build_primary_expr(pair),
            _ => Err(AstBuilderError::UnexpectedRule {
                expected: vec![Rule::assignment /* ... other expression rules */],
                found: pair.as_rule(),
            }),
        }
    }

    // This function handles all infix and prefix operators.
    fn build_binary_or_unary_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();

        // Handle prefix operators
        if inner.peek().unwrap().as_rule() == Rule::operator_symbol {
            let op_pair = inner.next().unwrap();
            let op = Spanned {
                value: op_pair.as_str().to_string(),
                span: Some(self.to_span(op_pair.as_span())),
            };
            let expr = self.build_expression(inner.next().unwrap())?;
            let unary_expr = Expression::Unary {
                op,
                expr,
                resolved_callable: None,
                resolved_type: None,
                span,
            };
            return Ok(self.program.expressions.alloc(unary_expr));
        }

        // Handle infix operators
        let mut lhs = self.build_expression(inner.next().unwrap())?;

        while let Some(op_pair) = inner.next() {
            let op = Spanned {
                value: op_pair.as_str().to_string(),
                span: Some(self.to_span(op_pair.as_span())),
            };
            let rhs = self.build_expression(inner.next().unwrap())?;

            let new_lhs_expr = if op.value == "=" {
                Expression::Assignment {
                    lhs,
                    rhs,
                    resolved_type: None,
                    span, // Note: span covers the whole assignment
                }
            } else {
                Expression::Binary {
                    left: lhs,
                    op,
                    right: rhs,
                    resolved_callable: None,
                    resolved_type: None,
                    span, // Note: span covers the whole binary expression
                }
            };
            lhs = self.program.expressions.alloc(new_lhs_expr);
        }

        Ok(lhs)
    }

    fn build_postfix_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let mut inner = pair.into_inner();
        let mut current_expr_id = self.build_primary_expr(inner.next().unwrap())?;

        for p in inner {
            let span = Some(self.to_span(p.as_span()));
            current_expr_id = match p.as_rule() {
                Rule::call => {
                    let args = p
                        .into_inner()
                        .next()
                        .map(|list| self.build_expression_list(list))
                        .transpose()?
                        .unwrap_or_default();
                    let call_expr = Expression::Call {
                        callee: current_expr_id,
                        args,
                        resolved_callable: None,
                        resolved_type: None,
                        span,
                    };
                    self.program.expressions.alloc(call_expr)
                }
                Rule::member_access => {
                    let member = self.build_identifier(p.into_inner().next().unwrap())?;
                    let member_access_expr = Expression::MemberAccess {
                        object: current_expr_id,
                        member,
                        resolved_table: None,
                        resolved_field: None,
                        resolved_type: None,
                        span,
                    };
                    self.program.expressions.alloc(member_access_expr)
                }
                Rule::table_row_access => {
                    let key_values = p
                        .into_inner()
                        .map(|kv_pair| self.build_key_value_pair(kv_pair))
                        .collect::<Result<Vec<_>>>()?;
                    let row_access_expr = Expression::TableRowAccess {
                        table: current_expr_id,
                        key_values,
                        resolved_table: None,
                        resolved_type: None,
                        span,
                    };
                    self.program.expressions.alloc(row_access_expr)
                }
                _ => {
                    return Err(AstBuilderError::UnexpectedRule {
                        expected: vec![Rule::call, Rule::member_access, Rule::table_row_access],
                        found: p.as_rule(),
                    })
                }
            };
        }
        Ok(current_expr_id)
    }

    fn build_primary_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let inner = pair.into_inner().next().unwrap();
        let span = Some(self.to_span(inner.as_span()));

        match inner.as_rule() {
            Rule::literal => self.build_literal(inner),
            Rule::identifier => {
                let ident = self.build_identifier(inner)?;
                let expr = Expression::Identifier {
                    name: ident,
                    resolved_declaration: None,
                    resolved_type: None,
                    span,
                };
                Ok(self.program.expressions.alloc(expr))
            }
            Rule::expression => {
                // This is a grouped expression `( ... )`
                let expr = self.build_expression(inner)?;
                let grouped_expr = Expression::Grouped {
                    expr,
                    resolved_type: None,
                    span,
                };
                Ok(self.program.expressions.alloc(grouped_expr))
            }
            Rule::lambda_expression => {
                let mut p = inner.into_inner();
                let params = p
                    .next()
                    .and_then(|pair| pair.into_inner().next())
                    .map(|param_list| self.build_parameter_list(param_list))
                    .transpose()?
                    .unwrap_or_default();
                let return_type = self.build_type(p.next().unwrap())?;
                let body = self.build_block(p.next().unwrap())?;
                let lambda = Expression::Lambda {
                    params,
                    return_type,
                    body,
                    resolved_type: None,
                    span,
                };
                Ok(self.program.expressions.alloc(lambda))
            }
            _ => Err(AstBuilderError::UnexpectedRule {
                expected: vec![
                    Rule::literal,
                    Rule::identifier,
                    Rule::expression,
                    Rule::lambda_expression,
                ],
                found: inner.as_rule(),
            }),
        }
    }

    // ========================================================================
    // --- Types
    // ========================================================================

    fn build_type(&mut self, pair: Pair<Rule>) -> Result<AstTypeId> {
        let inner = pair.into_inner().next().unwrap();
        let span = Some(self.to_span(inner.as_span()));

        let ast_type = match inner.as_rule() {
            Rule::primitive_type => {
                let name = self.build_identifier(inner.into_inner().next().unwrap())?;
                AstType::Named {
                    name,
                    resolved_type: None,
                }
            }
            Rule::generic_type => {
                let mut p = inner.into_inner();
                let base = self.build_identifier(p.next().unwrap())?;
                let args = p
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|pair| self.build_type(pair))
                    .collect::<Result<Vec<_>>>()?;
                AstType::Generic {
                    base,
                    args,
                    resolved_base_type: None,
                    span,
                }
            }
            Rule::function_type => {
                let mut p = inner.into_inner();
                let params = p
                    .next()
                    .and_then(|pair| pair.into_inner().next())
                    .map(|type_list| {
                        type_list
                            .into_inner()
                            .map(|pair| self.build_type(pair))
                            .collect::<Result<Vec<_>>>()
                    })
                    .transpose()?
                    .unwrap_or_default();
                let return_type = self.build_type(p.next().unwrap())?;
                AstType::Function {
                    params,
                    return_type,
                    span,
                }
            }
            _ => {
                return Err(AstBuilderError::UnexpectedRule {
                    expected: vec![
                        Rule::primitive_type,
                        Rule::generic_type,
                        Rule::function_type,
                    ],
                    found: inner.as_rule(),
                })
            }
        };

        Ok(self.program.types.alloc(ast_type))
    }

    // ========================================================================
    // --- Literals & Helpers
    // ========================================================================

    fn build_literal(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let span = Some(self.to_span(pair.as_span()));
        let inner = pair.into_inner().next().unwrap();
        let literal_value = match inner.as_rule() {
            Rule::integer => Literal::Integer(inner.as_str().to_string()),
            Rule::float => Literal::Float(inner.as_str().to_string()),
            Rule::string => {
                // Remove quotes from the string literal
                let s = inner.as_str();
                Literal::String(s[1..s.len() - 1].to_string())
            }
            Rule::bool => Literal::Bool(inner.as_str().parse().unwrap()),
            _ => {
                return Err(AstBuilderError::UnexpectedRule {
                    expected: vec![
                        Rule::integer,
                        Rule::float,
                        Rule::string,
                        Rule::bool, /* list/row */
                    ],
                    found: inner.as_rule(),
                });
            }
        };
        let expr = Expression::Literal {
            value: literal_value,
            resolved_type: None,
            span,
        };
        Ok(self.program.expressions.alloc(expr))
    }

    fn build_block(&mut self, pair: Pair<Rule>) -> Result<BlockId> {
        let span = Some(self.to_span(pair.as_span()));
        let statements = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::statement)
            .map(|stmt_pair| self.build_statement(stmt_pair))
            .collect::<Result<Vec<_>>>()?;
        let block = Block { statements, span };
        Ok(self.program.blocks.alloc(block))
    }

    fn build_parameter_list(&mut self, pair: Pair<Rule>) -> Result<Vec<ParamId>> {
        pair.into_inner().map(|p| self.build_parameter(p)).collect()
    }

    fn build_parameter(&mut self, pair: Pair<Rule>) -> Result<ParamId> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();
        let name = self.build_identifier(inner.next().unwrap())?;
        let ty = self.build_type(inner.next().unwrap())?;
        let param = Parameter {
            name,
            ty,
            resolved_type: None,
            span,
        };
        Ok(self.program.params.alloc(param))
    }

    fn build_generic_param_list(&mut self, pair: Pair<Rule>) -> Result<Vec<GenericParamId>> {
        pair.into_inner()
            .map(|p| {
                let span = Some(self.to_span(p.as_span()));
                let name = self.build_identifier(p)?;
                let param = GenericParam { name, span };
                Ok(self.program.generic_params.alloc(param))
            })
            .collect()
    }

    fn build_expression_list(&mut self, pair: Pair<Rule>) -> Result<Vec<ExprId>> {
        pair.into_inner()
            .map(|p| self.build_expression(p))
            .collect()
    }

    fn build_key_value_pair(&mut self, pair: Pair<Rule>) -> Result<KeyValue> {
        let span = Some(self.to_span(pair.as_span()));
        let mut inner = pair.into_inner();
        let key = self.build_identifier(inner.next().unwrap())?;
        let value = self.build_expression(inner.next().unwrap())?;
        Ok(KeyValue {
            key,
            value,
            resolved_table: None,
            resolved_field: None,
            span,
        })
    }

    fn build_decorators(&mut self, pairs: &mut Pairs<Rule>) -> Result<Vec<Decorator>> {
        let mut decorators = vec![];
        while let Some(p) = pairs.peek() {
            if p.as_rule() == Rule::decorator {
                let decorator_pair = pairs.next().unwrap();
                let span = Some(self.to_span(decorator_pair.as_span()));
                let name = self.build_identifier(decorator_pair.into_inner().next().unwrap())?;
                decorators.push(Decorator { name, span });
            } else {
                break;
            }
        }
        Ok(decorators)
    }

    fn build_identifier(&self, pair: Pair<Rule>) -> Result<Identifier> {
        if pair.as_rule() != Rule::identifier {
            return Err(AstBuilderError::UnexpectedRule {
                expected: vec![Rule::identifier],
                found: pair.as_rule(),
            });
        }
        Ok(Identifier {
            name: pair.as_str().to_string(),
            span: Some(self.to_span(pair.as_span())),
        })
    }
}

// ============================================================================
// --- Parse Function
// ============================================================================

pub fn parse_program(source: &str) -> Result<Program, Vec<AstError>> {
    AstBuilder::parse(source)
}
