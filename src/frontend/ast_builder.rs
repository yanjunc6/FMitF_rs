//! frontend/ast_builder.rs
//!
//! This module is responsible for walking the Pest parse tree and constructing
//! the Abstract Syntax Tree (AST) defined in `crate::ast`.

use crate::ast::{
    AstType, AstTypeId, Block, BlockId, CallableDecl, CallableKind, ConstDecl, ConstId, Decorator,
    ExprId, Expression, ForInit, FunctionId, GenericParam, GenericParamId, Identifier, Item,
    KeyValue, Literal, ParamId, Parameter, Program, Statement, StmtId, TableDecl, TableElement,
    TableField, TableId, TableNode, TypeDecl, TypeDeclId, VarDecl, VarId,
};
use crate::frontend::FrontEndErrorKind;
use crate::util::{CompilerError, Span};
use clap::Id;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use pest_derive::Parser;
use std::mem;

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

pub struct AstBuilder {
    program: Program,
    filename: &'static str,
}

impl AstBuilder {
    pub fn new(filename: &'static str) -> Self {
        AstBuilder {
            program: Program::default(),
            filename,
        }
    }

    /// Converts a `pest::Span` to our internal `Span` type.
    fn to_span(&self, span: pest::Span) -> Span {
        Span::new(span.start(), span.end(), self.filename)
    }

    /// Main build method that consumes Pest pairs and returns a complete `Program`.
    pub fn build(&mut self, pairs: Pairs<Rule>) -> Result<Program> {
        let program_pair = pairs.peek().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("program root".to_string()),
                Span::new(0, 0, self.filename),
            )
        })?;

        if program_pair.as_rule() != Rule::program {
            return Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!("{:?}", program_pair.as_rule())),
                self.to_span(program_pair.as_span()),
            ));
        }

        for pair in program_pair.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    let item = self.build_declaration(pair)?;
                    self.program.declarations.push(item);
                }
                Rule::EOI => (),
                rule => {
                    return Err(CompilerError::new(
                        FrontEndErrorKind::UnexpectedRule(format!("top-level: {:?}", rule)),
                        self.to_span(pair.as_span()),
                    ))
                }
            }
        }
        Ok(mem::take(&mut self.program))
    }

    // --- Declarations ---

    fn build_declaration(&mut self, pair: Pair<Rule>) -> Result<Item> {
        let inner = pair.clone().into_inner().next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("declaration body".to_string()),
                self.to_span(pair.as_span()),
            )
        })?;
        let span = self.to_span(inner.as_span());
        match inner.as_rule() {
            Rule::callable_declaration => {
                self.build_callable_declaration(inner).map(Item::Callable)
            }
            Rule::type_declaration => self.build_type_declaration(inner).map(Item::Type),
            Rule::const_declaration => self.build_const_declaration(inner).map(Item::Const),
            Rule::table_declaration => self.build_table_declaration(inner).map(Item::Table),
            rule => Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!("in declaration: {:?}", rule)),
                span,
            )),
        }
    }
    fn build_callable_declaration(&mut self, pair: Pair<Rule>) -> Result<FunctionId> {
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();

        let decorators = self.build_decorators(&mut inner)?;

        // The next item is a choice between `function`, `operator`, etc.
        let kind_name_pair = inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("callable kind and name".to_string()),
                span,
            )
        })?;
        let (kind, name) = {
            let mut p = kind_name_pair.into_inner();
            let keyword = p.next().unwrap(); // Cannot fail based on grammar
            let name_token = p.next().unwrap();

            let kind = match keyword.as_str() {
                "function" => CallableKind::Function,
                "partition" => CallableKind::Partition,
                "transaction" => CallableKind::Transaction,
                "operator" => CallableKind::Operator,
                _ => unreachable!(),
            };
            let name = self.build_identifier(name_token)?;
            (kind, name)
        };

        let mut generic_params = vec![];
        let mut params = vec![];
        let mut return_type = None;
        let mut assumptions = vec![];
        let mut body = None;

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
                // Semicolon for forward declaration
                Rule::COMMENT => {}
                rule => {
                    return Err(CompilerError::new(
                        FrontEndErrorKind::UnexpectedRule(format!("in callable: {:?}", rule)),
                        self.to_span(p.as_span()),
                    ))
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
            span: Some(span),
        };
        Ok(self.program.functions.alloc(decl))
    }

    fn build_type_declaration(&mut self, pair: Pair<Rule>) -> Result<TypeDeclId> {
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();

        let decorators = self.build_decorators(&mut inner)?;
        let _type_keyword = inner.next().unwrap(); // consume "type"
        let name = self.build_identifier(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("type name".to_string()),
                span,
            )
        })?)?;

        let generic_params = if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::generic_param_list {
                self.build_generic_param_list(inner.next().unwrap())?
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
            span: Some(span),
        };
        Ok(self.program.type_decls.alloc(decl))
    }

    fn build_const_declaration(&mut self, pair: Pair<Rule>) -> Result<ConstId> {
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();
        let _const_keyword = inner.next().unwrap();
        let name = self.build_identifier(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("const name".to_string()),
                span,
            )
        })?)?;
        let ty = self.build_type(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("const type".to_string()),
                span,
            )
        })?)?;
        let value = self.build_expression(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("const value".to_string()),
                span,
            )
        })?)?;

        let decl = ConstDecl {
            name,
            ty,
            value,
            resolved_type: None,
            span: Some(span),
        };
        Ok(self.program.const_decls.alloc(decl))
    }

    fn build_table_declaration(&mut self, pair: Pair<Rule>) -> Result<TableId> {
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();
        let _table_keyword = inner.next().unwrap();
        let name = self.build_identifier(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("table name".to_string()),
                span,
            )
        })?)?;
        let mut elements = vec![];

        for p in inner {
            if p.as_rule() == Rule::table_field {
                elements.push(self.build_table_element(p)?);
            }
        }

        let decl = TableDecl {
            name,
            elements,
            span: Some(span),
        };
        Ok(self.program.table_decls.alloc(decl))
    }

    fn build_table_element(&mut self, pair: Pair<Rule>) -> Result<TableElement> {
        let inner = pair.into_inner().next().unwrap();
        let span = self.to_span(inner.as_span());
        match inner.as_rule() {
            Rule::field_declaration => {
                let mut p_inner = inner.into_inner();
                let is_primary = if p_inner.peek().unwrap().as_rule() == Rule::primary {
                    p_inner.next(); // consume "primary"
                    true
                } else {
                    false
                };

                let name = self.build_identifier(p_inner.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("field name".to_string()),
                        span,
                    )
                })?)?;
                let ty = self.build_type(p_inner.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("field type".to_string()),
                        span,
                    )
                })?)?;

                let field = TableField {
                    is_primary,
                    name,
                    ty,
                    resolved_type: None,
                    span: Some(span),
                };
                Ok(TableElement::Field(field))
            }
            Rule::node_declaration => {
                let mut p_inner = inner.into_inner();
                let _node_keyword = p_inner.next().unwrap();
                let name = self.build_identifier(p_inner.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("node name".to_string()),
                        span,
                    )
                })?)?;
                let args = if let Some(expr_list) = p_inner.next() {
                    self.build_expression_list(expr_list)?
                } else {
                    vec![]
                };

                let node = TableNode {
                    name,
                    args,
                    resolved_partition: None,
                    span: Some(span),
                };
                Ok(TableElement::Node(node))
            }
            Rule::invariant_declaration => {
                let mut p_inner = inner.into_inner();
                let _invariant_keyword = p_inner.next().unwrap();
                let expr = self.build_expression(p_inner.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("invariant expression".to_string()),
                        span,
                    )
                })?)?;
                Ok(TableElement::Invariant(expr))
            }
            rule => Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!("in table element: {:?}", rule)),
                span,
            )),
        }
    }

    // --- Statements ---
    fn build_statement(&mut self, pair: Pair<Rule>) -> Result<StmtId> {
        let inner = pair.into_inner().next().unwrap();
        let span = self.to_span(inner.as_span());
        let statement = match inner.as_rule() {
            Rule::var_declaration => {
                let var_id = self.build_var_decl_from_pair(inner)?;
                Statement::VarDecl(var_id)
            }
            Rule::if_statement => {
                let mut p = inner.into_inner();
                let _if_keyword = p.next().unwrap();
                let condition = self.build_expression(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("if condition".to_string()),
                        span,
                    )
                })?)?;
                let then_block = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(FrontEndErrorKind::MissingField("if body".to_string()), span)
                })?)?;
                let else_block = if let Some(else_keyword) = p.next() {
                    Some(self.build_block(p.next().ok_or_else(|| {
                        CompilerError::new(
                            FrontEndErrorKind::MissingField("else body".to_string()),
                            self.to_span(else_keyword.as_span()),
                        )
                    })?)?)
                } else {
                    None
                };
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                    span: Some(span),
                }
            }
            Rule::for_statement => {
                let mut p = inner.into_inner();
                let _for_keyword = p.next().unwrap();

                let init_pair = p.next().unwrap();
                let init = self.build_for_init(init_pair)?;

                let cond_pair = p.next().unwrap();
                let condition = cond_pair
                    .into_inner()
                    .next()
                    .map(|pair| self.build_expression(pair))
                    .transpose()?;

                let update_pair = p.next().unwrap();
                let update = update_pair
                    .into_inner()
                    .next()
                    .map(|pair| self.build_expression(pair))
                    .transpose()?;

                let body = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("for loop body".to_string()),
                        span,
                    )
                })?)?;
                Statement::For {
                    init,
                    condition,
                    update,
                    body,
                    span: Some(span),
                }
            }
            Rule::return_statement => {
                let mut p = inner.into_inner();
                let _return_keyword = p.next().unwrap();
                let value = p
                    .next()
                    .map(|pair| self.build_expression(pair))
                    .transpose()?;
                Statement::Return {
                    value,
                    span: Some(span),
                }
            }
            Rule::assert_statement => {
                let mut p = inner.into_inner();
                let _assert_keyword = p.next().unwrap();
                let expr = self.build_expression(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("assert expression".to_string()),
                        span,
                    )
                })?)?;
                Statement::Assert {
                    expr,
                    span: Some(span),
                }
            }
            Rule::hop_block => {
                let mut p = inner.into_inner();
                let decorators = self.build_decorators(&mut p)?;
                let _hop_keyword = p.next().unwrap();
                let body = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hop body".to_string()),
                        span,
                    )
                })?)?;
                Statement::Hop {
                    decorators,
                    body,
                    span: Some(span),
                }
            }
            Rule::hops_for_loop => {
                let mut p = inner.into_inner();
                let decorators = self.build_decorators(&mut p)?;
                let _hops_keyword = p.next().unwrap();
                let _for_keyword = p.next().unwrap();
                let var_name = self.build_identifier(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hops for variable".to_string()),
                        span,
                    )
                })?)?;
                let ty = self.build_type(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hops for type".to_string()),
                        span,
                    )
                })?)?;
                let start = self.build_expression(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hops for start".to_string()),
                        span,
                    )
                })?)?;
                let _to_keyword = p.next().unwrap();
                let end = self.build_expression(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hops for end".to_string()),
                        span,
                    )
                })?)?;
                let body = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hops for body".to_string()),
                        span,
                    )
                })?)?;

                let var_decl = VarDecl {
                    name: var_name,
                    ty: Some(ty),
                    init: Some(start),
                    resolved_type: None,
                    span: None,
                };
                let var = self.program.var_decls.alloc(var_decl);

                Statement::HopsFor {
                    decorators,
                    var,
                    start,
                    end,
                    body,
                    span: Some(span),
                }
            }
            Rule::block => {
                let block_id = self.build_block(inner)?;
                Statement::Block(block_id)
            }
            Rule::expression_statement => {
                let expr = self.build_expression(inner.into_inner().next().unwrap())?;
                Statement::Expression {
                    expr,
                    span: Some(span),
                }
            }
            rule => {
                return Err(CompilerError::new(
                    FrontEndErrorKind::UnexpectedRule(format!("in statement: {:?}", rule)),
                    span,
                ))
            }
        };
        Ok(self.program.statements.alloc(statement))
    }
    fn build_for_init(&mut self, pair: Pair<Rule>) -> Result<Option<ForInit>> {
        let Some(inner) = pair.into_inner().next() else {
            return Ok(None); // Empty init statement (e.g., `for(;;){}`)
        };
        let span = self.to_span(inner.as_span());
        match inner.as_rule() {
            Rule::var_declaration => {
                let var_id = self.build_var_decl_from_pair(inner)?;
                Ok(Some(ForInit::VarDecl(var_id)))
            }
            Rule::expression_statement => {
                let expr_id = self.build_expression(inner.into_inner().next().unwrap())?;
                Ok(Some(ForInit::Expression(expr_id)))
            }
            rule => Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!("in for-init: {:?}", rule)),
                span,
            )),
        }
    }

    fn build_var_decl_from_pair(&mut self, pair: Pair<Rule>) -> Result<VarId> {
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();
        let _var_keyword = inner.next().unwrap();
        let name = self.build_identifier(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("var name".to_string()),
                span,
            )
        })?)?;

        let mut ty = None;
        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::type_rule {
                ty = Some(self.build_type(inner.next().unwrap())?);
            }
        }

        let mut init = None;
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
            span: Some(span),
        };
        Ok(self.program.var_decls.alloc(decl))
    }

    // --- Expressions ---
    fn build_expression(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        // Instantiate the PrattParser with the operator precedence and associativity
        // defined in the grammar. The order is from lowest to highest precedence.
        let pratt = PrattParser::new()
            // Infix operators are defined with their rule and associativity.
            .op(Op::infix(Rule::assignment, Assoc::Right))
            .op(Op::infix(Rule::op10, Assoc::Left))
            .op(Op::infix(Rule::op9, Assoc::Right))
            .op(Op::infix(Rule::op8, Assoc::Left))
            .op(Op::infix(Rule::op7, Assoc::Right))
            .op(Op::infix(Rule::op6, Assoc::Left))
            .op(Op::infix(Rule::op5, Assoc::Right))
            .op(Op::infix(Rule::op4, Assoc::Left))
            .op(Op::infix(Rule::op3, Assoc::Left))
            // Prefix operators are defined with just their rule.
            .op(Op::prefix(Rule::prefix_op))
            // Postfix operators are also defined with just their rule.
            .op(Op::postfix(Rule::call))
            .op(Op::postfix(Rule::member_access))
            .op(Op::postfix(Rule::table_row_access));

        let exp_span = self.to_span(pair.clone().as_span());

        // The `parse` method climbs the expression tree.
        // It takes closures to handle primary terms, prefix, postfix, and infix operators.
        pratt
            .map_primary(|primary| self.build_primary_expr(primary))
            .map_prefix(|op, rhs| {
                let op_span = self.to_span(op.as_span());
                let expr = Expression::Unary {
                    op: Identifier {
                        name: op.as_str().to_string(),
                        span: Some(op_span),
                    },
                    expr: rhs?,
                    resolved_callable: None,
                    resolved_type: None,
                    span: Some(exp_span),
                };
                Ok(self.program.expressions.alloc(expr))
            })
            .map_postfix(|lhs, op| {
                let lhs = lhs?;
                let op_span = self.to_span(op.as_span());
                let expr = match op.as_rule() {
                    Rule::call => {
                        let args = op
                            .into_inner()
                            .next()
                            .map_or(Ok(vec![]), |list| self.build_expression_list(list))?;
                        Expression::Call {
                            callee: lhs,
                            args,
                            resolved_callable: None,
                            resolved_type: None,
                            span: Some(exp_span),
                        }
                    }
                    Rule::member_access => {
                        let member = self.build_identifier(op.into_inner().next().unwrap())?;
                        Expression::MemberAccess {
                            object: lhs,
                            member,
                            resolved_table: None,
                            resolved_field: None,
                            resolved_type: None,
                            span: Some(exp_span),
                        }
                    }
                    Rule::table_row_access => {
                        let key_values = op
                            .into_inner()
                            .map(|kv| self.build_key_value_pair(kv))
                            .collect::<Result<Vec<_>>>()?;
                        Expression::TableRowAccess {
                            table: lhs,
                            key_values,
                            resolved_table: None,
                            resolved_type: None,
                            span: Some(exp_span),
                        }
                    }
                    _ => unreachable!(),
                };
                Ok(self.program.expressions.alloc(expr))
            })
            .map_infix(|lhs, op, rhs| {
                let op_span = self.to_span(op.as_span());
                let expr = if op.as_rule() == Rule::assignment {
                    Expression::Assignment {
                        lhs: lhs?,
                        rhs: rhs?,
                        resolved_type: None,
                        span: Some(exp_span),
                    }
                } else {
                    Expression::Binary {
                        left: lhs?,
                        op: Identifier {
                            name: op.as_str().to_string(),
                            span: Some(op_span),
                        },
                        right: rhs?,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(exp_span),
                    }
                };
                Ok(self.program.expressions.alloc(expr))
            })
            .parse(pair.into_inner())
    }

    fn build_postfix_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let mut inner = pair.into_inner();
        // First part is always the prefix/primary part
        let mut current_expr_id = self.build_prefix_expr(inner.next().unwrap())?;

        // Then, chain any postfix operations
        for p in inner {
            let span = self.to_span(p.as_span());
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
                        span: Some(span),
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
                        span: Some(span),
                    };
                    self.program.expressions.alloc(member_access_expr)
                }
                Rule::table_row_access => {
                    let key_values = p
                        .into_inner()
                        .map(|kv_pair| self.build_key_value_pair(kv_pair))
                        .collect::<std::result::Result<Vec<_>, _>>()?;
                    let row_access_expr = Expression::TableRowAccess {
                        table: current_expr_id,
                        key_values,
                        resolved_table: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    self.program.expressions.alloc(row_access_expr)
                }
                rule => {
                    return Err(CompilerError::new(
                        FrontEndErrorKind::UnexpectedRule(format!("in postfix: {:?}", rule)),
                        span,
                    ))
                }
            };
        }
        Ok(current_expr_id)
    }

    fn build_prefix_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let mut inner = pair.clone().into_inner();
        let first = inner.peek().unwrap();

        if first.as_rule() == Rule::prefix_op {
            let op_pair = inner.next().unwrap();
            let op = Identifier {
                name: op_pair.as_str().to_string(),
                span: Some(self.to_span(op_pair.as_span())),
            };
            let expr = self.build_prefix_expr(inner.next().unwrap())?;
            let unary_expr = Expression::Unary {
                op,
                expr,
                resolved_callable: None,
                resolved_type: None,
                span: Some(self.to_span(pair.clone().as_span())),
            };
            Ok(self.program.expressions.alloc(unary_expr))
        } else {
            self.build_primary_expr(inner.next().unwrap())
        }
    }

    fn build_primary_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let inner = pair.into_inner().next().unwrap();
        let span = self.to_span(inner.as_span());

        match inner.as_rule() {
            Rule::literal => self.build_literal(inner),
            Rule::identifier => {
                let ident = self.build_identifier(inner)?;
                let expr = Expression::Identifier {
                    name: ident,
                    resolved_declaration: None,
                    resolved_type: None,
                    span: Some(span),
                };
                Ok(self.program.expressions.alloc(expr))
            }
            Rule::expression => {
                let expr = self.build_expression(inner)?;
                let grouped_expr = Expression::Grouped {
                    expr,
                    resolved_type: None,
                    span: Some(span),
                };
                Ok(self.program.expressions.alloc(grouped_expr))
            }
            Rule::lambda_expression => {
                let mut p = inner.into_inner();
                let params = if p.peek().unwrap().as_rule() == Rule::parameter_list {
                    self.build_parameter_list(p.next().unwrap())?
                } else {
                    vec![]
                };
                let return_type = self.build_type(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("lambda return type".to_string()),
                        span,
                    )
                })?)?;
                let body = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("lambda body".to_string()),
                        span,
                    )
                })?)?;
                let lambda = Expression::Lambda {
                    params,
                    return_type,
                    body,
                    resolved_type: None,
                    span: Some(span),
                };
                Ok(self.program.expressions.alloc(lambda))
            }
            rule => Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!("in primary expr: {:?}", rule)),
                span,
            )),
        }
    }

    // --- Types & Literals ---

    fn build_type(&mut self, pair: Pair<Rule>) -> Result<AstTypeId> {
        let inner = pair.into_inner().next().unwrap();
        let span = self.to_span(inner.as_span());

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
                    .unwrap() // type_list
                    .into_inner()
                    .map(|pair| self.build_type(pair))
                    .collect::<Result<Vec<_>>>()?;
                AstType::Generic {
                    base,
                    args,
                    resolved_base_type: None,
                    span: Some(span),
                }
            }
            Rule::function_type => {
                let mut p = inner.into_inner();
                let params = if p.peek().unwrap().as_rule() == Rule::type_list {
                    p.next()
                        .unwrap()
                        .into_inner()
                        .map(|pair| self.build_type(pair))
                        .collect::<Result<Vec<_>>>()?
                } else {
                    vec![]
                };
                let return_type = self.build_type(p.next().unwrap())?;
                AstType::Function {
                    params,
                    return_type,
                    span: Some(span),
                }
            }
            rule => {
                return Err(CompilerError::new(
                    FrontEndErrorKind::UnexpectedRule(format!("in type: {:?}", rule)),
                    span,
                ))
            }
        };

        Ok(self.program.types.alloc(ast_type))
    }

    fn build_literal(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let span = self.to_span(pair.as_span());
        let inner = pair.into_inner().next().unwrap();
        let literal_value = match inner.as_rule() {
            Rule::integer => Literal::Integer(inner.as_str().to_string()),
            Rule::float => Literal::Float(inner.as_str().to_string()),
            Rule::string => {
                let s = inner.as_str();
                Literal::String(s[1..s.len() - 1].to_string())
            }
            Rule::bool => Literal::Bool(inner.as_str().parse().unwrap()),
            Rule::list_literal => {
                let items = inner
                    .into_inner()
                    .next()
                    .map(|list| self.build_expression_list(list))
                    .transpose()?
                    .unwrap_or_default();
                Literal::List(items)
            }
            Rule::row_literal => {
                let key_values = inner
                    .into_inner()
                    .map(|kv| self.build_key_value_pair(kv))
                    .collect::<Result<Vec<_>>>()?;
                Literal::RowLiteral(key_values)
            }
            rule => {
                return Err(CompilerError::new(
                    FrontEndErrorKind::UnexpectedRule(format!("in literal: {:?}", rule)),
                    self.to_span(inner.as_span()),
                ))
            }
        };
        let expr = Expression::Literal {
            value: literal_value,
            resolved_type: None,
            span: Some(span),
        };
        Ok(self.program.expressions.alloc(expr))
    }

    // --- Helpers ---

    fn build_block(&mut self, pair: Pair<Rule>) -> Result<BlockId> {
        let span = self.to_span(pair.as_span());
        let statements = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::statement)
            .map(|stmt_pair| self.build_statement(stmt_pair))
            .collect::<Result<Vec<_>>>()?;
        let block = Block {
            statements,
            span: Some(span),
        };
        Ok(self.program.blocks.alloc(block))
    }

    fn build_parameter_list(&mut self, pair: Pair<Rule>) -> Result<Vec<ParamId>> {
        pair.into_inner().map(|p| self.build_parameter(p)).collect()
    }

    fn build_parameter(&mut self, pair: Pair<Rule>) -> Result<ParamId> {
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();
        let name = self.build_identifier(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("parameter name".to_string()),
                span,
            )
        })?)?;
        let ty = self.build_type(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("parameter type".to_string()),
                span,
            )
        })?)?;
        let param = Parameter {
            name,
            ty,
            resolved_type: None,
            span: Some(span),
        };
        Ok(self.program.params.alloc(param))
    }

    fn build_generic_param_list(&mut self, pair: Pair<Rule>) -> Result<Vec<GenericParamId>> {
        pair.into_inner()
            .map(|p| {
                let span = self.to_span(p.as_span());
                let name = self.build_identifier(p)?;
                let param = GenericParam {
                    name,
                    span: Some(span),
                };
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
        let span = self.to_span(pair.as_span());
        let mut inner = pair.into_inner();
        let key = self.build_identifier(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("key in pair".to_string()),
                span,
            )
        })?)?;
        let value = self.build_expression(inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("value in pair".to_string()),
                span,
            )
        })?)?;
        Ok(KeyValue {
            key,
            value,
            resolved_table: None,
            resolved_field: None,
            span: Some(span),
        })
    }

    fn build_decorators(&mut self, pairs: &mut Pairs<Rule>) -> Result<Vec<Decorator>> {
        let mut decorators = vec![];
        while let Some(p) = pairs.peek() {
            if p.as_rule() == Rule::decorator {
                let decorator_pair = pairs.next().unwrap();
                let span = self.to_span(decorator_pair.as_span());
                let name = self.build_identifier(decorator_pair.into_inner().next().unwrap())?;
                decorators.push(Decorator {
                    name,
                    span: Some(span),
                });
            } else {
                break;
            }
        }
        Ok(decorators)
    }

    fn build_identifier(&self, pair: Pair<Rule>) -> Result<Identifier> {
        let span = self.to_span(pair.as_span());
        if pair.as_rule() != Rule::identifier && pair.as_rule() != Rule::operator_symbol {
            return Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!(
                    "expected identifier or operator, found {:?}",
                    pair.as_rule()
                )),
                span,
            ));
        }
        Ok(Identifier {
            name: pair.as_str().to_string(),
            span: Some(span),
        })
    }
}

// ============================================================================
// --- Top-Level Parsing Function
// ============================================================================

/// Parses a source string into an `ast::Program`.
///
/// This is the main entry point for the parser. It handles the initial
/// Pest parsing and then hands off to the `AstBuilder` to construct the
//  AST.
pub fn parse_program(
    source: &str,
    filename: &'static str,
) -> std::result::Result<Program, Vec<CompilerError>> {
    match TransactParser::parse(Rule::program, source) {
        Ok(pairs) => {
            let mut builder = AstBuilder::new(filename);
            match builder.build(pairs) {
                Ok(program) => Ok(program),
                Err(e) => Err(vec![e]),
            }
        }
        Err(e) => {
            // Convert Pest error to our custom CompilerError
            let span = Span::new(0, 0, filename);
            Err(vec![CompilerError::new(
                FrontEndErrorKind::ParseError(e.to_string()),
                span,
            )])
        }
    }
}
