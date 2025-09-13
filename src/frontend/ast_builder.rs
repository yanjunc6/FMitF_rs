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
use pest::iterators::{Pair, Pairs};
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
    pub fn new(program: Option<Program>, filename: &'static str) -> Self {
        AstBuilder {
            program: program.unwrap_or_else(Program::default),
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

        // The next item depends on whether it's a function or operator
        let first_token = inner.next().ok_or_else(|| {
            CompilerError::new(
                FrontEndErrorKind::MissingField("callable name or operator".to_string()),
                span,
            )
        })?;

        let (kind, name) = match first_token.as_rule() {
            Rule::callable_keyword => {
                // Extract the keyword to determine the callable kind
                let keyword = first_token.as_str();
                let kind = match keyword {
                    "function" => CallableKind::Function,
                    "partition" => CallableKind::Partition,
                    "transaction" => CallableKind::Transaction,
                    _ => {
                        return Err(CompilerError::new(
                            FrontEndErrorKind::UnexpectedRule(format!(
                                "unknown callable keyword: {}",
                                keyword
                            )),
                            self.to_span(first_token.as_span()),
                        ))
                    }
                };

                // Get the identifier name
                let name_token = inner.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("callable name".to_string()),
                        span,
                    )
                })?;
                let name = self.build_identifier(name_token)?;
                (kind, name)
            }
            Rule::operator_symbol => {
                // For operators, we get the operator symbol directly
                let name = self.build_identifier(first_token)?;
                (CallableKind::Operator, name)
            }
            rule => {
                return Err(CompilerError::new(
                    FrontEndErrorKind::UnexpectedRule(format!(
                        "expected callable_keyword or operator_symbol, found {:?}",
                        rule
                    )),
                    self.to_span(first_token.as_span()),
                ));
            }
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
        // Note: The literal "type" keyword is consumed by the grammar but doesn't appear as a token
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
        // Note: The literal "const" keyword is consumed by the grammar and doesn't appear as a token
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

        // Note: The literal "table" keyword is consumed by the grammar and doesn't appear as a token
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
        // Handle the table_field wrapper
        let actual_element = if pair.as_rule() == Rule::table_field {
            pair.into_inner().next().unwrap()
        } else {
            pair
        };

        let span = self.to_span(actual_element.as_span());
        match actual_element.as_rule() {
            Rule::field_declaration => {
                // Check for "primary" keyword by looking at the source text before consuming
                let is_primary = actual_element.as_str().trim_start().starts_with("primary");

                let mut p_inner = actual_element.into_inner();

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
                let mut p_inner = actual_element.into_inner();
                // Note: The literal "node" keyword is consumed by the grammar and doesn't appear as a token
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
                let mut p_inner = actual_element.into_inner();
                // The "invariant" keyword is consumed by the grammar pattern
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
        let span = self.to_span(pair.as_span());
        let statement = match pair.as_rule() {
            Rule::var_declaration => {
                let var_id = self.build_var_decl_from_pair(pair)?;
                Statement::VarDecl(var_id)
            }
            Rule::if_statement => {
                let mut p = pair.into_inner();
                // The "if" keyword is consumed by the grammar pattern
                let condition = self.build_expression(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("if condition".to_string()),
                        span,
                    )
                })?)?;
                let then_block = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(FrontEndErrorKind::MissingField("if body".to_string()), span)
                })?)?;
                // If there's a third element, it's the else block (the "else" keyword is consumed by grammar)
                let else_block = if let Some(else_block_pair) = p.next() {
                    Some(self.build_block(else_block_pair)?)
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
                let mut p = pair.into_inner();
                // The "for" keyword is consumed by the grammar pattern

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
                let mut p = pair.into_inner();
                // The "return" keyword is consumed by the grammar pattern
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
                let mut p = pair.into_inner();
                // The "assert" keyword is consumed by the grammar pattern
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
                let mut p = pair.into_inner();
                // The grammar pattern is "decorator* ~ "hop" ~ block"
                // But in practice, we only get the block token
                let body = self.build_block(p.next().ok_or_else(|| {
                    CompilerError::new(
                        FrontEndErrorKind::MissingField("hop body".to_string()),
                        span,
                    )
                })?)?;
                Statement::Hop {
                    decorators: vec![], // No decorators in this case
                    body,
                    span: Some(span),
                }
            }
            Rule::hops_for_loop => {
                let mut p = pair.into_inner();

                // The keywords "hops", "for", and "to" are consumed by the grammar pattern
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
                    decorators: vec![], // No decorators since they're consumed by grammar
                    var,
                    start,
                    end,
                    body,
                    span: Some(span),
                }
            }
            Rule::block => {
                let block_id = self.build_block(pair)?;
                Statement::Block(block_id)
            }
            Rule::expression_statement => {
                let expr = self.build_expression(pair.into_inner().next().unwrap())?;
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
        let span = self.to_span(pair.as_span());
        match pair.as_rule() {
            Rule::var_declaration => {
                let var_id = self.build_var_decl_from_pair(pair)?;
                Ok(Some(ForInit::VarDecl(var_id)))
            }
            Rule::expression_statement => {
                let expr_id = self.build_expression(pair.into_inner().next().unwrap())?;
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

        // The "var" keyword is already consumed by the grammar
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
        self.build_expression_recursive(pair)
    }

    fn build_expression_recursive(&mut self, pair: Pair<Rule>) -> Result<ExprId> {
        let span = self.to_span(pair.as_span());

        // Follow the grammar hierarchy properly
        match pair.as_rule() {
            Rule::expression => {
                // expression has one inner item: assignment
                let inner = pair.into_inner().next().unwrap();
                self.build_expression_recursive(inner)
            }
            Rule::assignment => {
                // assignment = { level10_expr ~ ("=" ~ assignment)? }
                let mut pairs = pair.into_inner();
                let left = self.build_expression_recursive(pairs.next().unwrap())?;

                if let Some(rhs_pair) = pairs.next() {
                    // This is an assignment: left = right
                    let right = self.build_expression_recursive(rhs_pair)?;
                    let expr = Expression::Assignment {
                        lhs: left,
                        rhs: right,
                        resolved_type: None,
                        span: Some(span),
                    };
                    Ok(self.program.expressions.alloc(expr))
                } else {
                    // No assignment, just return the left expression
                    Ok(left)
                }
            }
            Rule::level10_expr => {
                // level10_expr = { level9_expr ~ (op10 ~ level9_expr)* }
                // op10 = @{ & ("$" | "|") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let mut left = self.build_expression_recursive(pairs.next().unwrap())?;

                while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(right_pair)?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    left = self.program.expressions.alloc(expr);
                }

                Ok(left)
            }
            Rule::level9_expr => {
                // level9_expr = { level8_expr ~ (op9 ~ level9_expr)? } (right-associative)
                // op9 = @{ & ("^") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let left = self.build_expression_recursive(pairs.next().unwrap())?;

                if let Some(op_pair) = pairs.next() {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(pairs.next().unwrap())?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    Ok(self.program.expressions.alloc(expr))
                } else {
                    Ok(left)
                }
            }
            Rule::level8_expr => {
                // level8_expr = { level7_expr ~ (op8 ~ level7_expr)* }
                // op8 = @{ & ("&") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let mut left = self.build_expression_recursive(pairs.next().unwrap())?;

                while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(right_pair)?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    left = self.program.expressions.alloc(expr);
                }

                Ok(left)
            }
            Rule::level7_expr => {
                // level7_expr = { level6_expr ~ (op7 ~ level7_expr)? } (right-associative)
                // op7 = @{ & ("~") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let left = self.build_expression_recursive(pairs.next().unwrap())?;

                if let Some(op_pair) = pairs.next() {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(pairs.next().unwrap())?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    Ok(self.program.expressions.alloc(expr))
                } else {
                    Ok(left)
                }
            }
            Rule::level6_expr => {
                // level6_expr = { level5_expr ~ (op6 ~ level5_expr)* }
                // op6 = @{ (&("<" | ">" | "!") ~ operator_symbol) | ("=" ~ operator_char+) }
                let mut pairs = pair.into_inner();
                let mut left = self.build_expression_recursive(pairs.next().unwrap())?;

                while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(right_pair)?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    left = self.program.expressions.alloc(expr);
                }

                Ok(left)
            }
            Rule::level5_expr => {
                // level5_expr = { level4_expr ~ (op5 ~ level4_expr)? } (right-associative)
                // op5 = @{ & (":") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let left = self.build_expression_recursive(pairs.next().unwrap())?;

                if let Some(op_pair) = pairs.next() {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(pairs.next().unwrap())?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    Ok(self.program.expressions.alloc(expr))
                } else {
                    Ok(left)
                }
            }
            Rule::level4_expr => {
                // level4_expr = { level3_expr ~ (op4 ~ level3_expr)* }
                // op4 = @{ & ("+" | "-") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let mut left = self.build_expression_recursive(pairs.next().unwrap())?;

                while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(right_pair)?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    left = self.program.expressions.alloc(expr);
                }

                Ok(left)
            }
            Rule::level3_expr => {
                // level3_expr = { prefix ~ (op3 ~ prefix)* }
                // op3 = @{ & ("*" | "/" | "%") ~ operator_symbol }
                let mut pairs = pair.into_inner();
                let mut left = self.build_expression_recursive(pairs.next().unwrap())?;

                while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                    let op_str = op_pair.as_str();
                    let op_span = self.to_span(op_pair.as_span());
                    let right = self.build_expression_recursive(right_pair)?;

                    let op_ident = Identifier {
                        name: op_str.to_string(),
                        span: Some(op_span),
                    };

                    let expr = Expression::Binary {
                        left,
                        op: op_ident,
                        right,
                        resolved_callable: None,
                        resolved_type: None,
                        span: Some(span),
                    };
                    left = self.program.expressions.alloc(expr);
                }

                Ok(left)
            }
            Rule::prefix => {
                // prefix = { (prefix_op ~ prefix) | postfix }
                let mut pairs = pair.into_inner();
                let first = pairs.next().unwrap();

                match first.as_rule() {
                    Rule::prefix_op => {
                        // This is a unary prefix operator
                        let op_str = first.as_str();
                        let op_span = self.to_span(first.as_span());
                        let operand = self.build_expression_recursive(pairs.next().unwrap())?;

                        let op_ident = Identifier {
                            name: op_str.to_string(),
                            span: Some(op_span),
                        };

                        let expr = Expression::Unary {
                            op: op_ident,
                            expr: operand,
                            resolved_callable: None,
                            resolved_type: None,
                            span: Some(span),
                        };
                        Ok(self.program.expressions.alloc(expr))
                    }
                    _ => {
                        // No prefix operator, just pass through the postfix
                        self.build_expression_recursive(first)
                    }
                }
            }
            Rule::postfix => {
                // postfix = { primary ~ (call | member_access | table_row_access)* }
                let mut pairs = pair.into_inner();
                let mut expr = self.build_expression_recursive(pairs.next().unwrap())?;

                for postfix_op in pairs {
                    match postfix_op.as_rule() {
                        Rule::call => {
                            // call = { "(" ~ expression_list? ~ ")" }
                            let mut call_pairs = postfix_op.into_inner();
                            let args = if let Some(expr_list) = call_pairs.next() {
                                self.build_expression_list(expr_list)?
                            } else {
                                Vec::new()
                            };

                            let call_expr = Expression::Call {
                                callee: expr,
                                args,
                                resolved_callable: None,
                                resolved_type: None,
                                span: Some(span),
                            };
                            expr = self.program.expressions.alloc(call_expr);
                        }
                        Rule::member_access => {
                            // member_access = { "." ~ identifier }
                            let member_name =
                                self.build_identifier(postfix_op.into_inner().next().unwrap())?;

                            let member_expr = Expression::MemberAccess {
                                object: expr,
                                member: member_name,
                                resolved_table: None,
                                resolved_field: None,
                                resolved_type: None,
                                span: Some(span),
                            };
                            expr = self.program.expressions.alloc(member_expr);
                        }
                        Rule::table_row_access => {
                            // table_row_access = { "[" ~ (key_value_pair ~ ("," ~ key_value_pair)*)? ~ "]" }
                            let mut key_values = Vec::new();
                            for kv_pair in postfix_op.into_inner() {
                                key_values.push(self.build_key_value_pair(kv_pair)?);
                            }

                            let table_access_expr = Expression::TableRowAccess {
                                table: expr,
                                key_values,
                                resolved_table: None,
                                resolved_type: None,
                                span: Some(span),
                            };
                            expr = self.program.expressions.alloc(table_access_expr);
                        }
                        _ => {
                            return Err(CompilerError::new(
                                FrontEndErrorKind::UnexpectedRule(format!(
                                    "in postfix: {:?}",
                                    postfix_op.as_rule()
                                )),
                                self.to_span(postfix_op.as_span()),
                            ));
                        }
                    }
                }

                Ok(expr)
            }
            Rule::primary => self.build_primary_expr(pair),
            Rule::identifier => {
                let ident = self.build_identifier(pair)?;
                let expr = Expression::Identifier {
                    name: ident,
                    resolved_declaration: None,
                    resolved_type: None,
                    span: Some(span),
                };
                Ok(self.program.expressions.alloc(expr))
            }
            rule => Err(CompilerError::new(
                FrontEndErrorKind::UnexpectedRule(format!("in expression: {:?}", rule)),
                span,
            )),
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
    program: Option<Program>,
    source: &str,
    filename: &'static str,
) -> std::result::Result<Program, Vec<CompilerError>> {
    match TransactParser::parse(Rule::program, source) {
        Ok(pairs) => {
            let mut builder = AstBuilder::new(program, filename);
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
