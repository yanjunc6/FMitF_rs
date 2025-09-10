//! AST Builder
//!
//! This module provides the `AstBuilder` struct which converts Pest parse trees
//! into the AST defined in `mod.rs`. It handles:
//! - Converting Pest pairs to AST nodes
//! - Creating proper arena-based storage for all nodes
//! - Maintaining span information for error reporting
//! - Building the complete program structure

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

use crate::ast::errors::*;
use crate::ast::*;
use crate::util::Span;

// ============================================================================
// --- Pest Parser Definition
// ============================================================================

#[derive(Parser)]
#[grammar = "ast/grammar.pest"]
pub struct TransactParser;

// ============================================================================
// --- AST Builder
// ============================================================================

pub struct AstBuilder {
    pub program: Program,
    errors: Vec<AstError>,
}

impl AstBuilder {
    pub fn new() -> Self {
        AstBuilder {
            program: Program::default(),
            errors: Vec::new(),
        }
    }

    pub fn new_with_prelude() -> Result<Self, AstError> {
        let program = Program::with_prelude()
            .map_err(|e| AstError {
                kind: AstErrorKind::ParseError(format!("Failed to load prelude: {}", e)),
                span: None,
            })?;
        
        Ok(AstBuilder {
            program,
            errors: Vec::new(),
        })
    }

    /// Parse without loading prelude (used internally for parsing prelude.transact)
    pub fn parse_raw(source: &str) -> Result<Program, Vec<AstError>> {
        let mut builder = AstBuilder::new();

        // Parse with Pest
        let pairs = match TransactParser::parse(Rule::program, source) {
            Ok(pairs) => pairs,
            Err(e) => {
                return Err(vec![AstError {
                    kind: AstErrorKind::ParseError(e.to_string()),
                    span: None,
                }]);
            }
        };

        // Build AST
        for pair in pairs {
            if pair.as_rule() == Rule::program {
                builder.build_program(pair)?;
            }
        }

        Ok(builder.program)
    }

    pub fn parse(source: &str) -> Result<Program, Vec<AstError>> {
        // Start with program that has prelude loaded
        let mut builder = match AstBuilder::new_with_prelude() {
            Ok(builder) => builder,
            Err(e) => return Err(vec![e]),
        };

        // Parse user code with Pest
        let pairs = match TransactParser::parse(Rule::program, source) {
            Ok(pairs) => pairs,
            Err(e) => {
                return Err(vec![AstError {
                    kind: AstErrorKind::ParseError(e.to_string()),
                    span: None,
                }]);
            }
        };

        // Build user AST into the program that already has prelude
        for pair in pairs {
            if pair.as_rule() == Rule::program {
                builder.build_program(pair)?;
            }
        }

        if !builder.errors.is_empty() {
            return Err(builder.errors);
        }

        Ok(builder.program)
    }

    fn build_program(&mut self, pair: Pair<Rule>) -> Result<(), Vec<AstError>> {
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::callable_declaration => {
                    let id = self.build_callable_declaration(inner)?;
                    self.program.declarations.push(Declaration::Callable(id));
                }
                Rule::type_declaration => {
                    let id = self.build_type_declaration(inner)?;
                    self.program.declarations.push(Declaration::Type(id));
                }
                Rule::const_declaration => {
                    let id = self.build_const_declaration(inner)?;
                    self.program.declarations.push(Declaration::Const(id));
                }
                Rule::table_declaration => {
                    let id = self.build_table_declaration(inner)?;
                    self.program.declarations.push(Declaration::Table(id));
                }
                Rule::EOI => {}
                _ => {
                    self.errors.push(AstError {
                        kind: AstErrorKind::UnexpectedRule(format!("{:?}", inner.as_rule())),
                        span: Some(self.pair_to_span(&inner)),
                    });
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(())
    }

    fn build_callable_declaration(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<FunctionId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut decorators = Vec::new();
        let mut kind = CallableKind::Function;
        let mut name = None;
        let mut generic_params = Vec::new();
        let mut params = Vec::new();
        let mut return_type = None;
        let mut assumptions = Vec::new();
        let mut body = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::decorator => {
                    decorators.push(self.build_decorator(inner)?);
                }
                Rule::identifier => {
                    name = Some(CallableName::Identifier(self.build_identifier(inner)?));
                }
                Rule::operator_symbol => {
                    kind = CallableKind::Operator;
                    name = Some(CallableName::Operator(Spanned {
                        value: inner.as_str().to_string(),
                        span: Some(self.pair_to_span(&inner)),
                    }));
                }
                Rule::generic_param_list => {
                    generic_params = self.build_generic_param_list(inner)?;
                }
                Rule::parameter_list => {
                    params = self.build_parameter_list(inner)?;
                }
                Rule::r#type => {
                    return_type = Some(self.build_type(inner)?);
                }
                Rule::assume_statement => {
                    assumptions.push(self.build_assume_statement(inner)?);
                }
                Rule::block => {
                    body = Some(self.build_block(inner)?);
                }
                _ => {
                    // Check for callable kind keywords
                    let keyword = inner.as_str();
                    match keyword {
                        "function" => kind = CallableKind::Function,
                        "partition" => kind = CallableKind::Partition,
                        "transaction" => kind = CallableKind::Transaction,
                        "operator" => kind = CallableKind::Operator,
                        _ => {}
                    }
                }
            }
        }

        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("callable name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.functions.alloc(CallableDecl {
            decorators,
            kind,
            name,
            generic_params,
            params,
            return_type,
            assumptions,
            body,
            span: Some(span),
        }))
    }

    fn build_type_declaration(&mut self, pair: Pair<Rule>) -> Result<TypeDeclId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut decorators = Vec::new();
        let mut name = None;
        let mut generic_params = Vec::new();

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::decorator => {
                    decorators.push(self.build_decorator(inner)?);
                }
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::generic_param_list => {
                    generic_params = self.build_generic_param_list(inner)?;
                }
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("type name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.type_decls.alloc(TypeDecl {
            decorators,
            name,
            generic_params,
            span: Some(span),
        }))
    }

    fn build_const_declaration(&mut self, pair: Pair<Rule>) -> Result<ConstId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut name = None;
        let mut ty = None;
        let mut value = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::r#type => {
                    ty = Some(self.build_type(inner)?);
                }
                Rule::expression => {
                    value = Some(self.build_expression(inner)?);
                }
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("const name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let ty = ty.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("const type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let value = value.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("const value".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.const_decls.alloc(ConstDecl {
            name,
            ty,
            value,
            span: Some(span),
        }))
    }

    fn build_table_declaration(&mut self, pair: Pair<Rule>) -> Result<TableId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut name = None;
        let mut elements = Vec::new();

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::table_field => {
                    elements.push(self.build_table_field(inner)?);
                }
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("table name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.table_decls.alloc(TableDecl {
            name,
            elements,
            span: Some(span),
        }))
    }

    fn build_table_field(&mut self, pair: Pair<Rule>) -> Result<TableElement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::field_declaration => {
                    return self.build_field_declaration(inner);
                }
                Rule::node_declaration => {
                    return self.build_node_declaration(inner);
                }
                Rule::invariant_declaration => {
                    return self.build_invariant_declaration(inner);
                }
                _ => {}
            }
        }
        
        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Empty table field".to_string()),
            span: Some(span),
        }])
    }

    fn build_field_declaration(&mut self, pair: Pair<Rule>) -> Result<TableElement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut is_primary = false;
        let mut name = None;
        let mut ty = None;
        
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::r#type => {
                    ty = Some(self.build_type(inner)?);
                }
                _ => {
                    // Check for "primary" literal
                    if inner.as_str() == "primary" {
                        is_primary = true;
                    }
                }
            }
        }
        
        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("field name".to_string()),
                span: Some(span.clone()),
            }]
        })?;
        
        let ty = ty.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("field type".to_string()),
                span: Some(span.clone()),
            }]
        })?;
        
        Ok(TableElement::Field(TableField {
            is_primary,
            name,
            ty,
            span: Some(span),
        }))
    }

    fn build_node_declaration(&mut self, pair: Pair<Rule>) -> Result<TableElement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut name = None;
        let mut args = Vec::new();
        
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::expression_list => {
                    args = self.build_expression_list(inner)?;
                }
                _ => {}
            }
        }
        
        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("node name".to_string()),
                span: Some(span.clone()),
            }]
        })?;
        
        Ok(TableElement::Node(TableNode {
            name,
            args,
            resolved_partition: None,
            span: Some(span),
        }))
    }

    fn build_invariant_declaration(&mut self, pair: Pair<Rule>) -> Result<TableElement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        
        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression {
                let expr = self.build_expression(inner)?;
                return Ok(TableElement::Invariant(expr));
            }
        }
        
        Err(vec![AstError {
            kind: AstErrorKind::MissingField("invariant expression".to_string()),
            span: Some(span),
        }])
    }

    fn build_decorator(&mut self, pair: Pair<Rule>) -> Result<Decorator, Vec<AstError>> {
        let span = self.pair_to_span(&pair);

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::identifier {
                return Ok(Decorator {
                    name: self.build_identifier(inner)?,
                    span: Some(span),
                });
            }
        }

        Err(vec![AstError {
            kind: AstErrorKind::MissingField("decorator name".to_string()),
            span: Some(span),
        }])
    }

    fn build_identifier(&mut self, pair: Pair<Rule>) -> Result<Identifier, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let name = pair.as_str().to_string();

        Ok(Identifier {
            name,
            span: Some(span),
            resolved: None,
            resolved_type: None,
        })
    }

    fn build_generic_param_list(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<Vec<GenericParamId>, Vec<AstError>> {
        let mut params = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::identifier {
                let param = GenericParam {
                    name: self.build_identifier(inner)?,
                    span: None,
                };
                params.push(self.program.generic_params.alloc(param));
            }
        }

        Ok(params)
    }

    fn build_parameter_list(&mut self, pair: Pair<Rule>) -> Result<Vec<ParamId>, Vec<AstError>> {
        let mut params = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::parameter {
                params.push(self.build_parameter(inner)?);
            }
        }

        Ok(params)
    }

    fn build_parameter(&mut self, pair: Pair<Rule>) -> Result<ParamId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut name = None;
        let mut ty = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::r#type => {
                    ty = Some(self.build_type(inner)?);
                }
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("parameter name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let ty = ty.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("parameter type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.params.alloc(Parameter {
            name,
            ty,
            span: Some(span),
        }))
    }

    fn build_type(&mut self, pair: Pair<Rule>) -> Result<TypeId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::primitive_type => {
                    let ident = self.build_identifier(inner.into_inner().next().unwrap())?;
                    return Ok(self.program.types.alloc(Type::Named(ident)));
                }
                Rule::generic_type => {
                    return self.build_generic_type(inner);
                }
                Rule::function_type => {
                    return self.build_function_type(inner);
                }
                _ => {}
            }
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule(format!("Invalid type")),
            span: Some(span),
        }])
    }

    fn build_generic_type(&mut self, pair: Pair<Rule>) -> Result<TypeId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut base = None;
        let mut args = Vec::new();

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    base = Some(self.build_identifier(inner)?);
                }
                Rule::type_list => {
                    args = self.build_type_list(inner)?;
                }
                _ => {}
            }
        }

        let base = base.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("generic base type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.types.alloc(Type::Generic {
            base,
            args,
            span: Some(span),
        }))
    }

    fn build_function_type(&mut self, pair: Pair<Rule>) -> Result<TypeId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut params = Vec::new();
        let mut return_type = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::type_list => {
                    params = self.build_type_list(inner)?;
                }
                Rule::r#type => {
                    return_type = Some(self.build_type(inner)?);
                }
                _ => {}
            }
        }

        let return_type = return_type.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("function return type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.types.alloc(Type::Function {
            params,
            return_type,
            span: Some(span),
        }))
    }

    fn build_type_list(&mut self, pair: Pair<Rule>) -> Result<Vec<TypeId>, Vec<AstError>> {
        let mut types = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::r#type {
                types.push(self.build_type(inner)?);
            }
        }

        Ok(types)
    }

    fn build_assume_statement(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression {
                return self.build_expression(inner);
            }
        }

        Err(vec![AstError {
            kind: AstErrorKind::MissingField("assume expression".to_string()),
            span: Some(span),
        }])
    }

    fn build_block(&mut self, pair: Pair<Rule>) -> Result<BlockId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut statements = Vec::new();

        for inner in pair.into_inner() {
            statements.push(self.build_statement(inner)?);
        }

        Ok(self.program.blocks.alloc(Block {
            statements,
            span: Some(span),
        }))
    }

    fn build_statement(&mut self, pair: Pair<Rule>) -> Result<StmtId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);

        // Since statement is a silent rule in the grammar, we get the actual rule directly
        let stmt = match pair.as_rule() {
            Rule::var_declaration => {
                let var_id = self.build_var_declaration(pair)?;
                Statement::VarDecl(var_id)
            }
            Rule::if_statement => self.build_if_statement(pair)?,
            Rule::for_statement => self.build_for_statement(pair)?,
            Rule::return_statement => self.build_return_statement(pair)?,
            Rule::assert_statement => self.build_assert_statement(pair)?,
            Rule::hop_block => self.build_hop_block(pair)?,
            Rule::hops_for_loop => self.build_hops_for_loop(pair)?,
            Rule::block => {
                let block_id = self.build_block(pair)?;
                Statement::Block(block_id)
            }
            Rule::expression_statement => {
                let expr = self.build_expression_statement(pair)?;
                Statement::Expression {
                    expr,
                    span: Some(span),
                }
            }
            _ => {
                return Err(vec![AstError {
                    kind: AstErrorKind::UnexpectedRule(format!(
                        "Invalid statement: {:?}",
                        pair.as_rule()
                    )),
                    span: Some(span),
                }]);
            }
        };

        Ok(self.program.statements.alloc(stmt))
    }

    fn build_var_declaration(&mut self, pair: Pair<Rule>) -> Result<VarId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut name = None;
        let mut ty = None;
        let mut init = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    name = Some(self.build_identifier(inner)?);
                }
                Rule::r#type => {
                    ty = Some(self.build_type(inner)?);
                }
                Rule::expression => {
                    init = Some(self.build_expression(inner)?);
                }
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("variable name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.var_decls.alloc(VarDecl {
            name,
            ty,
            init,
            span: Some(span),
        }))
    }

    fn build_if_statement(&mut self, pair: Pair<Rule>) -> Result<Statement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut condition = None;
        let mut then_block = None;
        let mut else_block = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::expression => {
                    condition = Some(self.build_expression(inner)?);
                }
                Rule::block => {
                    if then_block.is_none() {
                        then_block = Some(self.build_block(inner)?);
                    } else {
                        else_block = Some(self.build_block(inner)?);
                    }
                }
                _ => {}
            }
        }

        let condition = condition.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("if condition".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let then_block = then_block.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("if then block".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(Statement::If {
            condition,
            then_block,
            else_block,
            span: Some(span),
        })
    }

    fn build_for_statement(&mut self, pair: Pair<Rule>) -> Result<Statement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut init = None;
        let mut condition = None;
        let mut update = None;
        let mut body = None;

        let mut inner_iter = pair.into_inner();

        // Parse init (var_declaration, expression_statement, or empty)
        if let Some(inner) = inner_iter.next() {
            match inner.as_rule() {
                Rule::var_declaration => {
                    let var_id = self.build_var_declaration(inner)?;
                    init = Some(ForInit::VarDecl(var_id));
                }
                Rule::expression_statement => {
                    let expr = self.build_expression_statement(inner)?;
                    init = Some(ForInit::Expression(expr));
                }
                _ => {}
            }
        }

        // Parse condition (expression or empty)
        if let Some(inner) = inner_iter.next() {
            if inner.as_rule() == Rule::expression {
                condition = Some(self.build_expression(inner)?);
            }
        }

        // Parse update (expression or empty)
        if let Some(inner) = inner_iter.next() {
            if inner.as_rule() == Rule::expression {
                update = Some(self.build_expression(inner)?);
            }
        }

        // Parse body (block)
        if let Some(inner) = inner_iter.next() {
            if inner.as_rule() == Rule::block {
                body = Some(self.build_block(inner)?);
            }
        }

        let body = body.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("for loop body".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(Statement::For {
            init,
            condition,
            update,
            body,
            span: Some(span),
        })
    }

    fn build_return_statement(&mut self, pair: Pair<Rule>) -> Result<Statement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut value = None;

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression {
                value = Some(self.build_expression(inner)?);
            }
        }

        Ok(Statement::Return {
            value,
            span: Some(span),
        })
    }

    fn build_assert_statement(&mut self, pair: Pair<Rule>) -> Result<Statement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut expr = None;

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression {
                expr = Some(self.build_expression(inner)?);
            }
        }

        let expr = expr.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("assert expression".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(Statement::Assert {
            expr,
            span: Some(span),
        })
    }

    fn build_hop_block(&mut self, pair: Pair<Rule>) -> Result<Statement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut decorators = Vec::new();
        let mut body = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::decorator => {
                    decorators.push(self.build_decorator(inner)?);
                }
                Rule::block => {
                    body = Some(self.build_block(inner)?);
                }
                _ => {}
            }
        }

        let body = body.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("hop body".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(Statement::Hop {
            decorators,
            body,
            span: Some(span),
        })
    }

    fn build_hops_for_loop(&mut self, pair: Pair<Rule>) -> Result<Statement, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut decorators = Vec::new();
        let mut var_name = None;
        let mut var_type = None;
        let mut start = None;
        let mut end = None;
        let mut body = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::decorator => {
                    decorators.push(self.build_decorator(inner)?);
                }
                Rule::identifier => {
                    if var_name.is_none() {
                        var_name = Some(self.build_identifier(inner)?);
                    }
                }
                Rule::r#type => {
                    var_type = Some(self.build_type(inner)?);
                }
                Rule::expression => {
                    if start.is_none() {
                        start = Some(self.build_expression(inner)?);
                    } else if end.is_none() {
                        end = Some(self.build_expression(inner)?);
                    }
                }
                Rule::block => {
                    body = Some(self.build_block(inner)?);
                }
                _ => {}
            }
        }

        let var_name = var_name.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("hops for variable name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let var_type = var_type.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("hops for variable type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let start = start.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("hops for start expression".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let end = end.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("hops for end expression".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let body = body.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("hops for body".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        // Create the loop variable declaration
        let var_decl = VarDecl {
            name: var_name,
            ty: Some(var_type),
            init: None,
            span: Some(span.clone()),
        };
        let var_id = self.program.var_decls.alloc(var_decl);

        Ok(Statement::HopsFor {
            decorators,
            var: var_id,
            start,
            end,
            body,
            span: Some(span),
        })
    }

    fn build_expression_statement(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression {
                return self.build_expression(inner);
            }
        }

        Err(vec![AstError {
            kind: AstErrorKind::MissingField("expression in statement".to_string()),
            span: Some(span),
        }])
    }

    fn build_expression(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);

        // Extract the assignment rule from the expression rule
        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::assignment {
                return self.build_assignment(inner);
            }
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Invalid expression structure".to_string()),
            span: Some(span),
        }])
    }

    fn build_assignment(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();
        
        match inner_pairs.len() {
            1 => {
                // No assignment, just the expression
                self.build_level10_expr(inner_pairs.into_iter().next().unwrap())
            }
            2 => {
                // Assignment: level10_expr and ("=" ~ assignment) group
                let lhs = self.build_level10_expr(inner_pairs[0].clone())?;
                
                // The second element should be the assignment part
                let rhs = self.build_assignment(inner_pairs[1].clone())?; // Right-associative
                
                Ok(self.program.expressions.alloc(Expression::Assignment {
                    lhs,
                    rhs,
                    span: Some(span),
                }))
            }
            _ => {
                Err(vec![AstError {
                    kind: AstErrorKind::UnexpectedRule(format!(
                        "Invalid assignment structure: expected 1 or 2 elements, got {}",
                        inner_pairs.len()
                    )),
                    span: Some(span),
                }])
            }
        }
    }

    fn build_level10_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // $ and | operators - left associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level9_expr(inner_pairs.into_iter().next().unwrap());
        }

        // Build left-associative binary expression
        let mut expr = self.build_level9_expr(inner_pairs[0].clone())?;

        let mut i = 1;
        while i + 1 < inner_pairs.len() {
            let op_str = inner_pairs[i].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[i])),
            };
            let right = self.build_level9_expr(inner_pairs[i + 1].clone())?;

            expr = self.program.expressions.alloc(Expression::Binary {
                left: expr,
                op,
                right,
                span: Some(span.clone()),
            });

            i += 2;
        }

        Ok(expr)
    }

    fn build_level9_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // ^ operators - right associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level8_expr(inner_pairs.into_iter().next().unwrap());
        }

        if inner_pairs.len() == 3 {
            let left = self.build_level8_expr(inner_pairs[0].clone())?;
            let op_str = inner_pairs[1].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[1])),
            };
            let right = self.build_level9_expr(inner_pairs[2].clone())?;

            return Ok(self.program.expressions.alloc(Expression::Binary {
                left,
                op,
                right,
                span: Some(span),
            }));
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Invalid level9 expression structure".to_string()),
            span: Some(span),
        }])
    }

    fn build_level8_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // & operators - left associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level7_expr(inner_pairs.into_iter().next().unwrap());
        }

        // Build left-associative binary expression
        let mut expr = self.build_level7_expr(inner_pairs[0].clone())?;

        let mut i = 1;
        while i + 1 < inner_pairs.len() {
            let op_str = inner_pairs[i].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[i])),
            };
            let right = self.build_level7_expr(inner_pairs[i + 1].clone())?;

            expr = self.program.expressions.alloc(Expression::Binary {
                left: expr,
                op,
                right,
                span: Some(span.clone()),
            });

            i += 2;
        }

        Ok(expr)
    }

    fn build_level7_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // @ and ~ operators - right associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level6_expr(inner_pairs.into_iter().next().unwrap());
        }

        if inner_pairs.len() == 3 {
            let left = self.build_level6_expr(inner_pairs[0].clone())?;
            let op_str = inner_pairs[1].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[1])),
            };
            let right = self.build_level7_expr(inner_pairs[2].clone())?;

            return Ok(self.program.expressions.alloc(Expression::Binary {
                left,
                op,
                right,
                span: Some(span),
            }));
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Invalid level7 expression structure".to_string()),
            span: Some(span),
        }])
    }

    fn build_level6_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // <, >, =, ! operators - left associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level5_expr(inner_pairs.into_iter().next().unwrap());
        }

        // Build left-associative binary expression
        let mut expr = self.build_level5_expr(inner_pairs[0].clone())?;

        let mut i = 1;
        while i + 1 < inner_pairs.len() {
            let op_str = inner_pairs[i].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[i])),
            };
            let right = self.build_level5_expr(inner_pairs[i + 1].clone())?;

            expr = self.program.expressions.alloc(Expression::Binary {
                left: expr,
                op,
                right,
                span: Some(span.clone()),
            });

            i += 2;
        }

        Ok(expr)
    }

    fn build_level5_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // : operators - right associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level4_expr(inner_pairs.into_iter().next().unwrap());
        }

        if inner_pairs.len() == 3 {
            let left = self.build_level4_expr(inner_pairs[0].clone())?;
            let op_str = inner_pairs[1].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[1])),
            };
            let right = self.build_level5_expr(inner_pairs[2].clone())?;

            return Ok(self.program.expressions.alloc(Expression::Binary {
                left,
                op,
                right,
                span: Some(span),
            }));
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Invalid level5 expression structure".to_string()),
            span: Some(span),
        }])
    }

    fn build_level4_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // + and - operators - left associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_level3_expr(inner_pairs.into_iter().next().unwrap());
        }

        // Build left-associative binary expression
        let mut expr = self.build_level3_expr(inner_pairs[0].clone())?;

        let mut i = 1;
        while i + 1 < inner_pairs.len() {
            let op_str = inner_pairs[i].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[i])),
            };
            let right = self.build_level3_expr(inner_pairs[i + 1].clone())?;

            expr = self.program.expressions.alloc(Expression::Binary {
                left: expr,
                op,
                right,
                span: Some(span.clone()),
            });

            i += 2;
        }

        Ok(expr)
    }

    fn build_level3_expr(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        // *, /, % operators - left associative
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            return self.build_prefix(inner_pairs.into_iter().next().unwrap());
        }

        // Build left-associative binary expression
        let mut expr = self.build_prefix(inner_pairs[0].clone())?;

        let mut i = 1;
        while i + 1 < inner_pairs.len() {
            let op_str = inner_pairs[i].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[i])),
            };
            let right = self.build_prefix(inner_pairs[i + 1].clone())?;

            expr = self.program.expressions.alloc(Expression::Binary {
                left: expr,
                op,
                right,
                span: Some(span.clone()),
            });

            i += 2;
        }

        Ok(expr)
    }

    fn build_prefix(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.len() == 1 {
            // No prefix operator
            return self.build_postfix(inner_pairs.into_iter().next().unwrap());
        }

        if inner_pairs.len() == 2 {
            // Prefix operator
            let op_str = inner_pairs[0].as_str().to_string();
            let op = Spanned {
                value: op_str,
                span: Some(self.pair_to_span(&inner_pairs[0])),
            };
            let expr = self.build_prefix(inner_pairs[1].clone())?; // Right-associative

            return Ok(self.program.expressions.alloc(Expression::Unary {
                op,
                expr,
                span: Some(span),
            }));
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Invalid prefix expression structure".to_string()),
            span: Some(span),
        }])
    }

    fn build_postfix(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let inner_pairs: Vec<_> = pair.into_inner().collect();

        if inner_pairs.is_empty() {
            return Err(vec![AstError {
                kind: AstErrorKind::UnexpectedRule("Empty postfix expression".to_string()),
                span: Some(span),
            }]);
        }

        // Start with the primary expression
        let mut expr = self.build_primary(inner_pairs[0].clone())?;

        // Apply postfix operations left to right
        for postfix_pair in inner_pairs.iter().skip(1) {
            match postfix_pair.as_rule() {
                Rule::call => {
                    expr = self.build_call(expr, postfix_pair.clone())?;
                }
                Rule::member_access => {
                    expr = self.build_member_access(expr, postfix_pair.clone())?;
                }
                Rule::table_row_access => {
                    expr = self.build_table_row_access(expr, postfix_pair.clone())?;
                }
                _ => {
                    return Err(vec![AstError {
                        kind: AstErrorKind::UnexpectedRule(format!(
                            "Unexpected postfix rule: {:?}",
                            postfix_pair.as_rule()
                        )),
                        span: Some(self.pair_to_span(postfix_pair)),
                    }]);
                }
            }
        }

        Ok(expr)
    }

    fn build_call(&mut self, callee: ExprId, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut args = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression_list {
                args = self.build_expression_list(inner)?;
                break;
            }
        }

        Ok(self.program.expressions.alloc(Expression::Call {
            callee,
            args,
            resolved_callable: None,
            span: Some(span),
        }))
    }

    fn build_member_access(
        &mut self,
        object: ExprId,
        pair: Pair<Rule>,
    ) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut member = None;

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::identifier {
                member = Some(self.build_identifier(inner)?);
                break;
            }
        }

        let member = member.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("member name".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.expressions.alloc(Expression::MemberAccess {
            object,
            member,
            resolved_field: None,
            span: Some(span),
        }))
    }

    fn build_table_row_access(
        &mut self,
        table: ExprId,
        pair: Pair<Rule>,
    ) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut key_values = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::key_value_pair {
                key_values.push(self.build_key_value_pair(inner)?);
            }
        }

        Ok(self.program.expressions.alloc(Expression::TableRowAccess {
            table,
            key_values,
            span: Some(span),
        }))
    }

    fn build_primary(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::literal => {
                    return self.build_literal(inner);
                }
                Rule::lambda_expression => {
                    return self.build_lambda_expression(inner);
                }
                Rule::identifier => {
                    let identifier = self.build_identifier(inner)?;
                    return Ok(self
                        .program
                        .expressions
                        .alloc(Expression::Identifier(identifier)));
                }
                Rule::expression => {
                    // Grouped expression
                    let expr = self.build_expression(inner)?;
                    return Ok(self.program.expressions.alloc(Expression::Grouped {
                        expr,
                        span: Some(span),
                    }));
                }
                _ => {}
            }
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Invalid primary expression".to_string()),
            span: Some(span),
        }])
    }

    fn build_literal(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);

        for inner in pair.into_inner() {
            let literal = match inner.as_rule() {
                Rule::integer => Literal::Integer(inner.as_str().to_string()),
                Rule::float => Literal::Float(inner.as_str().to_string()),
                Rule::string => {
                    // Remove quotes and handle escape sequences
                    let s = inner.as_str();
                    let unquoted = &s[1..s.len() - 1]; // Remove outer quotes
                    Literal::String(unquoted.to_string())
                }
                Rule::bool => Literal::Bool(inner.as_str() == "true"),
                Rule::list_literal => {
                    let mut elements = Vec::new();
                    for list_inner in inner.into_inner() {
                        if list_inner.as_rule() == Rule::expression_list {
                            elements = self.build_expression_list(list_inner)?;
                            break;
                        }
                    }
                    Literal::List(elements)
                }
                Rule::row_literal => {
                    let mut key_values = Vec::new();
                    for row_inner in inner.into_inner() {
                        if row_inner.as_rule() == Rule::key_value_pair {
                            key_values.push(self.build_key_value_pair(row_inner)?);
                        }
                    }
                    Literal::RowLiteral(key_values)
                }
                _ => {
                    return Err(vec![AstError {
                        kind: AstErrorKind::UnexpectedRule(format!(
                            "Unexpected literal rule: {:?}",
                            inner.as_rule()
                        )),
                        span: Some(self.pair_to_span(&inner)),
                    }]);
                }
            };

            return Ok(self.program.expressions.alloc(Expression::Literal {
                value: literal,
                span: Some(span),
            }));
        }

        Err(vec![AstError {
            kind: AstErrorKind::UnexpectedRule("Empty literal".to_string()),
            span: Some(span),
        }])
    }

    fn build_lambda_expression(&mut self, pair: Pair<Rule>) -> Result<ExprId, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut params = Vec::new();
        let mut return_type = None;
        let mut body = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::parameter_list => {
                    params = self.build_parameter_list(inner)?;
                }
                Rule::r#type => {
                    return_type = Some(self.build_type(inner)?);
                }
                Rule::block => {
                    body = Some(self.build_block(inner)?);
                }
                _ => {}
            }
        }

        let return_type = return_type.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("lambda return type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let body = body.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("lambda body".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(self.program.expressions.alloc(Expression::Lambda {
            params,
            return_type,
            body,
            span: Some(span),
        }))
    }

    fn build_expression_list(&mut self, pair: Pair<Rule>) -> Result<Vec<ExprId>, Vec<AstError>> {
        let mut expressions = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::expression {
                expressions.push(self.build_expression(inner)?);
            }
        }

        Ok(expressions)
    }

    fn build_key_value_pair(&mut self, pair: Pair<Rule>) -> Result<KeyValue, Vec<AstError>> {
        let span = self.pair_to_span(&pair);
        let mut key = None;
        let mut value = None;

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::identifier => {
                    key = Some(self.build_identifier(inner)?);
                }
                Rule::expression => {
                    value = Some(self.build_expression(inner)?);
                }
                _ => {}
            }
        }

        let key = key.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("key in key-value pair".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let value = value.ok_or_else(|| {
            vec![AstError {
                kind: AstErrorKind::MissingField("value in key-value pair".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(KeyValue {
            key,
            value,
            resolved_field: None,
            span: Some(span),
        })
    }

    // Helper method to convert Pest Pair to Span
    fn pair_to_span(&self, pair: &Pair<Rule>) -> Span {
        let pest_span = pair.as_span();
        let (line, column) = pest_span.start_pos().line_col();
        Span {
            start: pest_span.start(),
            end: pest_span.end(),
            line,
            column,
        }
    }
}

// ============================================================================
// --- Parse Function
// ============================================================================

pub fn parse_program(source: &str) -> Result<Program, Vec<AstError>> {
    AstBuilder::parse(source)
}
