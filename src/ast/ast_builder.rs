use id_arena::Arena;
use pest::iterators::Pair;
use pest::Parser;
use std::collections::HashMap;

use crate::ast::errors::*;
use crate::ast::*;

// Add the grammar rule enum
#[derive(pest_derive::Parser)]
#[grammar = "ast/grammar.pest"]
pub struct TransActParser;

/// Builds arena-based AST from parsed Pest pairs
pub struct AstBuilder {
    program: Program,
}

impl Program {
    pub fn new() -> Self {
        Program {
            nodes: Arena::new(),
            node_map: HashMap::new(),
            root_nodes: Vec::new(),
            tables: Arena::new(),
            table_map: HashMap::new(),
            root_tables: Vec::new(),
            fields: Arena::new(),
            functions: Arena::new(),
            function_map: HashMap::new(),
            root_functions: Vec::new(),
            hops: Arena::new(),
            parameters: Arena::new(),
            statements: Arena::new(),
            expressions: Arena::new(),
            variables: Arena::new(),
            scopes: Arena::new(),
            resolutions: HashMap::new(),
            var_types: HashMap::new(),
        }
    }
}

impl AstBuilder {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
        }
    }

    /// Build the program from a Pest program pair
    pub fn build_program(&mut self, pair: Pair<Rule>) -> Results<Program> {
        let mut errors = Vec::new();

        // First pass: collect nodes
        for item in pair.clone().into_inner() {
            if item.as_rule() == Rule::nodes_block {
                if let Err(mut errs) = self.build_nodes_block(item) {
                    errors.append(&mut errs);
                }
            }
        }

        // Second pass: collect tables
        for item in pair.clone().into_inner() {
            if item.as_rule() == Rule::table_declaration {
                if let Err(mut errs) = self.build_table_declaration(item) {
                    errors.append(&mut errs);
                }
            }
        }

        // Third pass: parse functions
        for item in pair.into_inner() {
            if item.as_rule() == Rule::function_declaration {
                if let Err(mut errs) = self.build_function_declaration(item) {
                    errors.append(&mut errs);
                }
            }
        }

        if errors.is_empty() {
            Ok(std::mem::replace(&mut self.program, Program::new()))
        } else {
            Err(errors)
        }
    }

    fn build_nodes_block(&mut self, pair: Pair<Rule>) -> Results<()> {
        for item in pair.into_inner() {
            if item.as_rule() == Rule::node_list {
                self.build_node_list(item)?;
            }
        }
        Ok(())
    }

    fn build_node_list(&mut self, pair: Pair<Rule>) -> Results<()> {
        for node_pair in pair.into_inner() {
            if node_pair.as_rule() == Rule::identifier {
                let name = node_pair.as_str().to_string();
                let span = Span::from_pest(node_pair.as_span());

                let node = NodeDef {
                    name: name.clone(),
                    span,
                };
                let node_id = self.program.nodes.alloc(node);

                self.program.node_map.insert(name, node_id);
                self.program.root_nodes.push(node_id);
            }
        }
        Ok(())
    }

    fn build_table_declaration(&mut self, pair: Pair<Rule>) -> Results<()> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let table_name = inner.next().unwrap().as_str().to_string();
        let node_name = inner.next().unwrap().as_str().to_string();

        let node_id = self
            .program
            .node_map
            .get(&node_name)
            .copied()
            .ok_or_else(|| {
                vec![SpannedError {
                    error: AstError::UndeclaredNode(node_name),
                    span: Some(span.clone()),
                }]
            })?;

        let mut field_ids = Vec::new();
        let mut primary_key_id = None;

        for field_pair in inner {
            if field_pair.as_rule() == Rule::field_declaration {
                let (field_id, is_primary) = self.build_field_declaration(field_pair)?;
                field_ids.push(field_id);

                if is_primary {
                    if primary_key_id.is_some() {
                        return Err(vec![SpannedError {
                            error: AstError::ParseError(format!(
                                "Table {} has multiple primary keys",
                                table_name
                            )),
                            span: Some(span),
                        }]);
                    }
                    primary_key_id = Some(field_id);
                }
            }
        }

        let primary_key_id = primary_key_id.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError(format!(
                    "Table {} must have exactly one primary key",
                    table_name
                )),
                span: Some(span.clone()),
            }]
        })?;

        let table = TableDeclaration {
            name: table_name.clone(),
            node: node_id,
            fields: field_ids,
            primary_key: primary_key_id,
            span,
        };

        let table_id = self.program.tables.alloc(table);
        self.program.table_map.insert(table_name, table_id);
        self.program.root_tables.push(table_id);

        Ok(())
    }

    fn build_field_declaration(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<(FieldId, bool), Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        // Check if first token is primary keyword
        let first = inner.next().unwrap();
        let (is_primary, field_type) = if first.as_rule() == Rule::primary_keyword {
            (true, self.parse_type_name(inner.next().unwrap())?)
        } else {
            (false, self.parse_type_name(first)?)
        };

        let field_name = inner.next().unwrap().as_str().to_string();

        let field = FieldDeclaration {
            field_type,
            field_name,
            is_primary,
            span,
        };

        let field_id = self.program.fields.alloc(field);
        Ok((field_id, is_primary))
    }

    fn build_function_declaration(&mut self, pair: Pair<Rule>) -> Results<()> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let return_type = self.parse_ret_type(inner.next().unwrap())?;
        let name = inner.next().unwrap().as_str().to_string();

        let mut parameter_ids = Vec::new();
        let mut hop_ids = Vec::new();

        for item in inner {
            match item.as_rule() {
                Rule::parameter_list => {
                    parameter_ids = self.build_parameter_list(item)?;
                }
                Rule::function_body_item => {
                    for hop_item in item.into_inner() {
                        if hop_item.as_rule() == Rule::hop_block {
                            let hop_id = self.build_hop_block(hop_item)?;
                            hop_ids.push(hop_id);
                        }
                    }
                }
                _ => {}
            }
        }

        let function = FunctionDeclaration {
            return_type,
            name: name.clone(),
            parameters: parameter_ids,
            hops: hop_ids,
            span,
        };

        let function_id = self.program.functions.alloc(function);
        self.program.function_map.insert(name, function_id);
        self.program.root_functions.push(function_id);

        Ok(())
    }

    fn build_parameter_list(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<Vec<ParameterId>, Vec<SpannedError>> {
        let mut parameter_ids = Vec::new();
        for param_pair in pair.into_inner() {
            if param_pair.as_rule() == Rule::parameter_decl {
                let param_id = self.build_parameter_decl(param_pair)?;
                parameter_ids.push(param_id);
            }
        }
        Ok(parameter_ids)
    }

    fn build_parameter_decl(&mut self, pair: Pair<Rule>) -> Result<ParameterId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let param_type = self.parse_type_name(inner.next().unwrap())?;
        let param_name = inner.next().unwrap().as_str().to_string();

        let parameter = ParameterDecl {
            param_type,
            param_name,
            span,
        };

        Ok(self.program.parameters.alloc(parameter))
    }

    fn build_hop_block(&mut self, pair: Pair<Rule>) -> Result<HopId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let node_name = inner.next().unwrap().as_str().to_string();

        let mut statement_ids = Vec::new();
        for item in inner {
            if item.as_rule() == Rule::block {
                statement_ids = self.build_block(item)?;
            }
        }

        let hop = HopBlock {
            node_name,
            statements: statement_ids,
            span,
            resolved_node: None,
        };

        Ok(self.program.hops.alloc(hop))
    }

    fn build_block(&mut self, pair: Pair<Rule>) -> Result<Vec<StatementId>, Vec<SpannedError>> {
        let mut statement_ids = Vec::new();
        for stmt_pair in pair.into_inner() {
            if stmt_pair.as_rule() == Rule::statement {
                let stmt_id = self.build_statement(stmt_pair)?;
                statement_ids.push(stmt_id);
            }
        }
        Ok(statement_ids)
    }

    fn build_statement(&mut self, pair: Pair<Rule>) -> Result<StatementId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let inner = pair.into_inner().next().unwrap();

        let kind = match inner.as_rule() {
            Rule::var_decl_statement => {
                StatementKind::VarDecl(self.build_var_decl_statement(inner)?)
            }
            Rule::var_assignment_statement => {
                StatementKind::VarAssignment(self.build_var_assignment_statement(inner)?)
            }
            Rule::assignment_statement => {
                StatementKind::Assignment(self.build_assignment_statement(inner)?)
            }
            Rule::if_statement => StatementKind::IfStmt(self.build_if_statement(inner)?),
            Rule::while_statement => StatementKind::WhileStmt(self.build_while_statement(inner)?),
            Rule::return_statement => StatementKind::Return(self.build_return_statement(inner)?),
            Rule::abort_statement => StatementKind::Abort(AbortStatement),
            Rule::break_statement => StatementKind::Break(BreakStatement),
            Rule::continue_statement => StatementKind::Continue(ContinueStatement),
            Rule::empty_statement => StatementKind::Empty,
            _ => {
                return Err(vec![SpannedError {
                    error: AstError::ParseError(format!(
                        "Unknown statement: {:?}",
                        inner.as_rule()
                    )),
                    span: Some(span),
                }]);
            }
        };

        let statement = Statement { node: kind, span };
        Ok(self.program.statements.alloc(statement))
    }

    fn build_var_decl_statement(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<VarDeclStatement, Vec<SpannedError>> {
        let mut inner = pair.into_inner();
        let var_type = self.parse_type_name(inner.next().unwrap())?;
        let var_name = inner.next().unwrap().as_str().to_string();
        let init_value = self.build_expression(inner.next().unwrap())?;

        Ok(VarDeclStatement {
            var_type,
            var_name,
            init_value,
        })
    }

    fn build_var_assignment_statement(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<VarAssignmentStatement, Vec<SpannedError>> {
        let mut inner = pair.into_inner();
        let var_name = inner.next().unwrap().as_str().to_string();
        let rhs = self.build_expression(inner.next().unwrap())?;

        Ok(VarAssignmentStatement {
            var_name,
            rhs,
            resolved_var: None,
        })
    }

    fn build_assignment_statement(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<AssignmentStatement, Vec<SpannedError>> {
        let mut inner = pair.into_inner();
        let table_name = inner.next().unwrap().as_str().to_string();
        let pk_field_name = inner.next().unwrap().as_str().to_string();
        let pk_expr = self.build_expression(inner.next().unwrap())?;
        let field_name = inner.next().unwrap().as_str().to_string();
        let rhs = self.build_expression(inner.next().unwrap())?;

        Ok(AssignmentStatement {
            table_name,
            pk_field_name,
            pk_expr,
            field_name,
            rhs,
            resolved_table: None,
            resolved_pk_field: None,
            resolved_field: None,
        })
    }

    fn build_if_statement(&mut self, pair: Pair<Rule>) -> Result<IfStatement, Vec<SpannedError>> {
        let mut inner = pair.into_inner();
        let condition = self.build_expression(inner.next().unwrap())?;
        let then_branch = self.build_block(inner.next().unwrap())?;
        let else_branch = if let Some(else_block) = inner.next() {
            Some(self.build_block(else_block)?)
        } else {
            None
        };

        Ok(IfStatement {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn build_while_statement(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<WhileStatement, Vec<SpannedError>> {
        let mut inner = pair.into_inner();
        let condition = self.build_expression(inner.next().unwrap())?;
        let body = self.build_block(inner.next().unwrap())?;

        Ok(WhileStatement { condition, body })
    }

    fn build_return_statement(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<ReturnStatement, Vec<SpannedError>> {
        let mut inner = pair.into_inner();
        let value = if let Some(expr_pair) = inner.next() {
            Some(self.build_expression(expr_pair)?)
        } else {
            None
        };

        Ok(ReturnStatement { value })
    }

    fn build_expression(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());

        let kind = match pair.as_rule() {
            Rule::expression => {
                let inner = pair.into_inner().next().unwrap();
                return self.build_expression(inner);
            }
            Rule::logic_or => return self.build_logic_or(pair),
            Rule::logic_and => return self.build_logic_and(pair),
            Rule::equality => return self.build_equality(pair),
            Rule::comparison => return self.build_comparison(pair),
            Rule::addition => return self.build_addition(pair),
            Rule::multiplication => return self.build_multiplication(pair),
            Rule::unary => return self.build_unary(pair),
            Rule::primary => return self.build_primary(pair),
            Rule::bool_literal => ExpressionKind::BoolLit(pair.as_str() == "true"),
            Rule::integer_literal => {
                let value = pair.as_str().parse().map_err(|_| {
                    vec![SpannedError {
                        error: AstError::ParseError(format!("Invalid integer: {}", pair.as_str())),
                        span: Some(span.clone()),
                    }]
                })?;
                ExpressionKind::IntLit(value)
            }
            Rule::float_literal => {
                let value = pair.as_str().parse().map_err(|_| {
                    vec![SpannedError {
                        error: AstError::ParseError(format!("Invalid float: {}", pair.as_str())),
                        span: Some(span.clone()),
                    }]
                })?;
                ExpressionKind::FloatLit(value)
            }
            Rule::string_literal => {
                let s = pair.as_str();
                let content = if s.len() >= 2 {
                    s[1..s.len() - 1].to_string()
                } else {
                    String::new()
                };
                ExpressionKind::StringLit(content)
            }
            Rule::identifier => ExpressionKind::Ident(pair.as_str().to_string()),
            Rule::table_field_access => return self.build_table_field_access(pair),
            _ => {
                return Err(vec![SpannedError {
                    error: AstError::ParseError(format!(
                        "Unknown expression rule: {:?}",
                        pair.as_rule()
                    )),
                    span: Some(span),
                }]);
            }
        };

        let expression = Expression { node: kind, span };
        Ok(self.program.expressions.alloc(expression))
    }

    fn build_logic_or(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        self.build_binary_expr(pair, BinaryOp::Or)
    }

    fn build_logic_and(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        self.build_binary_expr(pair, BinaryOp::And)
    }

    fn build_equality(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let mut left = self.build_expression(inner.next().unwrap())?;

        while let Some(op_pair) = inner.next() {
            let right = self.build_expression(inner.next().unwrap())?;
            let op = match op_pair.as_str() {
                "==" => BinaryOp::Eq,
                "!=" => BinaryOp::Neq,
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!(
                            "Unknown equality op: {}",
                            op_pair.as_str()
                        )),
                        span: Some(span),
                    }])
                }
            };

            let expr = Expression {
                node: ExpressionKind::BinaryOp { left, op, right },
                span: span.clone(),
            };
            left = self.program.expressions.alloc(expr);
        }

        Ok(left)
    }

    fn build_comparison(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let mut left = self.build_expression(inner.next().unwrap())?;

        while let Some(op_pair) = inner.next() {
            let right = self.build_expression(inner.next().unwrap())?;
            let op = match op_pair.as_str() {
                "<" => BinaryOp::Lt,
                "<=" => BinaryOp::Lte,
                ">" => BinaryOp::Gt,
                ">=" => BinaryOp::Gte,
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!(
                            "Unknown comparison op: {}",
                            op_pair.as_str()
                        )),
                        span: Some(span),
                    }])
                }
            };

            let expr = Expression {
                node: ExpressionKind::BinaryOp { left, op, right },
                span: span.clone(),
            };
            left = self.program.expressions.alloc(expr);
        }

        Ok(left)
    }

    fn build_addition(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let mut left = self.build_expression(inner.next().unwrap())?;

        while let Some(op_pair) = inner.next() {
            let right = self.build_expression(inner.next().unwrap())?;
            let op = match op_pair.as_str() {
                "+" => BinaryOp::Add,
                "-" => BinaryOp::Sub,
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!(
                            "Unknown addition op: {}",
                            op_pair.as_str()
                        )),
                        span: Some(span),
                    }])
                }
            };

            let expr = Expression {
                node: ExpressionKind::BinaryOp { left, op, right },
                span: span.clone(),
            };
            left = self.program.expressions.alloc(expr);
        }

        Ok(left)
    }

    fn build_multiplication(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let mut left = self.build_expression(inner.next().unwrap())?;

        while let Some(op_pair) = inner.next() {
            let right = self.build_expression(inner.next().unwrap())?;
            let op = match op_pair.as_str() {
                "*" => BinaryOp::Mul,
                "/" => BinaryOp::Div,
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!(
                            "Unknown multiplication op: {}",
                            op_pair.as_str()
                        )),
                        span: Some(span),
                    }])
                }
            };

            let expr = Expression {
                node: ExpressionKind::BinaryOp { left, op, right },
                span: span.clone(),
            };
            left = self.program.expressions.alloc(expr);
        }

        Ok(left)
    }

    fn build_unary(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();

        if first.as_rule() == Rule::unary_op {
            let op_str = first.as_str();
            let operand = self.build_expression(inner.next().unwrap())?;
            let op = match op_str {
                "!" => UnaryOp::Not,
                "-" => UnaryOp::Neg,
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!("Unknown unary op: {}", op_str)),
                        span: Some(span),
                    }])
                }
            };

            let expr = Expression {
                node: ExpressionKind::UnaryOp { op, expr: operand },
                span,
            };
            Ok(self.program.expressions.alloc(expr))
        } else {
            self.build_expression(first)
        }
    }

    fn build_primary(&mut self, pair: Pair<Rule>) -> Result<ExpressionId, Vec<SpannedError>> {
        let inner = pair.into_inner().next().unwrap();
        self.build_expression(inner)
    }

    fn build_table_field_access(
        &mut self,
        pair: Pair<Rule>,
    ) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let table_name = inner.next().unwrap().as_str().to_string();
        let pk_field_name = inner.next().unwrap().as_str().to_string();
        let pk_expr = self.build_expression(inner.next().unwrap())?;
        let field_name = inner.next().unwrap().as_str().to_string();

        let expr = Expression {
            node: ExpressionKind::TableFieldAccess {
                table_name,
                pk_field_name,
                pk_expr,
                field_name,
                resolved_table: None,
                resolved_pk_field: None,
                resolved_field: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    fn build_binary_expr(
        &mut self,
        pair: Pair<Rule>,
        default_op: BinaryOp,
    ) -> Result<ExpressionId, Vec<SpannedError>> {
        let span = Span::from_pest(pair.as_span());
        let mut inner = pair.into_inner();

        let mut left = self.build_expression(inner.next().unwrap())?;

        while inner.peek().is_some() {
            let _op_pair = inner.next().unwrap(); // Skip operator for now, use default
            let right = self.build_expression(inner.next().unwrap())?;

            let expr = Expression {
                node: ExpressionKind::BinaryOp {
                    left,
                    op: default_op.clone(),
                    right,
                },
                span: span.clone(),
            };
            left = self.program.expressions.alloc(expr);
        }

        Ok(left)
    }

    // Helper functions
    fn parse_type_name(&self, pair: Pair<Rule>) -> Result<TypeName, Vec<SpannedError>> {
        match pair.as_str() {
            "int" => Ok(TypeName::Int),
            "float" => Ok(TypeName::Float),
            "string" => Ok(TypeName::String),
            "bool" => Ok(TypeName::Bool),
            _ => Err(vec![SpannedError {
                error: AstError::ParseError(format!("Unknown type: {}", pair.as_str())),
                span: Some(Span::from_pest(pair.as_span())),
            }]),
        }
    }

    fn parse_ret_type(&self, pair: Pair<Rule>) -> Result<ReturnType, Vec<SpannedError>> {
        match pair.as_str() {
            "void" => Ok(ReturnType::Void),
            _ => Ok(ReturnType::Type(self.parse_type_name(pair)?)),
        }
    }
}

pub fn build_program_from_pair(pair: Pair<Rule>) -> Results<Program> {
    let mut builder = AstBuilder::new();
    builder.build_program(pair)
}

pub fn parse_and_build(source: &str) -> Results<Program> {
    // Parse using Pest
    let pairs = TransActParser::parse(Rule::program, source).map_err(|e| {
        vec![SpannedError {
            error: AstError::ParseError(e.to_string()),
            span: None,
        }]
    })?;

    let program_pair = pairs.into_iter().next().ok_or_else(|| {
        vec![SpannedError {
            error: AstError::ParseError("No program found".to_string()),
            span: None,
        }]
    })?;

    // Build arena-based AST
    build_program_from_pair(program_pair)
}
