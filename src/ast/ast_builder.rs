use pest::iterators::Pair;
use pest::Parser;
use std::collections::HashMap;

use crate::ast::errors::*;
use crate::ast::*;

// Add the grammar rule enum
#[derive(pest_derive::Parser)]
#[grammar = "ast/grammar.pest"]
pub struct TransActParser;

/// Builds arena-based AST from parsed Pest pairs.
pub struct AstBuilder {
    program: Program,
}

/// Result type for field declaration parsing
enum FieldDeclResult {
    Field { field_id: FieldId, is_primary: bool },
    NodePartition(NodePartition),
}

/// Enum for function body items.
enum FunctionBodyItem {
    Statement(()),
    Hop(HopId),
}

impl AstBuilder {
    /// Creates a new `AstBuilder` instance.
    pub fn new() -> Self {
        Self {
            program: Program {
                partitions: Arena::new(),
                constants: Arena::new(),
                tables: Arena::new(),
                fields: Arena::new(),
                functions: Arena::new(),
                hops: Arena::new(),
                parameters: Arena::new(),
                statements: Arena::new(),
                expressions: Arena::new(),
                variables: Arena::new(),
                scopes: Arena::new(),
                root_partitions: Vec::new(),
                root_constants: Vec::new(),
                root_tables: Vec::new(),
                root_functions: Vec::new(),
                partition_map: HashMap::new(),
                constant_map: HashMap::new(),
                table_map: HashMap::new(),
                function_map: HashMap::new(),
                resolutions: HashMap::new(),
                var_types: HashMap::new(),
                parameter_resolutions: HashMap::new(),
                table_resolutions: HashMap::new(),
                function_resolutions: HashMap::new(),
            },
        }
    }

    /// Builds the complete program from the root grammar rule.
    pub fn build_program(mut self, pair: Pair<Rule>) -> Results<Program> {
        let mut errors = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::top_level_declaration => {
                    // Handle the top-level declaration wrapper
                    for decl_pair in inner_pair.into_inner() {
                        match decl_pair.as_rule() {
                            Rule::partition_declaration => {
                                match self.build_partition_declaration(decl_pair) {
                                    Ok(partition_id) => {
                                        self.program.root_partitions.push(partition_id);
                                    }
                                    Err(mut errs) => errors.append(&mut errs),
                                }
                            }
                            Rule::table_declaration => {
                                match self.build_table_declaration(decl_pair) {
                                    Ok(table_id) => {
                                        self.program.root_tables.push(table_id);
                                    }
                                    Err(mut errs) => errors.append(&mut errs),
                                }
                            }
                            Rule::const_declaration => {
                                match self.build_const_declaration(decl_pair) {
                                    Ok(const_id) => {
                                        self.program.root_constants.push(const_id);
                                    }
                                    Err(mut errs) => errors.append(&mut errs),
                                }
                            }
                            Rule::function_declaration => {
                                match self.build_function_declaration(decl_pair) {
                                    Ok(function_id) => {
                                        self.program.root_functions.push(function_id);
                                    }
                                    Err(mut errs) => errors.append(&mut errs),
                                }
                            }
                            _ => {
                                errors.push(SpannedError {
                                    error: AstError::ParseError(format!(
                                        "Unexpected rule in top_level_declaration: {:?}",
                                        decl_pair.as_rule()
                                    )),
                                    span: Some(Span::from_pest(decl_pair.as_span())),
                                });
                            }
                        }
                    }
                }
                Rule::partition_declaration => match self.build_partition_declaration(inner_pair) {
                    Ok(partition_id) => {
                        self.program.root_partitions.push(partition_id);
                    }
                    Err(mut errs) => errors.append(&mut errs),
                },
                Rule::table_declaration => match self.build_table_declaration(inner_pair) {
                    Ok(table_id) => {
                        self.program.root_tables.push(table_id);
                    }
                    Err(mut errs) => errors.append(&mut errs),
                },
                Rule::const_declaration => match self.build_const_declaration(inner_pair) {
                    Ok(const_id) => {
                        self.program.root_constants.push(const_id);
                    }
                    Err(mut errs) => errors.append(&mut errs),
                },
                Rule::function_declaration => match self.build_function_declaration(inner_pair) {
                    Ok(function_id) => {
                        self.program.root_functions.push(function_id);
                    }
                    Err(mut errs) => errors.append(&mut errs),
                },
                Rule::EOI => break,
                _ => {
                    errors.push(SpannedError {
                        error: AstError::ParseError(format!(
                            "Unexpected rule in program: {:?}",
                            inner_pair.as_rule()
                        )),
                        span: Some(Span::from_pest(inner_pair.as_span())),
                    });
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(self.program)
        }
    }

    /// Builds a partition declaration.
    fn build_partition_declaration(&mut self, pair: Pair<Rule>) -> Results<PartitionId> {
        let span = Span::from_pest(pair.as_span());
        let mut name = String::new();
        let mut parameters = Vec::new();
        let mut implementation = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if name.is_empty() {
                        name = inner_pair.as_str().to_string();
                    }
                }
                Rule::partition_parameter_list => {
                    parameters = self.build_partition_parameter_list(inner_pair)?;
                }
                Rule::expression => {
                    implementation = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        // Check for duplicate partition names
        if self.program.partition_map.contains_key(&name) {
            return Err(vec![SpannedError {
                error: AstError::DuplicatePartition(name),
                span: Some(span),
            }]);
        }

        let partition = PartitionDeclaration {
            name: name.clone(),
            parameters,
            implementation,
            span,
        };

        let partition_id = self.program.partitions.alloc(partition);
        self.program.partition_map.insert(name, partition_id);
        Ok(partition_id)
    }

    /// Builds a constant declaration.
    fn build_const_declaration(&mut self, pair: Pair<Rule>) -> Results<VarId> {
        let span = Span::from_pest(pair.as_span());
        let mut const_type = None;
        let mut name = String::new();
        let mut value = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    const_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    value = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let const_type = const_type.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing type in constant declaration".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let value = value.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing value in constant declaration".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        // Check for duplicate constant names
        if self.program.constant_map.contains_key(&name) {
            return Err(vec![SpannedError {
                error: AstError::DuplicateConstant(name),
                span: Some(span.clone()),
            }]);
        }

        let const_decl = ConstDeclaration {
            const_type: const_type.clone(),
            name: name.clone(),
            value,
            span: span.clone(),
        };

        // Create a VarDecl for the constant
        let var_decl = VarDecl {
            name: name.clone(),
            ty: const_type,
            kind: VarKind::Global,
            defined_at: span,
            scope: self.program.scopes.alloc(Scope {
                parent: None,
                variables: HashMap::new(),
            }), // Global scope
        };

        let var_id = self.program.variables.alloc(var_decl);
        let _const_id = self.program.constants.alloc(const_decl);
        self.program.constant_map.insert(name, var_id);
        Ok(var_id)
    }

    /// Builds a table declaration.
    fn build_table_declaration(&mut self, pair: Pair<Rule>) -> Results<TableId> {
        let span = Span::from_pest(pair.as_span());
        let mut name = String::new();
        let mut fields = Vec::new();
        let mut primary_keys = Vec::new();
        let mut node_partition = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::field_declaration => match self.build_field_declaration(inner_pair)? {
                    FieldDeclResult::Field {
                        field_id,
                        is_primary,
                    } => {
                        if is_primary {
                            primary_keys.push(field_id);
                        }
                        fields.push(field_id);
                    }
                    FieldDeclResult::NodePartition(partition) => {
                        node_partition = Some(partition);
                    }
                },
                _ => {}
            }
        }

        // Check for duplicate table names
        if self.program.table_map.contains_key(&name) {
            return Err(vec![SpannedError {
                error: AstError::DuplicateTable(name),
                span: Some(span),
            }]);
        }

        let table = TableDeclaration {
            name: name.clone(),
            fields,
            primary_keys,
            node_partition,
            span,
        };

        let table_id = self.program.tables.alloc(table);
        self.program.table_map.insert(name, table_id);
        Ok(table_id)
    }

    /// Builds a field declaration or node partition.
    fn build_field_declaration(&mut self, pair: Pair<Rule>) -> Results<FieldDeclResult> {
        let span = Span::from_pest(pair.as_span());
        let mut is_primary = false;
        let mut field_type = None;
        let mut field_name = String::new();
        let mut is_node = false;
        let mut partition_name = String::new();
        let mut arguments = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::primary_keyword => {
                    is_primary = true;
                }
                Rule::node_keyword => {
                    is_node = true;
                }
                Rule::type_name => {
                    field_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    if is_node && partition_name.is_empty() {
                        partition_name = inner_pair.as_str().to_string();
                    } else if field_name.is_empty() {
                        field_name = inner_pair.as_str().to_string();
                    }
                }
                Rule::expression_list => {
                    arguments = self.build_expression_list_as_identifiers(inner_pair)?;
                }
                _ => {}
            }
        }

        if is_node {
            Ok(FieldDeclResult::NodePartition(NodePartition {
                partition_name,
                arguments,
                resolved_partition: None,
            }))
        } else {
            let field_type = field_type.ok_or_else(|| {
                vec![SpannedError {
                    error: AstError::ParseError("Missing type in field declaration".to_string()),
                    span: Some(span.clone()),
                }]
            })?;

            let field = FieldDeclaration {
                field_type,
                field_name,
                is_primary,
                span,
            };

            let field_id = self.program.fields.alloc(field);
            Ok(FieldDeclResult::Field {
                field_id,
                is_primary,
            })
        }
    }

    /// Builds a function declaration.
    fn build_function_declaration(&mut self, pair: Pair<Rule>) -> Results<FunctionId> {
        let span = Span::from_pest(pair.as_span());
        let mut return_type = ReturnType::Void;
        let mut name = String::new();
        let mut parameters = Vec::new();
        let mut hops = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::ret_type => {
                    return_type = self.build_ret_type(inner_pair)?;
                }
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::parameter_list => {
                    parameters = self.build_parameter_list(inner_pair)?;
                }
                Rule::function_body_item => match self.build_function_body_item(inner_pair)? {
                    FunctionBodyItem::Hop(hop_id) => hops.push(hop_id),
                    FunctionBodyItem::Statement(_) => {
                        return Err(vec![SpannedError {
                            error: AstError::ParseError(
                                "Statements outside hop blocks are not allowed".to_string(),
                            ),
                            span: Some(span),
                        }]);
                    }
                },
                _ => {}
            }
        }

        // Check for duplicate function names
        if self.program.function_map.contains_key(&name) {
            return Err(vec![SpannedError {
                error: AstError::DuplicateFunction(name),
                span: Some(span),
            }]);
        }

        let function = FunctionDeclaration {
            return_type,
            name: name.clone(),
            parameters,
            hops,
            span,
        };

        let function_id = self.program.functions.alloc(function);
        self.program.function_map.insert(name, function_id);
        Ok(function_id)
    }

    /// Builds a parameter list.
    fn build_parameter_list(&mut self, pair: Pair<Rule>) -> Results<Vec<ParameterId>> {
        let mut parameters = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::parameter_decl {
                parameters.push(self.build_parameter_decl(inner_pair)?);
            }
        }

        Ok(parameters)
    }

    /// Builds a parameter declaration.
    fn build_parameter_decl(&mut self, pair: Pair<Rule>) -> Results<ParameterId> {
        let span = Span::from_pest(pair.as_span());
        let mut param_type = None;
        let mut param_name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    param_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    param_name = inner_pair.as_str().to_string();
                }
                _ => {}
            }
        }

        let param_type = param_type.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing type in parameter declaration".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let parameter = ParameterDecl {
            param_type,
            param_name,
            span,
        };

        Ok(self.program.parameters.alloc(parameter))
    }

    /// Builds a partition parameter list (same as regular parameter list but with explicit naming).
    fn build_partition_parameter_list(&mut self, pair: Pair<Rule>) -> Results<Vec<ParameterId>> {
        let mut parameters = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::partition_parameter {
                parameters.push(self.build_partition_parameter(inner_pair)?);
            }
        }

        Ok(parameters)
    }

    /// Builds a partition parameter declaration (same as regular parameter).
    fn build_partition_parameter(&mut self, pair: Pair<Rule>) -> Results<ParameterId> {
        let span = Span::from_pest(pair.as_span());
        let mut param_type = None;
        let mut param_name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    param_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    param_name = inner_pair.as_str().to_string();
                }
                _ => {}
            }
        }

        let param_type = param_type.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError(
                    "Missing type in partition parameter declaration".to_string(),
                ),
                span: Some(span.clone()),
            }]
        })?;

        let parameter = ParameterDecl {
            param_type,
            param_name,
            span,
        };

        Ok(self.program.parameters.alloc(parameter))
    }

    /// Builds a return type.
    fn build_ret_type(&mut self, pair: Pair<Rule>) -> Results<ReturnType> {
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    return Ok(ReturnType::Type(self.build_type_name(inner_pair)?));
                }
                _ => {}
            }
        }
        Ok(ReturnType::Void)
    }

    /// Builds a type name.
    fn build_type_name(&mut self, pair: Pair<Rule>) -> Results<TypeName> {
        let span = Span::from_pest(pair.as_span());
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::basic_type => {
                    return self.build_basic_type(inner_pair);
                }
                Rule::array_type => {
                    return self.build_array_type(inner_pair);
                }
                Rule::user_defined_type => {
                    return self.build_user_defined_type(inner_pair);
                }
                _ => {}
            }
        }
        Err(vec![SpannedError {
            error: AstError::ParseError("Invalid type name".to_string()),
            span: Some(span),
        }])
    }

    /// Builds a basic type.
    fn build_basic_type(&mut self, pair: Pair<Rule>) -> Results<TypeName> {
        let type_str = pair.as_str();
        match type_str {
            "int" => Ok(TypeName::Int),
            "float" => Ok(TypeName::Float),
            "string" => Ok(TypeName::String),
            "bool" => Ok(TypeName::Bool),
            _ => Err(vec![SpannedError {
                error: AstError::ParseError(format!("Unknown basic type: {}", type_str)),
                span: Some(Span::from_pest(pair.as_span())),
            }]),
        }
    }

    /// Builds a user-defined type (table name).
    fn build_user_defined_type(&mut self, pair: Pair<Rule>) -> Results<TypeName> {
        let span = Span::from_pest(pair.as_span());
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    let type_name = inner_pair.as_str().to_string();
                    return Ok(TypeName::Table(type_name));
                }
                _ => {}
            }
        }
        Err(vec![SpannedError {
            error: AstError::ParseError("Invalid user-defined type".to_string()),
            span: Some(span),
        }])
    }

    /// Builds an array type.
    fn build_array_type(&mut self, pair: Pair<Rule>) -> Results<TypeName> {
        let span = Span::from_pest(pair.as_span());
        let mut base = None;
        let mut size = None;
        let mut size_expr = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::basic_type => {
                    base = Some(self.build_basic_type(inner_pair)?);
                }
                Rule::user_defined_type => {
                    base = Some(self.build_user_defined_type(inner_pair)?);
                }
                Rule::expression => {
                    // Parse the expression for array size
                    let expr_id = self.build_expression(inner_pair)?;
                    size_expr = Some(expr_id);

                    // Try to evaluate simple integer literals at parse time
                    if let Some(expr) = self.program.expressions.get(expr_id) {
                        match &expr.node {
                            ExpressionKind::IntLit(val) => {
                                size = Some(*val as usize);
                            }
                            _ => {
                                // For non-literal expressions, defer size resolution to semantic analysis
                                size = None;
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        let element_type = base.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing element type in array".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(TypeName::Array {
            element_type: Box::new(element_type),
            size,
            size_expr,
        })
    }

    /// Builds a function body item.
    fn build_function_body_item(&mut self, pair: Pair<Rule>) -> Results<FunctionBodyItem> {
        let span = Span::from_pest(pair.as_span());
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::statement => {
                    return Ok(FunctionBodyItem::Statement(()));
                }
                Rule::hop_block => {
                    return Ok(FunctionBodyItem::Hop(self.build_hop_block(inner_pair)?));
                }
                Rule::hops_for_loop => {
                    return Ok(FunctionBodyItem::Hop(self.build_hops_for_loop(inner_pair)?));
                }
                _ => {}
            }
        }
        Err(vec![SpannedError {
            error: AstError::ParseError("Invalid function body item".to_string()),
            span: Some(span),
        }])
    }

    /// Builds a hop block.
    fn build_hop_block(&mut self, pair: Pair<Rule>) -> Results<HopId> {
        let span = Span::from_pest(pair.as_span());
        let mut statements = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::block {
                statements = self.build_block(inner_pair)?;
            }
        }

        let hop = HopBlock {
            statements,
            span,
            hop_type: HopType::Simple,
        };

        Ok(self.program.hops.alloc(hop))
    }

    /// Builds a hops for loop.
    fn build_hops_for_loop(&mut self, pair: Pair<Rule>) -> Results<HopId> {
        let span = Span::from_pest(pair.as_span());
        let mut loop_var = String::new();
        let mut loop_var_type = None;
        let mut start = None;
        let mut end = None;
        let mut statements = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    loop_var_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    loop_var = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    if start.is_none() {
                        start = Some(self.build_expression(inner_pair)?);
                    } else if end.is_none() {
                        end = Some(self.build_expression(inner_pair)?);
                    }
                }
                Rule::block => {
                    statements = self.build_block(inner_pair)?;
                }
                _ => {}
            }
        }

        let loop_var_type = loop_var_type.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop variable type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let start = start.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop start value".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let end = end.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop end value".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let hop = HopBlock {
            statements,
            span,
            hop_type: HopType::ForLoop {
                loop_var,
                loop_var_type,
                start,
                end,
                start_value: None, // Will be populated during semantic analysis
                end_value: None,   // Will be populated during semantic analysis
            },
        };

        Ok(self.program.hops.alloc(hop))
    }

    /// Builds a block of statements.
    fn build_block(&mut self, pair: Pair<Rule>) -> Results<Vec<StatementId>> {
        let mut statements = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::statement {
                statements.push(self.build_statement(inner_pair)?);
            }
        }

        Ok(statements)
    }

    /// Builds a statement.
    fn build_statement(&mut self, pair: Pair<Rule>) -> Results<StatementId> {
        let span = Span::from_pest(pair.as_span());

        for inner_pair in pair.into_inner() {
            let statement_kind = match inner_pair.as_rule() {
                Rule::var_decl_statement => {
                    StatementKind::VarDecl(self.build_var_decl_statement(inner_pair)?)
                }
                Rule::assignment_statement => {
                    StatementKind::Assignment(self.build_assignment_statement(inner_pair)?)
                }
                Rule::if_statement => StatementKind::IfStmt(self.build_if_statement(inner_pair)?),
                Rule::for_statement => {
                    StatementKind::ForStmt(self.build_for_statement(inner_pair)?)
                }
                Rule::while_statement => {
                    StatementKind::WhileStmt(self.build_while_statement(inner_pair)?)
                }
                Rule::return_statement => {
                    StatementKind::Return(self.build_return_statement(inner_pair)?)
                }
                Rule::abort_statement => StatementKind::Abort(AbortStatement),
                Rule::break_statement => StatementKind::Break(BreakStatement),
                Rule::continue_statement => StatementKind::Continue(ContinueStatement),
                Rule::expression_statement => {
                    StatementKind::Expression(self.build_expression_statement(inner_pair)?)
                }
                Rule::empty_statement => StatementKind::Empty,
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!(
                            "Unexpected statement type: {:?}",
                            inner_pair.as_rule()
                        )),
                        span: Some(span),
                    }]);
                }
            };

            let statement = Spanned {
                node: statement_kind,
                span,
            };

            return Ok(self.program.statements.alloc(statement));
        }

        Err(vec![SpannedError {
            error: AstError::ParseError("Empty statement".to_string()),
            span: Some(span),
        }])
    }

    /// Builds a variable declaration statement.
    fn build_var_decl_statement(&mut self, pair: Pair<Rule>) -> Results<VarDeclStatement> {
        let span = Span::from_pest(pair.as_span());
        let mut var_type = None;
        let mut var_name = String::new();
        let mut init_value = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    var_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    var_name = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    init_value = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let var_type = var_type.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing variable type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        Ok(VarDeclStatement {
            var_type,
            var_name,
            init_value,
        })
    }

    /// Builds an assignment statement.
    fn build_assignment_statement(&mut self, pair: Pair<Rule>) -> Results<AssignmentStatement> {
        let span = Span::from_pest(pair.as_span());
        let mut lvalue = None;
        let mut operator = None;
        let mut rhs = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::lvalue => {
                    lvalue = Some(self.build_lvalue(inner_pair)?);
                }
                Rule::assignment_operator => {
                    operator = Some(self.build_assignment_operator(inner_pair)?);
                }
                Rule::expression => {
                    rhs = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let lvalue = lvalue.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing assignment target".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let operator = operator.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing assignment operator".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let rhs = rhs.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing assignment value".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(AssignmentStatement {
            lvalue,
            operator,
            rhs,
        })
    }

    /// Builds an assignment operator.
    fn build_assignment_operator(&mut self, pair: Pair<Rule>) -> Results<AssignmentOperator> {
        match pair.as_str() {
            "=" => Ok(AssignmentOperator::Assign),
            "+=" => Ok(AssignmentOperator::AddAssign),
            "-=" => Ok(AssignmentOperator::SubAssign),
            "*=" => Ok(AssignmentOperator::MulAssign),
            "/=" => Ok(AssignmentOperator::DivAssign),
            _ => Err(vec![SpannedError {
                error: AstError::ParseError(format!(
                    "Unknown assignment operator: {}",
                    pair.as_str()
                )),
                span: Some(Span::from_pest(pair.as_span())),
            }]),
        }
    }

    /// Builds an lvalue.
    fn build_lvalue(&mut self, pair: Pair<Rule>) -> Results<LValue> {
        let span = Span::from_pest(pair.as_span());
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::table_field_access => {
                    return self.build_table_field_access_lvalue(inner_pair);
                }
                Rule::field_access => {
                    return self.build_field_access_lvalue(inner_pair);
                }
                Rule::table_access => {
                    return self.build_table_access_lvalue(inner_pair);
                }
                Rule::array_access => {
                    return self.build_array_access_lvalue(inner_pair);
                }
                Rule::identifier => {
                    return Ok(LValue::Var {
                        name: inner_pair.as_str().to_string(),
                        resolved_var: None,
                    });
                }
                _ => {}
            }
        }
        Err(vec![SpannedError {
            error: AstError::ParseError("Invalid lvalue".to_string()),
            span: Some(span),
        }])
    }

    /// Builds a table field access lvalue.
    fn build_table_field_access_lvalue(&mut self, pair: Pair<Rule>) -> Results<LValue> {
        let mut table_name = String::new();
        let mut pk_fields = Vec::new();
        let mut pk_exprs = Vec::new();
        let mut field_name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if table_name.is_empty() {
                        table_name = inner_pair.as_str().to_string();
                    } else {
                        field_name = inner_pair.as_str().to_string();
                    }
                }
                Rule::primary_key_list => {
                    let (fields, exprs) = self.build_primary_key_list(inner_pair)?;
                    pk_fields = fields;
                    pk_exprs = exprs;
                }
                _ => {}
            }
        }

        let pk_len = pk_fields.len();
        Ok(LValue::TableField {
            table_name,
            pk_fields,
            pk_exprs,
            field_name,
            resolved_table: None,
            resolved_pk_fields: vec![None; pk_len],
            resolved_field: None,
        })
    }

    /// Builds a field access lvalue.
    fn build_field_access_lvalue(&mut self, pair: Pair<Rule>) -> Results<LValue> {
        let mut object_name = String::new();
        let mut field_name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if object_name.is_empty() {
                        object_name = inner_pair.as_str().to_string();
                    } else {
                        field_name = inner_pair.as_str().to_string();
                    }
                }
                _ => {}
            }
        }

        Ok(LValue::FieldAccess {
            object_name,
            field_name,
            resolved_var: None,
            resolved_field: None,
        })
    }

    /// Builds a table access lvalue.
    fn build_table_access_lvalue(&mut self, pair: Pair<Rule>) -> Results<LValue> {
        let mut table_name = String::new();
        let mut pk_fields = Vec::new();
        let mut pk_exprs = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    table_name = inner_pair.as_str().to_string();
                }
                Rule::primary_key_list => {
                    let (fields, exprs) = self.build_primary_key_list(inner_pair)?;
                    pk_fields = fields;
                    pk_exprs = exprs;
                }
                _ => {}
            }
        }

        let pk_len = pk_fields.len();
        Ok(LValue::TableRecord {
            table_name,
            pk_fields,
            pk_exprs,
            resolved_table: None,
            resolved_pk_fields: vec![None; pk_len],
        })
    }

    /// Builds an array access lvalue.
    fn build_array_access_lvalue(&mut self, pair: Pair<Rule>) -> Results<LValue> {
        let span = Span::from_pest(pair.as_span());
        let mut array_name = String::new();
        let mut index = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    array_name = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    index = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let index = index.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing array index".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(LValue::ArrayElement {
            array_name,
            index,
            resolved_var: None,
        })
    }

    /// Builds an if statement.
    fn build_if_statement(&mut self, pair: Pair<Rule>) -> Results<IfStatement> {
        let span = Span::from_pest(pair.as_span());
        let mut condition = None;
        let mut then_branch = Vec::new();
        let mut else_branch = None;
        let mut parsing_else = false;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::expression => {
                    condition = Some(self.build_expression(inner_pair)?);
                }
                Rule::block => {
                    if parsing_else {
                        else_branch = Some(self.build_block(inner_pair)?);
                    } else {
                        then_branch = self.build_block(inner_pair)?;
                        parsing_else = true;
                    }
                }
                _ => {}
            }
        }

        let condition = condition.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing if condition".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(IfStatement {
            condition,
            then_branch,
            else_branch,
        })
    }

    /// Builds a for statement.
    fn build_for_statement(&mut self, pair: Pair<Rule>) -> Results<ForStatement> {
        let span = Span::from_pest(pair.as_span());
        let mut loop_var = String::new();
        let mut loop_var_type = None;
        let mut init = None;
        let mut condition = None;
        let mut increment = None;
        let mut body = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    loop_var_type = Some(self.build_type_name(inner_pair)?);
                }
                Rule::identifier => {
                    loop_var = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    if init.is_none() {
                        init = Some(self.build_expression(inner_pair)?);
                    } else if condition.is_none() {
                        condition = Some(self.build_expression(inner_pair)?);
                    } else if increment.is_none() {
                        increment = Some(self.build_expression(inner_pair)?);
                    }
                }
                Rule::block => {
                    body = self.build_block(inner_pair)?;
                }
                _ => {}
            }
        }

        let loop_var_type = loop_var_type.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop variable type".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let init = init.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop initialization".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let condition = condition.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop condition".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let increment = increment.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing loop increment".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(ForStatement {
            loop_var,
            loop_var_type,
            init,
            condition,
            increment,
            body,
        })
    }

    /// Builds a while statement.
    fn build_while_statement(&mut self, pair: Pair<Rule>) -> Results<WhileStatement> {
        let span = Span::from_pest(pair.as_span());
        let mut condition = None;
        let mut body = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::expression => {
                    condition = Some(self.build_expression(inner_pair)?);
                }
                Rule::block => {
                    body = self.build_block(inner_pair)?;
                }
                _ => {}
            }
        }

        let condition = condition.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing while condition".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(WhileStatement { condition, body })
    }

    /// Builds a return statement.
    fn build_return_statement(&mut self, pair: Pair<Rule>) -> Results<ReturnStatement> {
        let mut value = None;

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                value = Some(self.build_expression(inner_pair)?);
            }
        }

        Ok(ReturnStatement { value })
    }

    /// Builds an expression statement.
    fn build_expression_statement(&mut self, pair: Pair<Rule>) -> Results<ExpressionStatement> {
        let span = Span::from_pest(pair.as_span());

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                let expression = self.build_expression(inner_pair)?;
                return Ok(ExpressionStatement { expression });
            }
        }

        Err(vec![SpannedError {
            error: AstError::ParseError("Empty expression statement".to_string()),
            span: Some(span),
        }])
    }

    /// Builds an expression.
    fn build_expression(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        self.build_logic_or(pair)
    }

    /// Builds a logical OR expression.
    fn build_logic_or(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut operands = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::logic_and => {
                    operands.push(self.build_logic_and(inner_pair)?);
                }
                Rule::logic_or => {
                    // Handle recursive logic_or by delegating
                    operands.push(self.build_logic_or(inner_pair)?);
                }
                Rule::logic_or_op => {
                    // Operator handled implicitly
                }
                _ => {}
            }
        }

        if operands.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError("No operands found in logic_or expression".to_string()),
                span: Some(span.clone()),
            }]);
        } else if operands.len() == 1 {
            Ok(operands.into_iter().next().unwrap())
        } else {
            // Build left-associative binary operations
            let mut result = operands[0];
            for i in 1..operands.len() {
                let expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: result,
                        op: BinaryOp::Or,
                        right: operands[i],
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                result = self.program.expressions.alloc(expr);
            }
            Ok(result)
        }
    }

    /// Builds a logical AND expression.
    fn build_logic_and(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut operands = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::equality => {
                    operands.push(self.build_equality(inner_pair)?);
                }
                Rule::logic_and_op => {
                    // Operator handled implicitly
                }
                _ => {}
            }
        }

        if operands.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError(
                    "No operands found in logic_and expression".to_string(),
                ),
                span: Some(span.clone()),
            }]);
        } else if operands.len() == 1 {
            Ok(operands.into_iter().next().unwrap())
        } else {
            // Build left-associative binary operations
            let mut result = operands[0];
            for i in 1..operands.len() {
                let expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: result,
                        op: BinaryOp::And,
                        right: operands[i],
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                result = self.program.expressions.alloc(expr);
            }
            Ok(result)
        }
    }

    /// Builds an equality expression.
    fn build_equality(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut operands = Vec::new();
        let mut operators = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::comparison => {
                    operands.push(self.build_comparison(inner_pair)?);
                }
                Rule::equality_op => {
                    operators.push(match inner_pair.as_str() {
                        "==" => BinaryOp::Eq,
                        "!=" => BinaryOp::Neq,
                        _ => unreachable!(),
                    });
                }
                _ => {}
            }
        }

        if operands.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError("No operands found in equality expression".to_string()),
                span: Some(span.clone()),
            }]);
        } else if operands.len() == 1 {
            Ok(operands.into_iter().next().unwrap())
        } else {
            // Build left-associative binary operations
            let mut result = operands[0];
            for (i, op) in operators.into_iter().enumerate() {
                let expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: result,
                        op,
                        right: operands[i + 1],
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                result = self.program.expressions.alloc(expr);
            }
            Ok(result)
        }
    }

    /// Builds a comparison expression.
    fn build_comparison(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut operands = Vec::new();
        let mut operators = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::addition => {
                    operands.push(self.build_addition(inner_pair)?);
                }
                Rule::comparison_op => {
                    operators.push(match inner_pair.as_str() {
                        "<" => BinaryOp::Lt,
                        "<=" => BinaryOp::Lte,
                        ">" => BinaryOp::Gt,
                        ">=" => BinaryOp::Gte,
                        _ => unreachable!(),
                    });
                }
                _ => {}
            }
        }

        if operands.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError(
                    "No operands found in comparison expression".to_string(),
                ),
                span: Some(span.clone()),
            }]);
        } else if operands.len() == 1 {
            Ok(operands.into_iter().next().unwrap())
        } else {
            // Build left-associative binary operations
            let mut result = operands[0];
            for (i, op) in operators.into_iter().enumerate() {
                let expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: result,
                        op,
                        right: operands[i + 1],
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                result = self.program.expressions.alloc(expr);
            }
            Ok(result)
        }
    }

    /// Builds an addition expression.
    fn build_addition(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut operands = Vec::new();
        let mut operators = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::multiplication => {
                    operands.push(self.build_multiplication(inner_pair)?);
                }
                Rule::addition_op => {
                    operators.push(match inner_pair.as_str() {
                        "+" => BinaryOp::Add,
                        "-" => BinaryOp::Sub,
                        "~" => BinaryOp::Concat,
                        _ => unreachable!(),
                    });
                }
                _ => {}
            }
        }

        if operands.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError("No operands found in addition expression".to_string()),
                span: Some(span.clone()),
            }]);
        } else if operands.len() == 1 {
            Ok(operands.into_iter().next().unwrap())
        } else {
            // Build left-associative binary operations
            let mut result = operands[0];
            for (i, op) in operators.into_iter().enumerate() {
                let expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: result,
                        op,
                        right: operands[i + 1],
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                result = self.program.expressions.alloc(expr);
            }
            Ok(result)
        }
    }

    /// Builds a multiplication expression.
    fn build_multiplication(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut operands = Vec::new();
        let mut operators = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::unary => {
                    operands.push(self.build_unary(inner_pair)?);
                }
                Rule::multiplication_op => {
                    operators.push(match inner_pair.as_str() {
                        "*" => BinaryOp::Mul,
                        "/" => BinaryOp::Div,
                        _ => unreachable!(),
                    });
                }
                _ => {}
            }
        }

        if operands.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError(
                    "No operands found in multiplication expression".to_string(),
                ),
                span: Some(span.clone()),
            }]);
        } else if operands.len() == 1 {
            Ok(operands.into_iter().next().unwrap())
        } else {
            // Build left-associative binary operations
            let mut result = operands[0];
            for (i, op) in operators.into_iter().enumerate() {
                let expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: result,
                        op,
                        right: operands[i + 1],
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                result = self.program.expressions.alloc(expr);
            }
            Ok(result)
        }
    }

    /// Builds a unary expression.
    fn build_unary(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let pairs_vec: Vec<_> = pair.into_inner().collect();

        if pairs_vec.len() == 1 && pairs_vec[0].as_rule() == Rule::postfix {
            return self.build_postfix(pairs_vec.into_iter().next().unwrap());
        }

        if pairs_vec.len() == 2 {
            if let (Some(first_pair), Some(second_pair)) = (pairs_vec.get(0), pairs_vec.get(1)) {
                // Handle prefix unary operations: unary_op unary OR prefix_unary_op postfix
                if (first_pair.as_rule() == Rule::unary_op && second_pair.as_rule() == Rule::unary)
                    || (first_pair.as_rule() == Rule::prefix_unary_op
                        && second_pair.as_rule() == Rule::postfix)
                {
                    let op = match first_pair.as_str() {
                        "!" => UnaryOp::Not,
                        "-" => UnaryOp::Neg,
                        "++" => UnaryOp::PreIncrement,
                        "--" => UnaryOp::PreDecrement,
                        _ => unreachable!(),
                    };

                    let expr_id = match second_pair.as_rule() {
                        Rule::unary => self.build_unary(second_pair.clone())?,
                        Rule::postfix => self.build_postfix(second_pair.clone())?,
                        _ => unreachable!(),
                    };

                    let expr = Spanned {
                        node: ExpressionKind::UnaryOp {
                            op,
                            expr: expr_id,
                            resolved_type: None,
                        },
                        span,
                    };
                    return Ok(self.program.expressions.alloc(expr));
                }
            }
        }

        Err(vec![SpannedError {
            error: AstError::ParseError("Invalid unary expression".to_string()),
            span: Some(span),
        }])
    }

    /// Builds a postfix expression.
    fn build_postfix(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let pairs_vec: Vec<_> = pair.into_inner().collect();

        if pairs_vec.is_empty() {
            return Err(vec![SpannedError {
                error: AstError::ParseError("Empty postfix expression".to_string()),
                span: Some(span),
            }]);
        }

        // First element should be primary
        let primary_pair = &pairs_vec[0];
        if primary_pair.as_rule() != Rule::primary {
            return Err(vec![SpannedError {
                error: AstError::ParseError("Expected primary in postfix expression".to_string()),
                span: Some(span),
            }]);
        }

        let mut expr_id = self.build_primary(primary_pair.clone())?;

        // Apply postfix operators from left to right
        for op_pair in &pairs_vec[1..] {
            if op_pair.as_rule() == Rule::postfix_unary_op {
                let op = match op_pair.as_str() {
                    "++" => UnaryOp::PostIncrement,
                    "--" => UnaryOp::PostDecrement,
                    _ => unreachable!(),
                };

                let expr = Spanned {
                    node: ExpressionKind::UnaryOp {
                        op,
                        expr: expr_id,
                        resolved_type: None,
                    },
                    span: span.clone(),
                };
                expr_id = self.program.expressions.alloc(expr);
            }
        }

        Ok(expr_id)
    }

    /// Builds a primary expression.
    fn build_primary(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());

        for inner_pair in pair.into_inner() {
            let expr_kind = match inner_pair.as_rule() {
                Rule::bool_literal => ExpressionKind::BoolLit(inner_pair.as_str() == "true"),
                Rule::integer_literal => {
                    let value = inner_pair.as_str().parse::<i64>().map_err(|_| {
                        vec![SpannedError {
                            error: AstError::ParseError("Invalid integer literal".to_string()),
                            span: Some(Span::from_pest(inner_pair.as_span())),
                        }]
                    })?;
                    ExpressionKind::IntLit(value)
                }
                Rule::float_literal => {
                    let value = inner_pair.as_str().parse::<f64>().map_err(|_| {
                        vec![SpannedError {
                            error: AstError::ParseError("Invalid float literal".to_string()),
                            span: Some(Span::from_pest(inner_pair.as_span())),
                        }]
                    })?;
                    ExpressionKind::FloatLit(value)
                }
                Rule::string_literal => {
                    let mut value = inner_pair.as_str().to_string();
                    // Remove quotes
                    if value.starts_with('"') && value.ends_with('"') {
                        value = value[1..value.len() - 1].to_string();
                    }
                    ExpressionKind::StringLit(value)
                }
                Rule::identifier => ExpressionKind::Ident(inner_pair.as_str().to_string()),
                Rule::table_field_access => {
                    return self.build_table_field_access_expression(inner_pair);
                }
                Rule::field_access => {
                    return self.build_field_access_expression(inner_pair);
                }
                Rule::table_access => {
                    return self.build_table_access_expression(inner_pair);
                }
                Rule::array_access => {
                    return self.build_array_access_expression(inner_pair);
                }
                Rule::array_literal => {
                    return self.build_array_literal(inner_pair);
                }
                Rule::record_literal => {
                    return self.build_record_literal(inner_pair);
                }
                Rule::expression => {
                    // Parenthesized expression
                    return self.build_expression(inner_pair);
                }
                _ => {
                    return Err(vec![SpannedError {
                        error: AstError::ParseError(format!(
                            "Unexpected primary expression: {:?}",
                            inner_pair.as_rule()
                        )),
                        span: Some(span),
                    }]);
                }
            };

            let expr = Spanned {
                node: expr_kind,
                span,
            };

            return Ok(self.program.expressions.alloc(expr));
        }

        Err(vec![SpannedError {
            error: AstError::ParseError("Empty primary expression".to_string()),
            span: Some(span),
        }])
    }

    /// Builds a table field access expression.
    fn build_table_field_access_expression(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut table_name = String::new();
        let mut pk_fields = Vec::new();
        let mut pk_exprs = Vec::new();
        let mut field_name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if table_name.is_empty() {
                        table_name = inner_pair.as_str().to_string();
                    } else {
                        field_name = inner_pair.as_str().to_string();
                    }
                }
                Rule::primary_key_list => {
                    let (fields, exprs) = self.build_primary_key_list(inner_pair)?;
                    pk_fields = fields;
                    pk_exprs = exprs;
                }
                _ => {}
            }
        }

        let pk_len = pk_fields.len();
        let expr = Spanned {
            node: ExpressionKind::TableFieldAccess {
                table_name,
                pk_fields,
                pk_exprs,
                field_name,
                resolved_table: None,
                resolved_pk_fields: vec![None; pk_len],
                resolved_field: None,
                resolved_type: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    /// Builds a field access expression.
    fn build_field_access_expression(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut object_name = String::new();
        let mut field_name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if object_name.is_empty() {
                        object_name = inner_pair.as_str().to_string();
                    } else {
                        field_name = inner_pair.as_str().to_string();
                    }
                }
                _ => {}
            }
        }

        let expr = Spanned {
            node: ExpressionKind::FieldAccess {
                object_name,
                field_name,
                resolved_var: None,
                resolved_field: None,
                resolved_type: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    /// Builds a table access expression.
    fn build_table_access_expression(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut table_name = String::new();
        let mut pk_fields = Vec::new();
        let mut pk_exprs = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    table_name = inner_pair.as_str().to_string();
                }
                Rule::primary_key_list => {
                    let (fields, exprs) = self.build_primary_key_list(inner_pair)?;
                    pk_fields = fields;
                    pk_exprs = exprs;
                }
                _ => {}
            }
        }

        let pk_len = pk_fields.len();
        let expr = Spanned {
            node: ExpressionKind::TableAccess {
                table_name,
                pk_fields,
                pk_exprs,
                resolved_table: None,
                resolved_pk_fields: vec![None; pk_len],
                resolved_type: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    /// Builds an array access expression.
    fn build_array_access_expression(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut array_name = String::new();
        let mut index = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    array_name = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    index = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let index = index.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing array index".to_string()),
                span: Some(span.clone()),
            }]
        })?;

        let expr = Spanned {
            node: ExpressionKind::ArrayAccess {
                array_name,
                index,
                resolved_var: None,
                resolved_type: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    /// Builds a record literal.
    fn build_record_literal(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut fields = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::field_assignment_list {
                fields = self.build_field_assignment_list(inner_pair)?;
            }
        }

        let expr = Spanned {
            node: ExpressionKind::RecordLiteral {
                fields,
                resolved_type: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    /// Builds an array literal.
    fn build_array_literal(&mut self, pair: Pair<Rule>) -> Results<ExpressionId> {
        let span = Span::from_pest(pair.as_span());
        let mut elements = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression_list {
                for expr_pair in inner_pair.into_inner() {
                    if expr_pair.as_rule() == Rule::expression {
                        elements.push(self.build_expression(expr_pair)?);
                    }
                }
            }
        }

        let expr = Spanned {
            node: ExpressionKind::ArrayLiteral {
                elements,
                resolved_type: None,
            },
            span,
        };

        Ok(self.program.expressions.alloc(expr))
    }

    /// Builds a field assignment list.
    fn build_field_assignment_list(&mut self, pair: Pair<Rule>) -> Results<Vec<FieldAssignment>> {
        let mut fields = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::field_assignment {
                fields.push(self.build_field_assignment(inner_pair)?);
            }
        }

        Ok(fields)
    }

    /// Builds a field assignment.
    fn build_field_assignment(&mut self, pair: Pair<Rule>) -> Results<FieldAssignment> {
        let span = Span::from_pest(pair.as_span());
        let mut field_name = String::new();
        let mut value = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    field_name = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    value = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let value = value.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing field assignment value".to_string()),
                span: Some(span),
            }]
        })?;

        Ok(FieldAssignment {
            field_name,
            value,
            resolved_field: None,
        })
    }

    /// Builds a primary key list.
    fn build_primary_key_list(
        &mut self,
        pair: Pair<Rule>,
    ) -> Results<(Vec<String>, Vec<ExpressionId>)> {
        let mut fields = Vec::new();
        let mut exprs = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::primary_key_pair {
                let (field, expr) = self.build_primary_key_pair(inner_pair)?;
                fields.push(field);
                exprs.push(expr);
            }
        }

        Ok((fields, exprs))
    }

    /// Builds a primary key pair.
    fn build_primary_key_pair(&mut self, pair: Pair<Rule>) -> Results<(String, ExpressionId)> {
        let span = Span::from_pest(pair.as_span());
        let mut field_name = String::new();
        let mut value = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    field_name = inner_pair.as_str().to_string();
                }
                Rule::expression => {
                    value = Some(self.build_expression(inner_pair)?);
                }
                _ => {}
            }
        }

        let value = value.ok_or_else(|| {
            vec![SpannedError {
                error: AstError::ParseError("Missing primary key value".to_string()),
                span: Some(span),
            }]
        })?;

        Ok((field_name, value))
    }

    /// Helper method to build expression list as identifiers (for node arguments).
    fn build_expression_list_as_identifiers(&mut self, pair: Pair<Rule>) -> Results<Vec<String>> {
        let mut identifiers = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                // For node arguments, we expect simple identifiers
                // Traverse the expression hierarchy to find the identifier
                if let Some(identifier) = self.extract_identifier_from_expression(inner_pair) {
                    identifiers.push(identifier);
                }
            }
        }

        Ok(identifiers)
    }

    /// Helper function to extract identifier from expression hierarchy
    fn extract_identifier_from_expression(&self, pair: Pair<Rule>) -> Option<String> {
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    return Some(inner_pair.as_str().to_string());
                }
                Rule::logic_or
                | Rule::logic_and
                | Rule::equality
                | Rule::comparison
                | Rule::addition
                | Rule::multiplication
                | Rule::unary
                | Rule::postfix
                | Rule::primary => {
                    if let Some(id) = self.extract_identifier_from_expression(inner_pair) {
                        return Some(id);
                    }
                }
                _ => {}
            }
        }
        None
    }
}

/// Parse and build the AST from source code.
pub fn parse_and_build(source: &str) -> Results<Program> {
    let mut pairs = TransActParser::parse(Rule::program, source).map_err(|e| {
        vec![SpannedError {
            error: AstError::ParseError(format!("Parse error: {}", e)),
            span: None,
        }]
    })?;

    let program_pair = pairs.next().ok_or_else(|| {
        vec![SpannedError {
            error: AstError::ParseError("Empty input".to_string()),
            span: None,
        }]
    })?;

    let builder = AstBuilder::new();
    builder.build_program(program_pair)
}
