//! The `semantics_analysis` module performs semantic analysis on the AST.
//! It checks for type correctness, control flow validity, and partition access rules.
//!
//! # Overview
//!
//! - **SemanticAnalyzer**: The main struct responsible for semantic checks and error reporting.
//! - **analyze_program**: Public interface for running semantic analysis on a `Program`.
//!
//! # Features
//!
//! - Type checking for expressions and assignments including array types.
//! - Validation of control flow constructs (loops, returns, aborts).
//! - Table access and primary key validation for partition-aware operations.
//! - Partition function validation and node assignment checking.
//!
//! # Usage
//!
//! Use the `analyze_program` function to perform semantic analysis:
//!
//! ```rust
//! use crate::ast::semantics_analysis::analyze_program;
//! use crate::ast::Program;
//!
//! let program = ...; // Constructed AST
//! analyze_program(&program).expect("Semantic analysis failed");
//! ```

use crate::ast::*;

/// The `SemanticAnalyzer` struct performs semantic analysis on a given program.
///
/// It checks for various semantic errors, such as type mismatches, invalid control flow,
/// and incorrect table access patterns.
pub struct SemanticAnalyzer<'p> {
    program: &'p Program,
    errors: Vec<SpannedError>,

    // Current context
    current_function: Option<FunctionId>,
    current_hop: Option<HopId>,
    return_type: Option<ReturnType>,
    has_return: bool,
    in_loop: bool,
}

impl<'p> SemanticAnalyzer<'p> {
    /// Create a new `SemanticAnalyzer` for the given program.
    pub fn new(program: &'p Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
            current_function: None,
            current_hop: None,
            return_type: None,
            has_return: false,
            in_loop: false,
        }
    }

    /// Run semantic analysis on the program.
    ///
    /// This checks all partitions, functions, hops, statements, and expressions for semantic errors.
    pub fn analyze(mut self) -> Results<()> {
        // Check partitions first (since tables may reference them)
        self.check_partitions();

        // Check tables (including their partition references)
        self.check_tables();

        // Check functions last
        self.check_functions();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }

    /// Checks all partition declarations to ensure they return int.
    fn check_partitions(&mut self) {
        for partition_id in &self.program.root_partitions {
            self.check_partition(*partition_id);
        }
    }

    /// Checks a single partition declaration.
    fn check_partition(&mut self, partition_id: PartitionId) {
        let partition = &self.program.partitions[partition_id];

        // Check if partition has an implementation
        if let Some(impl_expr_id) = partition.implementation {
            // Check that the implementation expression returns int
            if let Some(expr_type) = self.check_expression(impl_expr_id) {
                if !self.is_int_type(&expr_type) {
                    self.error_at(
                        &partition.span,
                        AstError::TypeMismatch {
                            expected: TypeName::Int, // Expect int return type
                            found: expr_type,
                        },
                    );
                }
            }
        }
        // Note: If no implementation is provided, that's allowed (it's declared elsewhere)
    }

    /// Checks all table declarations for validity.
    fn check_tables(&mut self) {
        for table_id in &self.program.root_tables {
            self.check_table(*table_id);
        }
    }

    /// Checks a single table declaration.
    fn check_table(&mut self, table_id: TableId) {
        let table = &self.program.tables[table_id];

        // Check that table has at least one field
        if table.fields.is_empty() {
            self.error_at(
                &table.span,
                AstError::MissingTableFields(table.name.clone()),
            );
            return;
        }

        // Check that table has at least one primary key
        if table.primary_keys.is_empty() {
            self.error_at(&table.span, AstError::EmptyPrimaryKey(table.name.clone()));
            return;
        }

        // Check that all primary key fields actually exist in the table
        for pk_field_id in &table.primary_keys {
            if !table.fields.contains(pk_field_id) {
                let pk_field = &self.program.fields[*pk_field_id];
                self.error_at(
                    &table.span,
                    AstError::InvalidPrimaryKeyField {
                        table: table.name.clone(),
                        field: pk_field.field_name.clone(),
                    },
                );
            }
        }

        // Check for duplicate field names
        let mut field_names = std::collections::HashMap::new();
        for field_id in &table.fields {
            let field = &self.program.fields[*field_id];
            if field_names.insert(&field.field_name, field_id).is_some() {
                self.error_at(
                    &field.span,
                    AstError::DuplicateField {
                        table: table.name.clone(),
                        field: field.field_name.clone(),
                    },
                );
            }
        }

        // Check node partition if present
        if let Some(node_partition) = &table.node_partition {
            self.check_table_partition(table, node_partition);
        } else {
            // Every table must have a partition function
            self.error_at(
                &table.span,
                AstError::MissingTablePartition(table.name.clone()),
            );
        }
    }

    /// Checks a table's node partition specification.
    fn check_table_partition(&mut self, table: &TableDeclaration, node_partition: &NodePartition) {
        // Check if the partition exists
        let partition_id = if let Some(pid) = node_partition.resolved_partition {
            pid
        } else {
            self.error_at(
                &table.span,
                AstError::UnresolvedTablePartition {
                    table: table.name.clone(),
                    partition: node_partition.partition_name.clone(),
                },
            );
            return;
        };

        let partition = &self.program.partitions[partition_id];

        // Check argument count matches partition parameters
        if node_partition.arguments.len() != partition.parameters.len() {
            self.error_at(
                &table.span,
                AstError::InvalidTablePartitionArguments {
                    table: table.name.clone(),
                    partition: node_partition.partition_name.clone(),
                    expected: partition.parameters.len(),
                    found: node_partition.arguments.len(),
                },
            );
            return;
        }

        // Check that all partition arguments refer to valid table fields
        for (arg_index, field_name) in node_partition.arguments.iter().enumerate() {
            let field_exists = table.fields.iter().any(|field_id| {
                let field = &self.program.fields[*field_id];
                field.field_name == *field_name
            });

            if !field_exists {
                self.error_at(
                    &table.span,
                    AstError::PartitionFieldNotInTable {
                        table: table.name.clone(),
                        partition: node_partition.partition_name.clone(),
                        field: field_name.clone(),
                    },
                );
                continue;
            }

            // Check type compatibility between partition parameter and table field
            if arg_index < partition.parameters.len() {
                let param_id = partition.parameters[arg_index];
                let param = &self.program.parameters[param_id];

                // Find the field type
                if let Some(field_id) = table.fields.iter().find(|field_id| {
                    let field = &self.program.fields[**field_id];
                    field.field_name == *field_name
                }) {
                    let field = &self.program.fields[*field_id];
                    if !self.types_compatible(&param.param_type, &field.field_type) {
                        self.error_at(
                            &table.span,
                            AstError::PartitionArgumentTypeMismatch {
                                partition: node_partition.partition_name.clone(),
                                param_index: arg_index,
                                expected: param.param_type.clone(),
                                found: field.field_type.clone(),
                            },
                        );
                    }
                }
            }
        }
    }

    /// Validates table access patterns (placeholder for future node access validation)
    fn validate_table_access(&mut self, _table: &TableDeclaration, _span: &Span) {
        // For now, this is a placeholder. In the future, this could validate:
        // - Cross-node access restrictions
        // - Partition-based access control
        // - Transaction isolation requirements
    }

    /// Checks all root functions in the program.
    fn check_functions(&mut self) {
        for func_id in &self.program.root_functions {
            self.check_function(*func_id);
        }
    }

    /// Checks a single function, including all hops and return requirements.
    fn check_function(&mut self, func_id: FunctionId) {
        let func = &self.program.functions[func_id];

        self.current_function = Some(func_id);
        self.return_type = Some(func.return_type.clone());
        self.has_return = false;

        // Check each hop
        for (hop_index, hop_id) in func.hops.iter().enumerate() {
            self.check_hop_block(*hop_id, hop_index, &func.name);
        }

        // Check return requirements
        if matches!(func.return_type, ReturnType::Type(_)) && !self.has_return {
            self.error_at(&func.span, AstError::MissingReturn(func.name.clone()));
        }

        self.current_function = None;
    }

    /// Checks a hop block, including all statements within it.
    fn check_hop_block(&mut self, hop_id: HopId, hop_index: usize, function_name: &str) {
        let hop = &self.program.hops[hop_id];

        self.current_hop = Some(hop_id);

        // Check each statement in the hop
        for stmt_id in &hop.statements {
            self.check_statement(*stmt_id, hop_index, function_name);
        }

        self.current_hop = None;
    }

    /// Checks a statement for semantic correctness.
    ///
    /// This dispatches to the appropriate check based on statement kind.
    fn check_statement(&mut self, stmt_id: StatementId, hop_index: usize, function_name: &str) {
        let stmt = &self.program.statements[stmt_id];

        match &stmt.node {
            StatementKind::Assignment(a) => self.check_assignment(a, &stmt.span),
            StatementKind::IfStmt(i) => {
                self.check_if_statement(i, &stmt.span, hop_index, function_name)
            }
            StatementKind::ForStmt(f) => {
                self.check_for_statement(f, &stmt.span, hop_index, function_name)
            }
            StatementKind::WhileStmt(w) => {
                self.check_while_statement(w, &stmt.span, hop_index, function_name)
            }
            StatementKind::VarDecl(v) => self.check_var_decl(v, &stmt.span),
            StatementKind::Return(r) => self.check_return_statement(r, &stmt.span),
            StatementKind::Abort(_) => {
                self.check_abort_statement(&stmt.span, hop_index, function_name)
            }
            StatementKind::Break(_) => self.check_break_statement(&stmt.span),
            StatementKind::Continue(_) => self.check_continue_statement(&stmt.span),
            StatementKind::Empty => {}
        }
    }

    fn check_abort_statement(&mut self, span: &Span, hop_index: usize, function_name: &str) {
        if hop_index != 0 {
            self.error_at(
                span,
                AstError::AbortNotInFirstHop {
                    function: function_name.to_string(),
                    hop_index,
                },
            );
        }
    }

    fn check_for_statement(
        &mut self,
        for_stmt: &ForStatement,
        _span: &Span,
        hop_index: usize,
        function_name: &str,
    ) {
        // Check initialization, condition, and increment expressions
        self.check_expression(for_stmt.init);

        if let Some(cond_type) = self.check_expression(for_stmt.condition) {
            if !self.is_bool_type(&cond_type) {
                let cond_expr = &self.program.expressions[for_stmt.condition];
                self.error_at(&cond_expr.span, AstError::InvalidCondition(cond_type));
            }
        }

        self.check_expression(for_stmt.increment);

        // Set loop context
        let previous_in_loop = self.in_loop;
        self.in_loop = true;

        // Check body
        for stmt_id in &for_stmt.body {
            self.check_statement(*stmt_id, hop_index, function_name);
        }

        // Restore loop context
        self.in_loop = previous_in_loop;
    }

    fn check_if_statement(
        &mut self,
        if_stmt: &IfStatement,
        _span: &Span,
        hop_index: usize,
        function_name: &str,
    ) {
        // Check condition
        if let Some(cond_type) = self.check_expression(if_stmt.condition) {
            if !self.is_bool_type(&cond_type) {
                let cond_expr = &self.program.expressions[if_stmt.condition];
                self.error_at(&cond_expr.span, AstError::InvalidCondition(cond_type));
            }
        }

        // Check then branch
        for stmt_id in &if_stmt.then_branch {
            self.check_statement(*stmt_id, hop_index, function_name);
        }

        // Check else branch if present
        if let Some(else_branch) = &if_stmt.else_branch {
            for stmt_id in else_branch {
                self.check_statement(*stmt_id, hop_index, function_name);
            }
        }
    }

    fn check_while_statement(
        &mut self,
        while_stmt: &WhileStatement,
        _span: &Span,
        hop_index: usize,
        function_name: &str,
    ) {
        // Check condition
        if let Some(cond_type) = self.check_expression(while_stmt.condition) {
            if !self.is_bool_type(&cond_type) {
                let cond_expr = &self.program.expressions[while_stmt.condition];
                self.error_at(&cond_expr.span, AstError::InvalidCondition(cond_type));
            }
        }

        // Set loop context
        let previous_in_loop = self.in_loop;
        self.in_loop = true;

        // Check body
        for stmt_id in &while_stmt.body {
            self.check_statement(*stmt_id, hop_index, function_name);
        }

        // Restore loop context
        self.in_loop = previous_in_loop;
    }

    fn check_break_statement(&mut self, span: &Span) {
        if !self.in_loop {
            self.error_at(span, AstError::BreakOutsideLoop);
        }
    }

    fn check_continue_statement(&mut self, span: &Span) {
        if !self.in_loop {
            self.error_at(span, AstError::ContinueOutsideLoop);
        }
    }

    fn check_assignment(&mut self, assign: &AssignmentStatement, span: &Span) {
        // Check RHS expression
        self.check_expression(assign.rhs);

        // Check LValue
        self.check_lvalue(&assign.lvalue, span);
    }

    fn check_lvalue(&mut self, lvalue: &LValue, span: &Span) {
        match lvalue {
            LValue::Var { name, resolved_var } => {
                if resolved_var.is_none() {
                    self.error_at(span, AstError::UndeclaredVariable(name.clone()));
                }
            }
            LValue::TableField {
                table_name,
                pk_exprs,
                resolved_table,
                resolved_pk_fields,
                resolved_field,
                ..
            } => {
                // Check all primary key expressions
                for &pk_expr in pk_exprs {
                    self.check_expression(pk_expr);
                }

                if resolved_table.is_none() {
                    self.error_at(span, AstError::UndeclaredTable(table_name.clone()));
                    return;
                }

                let table_id = resolved_table.unwrap();
                let table = &self.program.tables[table_id];

                // Check that all primary key fields are resolved
                let all_pk_fields_resolved = resolved_pk_fields.iter().all(|opt| opt.is_some());
                if !all_pk_fields_resolved {
                    self.error_at(
                        span,
                        AstError::ParseError(format!(
                            "Some primary key fields for table {} are not resolved",
                            table_name
                        )),
                    );
                    return;
                }

                // Check that we have the right number of primary keys
                if table.primary_keys.len() != resolved_pk_fields.len() {
                    self.error_at(
                        span,
                        AstError::ParseError(format!(
                            "Table {} requires {} primary key values, but {} were provided",
                            table_name,
                            table.primary_keys.len(),
                            resolved_pk_fields.len()
                        )),
                    );
                    return;
                }

                // Check that the target field is resolved
                if resolved_field.is_none() {
                    self.error_at(
                        span,
                        AstError::UndeclaredField {
                            table: table_name.clone(),
                            field: "unknown".to_string(),
                        },
                    );
                }
            }
            LValue::TableRecord {
                table_name,
                pk_exprs,
                resolved_table,
                resolved_pk_fields,
                ..
            } => {
                // Check all primary key expressions
                for &pk_expr in pk_exprs {
                    self.check_expression(pk_expr);
                }

                if resolved_table.is_none() {
                    self.error_at(span, AstError::UndeclaredTable(table_name.clone()));
                    return;
                }

                let table_id = resolved_table.unwrap();
                let table = &self.program.tables[table_id];

                // Check that all primary key fields are resolved
                let all_pk_fields_resolved = resolved_pk_fields.iter().all(|opt| opt.is_some());
                if !all_pk_fields_resolved {
                    self.error_at(
                        span,
                        AstError::ParseError(format!(
                            "Some primary key fields for table {} are not resolved",
                            table_name
                        )),
                    );
                    return;
                }

                // Check that we have the right number of primary keys
                if table.primary_keys.len() != resolved_pk_fields.len() {
                    self.error_at(
                        span,
                        AstError::ParseError(format!(
                            "Table {} requires {} primary key values, but {} were provided",
                            table_name,
                            table.primary_keys.len(),
                            resolved_pk_fields.len()
                        )),
                    );
                }
            }
            LValue::ArrayElement {
                array_name,
                index,
                resolved_var,
            } => {
                // Check index expression
                self.check_expression(*index);

                if resolved_var.is_none() {
                    self.error_at(span, AstError::UndeclaredVariable(array_name.clone()));
                }
            }
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDeclStatement, span: &Span) {
        // Check initializer expression type
        if let Some(init_type) = self.check_expression(var_decl.init_value) {
            if !self.types_compatible(&var_decl.var_type, &init_type) {
                self.error_at(
                    span,
                    AstError::TypeMismatch {
                        expected: var_decl.var_type.clone(),
                        found: init_type,
                    },
                );
            }
        }
    }

    /// Helper function to check if a type is boolean
    fn is_bool_type(&self, ty: &TypeName) -> bool {
        matches!(ty, TypeName::Bool)
    }

    /// Helper function to check if a type is numeric (int or float)
    fn is_numeric_type(&self, ty: &TypeName) -> bool {
        matches!(ty, TypeName::Int | TypeName::Float)
    }

    /// Helper function to check if a type is integer
    fn is_int_type(&self, ty: &TypeName) -> bool {
        matches!(ty, TypeName::Int)
    }

    /// Helper function to check if a type is float
    fn is_float_type(&self, ty: &TypeName) -> bool {
        matches!(ty, TypeName::Float)
    }

    fn check_return_statement(&mut self, ret_stmt: &ReturnStatement, span: &Span) {
        self.has_return = true;

        // Clone the return type to avoid borrowing conflicts
        let return_type = self.return_type.clone();

        match (&return_type, &ret_stmt.value) {
            (Some(ReturnType::Void), Some(_)) => {
                self.error_at(span, AstError::UnexpectedReturnValue);
            }
            (Some(ReturnType::Type(expected_type)), Some(expr_id)) => {
                if let Some(actual_type) = self.check_expression(*expr_id) {
                    if !self.types_compatible(expected_type, &actual_type) {
                        self.error_at(
                            span,
                            AstError::TypeMismatch {
                                expected: expected_type.clone(),
                                found: actual_type,
                            },
                        );
                    }
                }
            }
            (Some(ReturnType::Type(_)), None) => {
                self.error_at(span, AstError::MissingReturnValue);
            }
            (Some(ReturnType::Void), None) => {
                // Valid void return
            }
            (None, _) => {
                // Should not happen
            }
        }
    }

    fn check_expression(&mut self, expr_id: ExpressionId) -> Option<TypeName> {
        let expr = &self.program.expressions[expr_id];

        match &expr.node {
            ExpressionKind::Ident(name) => {
                // Check if this is a partition function call (should be caught by name resolver)
                // but let's validate it here as well
                if self.program.partition_map.contains_key(name) {
                    // This is a partition name used as identifier, which is invalid
                    // (partitions should be called as functions)
                    self.error_at(
                        &expr.span,
                        AstError::ParseError(format!(
                            "Partition '{}' cannot be used as a variable. Use it as a function call.",
                            name
                        )),
                    );
                    return None;
                }

                // Use name resolver's resolution to get the variable
                if let Some(var_id) = self.program.resolutions.get(&expr_id) {
                    let var = &self.program.variables[*var_id];
                    Some(var.ty.clone())
                } else {
                    // Name resolver should have caught this, but let's be safe
                    None
                }
            }
            ExpressionKind::IntLit(_) => Some(TypeName::Int),
            ExpressionKind::FloatLit(_) => Some(TypeName::Float),
            ExpressionKind::StringLit(_) => Some(TypeName::String),
            ExpressionKind::BoolLit(_) => Some(TypeName::Bool),
            ExpressionKind::TableFieldAccess {
                resolved_table,
                resolved_pk_fields,
                pk_exprs,
                resolved_field,
                table_name,
                field_name,
                ..
            } => {
                // Check all primary key expressions
                for pk_expr in pk_exprs {
                    self.check_expression(*pk_expr);
                }

                let table_id = resolved_table
                    .ok_or_else(|| {
                        let expr_span = expr.span.clone();
                        self.error_at(&expr_span, AstError::UndeclaredTable(table_name.clone()));
                    })
                    .ok()?;

                let table_obj = &self.program.tables[table_id];

                // Check that all primary key fields are resolved
                let all_pk_fields_resolved = resolved_pk_fields.iter().all(|opt| opt.is_some());

                if !all_pk_fields_resolved {
                    // Some primary key fields are not resolved, return None
                    return None;
                }

                // Validate each primary key field
                for (_, resolved_pk_field_opt) in resolved_pk_fields.iter().enumerate() {
                    if let Some(pk_field_id) = resolved_pk_field_opt {
                        // Check that this field is actually a primary key of this table
                        if !table_obj.primary_keys.contains(pk_field_id) {
                            let pk_field_obj = &self.program.fields[*pk_field_id];
                            let expr_span = expr.span.clone();
                            self.error_at(
                                &expr_span,
                                AstError::InvalidPrimaryKey {
                                    table: table_obj.name.clone(),
                                    column: pk_field_obj.field_name.clone(),
                                },
                            );
                            return None;
                        }
                    }
                }

                // Check that all table primary keys are provided
                if table_obj.primary_keys.len() != resolved_pk_fields.len() {
                    let expr_span = expr.span.clone();
                    self.error_at(
                        &expr_span,
                        AstError::ParseError(format!(
                            "Table {} requires {} primary key values, but {} were provided",
                            table_obj.name,
                            table_obj.primary_keys.len(),
                            resolved_pk_fields.len()
                        )),
                    );
                    return None;
                }

                // Check accessed field exists and return its type
                let field_id = resolved_field
                    .ok_or_else(|| {
                        let expr_span = expr.span.clone();
                        self.error_at(
                            &expr_span,
                            AstError::UndeclaredField {
                                table: table_name.clone(),
                                field: field_name.clone(),
                            },
                        );
                    })
                    .ok()?;

                let accessed_field = &self.program.fields[field_id];

                // Additional validation: Check if table access is allowed based on partitioning
                self.validate_table_access(table_obj, &expr.span);

                Some(accessed_field.field_type.clone())
            }
            ExpressionKind::TableAccess {
                resolved_table,
                resolved_pk_fields: _,
                pk_exprs,
                table_name,
                ..
            } => {
                // Check all primary key expressions
                for pk_expr in pk_exprs {
                    self.check_expression(*pk_expr);
                }

                if let Some(table_id) = resolved_table {
                    let table_obj = &self.program.tables[*table_id];
                    // Return table type
                    Some(TypeName::Table(table_obj.name.clone()))
                } else {
                    let expr_span = expr.span.clone();
                    self.error_at(&expr_span, AstError::UndeclaredTable(table_name.clone()));
                    None
                }
            }
            ExpressionKind::ArrayAccess {
                resolved_var,
                array_name,
                index,
                ..
            } => {
                // Check the index expression
                self.check_expression(*index);

                if let Some(var_id) = resolved_var {
                    let var = &self.program.variables[*var_id];
                    match &var.ty {
                        TypeName::Array { element_type, .. } => {
                            // Return the element type
                            Some((**element_type).clone())
                        }
                        _ => {
                            let expr_span = expr.span.clone();
                            self.error_at(
                                &expr_span,
                                AstError::ParseError(format!(
                                    "Variable {} is not an array",
                                    array_name
                                )),
                            );
                            None
                        }
                    }
                } else {
                    let expr_span = expr.span.clone();
                    self.error_at(&expr_span, AstError::UndeclaredVariable(array_name.clone()));
                    None
                }
            }
            ExpressionKind::RecordLiteral { fields, .. } => {
                // Check each field assignment expression
                for field_assign in fields {
                    self.check_expression(field_assign.value);
                }
                // For now, return None - we'd need more context to determine record type
                None
            }
            ExpressionKind::UnaryOp {
                op,
                expr: inner_expr,
                ..
            } => {
                let Some(operand_type) = self.check_expression(*inner_expr) else {
                    return None;
                };

                match op {
                    UnaryOp::Neg => {
                        if self.is_numeric_type(&operand_type) {
                            Some(operand_type)
                        } else {
                            let expr_span = expr.span.clone();
                            self.error_at(
                                &expr_span,
                                AstError::InvalidUnaryOp {
                                    op: "negation".to_string(),
                                    operand: operand_type,
                                },
                            );
                            None
                        }
                    }
                    UnaryOp::Not => {
                        if self.is_bool_type(&operand_type) {
                            Some(TypeName::Bool)
                        } else {
                            let expr_span = expr.span.clone();
                            self.error_at(
                                &expr_span,
                                AstError::InvalidUnaryOp {
                                    op: "logical not".to_string(),
                                    operand: operand_type,
                                },
                            );
                            None
                        }
                    }
                }
            }
            ExpressionKind::BinaryOp {
                left, op, right, ..
            } => {
                let left_type = self.check_expression(*left)?;
                let right_type = self.check_expression(*right)?;
                let expr_span = expr.span.clone();
                self.check_binary_op(op, &left_type, &right_type, &expr_span)
            }
        }
    }
    fn check_binary_op(
        &mut self,
        op: &BinaryOp,
        left: &TypeName,
        right: &TypeName,
        span: &Span,
    ) -> Option<TypeName> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                if self.is_numeric_type(left) && self.is_numeric_type(right) {
                    if self.is_float_type(left) || self.is_float_type(right) {
                        Some(TypeName::Float)
                    } else {
                        Some(TypeName::Int)
                    }
                } else {
                    self.error_at(
                        span,
                        AstError::InvalidBinaryOp {
                            op: format!("{:?}", op),
                            left: left.clone(),
                            right: right.clone(),
                        },
                    );
                    None
                }
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if self.types_compatible(left, right) {
                    Some(TypeName::Bool)
                } else {
                    self.error_at(
                        span,
                        AstError::InvalidBinaryOp {
                            op: format!("{:?}", op),
                            left: left.clone(),
                            right: right.clone(),
                        },
                    );
                    None
                }
            }
            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                if self.is_numeric_type(left) && self.is_numeric_type(right) {
                    Some(TypeName::Bool)
                } else {
                    self.error_at(
                        span,
                        AstError::InvalidBinaryOp {
                            op: format!("{:?}", op),
                            left: left.clone(),
                            right: right.clone(),
                        },
                    );
                    None
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                if self.is_bool_type(left) && self.is_bool_type(right) {
                    Some(TypeName::Bool)
                } else {
                    self.error_at(
                        span,
                        AstError::InvalidBinaryOp {
                            op: format!("{:?}", op),
                            left: left.clone(),
                            right: right.clone(),
                        },
                    );
                    None
                }
            }
        }
    }

    fn error_at(&mut self, span: &Span, error: AstError) {
        self.errors.push(SpannedError {
            error,
            span: Some(span.clone()),
        });
    }

    fn types_compatible(&self, expected: &TypeName, actual: &TypeName) -> bool {
        expected == actual || (self.is_float_type(expected) && self.is_int_type(actual))
    }
}

/// Public interface for semantic analysis.
pub fn analyze_program(program: &Program) -> Results<()> {
    let analyzer = SemanticAnalyzer::new(program);
    analyzer.analyze()
}

/// Analyze program and infer types, updating the AST with resolved types
pub fn analyze_program_with_types(program: &mut Program) -> Results<()> {
    // First do the regular analysis without mutation
    {
        let analyzer = SemanticAnalyzer::new(program);
        analyzer.analyze()?;
    }

    // For now, we don't do additional type inference
    // This could be expanded in the future

    Ok(())
}
