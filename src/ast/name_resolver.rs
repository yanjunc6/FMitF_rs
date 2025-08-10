//! The `name_resolver` module handles name resolution within the AST.
//! It ensures that all identifiers (variables, tables, partitions, etc.) are properly declared
//! and resolves their references within the program.
//!
//! # Overview
//!
//! - **NameResolver**: The main struct responsible for resolving names and managing scopes.
//! - **resolve_names**: Public interface for performing name resolution on a `Program`.
//!
//! # Features
//!
//! - Scope management for variables and functions.
//! - Error reporting for undeclared identifiers and duplicate declarations.
//! - Resolution of table field references and partition function calls.
//!
//! # Usage
//!
//! Use the `resolve_names` function to perform name resolution:
//!
//! ```rust
//! use crate::ast::name_resolver::resolve_names;
//! use crate::ast::Program;
//!
//! let mut program = Program::new();
//! resolve_names(&mut program).expect("Name resolution failed");
//! ```

use crate::ast::*;
use std::collections::HashMap;

pub struct NameResolver<'p> {
    program: &'p mut Program,
    errors: Vec<SpannedError>,

    // Symbol table - stack of scopes
    scope_stack: Vec<ScopeId>,
    current_scope: Option<ScopeId>,
}

impl<'p> NameResolver<'p> {
    /// Creates a new `NameResolver` instance.
    pub fn new(program: &'p mut Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
            scope_stack: Vec::new(),
            current_scope: None,
        }
    }

    /// Resolves all names in the program.
    ///
    /// This function iterates over all root functions and resolves their names,
    /// including parameters, hops, and statements.
    pub fn resolve(mut self) -> Results<()> {
        // Resolve table partitions first
        self.resolve_table_partitions();

        // Resolve all partition functions
        let partition_ids: Vec<_> = self.program.root_partitions.iter().copied().collect();
        for partition_id in partition_ids {
            self.resolve_partition(partition_id);
        }

        // Resolve all functions
        let function_ids: Vec<_> = self.program.root_functions.iter().copied().collect();
        for func_id in function_ids {
            self.resolve_function(func_id);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }

    /// Resolves partition references in table declarations.
    fn resolve_table_partitions(&mut self) {
        // Build a map of partition names to partition IDs
        let mut partition_map = HashMap::new();
        for &partition_id in &self.program.root_partitions {
            let partition = &self.program.partitions[partition_id];
            partition_map.insert(partition.name.clone(), partition_id);
        }

        // Resolve partition references in tables
        for &table_id in &self.program.root_tables {
            let table = &mut self.program.tables[table_id];
            if let Some(node_partition) = &mut table.node_partition {
                if let Some(&partition_id) = partition_map.get(&node_partition.partition_name) {
                    node_partition.resolved_partition = Some(partition_id);
                }
                // Note: If partition is not found, resolved_partition remains None
                // This will be caught later in semantic analysis
            }
        }
    }

    /// Resolves names within a partition function.
    ///
    /// This includes creating a scope for the partition, resolving parameters,
    /// and resolving the implementation expression if it exists.
    fn resolve_partition(&mut self, partition_id: PartitionId) {
        // Only resolve partitions that have implementations
        let implementation_expr = self.program.partitions[partition_id].implementation;
        if implementation_expr.is_none() {
            return;
        }

        // Create partition scope
        let partition_scope = self.program.scopes.alloc(Scope {
            parent: None,
            variables: HashMap::new(),
        });

        self.push_scope(partition_scope);

        // Add parameters to partition scope
        let param_ids: Vec<ParameterId> = self.program.partitions[partition_id]
            .parameters
            .iter()
            .copied()
            .collect();

        // Collect parameter data to avoid borrowing issues
        let params_to_declare: Vec<(String, TypeName, Span)> = param_ids
            .iter()
            .map(|&param_id| {
                let param_decl = &self.program.parameters[param_id];
                (
                    param_decl.param_name.clone(),
                    param_decl.param_type.clone(),
                    param_decl.span.clone(),
                )
            })
            .collect();

        for (name, ty, span) in params_to_declare {
            self.declare_variable(&name, ty, VarKind::Parameter, span, partition_scope);
        }

        // Resolve implementation expression
        if let Some(impl_expr_id) = implementation_expr {
            self.resolve_expression(impl_expr_id);
        }

        self.pop_scope();
    }

    /// Resolves names within a function.
    ///
    /// This includes creating a scope for the function, resolving parameters,
    /// and resolving hops and statements within the function.
    fn resolve_function(&mut self, func_id: FunctionId) {
        // Create function scope (top-level scope for this function)
        let func_scope = self.program.scopes.alloc(Scope {
            parent: None,
            variables: HashMap::new(),
        });

        self.push_scope(func_scope);

        // Add parameters to function scope
        let param_ids: Vec<ParameterId> = self.program.functions[func_id]
            .parameters
            .iter()
            .copied()
            .collect();

        // Collect parameter data to avoid borrowing issues
        let params_to_declare: Vec<(String, TypeName, Span)> = param_ids
            .iter()
            .map(|&param_id| {
                let param_decl = &self.program.parameters[param_id];
                (
                    param_decl.param_name.clone(),
                    param_decl.param_type.clone(),
                    param_decl.span.clone(),
                )
            })
            .collect();

        for (name, ty, span) in params_to_declare {
            self.declare_variable(&name, ty, VarKind::Parameter, span, func_scope);
        }

        // Resolve each hop
        let hop_ids: Vec<_> = self.program.functions[func_id]
            .hops
            .iter()
            .copied()
            .collect();
        for hop_id in hop_ids {
            self.resolve_hop(hop_id);
        }

        self.pop_scope();
    }

    /// Resolves names within a hop block.
    ///
    /// This includes resolving statements within the hop.
    fn resolve_hop(&mut self, hop_id: HopId) {
        let (hop_type, hop_span, stmt_ids) = {
            let hop = &self.program.hops[hop_id];
            (
                hop.hop_type.clone(),
                hop.span.clone(),
                hop.statements.clone(),
            )
        };

        // Check if this is a ForLoop hop that introduces a loop variable
        if let HopType::ForLoop {
            loop_var,
            loop_var_type,
            ..
        } = hop_type
        {
            // ForLoop hops create their own scope for each hop instance
            let hop_scope = self.program.scopes.alloc(Scope {
                parent: self.current_scope,
                variables: HashMap::new(),
            });

            self.push_scope(hop_scope);

            // Create the loop variable in the hop's scope
            self.declare_variable(
                &loop_var,
                loop_var_type,
                VarKind::Local,
                hop_span,
                hop_scope,
            );

            // Resolve statements in the hop's scope
            for stmt_id in stmt_ids {
                self.resolve_statement(stmt_id);
            }

            self.pop_scope();
        } else {
            // Regular hops do NOT create their own scopes - resolve statements in current function scope
            for stmt_id in stmt_ids {
                self.resolve_statement(stmt_id);
            }
        }
    }

    /// Resolves names within a statement.
    ///
    /// This includes resolving variables, assignments, and expressions.
    fn resolve_statement(&mut self, stmt_id: StatementId) {
        let (stmt_kind, stmt_span) = {
            let stmt = &self.program.statements[stmt_id];
            (stmt.node.clone(), stmt.span.clone())
        };

        match stmt_kind {
            StatementKind::VarDecl(var_decl) => {
                // Resolve initializer first, if present
                if let Some(init_expr_id) = var_decl.init_value {
                    self.resolve_expression(init_expr_id);
                }

                // Check for duplicate in the current scope (no shadowing within same scope)
                if let Some(current_scope_id) = self.current_scope {
                    let current_scope = &self.program.scopes[current_scope_id];
                    if current_scope.variables.contains_key(&var_decl.var_name) {
                        self.error_at(
                            &stmt_span,
                            AstError::DuplicateVariable(var_decl.var_name.clone()),
                        );
                        return;
                    }
                }

                // Declare the variable in the current scope
                let current_scope_id = self.current_scope.unwrap();
                self.declare_variable(
                    &var_decl.var_name,
                    var_decl.var_type.clone(),
                    VarKind::Local,
                    stmt_span.clone(),
                    current_scope_id,
                );
            }
            StatementKind::Assignment(assign) => {
                // Resolve RHS expression first
                self.resolve_expression(assign.rhs);

                // Resolve the LValue
                self.resolve_lvalue(&assign.lvalue, &stmt_span, stmt_id);
            }
            StatementKind::IfStmt(if_stmt) => {
                self.resolve_expression(if_stmt.condition);
                self.resolve_block(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.resolve_block(else_branch);
                }
            }
            StatementKind::ForStmt(for_stmt) => {
                // For statements create their own scope for the loop variable
                let for_scope = self.program.scopes.alloc(Scope {
                    parent: self.current_scope,
                    variables: HashMap::new(),
                });

                self.push_scope(for_scope);

                // Declare the loop variable
                self.declare_variable(
                    &for_stmt.loop_var,
                    for_stmt.loop_var_type.clone(),
                    VarKind::Local,
                    stmt_span.clone(),
                    for_scope,
                );

                // Resolve expressions
                self.resolve_expression(for_stmt.init);
                self.resolve_expression(for_stmt.condition);
                self.resolve_expression(for_stmt.increment);

                // Resolve body
                self.resolve_block(&for_stmt.body);

                self.pop_scope();
            }
            StatementKind::WhileStmt(while_stmt) => {
                self.resolve_expression(while_stmt.condition);
                self.resolve_block(&while_stmt.body);
            }
            StatementKind::Return(ret_stmt) => {
                if let Some(expr_id) = ret_stmt.value {
                    self.resolve_expression(expr_id);
                }
            }
            StatementKind::Expression(expr_stmt) => {
                self.resolve_expression(expr_stmt.expression);
            }
            StatementKind::Abort(_)
            | StatementKind::Break(_)
            | StatementKind::Continue(_)
            | StatementKind::Empty => {
                // No resolution needed
            }
        }
    }

    /// Resolves an LValue (left-hand side of assignment).
    fn resolve_lvalue(&mut self, lvalue: &LValue, stmt_span: &Span, stmt_id: StatementId) {
        match lvalue {
            LValue::Var { name, .. } => {
                // Look up the variable
                let var_id = self.lookup_variable(name);
                if var_id.is_none() {
                    self.error_at(stmt_span, AstError::UndeclaredVariable(name.clone()));
                } else {
                    // Update the statement with resolved variable
                    if let StatementKind::Assignment(ref mut assign) =
                        &mut self.program.statements[stmt_id].node
                    {
                        if let LValue::Var {
                            ref mut resolved_var,
                            ..
                        } = &mut assign.lvalue
                        {
                            *resolved_var = var_id;
                        }
                    }
                }
            }
            LValue::TableField {
                table_name,
                pk_exprs,
                pk_fields,
                field_name,
                ..
            } => {
                // First resolve all primary key expressions
                for &pk_expr in pk_exprs {
                    self.resolve_expression(pk_expr);
                }

                // Resolve table and field references
                self.resolve_table_field_access(
                    table_name, pk_fields, field_name, stmt_span, stmt_id,
                    false, // This is an assignment target
                );
            }
            LValue::TableRecord {
                table_name,
                pk_exprs,
                pk_fields,
                ..
            } => {
                // First resolve all primary key expressions
                for &pk_expr in pk_exprs {
                    self.resolve_expression(pk_expr);
                }

                // Resolve table references
                self.resolve_table_record_access(table_name, pk_fields, stmt_span, stmt_id);
            }
            LValue::ArrayElement {
                array_name, index, ..
            } => {
                // Resolve the index expression
                self.resolve_expression(*index);

                // Look up the array variable
                let var_id = self.lookup_variable(array_name);
                if var_id.is_none() {
                    self.error_at(stmt_span, AstError::UndeclaredVariable(array_name.clone()));
                } else {
                    // Update the statement with resolved variable
                    if let StatementKind::Assignment(ref mut assign) =
                        &mut self.program.statements[stmt_id].node
                    {
                        if let LValue::ArrayElement {
                            ref mut resolved_var,
                            ..
                        } = &mut assign.lvalue
                        {
                            *resolved_var = var_id;
                        }
                    }
                }
            }
            LValue::FieldAccess { object_name, .. } => {
                // Look up the object variable
                let var_id = self.lookup_variable(object_name);
                if var_id.is_none() {
                    self.error_at(stmt_span, AstError::UndeclaredVariable(object_name.clone()));
                } else {
                    // Update the statement with resolved variable
                    if let StatementKind::Assignment(ref mut assign) =
                        &mut self.program.statements[stmt_id].node
                    {
                        if let LValue::FieldAccess {
                            ref mut resolved_var,
                            ..
                        } = &mut assign.lvalue
                        {
                            *resolved_var = var_id;
                        }
                    }
                }
            }
        }
    }

    /// Resolves table field access for assignments.
    fn resolve_table_field_access(
        &mut self,
        table_name: &str,
        pk_fields: &[String],
        field_name: &str,
        stmt_span: &Span,
        stmt_id: StatementId,
        _is_read: bool,
    ) {
        // Look up table
        let table_lookup_result = self.program.table_map.get(table_name).copied();

        match table_lookup_result {
            Some(table_id) => {
                // Resolve each primary key field
                let table = &self.program.tables[table_id];
                let mut missing_pk_fields = Vec::new();
                let mut resolved_pk_field_ids = vec![None; pk_fields.len()];

                for (i, pk_field_name) in pk_fields.iter().enumerate() {
                    let pk_field_id = table
                        .fields
                        .iter()
                        .find(|&&field_id| {
                            self.program.fields[field_id].field_name == *pk_field_name
                        })
                        .copied();

                    match pk_field_id {
                        Some(field_id) => {
                            resolved_pk_field_ids[i] = Some(field_id);
                        }
                        None => {
                            missing_pk_fields.push(pk_field_name.clone());
                        }
                    }
                }

                // Resolve the target field
                let field_id = table
                    .fields
                    .iter()
                    .find(|&&field_id| self.program.fields[field_id].field_name == *field_name)
                    .copied();

                let missing_target_field = if field_id.is_none() {
                    Some(field_name.to_string())
                } else {
                    None
                };

                // Report all errors after borrowing
                for field_name in missing_pk_fields {
                    self.error_at(
                        stmt_span,
                        AstError::UndeclaredField {
                            table: table_name.to_string(),
                            field: field_name,
                        },
                    );
                }

                if let Some(field_name) = missing_target_field {
                    self.error_at(
                        stmt_span,
                        AstError::UndeclaredField {
                            table: table_name.to_string(),
                            field: field_name,
                        },
                    );
                }

                // Update the statement with resolved IDs
                if let StatementKind::Assignment(ref mut assign) =
                    &mut self.program.statements[stmt_id].node
                {
                    if let LValue::TableField {
                        ref mut resolved_table,
                        ref mut resolved_pk_fields,
                        ref mut resolved_field,
                        ..
                    } = &mut assign.lvalue
                    {
                        *resolved_table = Some(table_id);
                        *resolved_pk_fields = resolved_pk_field_ids;
                        *resolved_field = field_id;
                    }
                }
            }
            None => {
                self.error_at(stmt_span, AstError::UndeclaredTable(table_name.to_string()));
            }
        }
    }

    /// Resolves table record access for assignments.
    fn resolve_table_record_access(
        &mut self,
        table_name: &str,
        pk_fields: &[String],
        stmt_span: &Span,
        stmt_id: StatementId,
    ) {
        // Look up table
        let table_lookup_result = self.program.table_map.get(table_name).copied();

        match table_lookup_result {
            Some(table_id) => {
                // Resolve each primary key field
                let table = &self.program.tables[table_id];
                let mut missing_pk_fields = Vec::new();
                let mut resolved_pk_field_ids = vec![None; pk_fields.len()];

                for (i, pk_field_name) in pk_fields.iter().enumerate() {
                    let pk_field_id = table
                        .fields
                        .iter()
                        .find(|&&field_id| {
                            self.program.fields[field_id].field_name == *pk_field_name
                        })
                        .copied();

                    match pk_field_id {
                        Some(field_id) => {
                            resolved_pk_field_ids[i] = Some(field_id);
                        }
                        None => {
                            missing_pk_fields.push(pk_field_name.clone());
                        }
                    }
                }

                // Report all errors after borrowing
                for field_name in missing_pk_fields {
                    self.error_at(
                        stmt_span,
                        AstError::UndeclaredField {
                            table: table_name.to_string(),
                            field: field_name,
                        },
                    );
                }

                // Update the statement with resolved IDs
                if let StatementKind::Assignment(ref mut assign) =
                    &mut self.program.statements[stmt_id].node
                {
                    if let LValue::TableRecord {
                        ref mut resolved_table,
                        ref mut resolved_pk_fields,
                        ..
                    } = &mut assign.lvalue
                    {
                        *resolved_table = Some(table_id);
                        *resolved_pk_fields = resolved_pk_field_ids;
                    }
                }
            }
            None => {
                self.error_at(stmt_span, AstError::UndeclaredTable(table_name.to_string()));
            }
        }
    }

    /// Resolves names within a block of statements.
    fn resolve_block(&mut self, statements: &[StatementId]) {
        // Create a new block scope
        let block_scope = self.program.scopes.alloc(Scope {
            parent: self.current_scope,
            variables: HashMap::new(),
        });

        self.push_scope(block_scope);

        // Resolve each statement
        let stmt_ids: Vec<_> = statements.iter().copied().collect();
        for stmt_id in stmt_ids {
            self.resolve_statement(stmt_id);
        }

        self.pop_scope();
    }

    /// Resolves names within an expression.
    fn resolve_expression(&mut self, expr_id: ExpressionId) {
        let (expr_kind, expr_span) = {
            let expr = &self.program.expressions[expr_id];
            (expr.node.clone(), expr.span.clone())
        };

        match expr_kind {
            ExpressionKind::Ident(name) => {
                if let Some(var_id) = self.lookup_variable(&name) {
                    // Store the resolution
                    self.program.resolutions.insert(expr_id, var_id);
                } else {
                    self.error_at(&expr_span, AstError::UndeclaredVariable(name));
                }
            }
            ExpressionKind::TableFieldAccess {
                table_name,
                pk_fields,
                pk_exprs,
                field_name,
                ..
            } => {
                // First resolve all primary key expressions
                for &pk_expr in &pk_exprs {
                    self.resolve_expression(pk_expr);
                }

                // Resolve table name
                if let Some(&table_id) = self.program.table_map.get(&table_name) {
                    let (resolved_pk_field_ids, missing_pk_fields, field_id) = {
                        let table = &self.program.tables[table_id];

                        // Resolve each primary key field
                        let mut resolved_pk_field_ids = vec![None; pk_fields.len()];
                        let mut missing_pk_fields = Vec::new();

                        for (i, pk_field_name) in pk_fields.iter().enumerate() {
                            let pk_field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name == *pk_field_name
                                })
                                .copied();

                            resolved_pk_field_ids[i] = pk_field_id;

                            if pk_field_id.is_none() {
                                missing_pk_fields.push(pk_field_name.clone());
                            }
                        }

                        // Resolve the target field
                        let field_id = table
                            .fields
                            .iter()
                            .find(|&&field_id| {
                                self.program.fields[field_id].field_name == field_name
                            })
                            .copied();

                        (resolved_pk_field_ids, missing_pk_fields, field_id)
                    };

                    // Report missing primary key fields after borrowing
                    for field_name in missing_pk_fields {
                        self.error_at(
                            &expr_span,
                            AstError::UndeclaredField {
                                table: table_name.clone(),
                                field: field_name,
                            },
                        );
                    }

                    // Update the expression with resolved IDs
                    if let ExpressionKind::TableFieldAccess {
                        ref mut resolved_table,
                        ref mut resolved_pk_fields,
                        ref mut resolved_field,
                        ..
                    } = &mut self.program.expressions[expr_id].node
                    {
                        *resolved_table = Some(table_id);
                        *resolved_pk_fields = resolved_pk_field_ids;
                        *resolved_field = field_id;
                    }

                    if field_id.is_none() {
                        self.error_at(
                            &expr_span,
                            AstError::UndeclaredField {
                                table: table_name.clone(),
                                field: field_name,
                            },
                        );
                    }
                } else {
                    self.error_at(&expr_span, AstError::UndeclaredTable(table_name));
                }
            }
            ExpressionKind::TableAccess {
                table_name,
                pk_fields,
                pk_exprs,
                ..
            } => {
                // First resolve all primary key expressions
                for &pk_expr in &pk_exprs {
                    self.resolve_expression(pk_expr);
                }

                // Resolve table name
                if let Some(&table_id) = self.program.table_map.get(&table_name) {
                    let (resolved_pk_field_ids, missing_pk_fields) = {
                        let table = &self.program.tables[table_id];

                        // Resolve each primary key field
                        let mut resolved_pk_field_ids = vec![None; pk_fields.len()];
                        let mut missing_pk_fields = Vec::new();

                        for (i, pk_field_name) in pk_fields.iter().enumerate() {
                            let pk_field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name == *pk_field_name
                                })
                                .copied();

                            resolved_pk_field_ids[i] = pk_field_id;

                            if pk_field_id.is_none() {
                                missing_pk_fields.push(pk_field_name.clone());
                            }
                        }

                        (resolved_pk_field_ids, missing_pk_fields)
                    };

                    // Report missing primary key fields after borrowing
                    for field_name in missing_pk_fields {
                        self.error_at(
                            &expr_span,
                            AstError::UndeclaredField {
                                table: table_name.clone(),
                                field: field_name,
                            },
                        );
                    }

                    // Update the expression with resolved IDs
                    if let ExpressionKind::TableAccess {
                        ref mut resolved_table,
                        ref mut resolved_pk_fields,
                        ..
                    } = &mut self.program.expressions[expr_id].node
                    {
                        *resolved_table = Some(table_id);
                        *resolved_pk_fields = resolved_pk_field_ids;
                    }
                } else {
                    self.error_at(&expr_span, AstError::UndeclaredTable(table_name));
                }
            }
            ExpressionKind::ArrayAccess {
                array_name, index, ..
            } => {
                // Resolve the index expression
                self.resolve_expression(index);

                // Look up the array variable
                if let Some(var_id) = self.lookup_variable(&array_name) {
                    // Update the expression with resolved variable
                    if let ExpressionKind::ArrayAccess {
                        ref mut resolved_var,
                        ..
                    } = &mut self.program.expressions[expr_id].node
                    {
                        *resolved_var = Some(var_id);
                    }
                } else {
                    self.error_at(&expr_span, AstError::UndeclaredVariable(array_name));
                }
            }
            ExpressionKind::RecordLiteral { fields, .. } => {
                // Resolve each field assignment expression
                for field_assign in &fields {
                    self.resolve_expression(field_assign.value);
                }
                // Note: Field names in record literals might need table context for resolution
                // For now, we only resolve the value expressions
            }
            ExpressionKind::ArrayLiteral { elements, .. } => {
                // Resolve each element expression
                for element_id in &elements {
                    self.resolve_expression(*element_id);
                }
            }
            ExpressionKind::UnaryOp { expr, .. } => {
                self.resolve_expression(expr);
            }
            ExpressionKind::BinaryOp { left, right, .. } => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            ExpressionKind::FieldAccess {
                object_name,
                field_name,
                ..
            } => {
                // Look up the object variable
                if let Some(var_id) = self.lookup_variable(&object_name) {
                    // Store the resolution for the object
                    self.program.resolutions.insert(expr_id, var_id);

                    // Resolve the field within the object's type
                    let field_id =
                        if let Some(object_type) = self.program.var_types.get(&var_id).cloned() {
                            self.resolve_field_in_type(&object_type, &field_name, &expr_span)
                        } else {
                            // Object type unknown - can't resolve field
                            None
                        };

                    // Update the AST with the resolved variable and field
                    if let ExpressionKind::FieldAccess {
                        ref mut resolved_var,
                        ref mut resolved_field,
                        ..
                    } = &mut self.program.expressions[expr_id].node
                    {
                        *resolved_var = Some(var_id);
                        *resolved_field = field_id;
                    }
                } else {
                    self.error_at(
                        &expr_span,
                        AstError::UndeclaredVariable(object_name.clone()),
                    );
                }
            }
            ExpressionKind::IntLit(_)
            | ExpressionKind::FloatLit(_)
            | ExpressionKind::StringLit(_)
            | ExpressionKind::BoolLit(_) => {
                // Literals need no resolution
            }
        }
    }

    /// Resolves a field within a given type.
    /// Returns the FieldId if the field exists in the type, None otherwise.
    fn resolve_field_in_type(
        &mut self,
        object_type: &TypeName,
        field_name: &str,
        span: &Span,
    ) -> Option<FieldId> {
        match object_type {
            TypeName::Table(table_name) => {
                // Look up the table and find the field
                if let Some(&table_id) = self.program.table_map.get(table_name) {
                    let table = &self.program.tables[table_id];

                    // Find the field in the table
                    let field_id = table
                        .fields
                        .iter()
                        .find(|&&field_id| self.program.fields[field_id].field_name == field_name)
                        .copied();

                    if field_id.is_none() {
                        self.error_at(
                            span,
                            AstError::UndeclaredField {
                                table: table_name.clone(),
                                field: field_name.to_string(),
                            },
                        );
                    }

                    field_id
                } else {
                    // Table not found - this should have been caught earlier
                    self.error_at(span, AstError::UndeclaredTable(table_name.clone()));
                    None
                }
            }
            TypeName::Array { .. } => {
                // Arrays don't have named fields, this is likely a programming error
                // Use a generic parse error for unsupported operations
                self.error_at(
                    span,
                    AstError::ParseError(format!(
                        "Field access '{}' not supported on array types",
                        field_name
                    )),
                );
                None
            }
            TypeName::Int | TypeName::Float | TypeName::String | TypeName::Bool => {
                // Primitive types don't have fields
                self.error_at(
                    span,
                    AstError::ParseError(format!(
                        "Field access '{}' not supported on primitive types",
                        field_name
                    )),
                );
                None
            }
        }
    }

    /// Declares a variable in the current scope.
    fn declare_variable(
        &mut self,
        name: &str,
        ty: TypeName,
        kind: VarKind,
        span: Span,
        target_scope_id: ScopeId,
    ) {
        let var_id = self.program.variables.alloc(VarDecl {
            name: name.to_string(),
            ty: ty.clone(),
            kind,
            defined_at: span,
            scope: target_scope_id, // Use the provided target_scope_id
        });

        // Add to target scope
        // The duplicate check should be done before calling this function.
        let scope = &mut self.program.scopes[target_scope_id];
        scope.variables.insert(name.to_string(), var_id);

        // Store type information
        self.program.var_types.insert(var_id, ty);
    }

    /// Looks up a variable in the current scope stack.
    fn lookup_variable(&self, name: &str) -> Option<VarId> {
        // Search through scope stack from current to global
        for &scope_id in self.scope_stack.iter().rev() {
            let scope = &self.program.scopes[scope_id];
            if let Some(&var_id) = scope.variables.get(name) {
                return Some(var_id);
            }
        }

        // If not found in scope stack, check global constants
        self.program.constant_map.get(name).copied()
    }

    /// Pushes a new scope onto the stack.
    fn push_scope(&mut self, scope_id: ScopeId) {
        self.current_scope = Some(scope_id);
        self.scope_stack.push(scope_id);
    }

    /// Pops the current scope from the stack.
    fn pop_scope(&mut self) {
        self.scope_stack.pop();
        self.current_scope = self.scope_stack.last().copied();
    }

    /// Records an error at a specific span.
    fn error_at(&mut self, span: &Span, error: AstError) {
        self.errors.push(SpannedError {
            error,
            span: Some(span.clone()),
        });
    }
}

/// Public interface for name resolution.
pub fn resolve_names(program: &mut Program) -> Results<()> {
    let resolver = NameResolver::new(program);
    resolver.resolve()
}
