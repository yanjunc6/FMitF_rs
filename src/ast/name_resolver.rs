//! The `name_resolver` module handles name resolution within the AST.
//! It ensures that all identifiers (variables, tables, nodes, etc.) are properly declared
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
//! - Resolution of cross-node references and primary key fields.
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

        // Resolve each hop (but don't create scope for hops)
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
    /// This includes resolving the node name and statements within the hop.
    fn resolve_hop(&mut self, hop_id: HopId) {
        // Resolve the node name to node ID
        let node_name = self.program.hops[hop_id].node_name.clone();
        if let Some(&node_id) = self.program.node_map.get(&node_name) {
            // Update the resolved_node field
            self.program.hops[hop_id].resolved_node = Some(node_id);
        } else {
            let hop_span = self.program.hops[hop_id].span.clone();
            self.error_at(&hop_span, AstError::UndeclaredNode(node_name));
            return;
        }

        // Hops do NOT create their own scopes - resolve statements in current function scope
        let stmt_ids: Vec<_> = self.program.hops[hop_id]
            .statements
            .iter()
            .copied()
            .collect();
        for stmt_id in stmt_ids {
            self.resolve_statement(stmt_id);
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
                // Resolve initializer first
                self.resolve_expression(var_decl.init_value);

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
                    var_decl.var_type,
                    VarKind::Local,
                    stmt_span.clone(),
                    current_scope_id,
                );
            }
            StatementKind::VarAssignment(var_assign) => {
                // Resolve RHS expression
                self.resolve_expression(var_assign.rhs);

                // Look up the variable
                let var_id = self.lookup_variable(&var_assign.var_name);
                if var_id.is_none() {
                    self.error_at(
                        &stmt_span,
                        AstError::UndeclaredVariable(var_assign.var_name.clone()),
                    );
                } else {
                    // Update the statement with resolved variable
                    if let StatementKind::VarAssignment(ref mut var_assign_mut) =
                        &mut self.program.statements[stmt_id].node
                    {
                        var_assign_mut.resolved_var = var_id;
                    }
                }
            }
            StatementKind::Assignment(assign) => {
                // First resolve all primary key expressions and RHS
                for &pk_expr in &assign.pk_exprs {
                    self.resolve_expression(pk_expr);
                }
                self.resolve_expression(assign.rhs);

                // Clone the assign to avoid borrowing issues
                let mut assign_copy = assign.clone();

                // Look up table and collect error information without holding borrows
                let table_lookup_result =
                    self.program.table_map.get(&assign_copy.table_name).copied();

                match table_lookup_result {
                    Some(table_id) => {
                        assign_copy.resolved_table = Some(table_id);

                        // Resolve each primary key field
                        let table = &self.program.tables[table_id];
                        let mut missing_pk_fields = Vec::new();
                        let mut missing_target_field = None;

                        for (i, pk_field_name) in assign_copy.pk_fields.iter().enumerate() {
                            let pk_field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name == *pk_field_name
                                })
                                .copied();

                            match pk_field_id {
                                Some(field_id) => {
                                    assign_copy.resolved_pk_fields[i] = Some(field_id);
                                }
                                None => {
                                    missing_pk_fields.push((i, pk_field_name.clone()));
                                }
                            }
                        }

                        // Resolve the target field
                        let field_id = table
                            .fields
                            .iter()
                            .find(|&&field_id| {
                                self.program.fields[field_id].field_name == assign_copy.field_name
                            })
                            .copied();

                        match field_id {
                            Some(field_id) => {
                                assign_copy.resolved_field = Some(field_id);
                            }
                            None => {
                                missing_target_field = Some(assign_copy.field_name.clone());
                            }
                        }

                        // Now report all errors after borrowing
                        for (_, field_name) in missing_pk_fields {
                            self.error_at(
                                &stmt_span,
                                AstError::UndeclaredField {
                                    table: assign_copy.table_name.clone(),
                                    field: field_name,
                                },
                            );
                        }

                        if let Some(field_name) = missing_target_field {
                            self.error_at(
                                &stmt_span,
                                AstError::UndeclaredField {
                                    table: assign_copy.table_name.clone(),
                                    field: field_name,
                                },
                            );
                        }

                        // Update the statement with resolved assignment
                        if let StatementKind::Assignment(ref mut assign_mut) =
                            &mut self.program.statements[stmt_id].node
                        {
                            *assign_mut = assign_copy;
                        }
                    }
                    None => {
                        self.error_at(
                            &stmt_span,
                            AstError::UndeclaredTable(assign_copy.table_name.clone()),
                        );
                    }
                }
            }
            StatementKind::MultiAssignment(multi_assign) => {
                // First resolve all primary key expressions and assignment RHS expressions
                for &pk_expr in &multi_assign.pk_exprs {
                    self.resolve_expression(pk_expr);
                }
                for assignment in &multi_assign.assignments {
                    self.resolve_expression(assignment.rhs);
                }

                // Clone the multi_assign to avoid borrowing issues
                let mut multi_assign_copy = multi_assign.clone();

                // Look up table
                let table_lookup_result =
                    self.program.table_map.get(&multi_assign_copy.table_name).copied();

                match table_lookup_result {
                    Some(table_id) => {
                        multi_assign_copy.resolved_table = Some(table_id);

                        // Resolve each primary key field
                        let table = &self.program.tables[table_id];
                        let mut missing_pk_fields = Vec::new();

                        for (i, pk_field_name) in multi_assign_copy.pk_fields.iter().enumerate() {
                            let pk_field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name == *pk_field_name
                                })
                                .copied();

                            match pk_field_id {
                                Some(field_id) => {
                                    multi_assign_copy.resolved_pk_fields[i] = Some(field_id);
                                }
                                None => {
                                    missing_pk_fields.push((i, pk_field_name.clone()));
                                }
                            }
                        }

                        // Resolve each assignment field
                        let mut missing_target_fields = Vec::new();
                        for assignment in &mut multi_assign_copy.assignments {
                            let field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name == assignment.field_name
                                })
                                .copied();

                            match field_id {
                                Some(field_id) => {
                                    assignment.resolved_field = Some(field_id);
                                }
                                None => {
                                    missing_target_fields.push(assignment.field_name.clone());
                                }
                            }
                        }

                        // Report all errors after borrowing
                        for (_, field_name) in missing_pk_fields {
                            self.error_at(
                                &stmt_span,
                                AstError::UndeclaredField {
                                    table: multi_assign_copy.table_name.clone(),
                                    field: field_name,
                                },
                            );
                        }

                        for field_name in missing_target_fields {
                            self.error_at(
                                &stmt_span,
                                AstError::UndeclaredField {
                                    table: multi_assign_copy.table_name.clone(),
                                    field: field_name,
                                },
                            );
                        }

                        // Update the statement with resolved multi-assignment
                        if let StatementKind::MultiAssignment(ref mut multi_assign_mut) =
                            &mut self.program.statements[stmt_id].node
                        {
                            *multi_assign_mut = multi_assign_copy;
                        }
                    }
                    None => {
                        self.error_at(
                            &stmt_span,
                            AstError::UndeclaredTable(multi_assign_copy.table_name.clone()),
                        );
                    }
                }
            }
            StatementKind::IfStmt(if_stmt) => {
                self.resolve_expression(if_stmt.condition);
                self.resolve_block(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.resolve_block(else_branch);
                }
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
            StatementKind::Abort(_)
            | StatementKind::Break(_)
            | StatementKind::Continue(_)
            | StatementKind::Empty => {
                // No resolution needed
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
            ExpressionKind::UnaryOp { expr, .. } => {
                self.resolve_expression(expr);
            }
            ExpressionKind::BinaryOp { left, right, .. } => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            ExpressionKind::IntLit(_)
            | ExpressionKind::FloatLit(_)
            | ExpressionKind::StringLit(_)
            | ExpressionKind::BoolLit(_) => {
                // Literals need no resolution
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
        None
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
