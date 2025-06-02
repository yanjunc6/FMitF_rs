use crate::ast::*;
use std::collections::HashMap;

pub struct NameResolver<'p> {
    program: &'p mut Program,
    errors: Vec<SpannedError>,

    // Current resolution context
    current_function: Option<FunctionId>,
    current_scope: Option<ScopeId>,
    current_function_scope_id: Option<ScopeId>,

    // Scope stack for nested blocks
    scope_stack: Vec<ScopeId>,
}

impl<'p> NameResolver<'p> {
    pub fn new(program: &'p mut Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
            current_function: None,
            current_scope: None,
            current_function_scope_id: None,
            scope_stack: Vec::new(),
        }
    }

    pub fn resolve(mut self) -> Results<()> {
        // Create global scope first
        let global_scope = self.program.scopes.alloc(Scope {
            parent: None,
            kind: ScopeKind::Global,
            variables: HashMap::new(),
        });
        self.program.global_scope = Some(global_scope);
        self.current_scope = Some(global_scope);
        self.scope_stack.push(global_scope);

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

    fn resolve_function(&mut self, func_id: FunctionId) {
        self.current_function = Some(func_id);

        // Create function scope
        let func_scope = self.program.scopes.alloc(Scope {
            parent: self.current_scope,
            kind: ScopeKind::Function(func_id),
            variables: HashMap::new(),
        });
        self.current_function_scope_id = Some(func_scope);

        self.push_scope(func_scope);

        // Add parameters to function scope
        let param_ids: Vec<ParameterId> = self.program.functions[func_id]
            .parameters
            .iter()
            .copied()
            .collect();

        // Collect parameter data (name, type, span) to avoid borrowing issues
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
            // Pass func_scope explicitly
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
        self.current_function_scope_id = None; // Clear current function's scope ID
        self.current_function = None;
    }

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

        // Create hop scope and continue with existing logic
        let hop_scope = self.program.scopes.alloc(Scope {
            parent: self.current_scope,
            kind: ScopeKind::Hop(hop_id),
            variables: HashMap::new(),
        });

        self.push_scope(hop_scope);

        // Resolve all statements in the hop
        let stmt_ids: Vec<_> = self.program.hops[hop_id]
            .statements
            .iter()
            .copied()
            .collect();
        for stmt_id in stmt_ids {
            self.resolve_statement(stmt_id);
        }

        self.pop_scope();
    }

    fn resolve_statement(&mut self, stmt_id: StatementId) {
        let (stmt_kind, stmt_span) = {
            let stmt = &self.program.statements[stmt_id];
            (stmt.node.clone(), stmt.span.clone())
        };

        match stmt_kind {
            StatementKind::VarDecl(var_decl) => {
                // Resolve initializer first
                self.resolve_expression(var_decl.init_value);

                let target_scope_id = if var_decl.is_global {
                    self.current_function_scope_id.expect("Internal error: 'global' keyword used for a variable not inside a function, or function scope not set.")
                } else {
                    self.current_scope
                        .expect("Internal error: Variable declared outside of any scope context.")
                };

                // Check for duplicate in the target scope
                let target_scope_obj = &self.program.scopes[target_scope_id];
                if target_scope_obj.variables.contains_key(&var_decl.var_name) {
                    self.error_at(
                        &stmt_span,
                        AstError::DuplicateVariable(var_decl.var_name.clone()),
                    );
                    return;
                }

                // Declare the variable in the determined target scope
                self.declare_variable(
                    &var_decl.var_name,
                    var_decl.var_type,
                    VarKind::Local, // 'is_global' here means function-scoped local, not file-level global
                    stmt_span.clone(),
                    target_scope_id,
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
                // First resolve expressions to avoid borrowing conflicts
                self.resolve_expression(assign.pk_expr);
                self.resolve_expression(assign.rhs);

                // Clone the assign to avoid borrowing issues
                let mut assign_copy = assign.clone();

                // Look up table and collect error information without holding borrows
                let table_lookup_result =
                    self.program.table_map.get(&assign_copy.table_name).copied();

                match table_lookup_result {
                    Some(table_id) => {
                        assign_copy.resolved_table = Some(table_id);

                        // Collect field information without holding borrows
                        let (pk_field_result, field_result) = {
                            let table = &self.program.tables[table_id];

                            let pk_field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name
                                        == assign_copy.pk_field_name
                                })
                                .copied();

                            let field_id = table
                                .fields
                                .iter()
                                .find(|&&field_id| {
                                    self.program.fields[field_id].field_name
                                        == assign_copy.field_name
                                })
                                .copied();

                            (pk_field_id, field_id)
                        };

                        // Now handle results and report errors
                        match pk_field_result {
                            Some(pk_field_id) => {
                                assign_copy.resolved_pk_field = Some(pk_field_id);
                            }
                            None => {
                                self.error_at(
                                    &stmt_span,
                                    AstError::UndeclaredField {
                                        table: assign_copy.table_name.clone(),
                                        field: assign_copy.pk_field_name.clone(),
                                    },
                                );
                            }
                        }

                        match field_result {
                            Some(field_id) => {
                                assign_copy.resolved_field = Some(field_id);
                            }
                            None => {
                                self.error_at(
                                    &stmt_span,
                                    AstError::UndeclaredField {
                                        table: assign_copy.table_name.clone(),
                                        field: assign_copy.field_name.clone(),
                                    },
                                );
                            }
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

    fn resolve_block(&mut self, statements: &[StatementId]) {
        // Create a new block scope
        let block_scope = self.program.scopes.alloc(Scope {
            parent: self.current_scope,
            kind: ScopeKind::Block,
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
                pk_field_name,
                pk_expr,
                field_name,
                ..
            } => {
                // Resolve table name
                if let Some(&table_id) = self.program.table_map.get(&table_name) {
                    let table = &self.program.tables[table_id];

                    // Find fields and update the expression
                    let pk_field_id = table
                        .fields
                        .iter()
                        .find(|&&field_id| {
                            self.program.fields[field_id].field_name == pk_field_name
                        })
                        .copied();

                    let field_id = table
                        .fields
                        .iter()
                        .find(|&&field_id| self.program.fields[field_id].field_name == field_name)
                        .copied();

                    // Update the expression with resolved IDs
                    if let ExpressionKind::TableFieldAccess {
                        ref mut resolved_table,
                        ref mut resolved_pk_field,
                        ref mut resolved_field,
                        ..
                    } = &mut self.program.expressions[expr_id].node
                    {
                        *resolved_table = Some(table_id);
                        *resolved_pk_field = pk_field_id;
                        *resolved_field = field_id;
                    }

                    if pk_field_id.is_none() {
                        self.error_at(
                            &expr_span,
                            AstError::UndeclaredField {
                                table: table_name.clone(),
                                field: pk_field_name,
                            },
                        );
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

                self.resolve_expression(pk_expr);
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

    fn push_scope(&mut self, scope_id: ScopeId) {
        self.current_scope = Some(scope_id);
        self.scope_stack.push(scope_id);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
        self.current_scope = self.scope_stack.last().copied();
    }

    fn error_at(&mut self, span: &Span, error: AstError) {
        self.errors.push(SpannedError {
            error,
            span: Some(span.clone()),
        });
    }
}

/// Public interface for name resolution
pub fn resolve_names(program: &mut Program) -> Results<()> {
    let resolver = NameResolver::new(program);
    resolver.resolve()
}
