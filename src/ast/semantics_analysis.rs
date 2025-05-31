use crate::ast::*;

pub struct SemanticAnalyzer<'p> {
    program: &'p Program,
    errors: Vec<SpannedError>,
    
    // Current context
    current_function: Option<FunctionId>,
    current_hop: Option<HopId>,
    return_type: Option<ReturnType>,
    has_return: bool,
    current_node: Option<NodeId>,
    in_loop: bool,
}

impl<'p> SemanticAnalyzer<'p> {
    pub fn new(program: &'p Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
            current_function: None,
            current_hop: None,
            return_type: None,
            has_return: false,
            current_node: None,
            in_loop: false,
        }
    }

    pub fn analyze(mut self) -> Results<()> {
        self.check_functions();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }

    fn check_functions(&mut self) {
        for func_id in &self.program.root_functions {
            self.check_function(*func_id);
        }
    }

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

    fn check_hop_block(&mut self, hop_id: HopId, hop_index: usize, function_name: &str) {
        let hop = &self.program.hops[hop_id];
        
        self.current_hop = Some(hop_id);
        self.current_node = Some(hop.node);

        // Check each statement in the hop
        for stmt_id in &hop.statements {
            self.check_statement(*stmt_id, hop_index, function_name);
        }

        self.current_hop = None;
        self.current_node = None;
    }

    fn check_statement(&mut self, stmt_id: StatementId, hop_index: usize, function_name: &str) {
        let stmt = &self.program.statements[stmt_id];
        
        match &stmt.node {
            StatementKind::Assignment(a) => self.check_assignment(a, &stmt.span),
            StatementKind::VarAssignment(a) => self.check_var_assignment(a, &stmt.span),
            StatementKind::IfStmt(i) => {
                self.check_if_statement(i, &stmt.span, hop_index, function_name)
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

    fn check_if_statement(
        &mut self,
        if_stmt: &IfStatement,
        _span: &Span,
        hop_index: usize,
        function_name: &str,
    ) {
        // Check condition
        if let Some(cond_type) = self.check_expression(if_stmt.condition) {
            if cond_type != TypeName::Bool {
                let cond_expr = &self.program.expressions[if_stmt.condition];
                self.error_at(
                    &cond_expr.span,
                    AstError::InvalidCondition(cond_type),
                );
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
            if cond_type != TypeName::Bool {
                let cond_expr = &self.program.expressions[while_stmt.condition];
                self.error_at(
                    &cond_expr.span,
                    AstError::InvalidCondition(cond_type),
                );
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
        // Get the actual table
        let table = &self.program.tables[assign.table];
        
        // Check cross-node access
        if let Some(current_node_id) = self.current_node {
            if table.node != current_node_id {
                let current_node_name = &self.program.nodes[current_node_id].name;
                let table_node_name = &self.program.nodes[table.node].name;
                self.error_at(
                    span,
                    AstError::CrossNodeAccess {
                        table: table.name.clone(),
                        table_node: table_node_name.clone(),
                        current_node: current_node_name.clone(),
                    },
                );
                return;
            }
        }

        // Check that pk_field is actually the primary key
        let pk_field = &self.program.fields[assign.pk_field];
        let primary_key_field = &self.program.fields[table.primary_key];
        
        if pk_field.field_name != primary_key_field.field_name {
            self.error_at(
                span,
                AstError::InvalidPrimaryKey {
                    table: table.name.clone(),
                    column: pk_field.field_name.clone(),
                },
            );
            return;
        }

        // Check primary key expression type
        if let Some(pk_type) = self.check_expression(assign.pk_expr) {
            if !self.types_compatible(&primary_key_field.field_type, &pk_type) {
                self.error_at(
                    span,
                    AstError::TypeMismatch {
                        expected: primary_key_field.field_type.clone(),
                        found: pk_type,
                    },
                );
            }
        }

        // Check the assigned field exists and type matches
        let assigned_field = &self.program.fields[assign.field];
        let field_exists = table.fields.iter().any(|&field_id| {
            self.program.fields[field_id].field_name == assigned_field.field_name
        });

        if !field_exists {
            self.error_at(
                span,
                AstError::UndeclaredField {
                    table: table.name.clone(),
                    field: assigned_field.field_name.clone(),
                },
            );
            return;
        }

        // Check RHS type
        if let Some(rhs_type) = self.check_expression(assign.rhs) {
            if !self.types_compatible(&assigned_field.field_type, &rhs_type) {
                self.error_at(
                    span,
                    AstError::TypeMismatch {
                        expected: assigned_field.field_type.clone(),
                        found: rhs_type,
                    },
                );
            }
        }
    }

    fn check_var_assignment(&mut self, var_assign: &VarAssignmentStatement, _span: &Span) {
        // The name resolver should have already checked if the variable exists
        // We can use the resolutions to get the variable type directly
        // For now, we'll check the type compatibility
        
        if let Some(_rhs_type) = self.check_expression(var_assign.rhs) {
            // We would need to look up the variable type from the name resolver's results
            // For now, we'll skip detailed type checking and let the name resolver handle existence
            // In a full implementation, we'd look up the resolved variable and check its type
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDeclStatement, span: &Span) {
        // Check if global variables are not allowed (based on your language design)
        if var_decl.is_global {
            self.error_at(
                span,
                AstError::GlobalVariableNotAllowed(var_decl.var_name.clone()),
            );
            return;
        }

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
            ExpressionKind::Ident(_name) => {
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
                table,
                pk_field,
                pk_expr,
                field,
            } => {
                // Check primary key expression
                self.check_expression(*pk_expr);

                let table_obj = &self.program.tables[*table];
                
                // Check cross-node access
                if let Some(current_node_id) = self.current_node {
                    if table_obj.node != current_node_id {
                        let current_node_name = &self.program.nodes[current_node_id].name;
                        let table_node_name = &self.program.nodes[table_obj.node].name;
                        let expr_span = expr.span.clone(); // Clone the span to avoid borrowing issues
                        self.error_at(
                            &expr_span,
                            AstError::CrossNodeAccess {
                                table: table_obj.name.clone(),
                                table_node: table_node_name.clone(),
                                current_node: current_node_name.clone(),
                            },
                        );
                        return None;
                    }
                }

                // Check primary key field
                let pk_field_obj = &self.program.fields[*pk_field];
                let primary_key_field = &self.program.fields[table_obj.primary_key];
                
                if pk_field_obj.field_name != primary_key_field.field_name {
                    let expr_span = expr.span.clone(); // Clone the span to avoid borrowing issues
                    self.error_at(
                        &expr_span,
                        AstError::InvalidPrimaryKey {
                            table: table_obj.name.clone(),
                            column: pk_field_obj.field_name.clone(),
                        },
                    );
                    return None;
                }

                // Check accessed field exists and return its type
                let accessed_field = &self.program.fields[*field];
                let field_exists = table_obj.fields.iter().any(|&field_id| {
                    self.program.fields[field_id].field_name == accessed_field.field_name
                });

                if field_exists {
                    Some(accessed_field.field_type.clone())
                } else {
                    let expr_span = expr.span.clone(); // Clone the span to avoid borrowing issues
                    self.error_at(
                        &expr_span,
                        AstError::UndeclaredField {
                            table: table_obj.name.clone(),
                            field: accessed_field.field_name.clone(),
                        },
                    );
                    None
                }
            }
            ExpressionKind::UnaryOp { op, expr: inner_expr } => {
                let Some(operand_type) = self.check_expression(*inner_expr) else {
                    return None;
                };

                match op {
                    UnaryOp::Neg => {
                        if matches!(operand_type, TypeName::Int | TypeName::Float) {
                            Some(operand_type)
                        } else {
                            let expr_span = expr.span.clone(); // Clone the span to avoid borrowing issues
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
                        if operand_type == TypeName::Bool {
                            Some(TypeName::Bool)
                        } else {
                            let expr_span = expr.span.clone(); // Clone the span to avoid borrowing issues
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
            ExpressionKind::BinaryOp { left, op, right } => {
                let left_type = self.check_expression(*left)?;
                let right_type = self.check_expression(*right)?;
                let expr_span = expr.span.clone(); // Clone the span to avoid borrowing issues
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
                if matches!(left, TypeName::Int | TypeName::Float)
                    && matches!(right, TypeName::Int | TypeName::Float)
                {
                    if matches!(left, TypeName::Float) || matches!(right, TypeName::Float) {
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
                if matches!(left, TypeName::Int | TypeName::Float)
                    && matches!(right, TypeName::Int | TypeName::Float)
                {
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
                if left == &TypeName::Bool && right == &TypeName::Bool {
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
        expected == actual || (expected == &TypeName::Float && actual == &TypeName::Int)
    }
}

/// Public interface for semantic analysis
pub fn analyze_program(program: &Program) -> Results<()> {
    let analyzer = SemanticAnalyzer::new(program);
    analyzer.analyze()
}