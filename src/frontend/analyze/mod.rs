use crate::ast::*;
use crate::frontend::parse::{Results, SpannedError, TransActError};
use std::collections::HashMap;
use std::rc::Rc;

pub struct SemanticAnalyzer {
    errors: Vec<SpannedError>,
    functions: HashMap<String, Rc<FunctionDeclaration>>, // Changed to Rc
    tables: HashMap<String, Rc<TableDeclaration>>,
    nodes: HashMap<String, Rc<NodeDef>>,
    // Current context
    parameters: HashMap<String, TypeName>,
    variables: HashMap<String, TypeName>,
    globals: HashMap<String, TypeName>,
    return_type: Option<ReturnType>,
    has_return: bool,
    current_node: Option<String>,
    in_loop: bool, // NEW: Track if inside a loop
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            functions: HashMap::new(),
            tables: HashMap::new(),
            nodes: HashMap::new(),
            parameters: HashMap::new(),
            variables: HashMap::new(),
            globals: HashMap::new(),
            return_type: None,
            has_return: false,
            current_node: None,
            in_loop: false, // NEW: Initialize in_loop
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Results<()> {
        self.collect_declarations(program);
        self.check_functions(program);

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn collect_declarations(&mut self, program: &Program) {
        for node in &program.nodes {
            self.nodes.insert(node.name.clone(), node.clone());
        }

        for table in &program.tables {
            self.tables.insert(table.name.clone(), table.clone());
        }

        for func in &program.functions {
            if self.functions.contains_key(&func.name) {
                self.error_at(
                    &func.span,
                    TransActError::DuplicateFunction(func.name.clone()),
                );
            } else {
                self.functions.insert(func.name.clone(), func.clone()); // Clone the Rc
            }
        }
    }

    fn check_functions(&mut self, program: &Program) {
        for func in &program.functions {
            self.check_function(func); // func is now &Rc<FunctionDeclaration>
        }
    }

    fn check_function(&mut self, func: &FunctionDeclaration) { // Keep the same signature
        self.return_type = Some(func.return_type.clone());
        self.has_return = false;
        self.parameters.clear();
        self.variables.clear();
        self.globals.clear();

        for param in &func.parameters {
            if self.parameters.contains_key(&param.param_name) {
                self.error_at(
                    &param.span,
                    TransActError::DuplicateVariable(param.param_name.clone()),
                );
            } else {
                self.parameters
                    .insert(param.param_name.clone(), param.param_type.clone());
            }
        }

        for (hop_index, hop) in func.hops.iter().enumerate() {
            self.check_hop_block(hop, hop_index, &func.name);
        }

        if matches!(func.return_type, ReturnType::Type(_)) && !self.has_return {
            self.error_at(&func.span, TransActError::MissingReturn(func.name.clone()));
        }
    }

    fn check_hop_block(&mut self, hop: &HopBlock, hop_index: usize, function_name: &str) {
        if !self.nodes.contains_key(&hop.node.name) {
            self.error_at(
                &hop.span,
                TransActError::UndeclaredNode(hop.node.name.clone()),
            );
        }

        self.current_node = Some(hop.node.name.clone());
        self.variables.clear();

        for stmt in &hop.statements {
            self.check_statement(stmt, hop_index, function_name);
        }
    }

    fn check_statement(&mut self, stmt: &Statement, hop_index: usize, function_name: &str) {
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
            StatementKind::Break(_) => self.check_break_statement(&stmt.span), // NEW: Add break
            StatementKind::Continue(_) => self.check_continue_statement(&stmt.span), // NEW: Add continue
            StatementKind::Empty => {}
        }
    }

    fn check_abort_statement(&mut self, span: &Span, hop_index: usize, function_name: &str) {
        if hop_index != 0 {
            self.error_at(
                span,
                TransActError::AbortNotInFirstHop {
                    function: function_name.to_string(),
                    hop_index,
                },
            );
        }
    }

    fn check_if_statement(
        &mut self,
        if_stmt: &IfStatement,
        _span: &Span, // _span is for the whole if statement, condition has its own span
        hop_index: usize,
        function_name: &str,
    ) {
        if let Some(cond_type) = self.check_expression(&if_stmt.condition) {
            if cond_type != TypeName::Bool {
                self.error_at(
                    &if_stmt.condition.span,
                    TransActError::InvalidCondition(cond_type),
                );
            }
        }

        for stmt in &if_stmt.then_branch {
            self.check_statement(stmt, hop_index, function_name);
        }

        if let Some(else_branch) = &if_stmt.else_branch {
            for stmt in else_branch {
                self.check_statement(stmt, hop_index, function_name);
            }
        }
    }

    fn check_while_statement(
        &mut self,
        while_stmt: &WhileStatement,
        _span: &Span, // _span is for the whole while statement
        hop_index: usize,
        function_name: &str,
    ) {
        if let Some(cond_type) = self.check_expression(&while_stmt.condition) {
            if cond_type != TypeName::Bool {
                self.error_at(
                    &while_stmt.condition.span, // Use condition's span for more precise error
                    TransActError::InvalidCondition(cond_type),
                );
            }
        }

        let previous_in_loop = self.in_loop; // Save previous state
        self.in_loop = true; // Set in_loop to true
        for stmt in &while_stmt.body {
            self.check_statement(stmt, hop_index, function_name);
        }
        self.in_loop = previous_in_loop; // Restore previous state
    }

    fn check_break_statement(&mut self, span: &Span) {
        if !self.in_loop {
            self.error_at(span, TransActError::BreakOutsideLoop);
        }
    }

    fn check_continue_statement(&mut self, span: &Span) {
        if !self.in_loop {
            self.error_at(span, TransActError::ContinueOutsideLoop);
        }
    }

    fn check_assignment(&mut self, assign: &AssignmentStatement, span: &Span) {
        // Look up the actual table by name to get real information
        let actual_table = if let Some(real_table) = self.tables.get(&assign.table.name) {
            real_table.clone()
        } else {
            self.error_at(
                span,
                TransActError::UndeclaredTable(assign.table.name.clone()),
            );
            return;
        };

        // Check cross-node access
        if let Some(current_node) = &self.current_node {
            if actual_table.node.name != *current_node {
                self.error_at(
                    span,
                    TransActError::CrossNodeAccess {
                        table: actual_table.name.clone(),
                        table_node: actual_table.node.name.clone(),
                        current_node: current_node.clone(),
                    },
                );
                return;
            }
        }

        // Check that pk_field is actually the primary key of the table
        if assign.pk_field.field_name != actual_table.primary_key.field_name {
            self.error_at(
                span,
                TransActError::InvalidPrimaryKey {
                    table: actual_table.name.clone(),
                    column: assign.pk_field.field_name.clone(),
                },
            );
            return;
        }

        // Check primary key expression type
        if let Some(pk_type) = self.check_expression(&assign.pk_expr) {
            if !self.types_compatible(&actual_table.primary_key.field_type, &pk_type) {
                self.error_at(
                    span,
                    TransActError::TypeMismatch {
                        expected: actual_table.primary_key.field_type.clone(),
                        found: pk_type,
                    },
                );
            }
        }

        // Find the actual field being assigned
        let actual_field = actual_table
            .fields
            .iter()
            .find(|f| f.field_name == assign.field.field_name);

        if let Some(actual_field) = actual_field {
            if let Some(rhs_type) = self.check_expression(&assign.rhs) {
                if !self.types_compatible(&actual_field.field_type, &rhs_type) {
                    self.error_at(
                        span,
                        TransActError::TypeMismatch {
                            expected: actual_field.field_type.clone(),
                            found: rhs_type,
                        },
                    );
                }
            }
        } else {
            self.error_at(
                span,
                TransActError::UndeclaredField {
                    table: actual_table.name.clone(),
                    field: assign.field.field_name.clone(),
                },
            );
        }
    }

    fn check_expression(&mut self, expr: &Expression) -> Option<TypeName> {
        match &expr.node {
            ExpressionKind::Ident(name) => {
                if let Some(typ) = self.lookup_variable(name) {
                    Some(typ)
                } else {
                    self.error_at(&expr.span, TransActError::UndeclaredVariable(name.clone()));
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
                self.check_expression(pk_expr);

                // Look up the actual table by name
                let actual_table = if let Some(real_table) = self.tables.get(&table.name) {
                    real_table.clone()
                } else {
                    self.error_at(
                        &expr.span,
                        TransActError::UndeclaredTable(table.name.clone()),
                    );
                    return None;
                };

                // Check cross-node access
                if let Some(current_node) = &self.current_node {
                    if actual_table.node.name != *current_node {
                        self.error_at(
                            &expr.span,
                            TransActError::CrossNodeAccess {
                                table: actual_table.name.clone(),
                                table_node: actual_table.node.name.clone(),
                                current_node: current_node.clone(),
                            },
                        );
                        return None;
                    }
                }

                // Check that the pk_field is actually the primary key of the table
                if pk_field.field_name != actual_table.primary_key.field_name {
                    self.error_at(
                        &expr.span,
                        TransActError::InvalidPrimaryKey {
                            table: actual_table.name.clone(),
                            column: pk_field.field_name.clone(),
                        },
                    );
                    return None;
                }

                // Find the actual field by name
                let actual_field = actual_table
                    .fields
                    .iter()
                    .find(|f| f.field_name == field.field_name);

                if let Some(actual_field) = actual_field {
                    Some(actual_field.field_type.clone())
                } else {
                    self.error_at(
                        &expr.span,
                        TransActError::UndeclaredField {
                            table: actual_table.name.clone(),
                            field: field.field_name.clone(),
                        },
                    );
                    None
                }
            }
            ExpressionKind::UnaryOp { op, expr } => {
                let Some(operand_type) = self.check_expression(expr) else {
                    return None;
                };

                match op {
                    UnaryOp::Neg => {
                        if matches!(operand_type, TypeName::Int | TypeName::Float) {
                            Some(operand_type)
                        } else {
                            self.error_at(
                                &expr.span,
                                TransActError::InvalidUnaryOp {
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
                            self.error_at(
                                &expr.span,
                                TransActError::InvalidUnaryOp {
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
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;
                self.check_binary_op(op, &left_type, &right_type, &expr.span)
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
                        TransActError::InvalidBinaryOp {
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
                        TransActError::InvalidBinaryOp {
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
                        TransActError::InvalidBinaryOp {
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
                        TransActError::InvalidBinaryOp {
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

    fn error_at(&mut self, span: &Span, error: TransActError) {
        self.errors.push(SpannedError {
            error,
            span: Some(span.clone()),
        });
    }

    fn lookup_variable(&self, name: &str) -> Option<TypeName> {
        self.parameters
            .get(name)
            .or_else(|| self.globals.get(name))
            .or_else(|| self.variables.get(name))
            .cloned()
    }

    fn is_duplicate_variable(&self, name: &str) -> bool {
        self.parameters.contains_key(name)
            || self.globals.contains_key(name)
            || self.variables.contains_key(name)
    }

    fn types_compatible(&self, expected: &TypeName, actual: &TypeName) -> bool {
        expected == actual || (expected == &TypeName::Float && actual == &TypeName::Int)
    }

    fn check_var_assignment(&mut self, var_assign: &VarAssignmentStatement, span: &Span) {
        if !self.is_variable_declared(&var_assign.var_name) {
            self.error_at(
                span,
                TransActError::UndeclaredVariable(var_assign.var_name.clone()),
            );
            return;
        }

        if let Some(rhs_type) = self.check_expression(&var_assign.rhs) {
            if let Some(var_type) = self.lookup_variable(&var_assign.var_name) {
                if !self.types_compatible(&var_type, &rhs_type) {
                    self.error_at(
                        span,
                        TransActError::TypeMismatch {
                            expected: var_type,
                            found: rhs_type,
                        },
                    );
                }
            }
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDeclStatement, span: &Span) {
        if self.is_duplicate_variable(&var_decl.var_name) {
            self.error_at(
                span,
                TransActError::DuplicateVariable(var_decl.var_name.clone()),
            );
            return;
        }

        if let Some(init_type) = self.check_expression(&var_decl.init_value) {
            if !self.types_compatible(&var_decl.var_type, &init_type) {
                self.error_at(
                    span,
                    TransActError::TypeMismatch {
                        expected: var_decl.var_type.clone(),
                        found: init_type,
                    },
                );
            }
        }

        if var_decl.is_global {
            self.globals
                .insert(var_decl.var_name.clone(), var_decl.var_type.clone());
        } else {
            self.variables
                .insert(var_decl.var_name.clone(), var_decl.var_type.clone());
        }
    }

    fn check_return_statement(&mut self, ret_stmt: &ReturnStatement, span: &Span) {
        self.has_return = true;

        let return_type = self.return_type.clone();

        match (&return_type, &ret_stmt.value) {
            (Some(ReturnType::Void), Some(_)) => {
                self.error_at(span, TransActError::UnexpectedReturnValue);
            }
            (Some(ReturnType::Type(expected_type)), Some(expr)) => {
                if let Some(actual_type) = self.check_expression(expr) {
                    if !self.types_compatible(expected_type, &actual_type) {
                        self.error_at(
                            span,
                            TransActError::TypeMismatch {
                                expected: expected_type.clone(),
                                found: actual_type,
                            },
                        );
                    }
                }
            }
            (Some(ReturnType::Type(_)), None) => {
                self.error_at(span, TransActError::MissingReturnValue);
            }
            (Some(ReturnType::Void), None) => {
                // Valid void return
            }
            (None, _) => {
                // Should not happen
            }
        }
    }

    fn is_variable_declared(&self, name: &str) -> bool {
        self.parameters.contains_key(name)
            || self.globals.contains_key(name)
            || self.variables.contains_key(name)
    }
}
