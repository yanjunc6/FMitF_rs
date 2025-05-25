use crate::ast::*;
use crate::errors::{TransActError, Results, SpannedError};
use std::collections::HashMap;
use std::rc::Rc;

pub struct SemanticAnalyzer {
    errors: Vec<SpannedError>,
    functions: HashMap<String, FunctionDeclaration>,
    tables: HashMap<String, Rc<TableDeclaration>>,
    nodes: HashMap<String, Rc<NodeDef>>,
    // Current context
    parameters: HashMap<String, TypeName>,
    variables: HashMap<String, TypeName>,
    globals: HashMap<String, TypeName>,
    return_type: Option<ReturnType>,
    has_return: bool,
    current_node: Option<String>,
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
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Results<()> {
        self.collect_declarations(program);
        self.check_functions(program);
        
        if self.errors.is_empty() { Ok(()) } else { Err(self.errors.clone()) }
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
                self.error_at(&func.span, TransActError::DuplicateFunction(func.name.clone()));
            } else {
                self.functions.insert(func.name.clone(), func.clone());
            }
        }
    }

    fn check_functions(&mut self, program: &Program) {
        for func in &program.functions {
            self.check_function(func);
        }
    }

    fn check_function(&mut self, func: &FunctionDeclaration) {
        self.return_type = Some(func.return_type.clone());
        self.has_return = false;
        self.parameters.clear();
        self.variables.clear();
        self.globals.clear();
        
        for param in &func.parameters {
            self.parameters.insert(param.param_name.clone(), param.param_type.clone());
        }

        for hop in &func.hops {
            self.check_hop_block(hop);
        }

        if matches!(func.return_type, ReturnType::Type(_)) && !self.has_return {
            self.error_at(&func.span, TransActError::MissingReturn(func.name.clone()));
        }
    }

    fn check_hop_block(&mut self, hop: &HopBlock) {
        if !self.nodes.contains_key(&hop.node.name) {
            self.error_at(&hop.span, TransActError::UndeclaredNode(hop.node.name.clone()));
        }

        self.current_node = Some(hop.node.name.clone());
        self.variables.clear(); // New hop = new local scope

        for stmt in &hop.statements {
            self.check_statement(stmt);
        }
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match &stmt.node {
            StatementKind::Assignment(a) => self.check_assignment(a, &stmt.span),
            StatementKind::VarAssignment(a) => self.check_var_assignment(a, &stmt.span),
            StatementKind::IfStmt(i) => self.check_if_statement(i, &stmt.span),
            StatementKind::VarDecl(v) => self.check_var_decl(v, &stmt.span),
            StatementKind::Return(r) => self.check_return_statement(r, &stmt.span),
            StatementKind::Empty => {}
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDeclStatement, span: &Span) {
        if self.is_duplicate_variable(&var_decl.var_name) {
            self.error_at(span, TransActError::DuplicateVariable(var_decl.var_name.clone()));
            return;
        }
        
        if let Some(init_type) = self.check_expression(&var_decl.init_value) {
            if !self.types_compatible(&var_decl.var_type, &init_type) {
                self.error_at(span, TransActError::TypeMismatch {
                    expected: var_decl.var_type.clone(),
                    found: init_type,
                });
            }
        }
        
        if var_decl.is_global {
            self.globals.insert(var_decl.var_name.clone(), var_decl.var_type.clone());
        } else {
            self.variables.insert(var_decl.var_name.clone(), var_decl.var_type.clone());
        }
    }

    fn check_var_assignment(&mut self, var_assign: &VarAssignmentStatement, span: &Span) {
        let var_type = self.lookup_variable(&var_assign.var_name);
        
        let Some(expected_type) = var_type else {
            self.error_at(span, TransActError::UndeclaredVariable(var_assign.var_name.clone()));
            return;
        };

        if let Some(rhs_type) = self.check_expression(&var_assign.rhs) {
            if !self.types_compatible(&expected_type, &rhs_type) {
                self.error_at(span, TransActError::TypeMismatch {
                    expected: expected_type,
                    found: rhs_type,
                });
            }
        }
    }

    fn check_assignment(&mut self, assign: &AssignmentStatement, span: &Span) {
        let table = &assign.table;

        if let Some(current_node) = &self.current_node {
            if table.node.name != *current_node {
                self.error_at(span, TransActError::CrossNodeAccess {
                    table: table.name.clone(),
                    table_node: table.node.name.clone(),
                    current_node: current_node.clone(),
                });
                return;
            }
        }

        let Some(field) = table.fields.iter().find(|f| f.field_name == assign.field_name) else {
            self.error_at(span, TransActError::UndeclaredField {
                table: table.name.clone(),
                field: assign.field_name.clone(),
            });
            return;
        };

        let Some(pk_field) = table.fields.iter().find(|f| f.field_name == assign.pk_column) else {
            self.error_at(span, TransActError::InvalidPrimaryKey {
                table: table.name.clone(),
                column: assign.pk_column.clone(),
            });
            return;
        };

        if let Some(pk_type) = self.check_expression(&assign.pk_expr) {
            if !self.types_compatible(&pk_field.field_type, &pk_type) {
                self.error_at(span, TransActError::TypeMismatch {
                    expected: pk_field.field_type.clone(),
                    found: pk_type,
                });
            }
        }

        if let Some(rhs_type) = self.check_expression(&assign.rhs) {
            if !self.types_compatible(&field.field_type, &rhs_type) {
                self.error_at(span, TransActError::TypeMismatch {
                    expected: field.field_type.clone(),
                    found: rhs_type,
                });
            }
        }
    }

    fn check_if_statement(&mut self, if_stmt: &IfStatement, _span: &Span) {
        if let Some(cond_type) = self.check_expression(&if_stmt.condition) {
            if cond_type != TypeName::Bool {
                self.error_at(&if_stmt.condition.span, TransActError::InvalidCondition(cond_type));
            }
        }

        for stmt in &if_stmt.then_branch {
            self.check_statement(stmt);
        }

        if let Some(else_branch) = &if_stmt.else_branch {
            for stmt in else_branch {
                self.check_statement(stmt);
            }
        }
    }

    fn check_return_statement(&mut self, ret_stmt: &ReturnStatement, span: &Span) {
        self.has_return = true;
        
        // Clone to avoid borrow checker issues
        let return_type = self.return_type.clone();
        
        match (&return_type, &ret_stmt.value) {
            (Some(ReturnType::Void), Some(_)) => {
                self.error_at(span, TransActError::ReturnInVoidFunction);
            }
            (Some(ReturnType::Type(expected)), Some(expr)) => {
                if let Some(actual) = self.check_expression(expr) {
                    if !self.types_compatible(expected, &actual) {
                        self.error_at(span, TransActError::ReturnTypeMismatch {
                            expected: ReturnType::Type(expected.clone()),
                            found: Some(actual),
                        });
                    }
                }
            }
            (Some(ReturnType::Type(expected)), None) => {
                self.error_at(span, TransActError::ReturnTypeMismatch {
                    expected: ReturnType::Type(expected.clone()),
                    found: None,
                });
            }
            _ => {}
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
            ExpressionKind::TableFieldAccess { table_name, pk_expr, field_name, .. } => {
                self.check_expression(pk_expr);
                
                let Some(table) = self.tables.get(table_name) else {
                    self.error_at(&expr.span, TransActError::UndeclaredTable(table_name.clone()));
                    return None;
                };

                if let Some(current_node) = &self.current_node {
                    if table.node.name != *current_node {
                        self.error_at(&expr.span, TransActError::CrossNodeAccess {
                            table: table_name.clone(),
                            table_node: table.node.name.clone(),
                            current_node: current_node.clone(),
                        });
                        return None;
                    }
                }

                table.fields.iter()
                    .find(|f| f.field_name == *field_name)
                    .map(|f| f.field_type.clone())
                    .or_else(|| {
                        self.error_at(&expr.span, TransActError::UndeclaredField {
                            table: table_name.clone(),
                            field: field_name.clone(),
                        });
                        None
                    })
            }
            ExpressionKind::UnaryOp { op, expr } => {
                let Some(operand_type) = self.check_expression(expr) else { return None };
                
                match op {
                    UnaryOp::Neg => {
                        if matches!(operand_type, TypeName::Int | TypeName::Float) {
                            Some(operand_type)
                        } else {
                            self.error_at(&expr.span, TransActError::InvalidUnaryOp {
                                op: "negation".to_string(),
                                operand: operand_type,
                            });
                            None
                        }
                    }
                    UnaryOp::Not => Some(TypeName::Bool),
                }
            }
            ExpressionKind::BinaryOp { left, op, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;
                self.check_binary_op(op, &left_type, &right_type, &expr.span)
            }
        }
    }

    fn check_binary_op(&mut self, op: &BinaryOp, left: &TypeName, right: &TypeName, span: &Span) -> Option<TypeName> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                if matches!(left, TypeName::Int | TypeName::Float) && 
                   matches!(right, TypeName::Int | TypeName::Float) {
                    if matches!(left, TypeName::Float) || matches!(right, TypeName::Float) {
                        Some(TypeName::Float)
                    } else {
                        Some(TypeName::Int)
                    }
                } else {
                    self.error_at(span, TransActError::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: left.clone(),
                        right: right.clone(),
                    });
                    None
                }
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if self.types_compatible(left, right) {
                    Some(TypeName::Bool)
                } else {
                    self.error_at(span, TransActError::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: left.clone(),
                        right: right.clone(),
                    });
                    None
                }
            }
            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                if matches!(left, TypeName::Int | TypeName::Float) && 
                   matches!(right, TypeName::Int | TypeName::Float) {
                    Some(TypeName::Bool)
                } else {
                    self.error_at(span, TransActError::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: left.clone(),
                        right: right.clone(),
                    });
                    None
                }
            }
            BinaryOp::And | BinaryOp::Or => Some(TypeName::Bool),
        }
    }

    fn error_at(&mut self, span: &Span, error: TransActError) {
        self.errors.push(SpannedError {
            error,
            span: Some(span.clone()),
        });
    }

    fn lookup_variable(&self, name: &str) -> Option<TypeName> {
        self.parameters.get(name)
            .or_else(|| self.globals.get(name))
            .or_else(|| self.variables.get(name))
            .cloned()
    }

    fn is_duplicate_variable(&self, name: &str) -> bool {
        self.parameters.contains_key(name) || 
        self.globals.contains_key(name) || 
        self.variables.contains_key(name)
    }

    fn types_compatible(&self, expected: &TypeName, actual: &TypeName) -> bool {
        expected == actual || (expected == &TypeName::Float && actual == &TypeName::Int)
    }
}
