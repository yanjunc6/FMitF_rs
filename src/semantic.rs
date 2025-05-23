use crate::ast::*;
use std::collections::HashMap; // Remove HashSet import
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum SemanticError {
    // Type checking errors
    TypeMismatch {
        expected: TypeName,
        found: TypeName,
        location: String,
    },

    // Undeclared identifiers
    UndeclaredVariable {
        name: String,
        location: String,
    },

    // Table/Field errors
    UndeclaredTable {
        name: String,
        location: String,
    },
    UndeclaredField {
        table_name: String,
        field_name: String,
        location: String,
    },

    // Function errors
    DuplicateFunction {
        name: String,
    },
    UndeclaredFunction {
        name: String,
        location: String,
    },

    // Node errors
    UndeclaredNode {
        name: String,
        location: String,
    },

    // Assignment errors
    AssignmentToNonField {
        table: String,        // Add table field
        field: String,        // Change field_name to field
        location: String,
    },
    InvalidPrimaryKey {
        table_name: String,
        pk_column: String,
        location: String,
    },

    // Expression errors
    InvalidUnaryOperation {
        op: String,
        operand_type: TypeName,
        location: String,
    },
    InvalidBinaryOperation {
        op: String,
        left_type: TypeName,
        right_type: TypeName,
        location: String,
    },

    // Control flow errors
    InvalidConditionType {
        found: TypeName,
        location: String,
    },

    // Variable errors
    DuplicateVariable {
        name: String,
        location: String,
    },

    // Return statement errors
    ReturnTypeMismatch {
        expected: ReturnType,
        found: Option<TypeName>,
        location: String,
    },
    ReturnInVoidFunction {
        location: String,
    },
    MissingReturnStatement {
        function_name: String,
    },
}

pub struct SemanticAnalyzer {
    errors: Vec<SemanticError>,
    // Symbol tables
    functions: HashMap<String, FunctionDeclaration>, // Changed from Rc<FunctionDeclaration>
    tables: HashMap<String, Rc<TableDeclaration>>,
    nodes: HashMap<String, Rc<NodeDef>>,
    // Current scope context
    current_function: Option<String>,
    current_parameters: HashMap<String, TypeName>,
    current_variables: HashMap<String, TypeName>,      // Local variables (hop-scoped)
    current_global_vars: HashMap<String, TypeName>,    // Global variables (function-scoped)
    current_return_type: Option<ReturnType>,      // NEW: Expected return type
    has_return_statement: bool,                   // NEW: Track if function has return
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            functions: HashMap::new(),
            tables: HashMap::new(),
            nodes: HashMap::new(),
            current_function: None,
            current_parameters: HashMap::new(),
            current_variables: HashMap::new(),
            current_global_vars: HashMap::new(),  // Add this
            current_return_type: None,
            has_return_statement: false,
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<SemanticError>> {
        // First pass: collect all declarations
        self.collect_declarations(program);

        // Second pass: check all function bodies
        self.check_functions(program);

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn collect_declarations(&mut self, program: &Program) {
        // Collect nodes
        for node in &program.nodes {
            self.nodes.insert(node.name.clone(), node.clone());
        }

        // Collect tables
        for table in &program.tables {
            self.tables.insert(table.name.clone(), table.clone());
        }

        // Collect functions and check for duplicates
        for func in &program.functions {
            if self.functions.contains_key(&func.name) {
                self.errors.push(SemanticError::DuplicateFunction {
                    name: func.name.clone(),
                });
            } else {
                // Create a copy of the function data for storage
                let func_copy = FunctionDeclaration {
                    return_type: func.return_type.clone(),
                    name: func.name.clone(),
                    parameters: func.parameters.clone(),
                    hops: func.hops.clone(),
                };
                self.functions.insert(func.name.clone(), func_copy);
            }
        }
    }

    fn check_functions(&mut self, program: &Program) {
        for func in &program.functions {
            self.check_function(func);
        }
    }

    fn check_function(&mut self, func: &FunctionDeclaration) {
        self.current_function = Some(func.name.clone());
        self.current_return_type = Some(func.return_type.clone());
        self.has_return_statement = false;

        // Clear all variable scopes
        self.current_parameters.clear();
        self.current_variables.clear();
        self.current_global_vars.clear();  // Add this
        
        for param in &func.parameters {
            self.current_parameters
                .insert(param.param_name.clone(), param.param_type.clone());
        }

        // Check each hop block
        for hop in &func.hops {
            self.check_hop_block(hop);
        }

        // Check if non-void functions have return statements
        if let ReturnType::Type(_) = func.return_type {
            if !self.has_return_statement {
                self.errors.push(SemanticError::MissingReturnStatement {
                    function_name: func.name.clone(),
                });
            }
        }

        self.current_function = None;
        self.current_return_type = None;
        self.current_parameters.clear();
        self.current_variables.clear();
        self.current_global_vars.clear();  // Add this
    }

    fn check_hop_block(&mut self, hop: &HopBlock) {
        // Check if the node exists
        if !self.nodes.contains_key(&hop.node.name) {
            self.errors.push(SemanticError::UndeclaredNode {
                name: hop.node.name.clone(),
                location: format!("hop block"),
            });
        }

        // Clear local variables at the start of each hop (but keep globals)
        self.current_variables.clear();

        // Check all statements in the hop block
        for stmt in &hop.statements {
            self.check_statement(stmt);
        }
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assignment(assign) => self.check_assignment(assign),
            Statement::VarAssignment(var_assign) => self.check_var_assignment(var_assign),
            Statement::IfStmt(if_stmt) => self.check_if_statement(if_stmt),
            Statement::VarDecl(var_decl) => self.check_var_decl(var_decl),
            Statement::Return(ret_stmt) => self.check_return_statement(ret_stmt),
            Statement::Empty => {} // Empty statements are always valid
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDeclStatement) {
        let location = format!("variable declaration '{}'", var_decl.var_name);
        
        if var_decl.is_global {
            // Global variable declaration
            if self.current_global_vars.contains_key(&var_decl.var_name) || 
               self.current_parameters.contains_key(&var_decl.var_name) {
                self.errors.push(SemanticError::DuplicateVariable {
                    name: var_decl.var_name.clone(),
                    location,
                });
                return;
            }
            
            // Type check the initial value
            let init_type = self.check_expression(&var_decl.init_value);
            if let Some(init_type) = init_type {
                if !self.types_compatible(&var_decl.var_type, &init_type) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: var_decl.var_type.clone(),
                        found: init_type,
                        location,
                    });
                }
            }
            
            // Add to global scope
            self.current_global_vars.insert(var_decl.var_name.clone(), var_decl.var_type.clone());
        } else {
            // Local variable declaration
            if self.current_variables.contains_key(&var_decl.var_name) || 
               self.current_global_vars.contains_key(&var_decl.var_name) ||
               self.current_parameters.contains_key(&var_decl.var_name) {
                self.errors.push(SemanticError::DuplicateVariable {
                    name: var_decl.var_name.clone(),
                    location,
                });
                return;
            }
            
            // Type check the initial value
            let init_type = self.check_expression(&var_decl.init_value);
            if let Some(init_type) = init_type {
                if !self.types_compatible(&var_decl.var_type, &init_type) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: var_decl.var_type.clone(),
                        found: init_type,
                        location,
                    });
                }
            }
            
            // Add to local scope
            self.current_variables.insert(var_decl.var_name.clone(), var_decl.var_type.clone());
        }
    }

    fn check_var_assignment(&mut self, var_assign: &VarAssignmentStatement) {
        let location = format!("variable assignment to '{}'", var_assign.var_name);

        // Check if variable exists in any scope (parameters, globals, locals)
        let var_type = if let Some(param_type) = self.current_parameters.get(&var_assign.var_name) {
            Some(param_type.clone())
        } else if let Some(global_type) = self.current_global_vars.get(&var_assign.var_name) {
            Some(global_type.clone())
        } else if let Some(var_type) = self.current_variables.get(&var_assign.var_name) {
            Some(var_type.clone())
        } else {
            self.errors.push(SemanticError::UndeclaredVariable {
                name: var_assign.var_name.clone(),
                location,
            });
            return;
        };

        // Type check the RHS expression
        if let Some(expected_type) = var_type {
            let rhs_type = self.check_expression(&var_assign.rhs);
            if let Some(rhs_type) = rhs_type {
                if !self.types_compatible(&expected_type, &rhs_type) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: expected_type,
                        found: rhs_type,
                        location,
                    });
                }
            }
        }
    }

    fn check_assignment(&mut self, assign: &AssignmentStatement) {
        let table_name = &assign.table.name;
        let location = format!("assignment to {}.{}", table_name, assign.field_name);

        // Check if table exists (should always be true since we got Rc from parser)
        if !self.tables.contains_key(table_name) {
            self.errors.push(SemanticError::UndeclaredTable {
                name: table_name.clone(),
                location: location.clone(),
            });
            return;
        }

        let table = &assign.table;

        // Check if field exists in the table
        let field = table
            .fields
            .iter()
            .find(|f| f.field_name == assign.field_name);
        if field.is_none() {
            self.errors.push(SemanticError::UndeclaredField {
                table_name: table_name.clone(),
                field_name: assign.field_name.clone(),
                location: location.clone(),
            });
            return;
        }

        let field = field.unwrap();

        // Check if pk_column exists in the table
        let pk_field = table
            .fields
            .iter()
            .find(|f| f.field_name == assign.pk_column);
        if pk_field.is_none() {
            self.errors.push(SemanticError::InvalidPrimaryKey {
                table_name: table_name.clone(),
                pk_column: assign.pk_column.clone(),
                location: location.clone(),
            });
        }

        // Type check the primary key expression
        if let Some(pk_field) = pk_field {
            let pk_expr_type = self.check_expression(&assign.pk_expr);
            if let Some(pk_type) = pk_expr_type {
                if !self.types_compatible(&pk_field.field_type, &pk_type) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: pk_field.field_type.clone(),
                        found: pk_type,
                        location: format!("primary key expression in {}", location),
                    });
                }
            }
        }

        // Type check the RHS expression
        let rhs_type = self.check_expression(&assign.rhs);
        if let Some(rhs_type) = rhs_type {
            if !self.types_compatible(&field.field_type, &rhs_type) {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: field.field_type.clone(),
                    found: rhs_type,
                    location: location.clone(),
                });
            }
        }
    }

    fn check_if_statement(&mut self, if_stmt: &IfStatement) {
        // Check condition type - should be boolean-like
        let cond_type = self.check_expression(&if_stmt.condition);
        // Now we can enable strict boolean checking for conditions
        if let Some(cond_type) = cond_type {
            if !matches!(cond_type, TypeName::Bool) {
                self.errors.push(SemanticError::InvalidConditionType {
                    found: cond_type,
                    location: "if statement condition".to_string(),
                });
            }
        }

        // Check then branch
        for stmt in &if_stmt.then_branch {
            self.check_statement(stmt);
        }

        // Check else branch if it exists
        if let Some(else_branch) = &if_stmt.else_branch {
            for stmt in else_branch {
                self.check_statement(stmt);
            }
        }
    }

    fn check_return_statement(&mut self, ret_stmt: &ReturnStatement) {
        self.has_return_statement = true;
        let location = "return statement".to_string();

        // Clone the current return type to avoid borrowing conflicts
        let current_return_type = self.current_return_type.clone();

        match (&current_return_type, &ret_stmt.value) {
            (Some(ReturnType::Void), Some(_)) => {
                self.errors
                    .push(SemanticError::ReturnInVoidFunction { location });
            }
            (Some(ReturnType::Type(expected_type)), Some(expr)) => {
                let actual_type = self.check_expression(expr);
                if let Some(actual_type) = actual_type {
                    if !self.types_compatible(expected_type, &actual_type) {
                        self.errors.push(SemanticError::ReturnTypeMismatch {
                            expected: ReturnType::Type(expected_type.clone()),
                            found: Some(actual_type),
                            location,
                        });
                    }
                }
            }
            (Some(ReturnType::Type(_)), None) => {
                self.errors.push(SemanticError::ReturnTypeMismatch {
                    expected: current_return_type.unwrap(),
                    found: None,
                    location,
                });
            }
            _ => {} // void return with no value is OK
        }
    }

    // Update check_expression to handle all expression types:
    fn check_expression(&mut self, expr: &Expression) -> Option<TypeName> {
        match expr {
            Expression::Ident(name) => {
                // Check parameters first, then globals, then local variables
                if let Some(param_type) = self.current_parameters.get(name) {
                    Some(param_type.clone())
                } else if let Some(global_type) = self.current_global_vars.get(name) {
                    Some(global_type.clone())
                } else if let Some(var_type) = self.current_variables.get(name) {
                    Some(var_type.clone())
                } else {
                    self.errors.push(SemanticError::UndeclaredVariable {
                        name: name.clone(),
                        location: format!("expression"),
                    });
                    None
                }
            },
            
            // Literal types - these should return their corresponding types
            Expression::IntLit(_) => Some(TypeName::Int),
            Expression::FloatLit(_) => Some(TypeName::Float),
            Expression::StringLit(_) => Some(TypeName::String),
            Expression::BoolLit(_) => Some(TypeName::Bool),  // This should handle true/false
            
            Expression::TableFieldAccess { 
                table_name, 
                pk_column: _, 
                pk_expr, 
                field_name 
            } => {
                // First, check the primary key expression
                let _pk_type = self.check_expression(pk_expr);
                
                // Now check if table exists (after the mutable borrow is done)
                if let Some(table) = self.tables.get(table_name) {
                    // Check if field exists in table
                    if let Some(field) = table.fields.iter().find(|f| f.field_name == *field_name) {
                        Some(field.field_type.clone())
                    } else {
                        self.errors.push(SemanticError::AssignmentToNonField {
                            table: table_name.clone(),
                            field: field_name.clone(),
                            location: "table field access".to_string(),
                        });
                        None
                    }
                } else {
                    self.errors.push(SemanticError::UndeclaredTable {
                        name: table_name.clone(),
                        location: "table field access".to_string(),
                    });
                    None
                }
            },
            
            Expression::UnaryOp { op, expr } => {
                let operand_type = self.check_expression(expr);
                if let Some(operand_type) = operand_type {
                    match op {
                        UnaryOp::Neg => {
                            if matches!(operand_type, TypeName::Int | TypeName::Float) {
                                Some(operand_type)
                            } else {
                                self.errors.push(SemanticError::InvalidUnaryOperation {
                                    op: "negation".to_string(),
                                    operand_type,
                                    location: "unary expression".to_string(),
                                });
                                None
                            }
                        }
                        UnaryOp::Not => Some(TypeName::Bool),
                    }
                } else {
                    None
                }
            }

            Expression::BinaryOp { left, op, right } => {
                let left_type = self.check_expression(left);
                let right_type = self.check_expression(right);

                if let (Some(left_type), Some(right_type)) = (left_type, right_type) {
                    self.check_binary_operation(op, &left_type, &right_type)
                } else {
                    None
                }
            }
        }
    }

    fn check_binary_operation(
        &mut self,
        op: &BinaryOp,
        left_type: &TypeName,
        right_type: &TypeName,
    ) -> Option<TypeName> {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                if matches!(left_type, TypeName::Int | TypeName::Float)
                    && matches!(right_type, TypeName::Int | TypeName::Float)
                {
                    // Return the "wider" type
                    if matches!(left_type, TypeName::Float) || matches!(right_type, TypeName::Float)
                    {
                        Some(TypeName::Float)
                    } else {
                        Some(TypeName::Int)
                    }
                } else {
                    self.errors.push(SemanticError::InvalidBinaryOperation {
                        op: format!("{:?}", op),
                        left_type: left_type.clone(),
                        right_type: right_type.clone(),
                        location: "binary expression".to_string(),
                    });
                    None
                }
            }

            BinaryOp::Eq | BinaryOp::Neq => {
                if self.types_compatible(left_type, right_type) {
                    Some(TypeName::Bool) // Return bool instead of int
                } else {
                    self.errors.push(SemanticError::InvalidBinaryOperation {
                        op: format!("{:?}", op),
                        left_type: left_type.clone(),
                        right_type: right_type.clone(),
                        location: "equality expression".to_string(),
                    });
                    None
                }
            }

            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                if matches!(left_type, TypeName::Int | TypeName::Float)
                    && matches!(right_type, TypeName::Int | TypeName::Float)
                {
                    Some(TypeName::Bool) // Return bool instead of int
                } else {
                    self.errors.push(SemanticError::InvalidBinaryOperation {
                        op: format!("{:?}", op),
                        left_type: left_type.clone(),
                        right_type: right_type.clone(),
                        location: "comparison expression".to_string(),
                    });
                    None
                }
            }

            BinaryOp::And | BinaryOp::Or => {
                // Logical operations can be applied to any types (truthy/falsy)
                // but should preferably be boolean
                Some(TypeName::Bool) // Return bool instead of int
            }
        }
    }

    fn types_compatible(&self, expected: &TypeName, actual: &TypeName) -> bool {
        match (expected, actual) {
            // Exact matches
            (TypeName::Int, TypeName::Int)
            | (TypeName::Float, TypeName::Float)
            | (TypeName::String, TypeName::String)
            | (TypeName::Bool, TypeName::Bool) => true, // Add Boolean compatibility

            // Allow int to float conversion
            (TypeName::Float, TypeName::Int) => true,

            // No other conversions allowed
            _ => false,
        }
    }
}

// Helper function to format errors nicely
pub fn format_semantic_errors(errors: &[SemanticError]) -> String {
    let mut result = String::new();
    result.push_str("Semantic Analysis Errors:\n");

    for (i, error) in errors.iter().enumerate() {
        result.push_str(&format!("{}. ", i + 1));
        match error {
            SemanticError::TypeMismatch {
                expected,
                found,
                location,
            } => {
                result.push_str(&format!(
                    "Type mismatch in {}: expected {:?}, found {:?}\n",
                    location, expected, found
                ));
            }
            SemanticError::UndeclaredVariable { name, location } => {
                result.push_str(&format!("Undeclared variable '{}' in {}\n", name, location));
            }
            SemanticError::UndeclaredTable { name, location } => {
                result.push_str(&format!("Undeclared table '{}' in {}\n", name, location));
            }
            SemanticError::UndeclaredField {
                table_name,
                field_name,
                location,
            } => {
                result.push_str(&format!(
                    "Field '{}' does not exist in table '{}' ({})\n",
                    field_name, table_name, location
                ));
            }
            SemanticError::DuplicateFunction { name } => {
                result.push_str(&format!("Duplicate function declaration: '{}'\n", name));
            }
            SemanticError::UndeclaredFunction { name, location } => {
                result.push_str(&format!("Undeclared function '{}' in {}\n", name, location));
            }
            SemanticError::UndeclaredNode { name, location } => {
                result.push_str(&format!("Undeclared node '{}' in {}\n", name, location));
            }
            SemanticError::AssignmentToNonField {
                table,
                field,
                location,
            } => {
                result.push_str(&format!(
                    "Field '{}' does not exist in table '{}' in {}\n",
                    field, table, location
                ));
            }
            SemanticError::InvalidPrimaryKey {
                table_name,
                pk_column,
                location,
            } => {
                result.push_str(&format!(
                    "Invalid primary key '{}' for table '{}' in {}\n",
                    pk_column, table_name, location
                ));
            }
            SemanticError::InvalidUnaryOperation {
                op,
                operand_type,
                location,
            } => {
                result.push_str(&format!(
                    "Invalid {} operation on {:?} in {}\n",
                    op, operand_type, location
                ));
            }
            SemanticError::InvalidBinaryOperation {
                op,
                left_type,
                right_type,
                location,
            } => {
                result.push_str(&format!(
                    "Invalid {} operation between {:?} and {:?} in {}\n",
                    op, left_type, right_type, location
                ));
            }
            SemanticError::InvalidConditionType { found, location } => {
                result.push_str(&format!(
                    "Invalid condition type {:?} in {} (expected boolean)\n",
                    found, location
                ));
            }
            SemanticError::DuplicateVariable { name, location } => {
                result.push_str(&format!("Duplicate variable '{}' in {}\n", name, location));
            }
            SemanticError::ReturnTypeMismatch {
                expected,
                found,
                location,
            } => match (expected, found) {
                (ReturnType::Void, Some(actual)) => {
                    result.push_str(&format!(
                        "Cannot return value in void function (found {:?}) in {}\n",
                        actual, location
                    ));
                }
                (ReturnType::Type(expected_type), Some(actual)) => {
                    result.push_str(&format!(
                        "Return type mismatch: expected {:?}, found {:?} in {}\n",
                        expected_type, actual, location
                    ));
                }
                (ReturnType::Type(expected_type), None) => {
                    result.push_str(&format!(
                        "Missing return value: expected {:?} in {}\n",
                        expected_type, location
                    ));
                }
                _ => {}
            },
            SemanticError::ReturnInVoidFunction { location } => {
                result.push_str(&format!(
                    "Cannot return value in void function in {}\n",
                    location
                ));
            }
            SemanticError::MissingReturnStatement { function_name } => {
                result.push_str(&format!(
                    "Function '{}' must return a value\n",
                    function_name
                ));
            }
        }
    }

    result
}
