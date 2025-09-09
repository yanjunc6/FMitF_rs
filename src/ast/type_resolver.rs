//! Type Resolution
//!
//! This module implements type resolution and checking for the AST. It performs:
//! - Type inference for expressions
//! - Type compatibility checking
//! - Generic type instantiation
//! - Function signature validation
//! - Table field type checking

use std::collections::HashMap;

use crate::ast::*;
use crate::ast::errors::*;

// ============================================================================
// --- Type Resolution Context
// ============================================================================

/// Main type resolver that performs type checking and inference
pub struct TypeResolver {
    /// Built-in type declarations
    builtin_types: HashMap<String, ResolvedType>,
    
    /// Generic type parameters in current scope
    generic_context: Vec<HashMap<String, GenericParamId>>,
    
    /// Current function context for return type checking
    current_function: Option<FunctionId>,
    
    /// Error collector
    errors: ErrorCollector,
}

/// Type checking context for expressions
#[derive(Debug, Clone)]
pub struct TypeContext {
    /// Expected type for the current expression (for type inference)
    pub expected_type: Option<ResolvedType>,
    
    /// Whether we're in a constant context
    pub is_constant_context: bool,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            expected_type: None,
            is_constant_context: false,
        }
    }

    pub fn with_expected_type(expected: ResolvedType) -> Self {
        Self {
            expected_type: Some(expected),
            is_constant_context: false,
        }
    }

    pub fn constant_context() -> Self {
        Self {
            expected_type: None,
            is_constant_context: true,
        }
    }
}

// ============================================================================
// --- Type Resolution Implementation
// ============================================================================

impl TypeResolver {
    pub fn new() -> Self {
        let mut builtin_types = HashMap::new();
        
        // Add built-in types
        builtin_types.insert("void".to_string(), ResolvedType::Void);
        builtin_types.insert("int".to_string(), ResolvedType::Type(TypeDeclId::from(0usize))); // Placeholder
        builtin_types.insert("bool".to_string(), ResolvedType::Type(TypeDeclId::from(1usize))); // Placeholder
        builtin_types.insert("string".to_string(), ResolvedType::Type(TypeDeclId::from(2usize))); // Placeholder
        builtin_types.insert("float".to_string(), ResolvedType::Type(TypeDeclId::from(3usize))); // Placeholder
        
        Self {
            builtin_types,
            generic_context: Vec::new(),
            current_function: None,
            errors: ErrorCollector::new(),
        }
    }

    /// Perform type resolution on a program
    pub fn resolve_types(mut program: Program) -> Results<Program> {
        let mut resolver = Self::new();
        resolver.resolve_program_types(&mut program);
        resolver.errors.into_result(Some(program))
    }

    fn resolve_program_types(&mut self, program: &mut Program) {
        // Resolve types in all declarations
        for declaration in &program.declarations {
            match declaration {
                Declaration::Callable(id) => {
                    self.resolve_callable_types(*id, program);
                }
                Declaration::Type(id) => {
                    self.resolve_type_declaration_types(*id, program);
                }
                Declaration::Const(id) => {
                    self.resolve_const_declaration_types(*id, program);
                }
                Declaration::Table(id) => {
                    self.resolve_table_declaration_types(*id, program);
                }
            }
        }
    }

    // ========================================================================
    // --- Declaration Type Resolution
    // ========================================================================

    fn resolve_callable_types(&mut self, func_id: FunctionId, program: &mut Program) {
        self.current_function = Some(func_id);
        self.push_generic_scope();

        let func = &program.functions[func_id];
        
        // Add generic parameters to scope
        for generic_param_id in &func.generic_params {
            let generic_param = &program.generic_params[*generic_param_id];
            self.add_generic_param(generic_param.name.value.clone(), *generic_param_id);
        }

        // Resolve parameter types
        for param_id in &func.params {
            let param = &program.params[*param_id];
            if let Some(type_id) = param.ty {
                if let Some(resolved_type) = self.resolve_type_reference(type_id, program) {
                    // Set resolved type on parameter (would need mutable access)
                }
            }
        }

        // Resolve return type
        let return_type = if let Some(return_type_id) = func.return_type {
            self.resolve_type_reference(return_type_id, program)
        } else {
            Some(ResolvedType::Void)
        };

        // Resolve assumption expressions
        for assumption_id in &func.assumptions {
            let context = TypeContext::with_expected_type(ResolvedType::Type(TypeDeclId::from(1usize))); // bool
            self.resolve_expression_type(*assumption_id, program, &context);
        }

        // Resolve function body
        if let Some(body_id) = func.body {
            self.resolve_block_types(body_id, program);
        }

        self.pop_generic_scope();
        self.current_function = None;
    }

    fn resolve_type_declaration_types(&mut self, type_id: TypeDeclId, program: &mut Program) {
        self.push_generic_scope();

        let type_decl = &program.type_decls[type_id];
        
        // Add generic parameters to scope
        for generic_param_id in &type_decl.generic_params {
            let generic_param = &program.generic_params[*generic_param_id];
            self.add_generic_param(generic_param.name.value.clone(), *generic_param_id);
        }

        self.pop_generic_scope();
    }

    fn resolve_const_declaration_types(&mut self, const_id: ConstId, program: &mut Program) {
        let const_decl = &program.const_decls[const_id];
        
        // Resolve type
        let declared_type = self.resolve_type_reference(const_decl.ty, program);
        
        // Resolve value expression in constant context
        let context = if let Some(expected) = declared_type {
            TypeContext::with_expected_type(expected).constant_context()
        } else {
            TypeContext::constant_context()
        };
        
        let inferred_type = self.resolve_expression_type(const_decl.value, program, &context);
        
        // Check type compatibility
        if let (Some(declared), Some(inferred)) = (declared_type, inferred_type) {
            if !self.types_compatible(&declared, &inferred) {
                let error = TypeResolutionError::TypeMismatch {
                    expected: self.type_to_string(&declared),
                    found: self.type_to_string(&inferred),
                };
                self.errors.add_error(AstError::TypeResolution(error), const_decl.span);
            }
        }
    }

    fn resolve_table_declaration_types(&mut self, table_id: TableId, program: &mut Program) {
        let table_decl = &program.table_decls[table_id];
        
        // Resolve field types
        for field in &table_decl.fields {
            self.resolve_type_reference(field.ty, program);
        }
        
        // Validate partition function compatibility
        for node in &table_decl.nodes {
            if let Some(partition_id) = node.resolved_partition {
                self.validate_partition_usage(partition_id, node, table_decl, program);
            }
            
            // Resolve node arguments
            for arg_id in &node.args {
                self.resolve_expression_type(*arg_id, program, &TypeContext::new());
            }
        }
        
        // Resolve invariant expressions
        for invariant_id in &table_decl.invariants {
            let context = TypeContext::with_expected_type(ResolvedType::Type(TypeDeclId::from(1usize))); // bool
            self.resolve_expression_type(*invariant_id, program, &context);
        }
    }

    fn validate_partition_usage(&mut self, partition_id: FunctionId, node: &TableNode, 
                               table: &TableDecl, program: &Program) {
        let partition_func = &program.functions[partition_id];
        
        // Check parameter count
        if partition_func.params.len() != node.args.len() {
            let error = TypeResolutionError::InvalidFunctionCall {
                function_name: partition_func.name.value.clone(),
                argument_types: vec!["...".to_string()], // Would need actual types
                parameter_types: vec!["...".to_string()], // Would need actual types
            };
            self.errors.add_error(AstError::TypeResolution(error), node.span);
        }
        
        // Additional validation for partition functions would go here
    }

    // ========================================================================
    // --- Type Reference Resolution
    // ========================================================================

    fn resolve_type_reference(&mut self, type_id: TypeId, program: &Program) -> Option<ResolvedType> {
        let type_ref = &program.types[type_id];
        
        match type_ref {
            Type::Named(identifier) => {
                if let Some(resolved_decl) = identifier.resolved {
                    match resolved_decl {
                        DeclRef::Type(type_decl_id) => {
                            Some(ResolvedType::Type(type_decl_id))
                        }
                        DeclRef::Table(table_id) => {
                            Some(ResolvedType::Table(table_id))
                        }
                        DeclRef::GenericParam(generic_param_id) => {
                            // This represents a generic type parameter
                            // For now, we'll use a placeholder
                            Some(ResolvedType::Type(TypeDeclId::from(0usize)))
                        }
                        _ => {
                            let error = TypeResolutionError::InvalidReference {
                                name: identifier.name.clone(),
                                expected_kind: "type".to_string(),
                                found_kind: "non-type declaration".to_string(),
                            };
                            self.errors.add_error(AstError::TypeResolution(error), identifier.span);
                            None
                        }
                    }
                } else {
                    // Check if it's a built-in type
                    if let Some(builtin_type) = self.builtin_types.get(&identifier.name) {
                        Some(builtin_type.clone())
                    } else {
                        let error = TypeResolutionError::UndefinedType {
                            type_name: identifier.name.clone(),
                        };
                        self.errors.add_error(AstError::TypeResolution(error), identifier.span);
                        None
                    }
                }
            }
            
            Type::Generic { base, args, .. } => {
                let base_type = if let Some(resolved_decl) = base.resolved {
                    match resolved_decl {
                        DeclRef::Type(type_decl_id) => Some(type_decl_id),
                        _ => {
                            let error = TypeResolutionError::InvalidReference {
                                name: base.name.clone(),
                                expected_kind: "generic type".to_string(),
                                found_kind: "non-type declaration".to_string(),
                            };
                            self.errors.add_error(AstError::TypeResolution(error), base.span);
                            None
                        }
                    }
                } else {
                    let error = TypeResolutionError::UndefinedType {
                        type_name: base.name.clone(),
                    };
                    self.errors.add_error(AstError::TypeResolution(error), base.span);
                    None
                };
                
                if let Some(base_type) = base_type {
                    let mut resolved_args = Vec::new();
                    for arg_type_id in args {
                        if let Some(resolved_arg) = self.resolve_type_reference(*arg_type_id, program) {
                            resolved_args.push(resolved_arg);
                        }
                    }
                    
                    Some(ResolvedType::Generic {
                        base: base_type,
                        args: resolved_args,
                    })
                } else {
                    None
                }
            }
            
            Type::Function { params, return_type, .. } => {
                let mut resolved_params = Vec::new();
                for param_type_id in params {
                    if let Some(resolved_param) = self.resolve_type_reference(*param_type_id, program) {
                        resolved_params.push(resolved_param);
                    }
                }
                
                if let Some(resolved_return) = self.resolve_type_reference(*return_type, program) {
                    Some(ResolvedType::Function {
                        params: resolved_params,
                        return_type: Box::new(resolved_return),
                    })
                } else {
                    None
                }
            }
        }
    }

    // ========================================================================
    // --- Statement Type Resolution
    // ========================================================================

    fn resolve_block_types(&mut self, block_id: BlockId, program: &mut Program) {
        let statement_ids = program.blocks[block_id].statements.clone();
        for stmt_id in statement_ids {
            self.resolve_statement_types(stmt_id, program);
        }
    }

    fn resolve_statement_types(&mut self, stmt_id: StmtId, program: &mut Program) {
        let stmt = &program.statements[stmt_id];
        
        match stmt {
            Statement::VarDecl(var_id) => {
                let var_decl = &program.var_decls[*var_id];
                
                let declared_type = if let Some(type_id) = var_decl.ty {
                    self.resolve_type_reference(type_id, program)
                } else {
                    None
                };
                
                let inferred_type = if let Some(init_id) = var_decl.init {
                    let context = if let Some(expected) = declared_type.clone() {
                        TypeContext::with_expected_type(expected)
                    } else {
                        TypeContext::new()
                    };
                    self.resolve_expression_type(init_id, program, &context)
                } else {
                    None
                };
                
                // Check type compatibility if both are present
                if let (Some(declared), Some(inferred)) = (declared_type, inferred_type) {
                    if !self.types_compatible(&declared, &inferred) {
                        let error = TypeResolutionError::TypeMismatch {
                            expected: self.type_to_string(&declared),
                            found: self.type_to_string(&inferred),
                        };
                        self.errors.add_error(AstError::TypeResolution(error), var_decl.span);
                    }
                }
            }
            
            Statement::If { condition, then_block, else_block, .. } => {
                let bool_type = ResolvedType::Type(TypeDeclId::from(1usize)); // bool
                let context = TypeContext::with_expected_type(bool_type);
                self.resolve_expression_type(*condition, program, &context);
                
                self.resolve_block_types(*then_block, program);
                if let Some(else_block_id) = else_block {
                    self.resolve_block_types(*else_block_id, program);
                }
            }
            
            Statement::For { init, condition, update, body, .. } => {
                if let Some(for_init) = init {
                    match for_init {
                        ForInit::VarDecl(var_id) => {
                            // Already handled by resolve_statement_types for VarDecl
                        }
                        ForInit::Expression(expr_id) => {
                            self.resolve_expression_type(*expr_id, program, &TypeContext::new());
                        }
                    }
                }
                
                if let Some(condition_id) = condition {
                    let bool_type = ResolvedType::Type(TypeDeclId::from(1usize)); // bool
                    let context = TypeContext::with_expected_type(bool_type);
                    self.resolve_expression_type(*condition_id, program, &context);
                }
                
                if let Some(update_id) = update {
                    self.resolve_expression_type(*update_id, program, &TypeContext::new());
                }
                
                self.resolve_block_types(*body, program);
            }
            
            Statement::Return { value, .. } => {
                if let Some(value_id) = value {
                    let expected_return_type = self.get_current_function_return_type(program);
                    let context = if let Some(expected) = expected_return_type {
                        TypeContext::with_expected_type(expected)
                    } else {
                        TypeContext::new()
                    };
                    self.resolve_expression_type(*value_id, program, &context);
                }
            }
            
            Statement::Assert { expr, .. } => {
                let bool_type = ResolvedType::Type(TypeDeclId::from(1usize)); // bool
                let context = TypeContext::with_expected_type(bool_type);
                self.resolve_expression_type(*expr, program, &context);
            }
            
            Statement::Hop { body, .. } => {
                self.resolve_block_types(*body, program);
            }
            
            Statement::HopsFor { start, end, body, .. } => {
                let int_type = ResolvedType::Type(TypeDeclId::from(0usize)); // int
                let context = TypeContext::with_expected_type(int_type.clone());
                self.resolve_expression_type(*start, program, &context);
                self.resolve_expression_type(*end, program, &context);
                self.resolve_block_types(*body, program);
            }
            
            Statement::Expression { expr, .. } => {
                self.resolve_expression_type(*expr, program, &TypeContext::new());
            }
            
            Statement::Block(block_id) => {
                self.resolve_block_types(*block_id, program);
            }
        }
    }

    // ========================================================================
    // --- Expression Type Resolution
    // ========================================================================

    fn resolve_expression_type(&mut self, expr_id: ExprId, program: &mut Program, 
                              context: &TypeContext) -> Option<ResolvedType> {
        let expr = &program.expressions[expr_id];
        
        match expr {
            Expression::Literal { value, .. } => {
                self.infer_literal_type(value)
            }
            
            Expression::Identifier(identifier) => {
                self.resolve_identifier_type(identifier, program)
            }
            
            Expression::Binary { left, op, right, .. } => {
                let left_type = self.resolve_expression_type(*left, program, &TypeContext::new());
                let right_type = self.resolve_expression_type(*right, program, &TypeContext::new());
                
                self.resolve_binary_operation_type(&op.value, left_type, right_type, expr_id, program)
            }
            
            Expression::Unary { op, expr, .. } => {
                let operand_type = self.resolve_expression_type(*expr, program, &TypeContext::new());
                self.resolve_unary_operation_type(&op.value, operand_type, expr_id, program)
            }
            
            Expression::Assignment { lhs, rhs, .. } => {
                let lhs_type = self.resolve_expression_type(*lhs, program, &TypeContext::new());
                let rhs_context = if let Some(expected) = lhs_type.clone() {
                    TypeContext::with_expected_type(expected)
                } else {
                    TypeContext::new()
                };
                let rhs_type = self.resolve_expression_type(*rhs, program, &rhs_context);
                
                if let (Some(lhs_t), Some(rhs_t)) = (lhs_type.clone(), rhs_type) {
                    if !self.types_compatible(&lhs_t, &rhs_t) {
                        let error = TypeResolutionError::InvalidAssignment {
                            lhs_type: self.type_to_string(&lhs_t),
                            rhs_type: self.type_to_string(&rhs_t),
                        };
                        self.errors.add_error(AstError::TypeResolution(error), None);
                    }
                }
                
                lhs_type
            }
            
            Expression::Call { callee, args, resolved_callable, .. } => {
                // Resolve argument types
                let mut arg_types = Vec::new();
                for arg_id in args {
                    if let Some(arg_type) = self.resolve_expression_type(*arg_id, program, &TypeContext::new()) {
                        arg_types.push(arg_type);
                    }
                }
                
                // Try to resolve the function call
                if let Some(func_id) = resolved_callable {
                    self.resolve_function_call_type(*func_id, &arg_types, program)
                } else {
                    // Try to infer from callee expression
                    let callee_type = self.resolve_expression_type(*callee, program, &TypeContext::new());
                    if let Some(ResolvedType::Function { return_type, .. }) = callee_type {
                        Some(*return_type)
                    } else {
                        None
                    }
                }
            }
            
            Expression::MemberAccess { object, member, .. } => {
                let object_type = self.resolve_expression_type(*object, program, &TypeContext::new());
                self.resolve_member_access_type(object_type, &member.value, program)
            }
            
            Expression::TableRowAccess { table, key_values, .. } => {
                let table_type = self.resolve_expression_type(*table, program, &TypeContext::new());
                
                // Resolve key-value expressions
                for key_value in key_values {
                    self.resolve_expression_type(key_value.value, program, &TypeContext::new());
                }
                
                // Table row access returns the table type itself
                table_type
            }
            
            Expression::Grouped { expr, .. } => {
                self.resolve_expression_type(*expr, program, context)
            }
        }
    }

    fn infer_literal_type(&self, literal: &Literal) -> Option<ResolvedType> {
        match literal {
            Literal::Integer(_) => Some(ResolvedType::Type(TypeDeclId::from(0usize))), // int
            Literal::Float(_) => Some(ResolvedType::Type(TypeDeclId::from(3usize))), // float
            Literal::String(_) => Some(ResolvedType::Type(TypeDeclId::from(2usize))), // string
            Literal::Bool(_) => Some(ResolvedType::Type(TypeDeclId::from(1usize))), // bool
            Literal::List(_) => {
                // Would need element type analysis
                None
            }
            Literal::RowLiteral(_) => {
                // Would need field type analysis
                None
            }
        }
    }

    fn resolve_identifier_type(&self, identifier: &Identifier, program: &Program) -> Option<ResolvedType> {
        if let Some(resolved_decl) = identifier.resolved {
            match resolved_decl {
                DeclRef::Function(func_id) => {
                    let func = &program.functions[func_id];
                    // Create function type
                    let mut param_types = Vec::new();
                    for param_id in &func.params {
                        let param = &program.params[*param_id];
                        if let Some(type_id) = param.ty {
                            if let Some(param_type) = self.resolve_type_reference(type_id, program) {
                                param_types.push(param_type);
                            }
                        }
                    }
                    
                    let return_type = if let Some(return_type_id) = func.return_type {
                        self.resolve_type_reference(return_type_id, program).unwrap_or(ResolvedType::Void)
                    } else {
                        ResolvedType::Void
                    };
                    
                    Some(ResolvedType::Function {
                        params: param_types,
                        return_type: Box::new(return_type),
                    })
                }
                DeclRef::Type(type_id) => {
                    Some(ResolvedType::Type(type_id))
                }
                DeclRef::Table(table_id) => {
                    Some(ResolvedType::Table(table_id))
                }
                DeclRef::Const(const_id) => {
                    let const_decl = &program.const_decls[const_id];
                    self.resolve_type_reference(const_decl.ty, program)
                }
                DeclRef::Var(var_id) => {
                    let var_decl = &program.var_decls[var_id];
                    if let Some(type_id) = var_decl.ty {
                        self.resolve_type_reference(type_id, program)
                    } else {
                        // Type should be inferred from initializer
                        None
                    }
                }
                DeclRef::Param(param_id) => {
                    let param = &program.params[param_id];
                    if let Some(type_id) = param.ty {
                        self.resolve_type_reference(type_id, program)
                    } else {
                        None
                    }
                }
                DeclRef::GenericParam(_) => {
                    // Generic parameter - would need proper handling
                    None
                }
            }
        } else {
            None
        }
    }

    fn resolve_binary_operation_type(&mut self, op: &str, left_type: Option<ResolvedType>, 
                                   right_type: Option<ResolvedType>, expr_id: ExprId, 
                                   program: &Program) -> Option<ResolvedType> {
        match (left_type, right_type) {
            (Some(left), Some(right)) => {
                match op {
                    // Arithmetic operators
                    "+" | "-" | "*" | "/" | "%" => {
                        if self.types_compatible(&left, &right) && self.is_numeric_type(&left) {
                            Some(left)
                        } else {
                            let error = TypeResolutionError::InvalidOperation {
                                operation: op.to_string(),
                                operand_types: vec![self.type_to_string(&left), self.type_to_string(&right)],
                            };
                            self.errors.add_error(AstError::TypeResolution(error), None);
                            None
                        }
                    }
                    
                    // Comparison operators
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        if self.types_compatible(&left, &right) {
                            Some(ResolvedType::Type(TypeDeclId::from(1usize))) // bool
                        } else {
                            let error = TypeResolutionError::InvalidOperation {
                                operation: op.to_string(),
                                operand_types: vec![self.type_to_string(&left), self.type_to_string(&right)],
                            };
                            self.errors.add_error(AstError::TypeResolution(error), None);
                            None
                        }
                    }
                    
                    // Logical operators
                    "&&" | "||" => {
                        let bool_type = ResolvedType::Type(TypeDeclId::from(1usize));
                        if self.types_compatible(&left, &bool_type) && self.types_compatible(&right, &bool_type) {
                            Some(bool_type)
                        } else {
                            let error = TypeResolutionError::InvalidOperation {
                                operation: op.to_string(),
                                operand_types: vec![self.type_to_string(&left), self.type_to_string(&right)],
                            };
                            self.errors.add_error(AstError::TypeResolution(error), None);
                            None
                        }
                    }
                    
                    _ => {
                        // Custom operators - would need more complex resolution
                        None
                    }
                }
            }
            _ => None
        }
    }

    fn resolve_unary_operation_type(&mut self, op: &str, operand_type: Option<ResolvedType>, 
                                  expr_id: ExprId, program: &Program) -> Option<ResolvedType> {
        if let Some(operand) = operand_type {
            match op {
                "-" => {
                    if self.is_numeric_type(&operand) {
                        Some(operand)
                    } else {
                        let error = TypeResolutionError::InvalidOperation {
                            operation: op.to_string(),
                            operand_types: vec![self.type_to_string(&operand)],
                        };
                        self.errors.add_error(AstError::TypeResolution(error), None);
                        None
                    }
                }
                "!" => {
                    let bool_type = ResolvedType::Type(TypeDeclId::from(1usize));
                    if self.types_compatible(&operand, &bool_type) {
                        Some(bool_type)
                    } else {
                        let error = TypeResolutionError::InvalidOperation {
                            operation: op.to_string(),
                            operand_types: vec![self.type_to_string(&operand)],
                        };
                        self.errors.add_error(AstError::TypeResolution(error), None);
                        None
                    }
                }
                _ => {
                    // Custom operators
                    None
                }
            }
        } else {
            None
        }
    }

    fn resolve_function_call_type(&mut self, func_id: FunctionId, arg_types: &[ResolvedType], 
                                 program: &Program) -> Option<ResolvedType> {
        let func = &program.functions[func_id];
        
        // Check parameter count
        if func.params.len() != arg_types.len() {
            let error = TypeResolutionError::InvalidFunctionCall {
                function_name: func.name.value.clone(),
                argument_types: arg_types.iter().map(|t| self.type_to_string(t)).collect(),
                parameter_types: vec!["...".to_string()], // Would need actual parameter types
            };
            self.errors.add_error(AstError::TypeResolution(error), func.span);
            return None;
        }
        
        // Check parameter types
        for (i, param_id) in func.params.iter().enumerate() {
            let param = &program.params[*param_id];
            if let Some(param_type_id) = param.ty {
                if let Some(param_type) = self.resolve_type_reference(param_type_id, program) {
                    if i < arg_types.len() && !self.types_compatible(&param_type, &arg_types[i]) {
                        let error = TypeResolutionError::InvalidFunctionCall {
                            function_name: func.name.value.clone(),
                            argument_types: arg_types.iter().map(|t| self.type_to_string(t)).collect(),
                            parameter_types: vec![self.type_to_string(&param_type)],
                        };
                        self.errors.add_error(AstError::TypeResolution(error), func.span);
                        return None;
                    }
                }
            }
        }
        
        // Return the function's return type
        if let Some(return_type_id) = func.return_type {
            self.resolve_type_reference(return_type_id, program)
        } else {
            Some(ResolvedType::Void)
        }
    }

    fn resolve_member_access_type(&mut self, object_type: Option<ResolvedType>, 
                                 member_name: &str, program: &Program) -> Option<ResolvedType> {
        if let Some(ResolvedType::Table(table_id)) = object_type {
            let table = &program.table_decls[table_id];
            
            // Look for field with matching name
            for field in &table.fields {
                if field.name.value == member_name {
                    return self.resolve_type_reference(field.ty, program);
                }
            }
            
            let error = TypeResolutionError::InvalidTableAccess {
                table_name: table.name.value.clone(),
                field_name: member_name.to_string(),
            };
            self.errors.add_error(AstError::TypeResolution(error), None);
            None
        } else {
            // For non-table types, member access is not supported yet
            None
        }
    }

    // ========================================================================
    // --- Utility Functions
    // ========================================================================

    fn types_compatible(&self, expected: &ResolvedType, actual: &ResolvedType) -> bool {
        match (expected, actual) {
            (ResolvedType::Type(a), ResolvedType::Type(b)) => a == b,
            (ResolvedType::Table(a), ResolvedType::Table(b)) => a == b,
            (ResolvedType::Void, ResolvedType::Void) => true,
            (ResolvedType::Generic { base: base1, args: args1 }, 
             ResolvedType::Generic { base: base2, args: args2 }) => {
                base1 == base2 && args1.len() == args2.len() &&
                args1.iter().zip(args2.iter()).all(|(a, b)| self.types_compatible(a, b))
            }
            (ResolvedType::Function { params: params1, return_type: ret1 },
             ResolvedType::Function { params: params2, return_type: ret2 }) => {
                params1.len() == params2.len() &&
                params1.iter().zip(params2.iter()).all(|(a, b)| self.types_compatible(a, b)) &&
                self.types_compatible(ret1, ret2)
            }
            _ => false,
        }
    }

    fn is_numeric_type(&self, ty: &ResolvedType) -> bool {
        match ty {
            ResolvedType::Type(id) => {
                // Check if it's int or float (using placeholder IDs)
                *id == TypeDeclId::from(0usize) || *id == TypeDeclId::from(3usize)
            }
            _ => false,
        }
    }

    fn type_to_string(&self, ty: &ResolvedType) -> String {
        match ty {
            ResolvedType::Type(id) => format!("Type({})", id.index()),
            ResolvedType::Table(id) => format!("Table({})", id.index()),
            ResolvedType::Void => "void".to_string(),
            ResolvedType::Generic { base, args } => {
                format!("Generic({}, [{}])", base.index(), 
                       args.iter().map(|a| self.type_to_string(a)).collect::<Vec<_>>().join(", "))
            }
            ResolvedType::Function { params, return_type } => {
                format!("({}) -> {}", 
                       params.iter().map(|p| self.type_to_string(p)).collect::<Vec<_>>().join(", "),
                       self.type_to_string(return_type))
            }
        }
    }

    fn get_current_function_return_type(&self, program: &Program) -> Option<ResolvedType> {
        if let Some(func_id) = self.current_function {
            let func = &program.functions[func_id];
            if let Some(return_type_id) = func.return_type {
                self.resolve_type_reference(return_type_id, program)
            } else {
                Some(ResolvedType::Void)
            }
        } else {
            None
        }
    }

    // ========================================================================
    // --- Generic Type Management
    // ========================================================================

    fn push_generic_scope(&mut self) {
        self.generic_context.push(HashMap::new());
    }

    fn pop_generic_scope(&mut self) {
        self.generic_context.pop();
    }

    fn add_generic_param(&mut self, name: String, param_id: GenericParamId) {
        if let Some(current_scope) = self.generic_context.last_mut() {
            current_scope.insert(name, param_id);
        }
    }

    fn lookup_generic_param(&self, name: &str) -> Option<GenericParamId> {
        for scope in self.generic_context.iter().rev() {
            if let Some(param_id) = scope.get(name) {
                return Some(*param_id);
            }
        }
        None
    }
}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Perform type resolution on a program
pub fn resolve_types(program: Program) -> Results<Program> {
    TypeResolver::resolve_types(program)
}
