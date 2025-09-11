use std::collections::HashMap;
use crate::ast_old::*;

pub struct TypeResolver<'a> {
    program: &'a mut Program,
    /// Current type substitutions for generics
    type_substitutions: HashMap<TypeVarId, ResolvedType>,
    /// Current scope for type variables
    type_var_counter: u32,
}

impl<'a> TypeResolver<'a> {
    pub fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            type_substitutions: HashMap::new(),
            type_var_counter: 0,
        }
    }

    /// Main entry point for type resolution
    pub fn resolve_types(&mut self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Resolve types for all functions first (for forward references)
        for (func_id, _) in self.program.functions.iter() {
            if let Err(mut errs) = self.resolve_function_signature(func_id) {
                errors.append(&mut errs);
            }
        }

        // Resolve types for all tables
        for (table_id, _) in self.program.tables.iter() {
            if let Err(mut errs) = self.resolve_table(table_id) {
                errors.append(&mut errs);
            }
        }

        // Resolve types for all constants
        for (const_id, _) in self.program.const_decls.iter() {
            if let Err(mut errs) = self.resolve_const_decl(const_id) {
                errors.append(&mut errs);
            }
        }

        // Resolve function bodies after signatures are resolved
        for (func_id, _) in self.program.functions.iter() {
            if let Err(mut errs) = self.resolve_function_body(func_id) {
                errors.append(&mut errs);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Resolve a type ID to a ResolvedType
    fn resolve_type_id(&self, type_id: TypeId) -> Result<ResolvedType, String> {
        let type_decl = &self.program.types[type_id];
        match &type_decl.kind {
            TypeKind::Primitive(prim) => Ok(ResolvedType::Primitive(*prim)),
            TypeKind::UserDefined { name, type_args } => {
                // Check if this is a generic type
                if let Some(resolved) = name.resolved_declaration {
                    match resolved {
                        IdentifierResolution::Type(target_type_id) => {
                            let target_type = &self.program.types[target_type_id];
                            match &target_type.kind {
                                TypeKind::Generic { name: _, params } => {
                                    // Handle generic instantiation
                                    let mut substitutions = HashMap::new();
                                    if type_args.len() != params.len() {
                                        return Err(format!("Generic type {} expects {} type arguments, got {}", 
                                            name.name, params.len(), type_args.len()));
                                    }
                                    for (param_id, arg_id) in params.iter().zip(type_args.iter()) {
                                        let param = &self.program.generic_params[*param_id];
                                        let arg_type = self.resolve_type_id(*arg_id)?;
                                        substitutions.insert(param.type_var_id, arg_type);
                                    }
                                    // For now, return the user-defined type with substitutions
                                    // TODO: Apply substitutions to the generic type body
                                    Ok(ResolvedType::UserDefined {
                                        name: name.name.clone(),
                                        type_args: type_args.iter()
                                            .map(|&arg_id| self.resolve_type_id(arg_id))
                                            .collect::<Result<Vec<_>, _>>()?,
                                    })
                                }
                                _ => Ok(ResolvedType::UserDefined {
                                    name: name.name.clone(),
                                    type_args: type_args.iter()
                                        .map(|&arg_id| self.resolve_type_id(arg_id))
                                        .collect::<Result<Vec<_>, _>>()?,
                                })
                            }
                        }
                        IdentifierResolution::Table(table_id) => {
                            // This is Table<T> where T is the table name, not a type
                            if type_args.len() != 1 {
                                return Err(format!("Table type expects exactly 1 argument (table name), got {}", type_args.len()));
                            }
                            let table_name_type = self.resolve_type_id(type_args[0])?;
                            // Extract table name from the type argument
                            let table_name = match table_name_type {
                                ResolvedType::UserDefined { name, .. } => name,
                                _ => return Err("Table type argument must be a table name".to_string()),
                            };
                            Ok(ResolvedType::Table { table_name })
                        }
                        _ => Err(format!("Type name {} does not resolve to a type", name.name)),
                    }
                } else {
                    // Unresolved type - this might be a built-in or should have been resolved by name resolver
                    match name.name.as_str() {
                        "List" => {
                            if type_args.len() != 1 {
                                return Err(format!("List type expects exactly 1 type argument, got {}", type_args.len()));
                            }
                            let element_type = self.resolve_type_id(type_args[0])?;
                            Ok(ResolvedType::List {
                                element_type: Box::new(element_type),
                            })
                        }
                        _ => Ok(ResolvedType::UserDefined {
                            name: name.name.clone(),
                            type_args: type_args.iter()
                                .map(|&arg_id| self.resolve_type_id(arg_id))
                                .collect::<Result<Vec<_>, _>>()?,
                        })
                    }
                }
            }
            TypeKind::Generic { name, params: _ } => {
                // Look up in current substitutions
                let type_var_id = TypeVarId(name.name.clone());
                if let Some(substituted) = self.type_substitutions.get(&type_var_id) {
                    Ok(substituted.clone())
                } else {
                    Ok(ResolvedType::TypeVariable { id: type_var_id })
                }
            }
        }
    }

    /// Resolve function signature types
    fn resolve_function_signature(&mut self, func_id: CallableDeclId) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Save current substitutions
        let saved_substitutions = self.type_substitutions.clone();

        // Set up generic parameters for this function
        let func = &self.program.functions[func_id];
        for &generic_param_id in &func.generic_params {
            let param = &self.program.generic_params[generic_param_id];
            self.type_substitutions.insert(param.type_var_id.clone(), 
                ResolvedType::TypeVariable { id: param.type_var_id.clone() });
        }

        // Resolve parameter types
        let mut param_types = Vec::new();
        for &param_id in &func.params {
            match self.resolve_type_id(self.program.params[param_id].ty) {
                Ok(resolved_type) => {
                    param_types.push(resolved_type.clone());
                    // Update the parameter's resolved_type
                    let param = &mut self.program.params[param_id];
                    param.resolved_type = Some(resolved_type);
                }
                Err(err) => errors.push(format!("Parameter type error in function {}: {}", 
                    func.name.name, err)),
            }
        }

        // Resolve return type
        let resolved_return_type = if let Some(return_type_id) = func.return_type {
            match self.resolve_type_id(return_type_id) {
                Ok(resolved) => Some(resolved),
                Err(err) => {
                    errors.push(format!("Return type error in function {}: {}", func.name.name, err));
                    None
                }
            }
        } else {
            Some(ResolvedType::Void)
        };

        // Update function's resolved types
        let func = &mut self.program.functions[func_id];
        func.resolved_return_type = resolved_return_type.clone();
        func.resolved_function_type = Some(ResolvedType::Function {
            params: param_types,
            return_type: Box::new(resolved_return_type.unwrap_or(ResolvedType::Void)),
        });

        // Restore substitutions
        self.type_substitutions = saved_substitutions;

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// Resolve function body (statements and expressions)
    fn resolve_function_body(&mut self, func_id: CallableDeclId) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Save current substitutions
        let saved_substitutions = self.type_substitutions.clone();

        // Set up generic parameters for this function
        let func = &self.program.functions[func_id];
        for &generic_param_id in &func.generic_params {
            let param = &self.program.generic_params[generic_param_id];
            self.type_substitutions.insert(param.type_var_id.clone(), 
                ResolvedType::TypeVariable { id: param.type_var_id.clone() });
        }

        // Resolve statements in function body
        if let Some(body_id) = func.body {
            if let Err(mut stmt_errors) = self.resolve_statement(body_id) {
                errors.append(&mut stmt_errors);
            }
        }

        // Restore substitutions
        self.type_substitutions = saved_substitutions;

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// Resolve table types
    fn resolve_table(&mut self, table_id: TableDeclId) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        let table = &self.program.tables[table_id];

        for element in &table.elements {
            match element {
                TableElement::Field(field) => {
                    match self.resolve_type_id(field.ty) {
                        Ok(resolved_type) => {
                            // Update the field's resolved_type
                            // Note: We need to find a way to mutably access the field within the table
                            // For now, we'll track this separately and update later
                        }
                        Err(err) => errors.push(format!("Table field {} type error: {}", 
                            field.name.name, err)),
                    }
                }
                TableElement::Index(_) => {
                    // Indexes don't have types to resolve
                }
            }
        }

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// Resolve constant declaration types
    fn resolve_const_decl(&mut self, const_id: ConstDeclId) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        let const_decl = &self.program.const_decls[const_id];

        // Resolve the explicit type
        match self.resolve_type_id(const_decl.ty) {
            Ok(explicit_type) => {
                // Resolve the value expression
                if let Err(mut expr_errors) = self.resolve_expression(const_decl.value) {
                    errors.append(&mut expr_errors);
                }

                // Check if value type matches explicit type
                let value_expr = &self.program.expressions[const_decl.value];
                if let Some(ref value_type) = value_expr.resolved_type() {
                    if !self.types_compatible(&explicit_type, value_type) {
                        errors.push(format!("Constant {} value type {:?} does not match declared type {:?}",
                            const_decl.name.name, value_type, explicit_type));
                    }
                }

                // Update the constant's resolved_type
                let const_decl = &mut self.program.const_decls[const_id];
                const_decl.resolved_type = Some(explicit_type);
            }
            Err(err) => errors.push(format!("Constant {} type error: {}", const_decl.name.name, err)),
        }

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// Resolve statement types
    fn resolve_statement(&mut self, stmt_id: StmtId) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        let stmt = &self.program.statements[stmt_id];

        match &stmt.kind {
            StatementKind::VarDecl(var_id) => {
                let var_decl = &self.program.var_decls[*var_id];
                
                // Resolve explicit type if provided
                let explicit_type = if let Some(type_id) = var_decl.ty {
                    match self.resolve_type_id(type_id) {
                        Ok(t) => Some(t),
                        Err(err) => {
                            errors.push(format!("Variable {} type error: {}", var_decl.name.name, err));
                            None
                        }
                    }
                } else {
                    None
                };

                // Resolve initializer expression if provided
                let inferred_type = if let Some(init_id) = var_decl.init {
                    if let Err(mut expr_errors) = self.resolve_expression(init_id) {
                        errors.append(&mut expr_errors);
                    }
                    self.program.expressions[init_id].resolved_type().cloned()
                } else {
                    None
                };

                // Determine final type
                let final_type = match (explicit_type, inferred_type) {
                    (Some(explicit), Some(inferred)) => {
                        if self.types_compatible(&explicit, &inferred) {
                            Some(explicit)
                        } else {
                            errors.push(format!("Variable {} explicit type {:?} does not match inferred type {:?}",
                                var_decl.name.name, explicit, inferred));
                            Some(explicit) // Use explicit type despite mismatch
                        }
                    }
                    (Some(explicit), None) => Some(explicit),
                    (None, Some(inferred)) => Some(inferred),
                    (None, None) => {
                        errors.push(format!("Variable {} has no type annotation and no initializer", 
                            var_decl.name.name));
                        None
                    }
                };

                // Update variable's resolved type
                if let Some(resolved_type) = final_type {
                    let var_decl = &mut self.program.var_decls[*var_id];
                    var_decl.resolved_type = Some(resolved_type);
                }
            }
            StatementKind::Assignment(lhs, rhs) => {
                if let Err(mut lhs_errors) = self.resolve_expression(*lhs) {
                    errors.append(&mut lhs_errors);
                }
                if let Err(mut rhs_errors) = self.resolve_expression(*rhs) {
                    errors.append(&mut rhs_errors);
                }

                // Check type compatibility
                let lhs_expr = &self.program.expressions[*lhs];
                let rhs_expr = &self.program.expressions[*rhs];
                if let (Some(lhs_type), Some(rhs_type)) = (lhs_expr.resolved_type(), rhs_expr.resolved_type()) {
                    if !self.types_compatible(lhs_type, rhs_type) {
                        errors.push(format!("Assignment type mismatch: {:?} = {:?}", lhs_type, rhs_type));
                    }
                }
            }
            StatementKind::Expression(expr_id) => {
                if let Err(mut expr_errors) = self.resolve_expression(*expr_id) {
                    errors.append(&mut expr_errors);
                }
            }
            StatementKind::If { condition, then_branch, else_branch } => {
                if let Err(mut cond_errors) = self.resolve_expression(*condition) {
                    errors.append(&mut cond_errors);
                }
                if let Err(mut then_errors) = self.resolve_statement(*then_branch) {
                    errors.append(&mut then_errors);
                }
                if let Some(else_id) = else_branch {
                    if let Err(mut else_errors) = self.resolve_statement(*else_id) {
                        errors.append(&mut else_errors);
                    }
                }

                // Check condition type
                let cond_expr = &self.program.expressions[*condition];
                if let Some(cond_type) = cond_expr.resolved_type() {
                    if !matches!(cond_type, ResolvedType::Primitive(PrimitiveType::Bool)) {
                        errors.push(format!("If condition must be boolean, got {:?}", cond_type));
                    }
                }
            }
            StatementKind::While { condition, body } => {
                if let Err(mut cond_errors) = self.resolve_expression(*condition) {
                    errors.append(&mut cond_errors);
                }
                if let Err(mut body_errors) = self.resolve_statement(*body) {
                    errors.append(&mut body_errors);
                }

                // Check condition type
                let cond_expr = &self.program.expressions[*condition];
                if let Some(cond_type) = cond_expr.resolved_type() {
                    if !matches!(cond_type, ResolvedType::Primitive(PrimitiveType::Bool)) {
                        errors.push(format!("While condition must be boolean, got {:?}", cond_type));
                    }
                }
            }
            StatementKind::For { var_decl: _, iterable, body } => {
                if let Err(mut iter_errors) = self.resolve_expression(*iterable) {
                    errors.append(&mut iter_errors);
                }
                if let Err(mut body_errors) = self.resolve_statement(*body) {
                    errors.append(&mut body_errors);
                }
                // TODO: Resolve loop variable type based on iterable type
            }
            StatementKind::Return(expr_opt) => {
                if let Some(expr_id) = expr_opt {
                    if let Err(mut expr_errors) = self.resolve_expression(*expr_id) {
                        errors.append(&mut expr_errors);
                    }
                }
            }
            StatementKind::Block(stmt_ids) => {
                for &stmt_id in stmt_ids {
                    if let Err(mut stmt_errors) = self.resolve_statement(stmt_id) {
                        errors.append(&mut stmt_errors);
                    }
                }
            }
        }

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// Resolve expression types with inference
    fn resolve_expression(&mut self, expr_id: ExprId) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Get a clone of the expression to avoid borrowing issues
        let expr = self.program.expressions[expr_id].clone();
        
        let resolved_type = match &expr {
            Expression::Literal { value, .. } => {
                self.infer_literal_type(value)
            }
            Expression::Identifier { name, .. } => {
                self.resolve_identifier_type(name)
            }
            Expression::Binary { op, left, right, .. } => {
                // Recursively resolve operands
                if let Err(mut left_errors) = self.resolve_expression(*left) {
                    errors.append(&mut left_errors);
                }
                if let Err(mut right_errors) = self.resolve_expression(*right) {
                    errors.append(&mut right_errors);
                }

                let left_type = self.program.expressions[*left].resolved_type().cloned();
                let right_type = self.program.expressions[*right].resolved_type().cloned();
                
                self.resolve_binary_op_type(*op, left_type, right_type)
            }
            Expression::Unary { op, operand, .. } => {
                if let Err(mut operand_errors) = self.resolve_expression(*operand) {
                    errors.append(&mut operand_errors);
                }

                let operand_type = self.program.expressions[*operand].resolved_type().cloned();
                self.resolve_unary_op_type(*op, operand_type)
            }
            Expression::Call { callable, args, .. } => {
                // Resolve arguments
                for &arg_id in args {
                    if let Err(mut arg_errors) = self.resolve_expression(arg_id) {
                        errors.append(&mut arg_errors);
                    }
                }

                // Resolve callable type
                if let Err(mut callable_errors) = self.resolve_expression(*callable) {
                    errors.append(&mut callable_errors);
                }

                let callable_type = self.program.expressions[*callable].resolved_type().cloned();
                self.resolve_call_type(callable_type, args)
            }
            Expression::MemberAccess { object, member, .. } => {
                if let Err(mut obj_errors) = self.resolve_expression(*object) {
                    errors.append(&mut obj_errors);
                }

                let object_type = self.program.expressions[*object].resolved_type().cloned();
                self.resolve_member_access_type(object_type, member)
            }
            Expression::TableRowAccess { table, key, .. } => {
                if let Err(mut table_errors) = self.resolve_expression(*table) {
                    errors.append(&mut table_errors);
                }
                if let Err(mut key_errors) = self.resolve_expression(*key) {
                    errors.append(&mut key_errors);
                }

                let table_type = self.program.expressions[*table].resolved_type().cloned();
                self.resolve_table_access_type(table_type)
            }
            Expression::Assignment { lhs, rhs, .. } => {
                if let Err(mut lhs_errors) = self.resolve_expression(*lhs) {
                    errors.append(&mut lhs_errors);
                }
                if let Err(mut rhs_errors) = self.resolve_expression(*rhs) {
                    errors.append(&mut rhs_errors);
                }

                let lhs_type = self.program.expressions[*lhs].resolved_type().cloned();
                let rhs_type = self.program.expressions[*rhs].resolved_type().cloned();
                
                // Assignment expression type is the type of the assigned value
                match (lhs_type, rhs_type.clone()) {
                    (Some(lhs_t), Some(rhs_t)) => {
                        if self.types_compatible(&lhs_t, &rhs_t) {
                            Ok(rhs_t)
                        } else {
                            errors.push(format!("Assignment type mismatch: {:?} = {:?}", lhs_t, rhs_t));
                            Ok(rhs_t) // Use RHS type anyway
                        }
                    }
                    (_, Some(rhs_t)) => Ok(rhs_t),
                    _ => Err("Could not determine assignment type".to_string()),
                }
            }
            Expression::Lambda { params, return_type, body, .. } => {
                // Resolve parameter types
                let mut param_types = Vec::new();
                for &param_id in params {
                    let param = &self.program.params[param_id];
                    match self.resolve_type_id(param.ty) {
                        Ok(param_type) => {
                            param_types.push(param_type.clone());
                            // Update parameter's resolved type
                            let param_mut = &mut self.program.params[param_id];
                            param_mut.resolved_type = Some(param_type);
                        }
                        Err(err) => {
                            errors.push(format!("Lambda parameter type error: {}", err));
                        }
                    }
                }

                // Resolve body
                if let Err(mut body_errors) = self.resolve_statement(*body) {
                    errors.append(&mut body_errors);
                }

                // Resolve return type
                let resolved_return_type = if let Some(return_type_id) = return_type {
                    match self.resolve_type_id(*return_type_id) {
                        Ok(t) => t,
                        Err(err) => {
                            errors.push(format!("Lambda return type error: {}", err));
                            ResolvedType::Unknown
                        }
                    }
                } else {
                    // Infer from body - simplified for now
                    ResolvedType::Void
                };

                Ok(ResolvedType::Function {
                    params: param_types,
                    return_type: Box::new(resolved_return_type),
                })
            }
            Expression::Grouped { expr, .. } => {
                if let Err(mut inner_errors) = self.resolve_expression(*expr) {
                    errors.append(&mut inner_errors);
                }
                
                // Grouped expression has same type as inner expression
                match self.program.expressions[*expr].resolved_type() {
                    Some(t) => Ok(t.clone()),
                    None => Err("Could not determine grouped expression type".to_string()),
                }
            }
        };

        // Update the expression's resolved type
        match resolved_type {
            Ok(resolved) => {
                let expr_mut = &mut self.program.expressions[expr_id];
                expr_mut.set_resolved_type(Some(resolved));
            }
            Err(err) => {
                errors.push(err);
            }
        }

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    /// Infer type from literal value
    fn infer_literal_type(&self, literal: &Literal) -> Result<ResolvedType, String> {
        match literal {
            Literal::Int(_) => Ok(ResolvedType::Primitive(PrimitiveType::Int)),
            Literal::Float(_) => Ok(ResolvedType::Primitive(PrimitiveType::Float)),
            Literal::String(_) => Ok(ResolvedType::Primitive(PrimitiveType::String)),
            Literal::Bool(_) => Ok(ResolvedType::Primitive(PrimitiveType::Bool)),
        }
    }

    /// Resolve identifier type from its resolution
    fn resolve_identifier_type(&self, name: &Identifier) -> Result<ResolvedType, String> {
        match &name.resolved_declaration {
            Some(IdentifierResolution::Variable(var_id)) => {
                let var_decl = &self.program.var_decls[*var_id];
                var_decl.resolved_type.clone()
                    .ok_or_else(|| format!("Variable {} type not resolved", name.name))
            }
            Some(IdentifierResolution::Parameter(param_id)) => {
                let param = &self.program.params[*param_id];
                param.resolved_type.clone()
                    .ok_or_else(|| format!("Parameter {} type not resolved", name.name))
            }
            Some(IdentifierResolution::Function(func_id)) => {
                let func = &self.program.functions[*func_id];
                func.resolved_function_type.clone()
                    .ok_or_else(|| format!("Function {} type not resolved", name.name))
            }
            Some(IdentifierResolution::Constant(const_id)) => {
                let const_decl = &self.program.const_decls[*const_id];
                const_decl.resolved_type.clone()
                    .ok_or_else(|| format!("Constant {} type not resolved", name.name))
            }
            Some(IdentifierResolution::Table(table_id)) => {
                let table = &self.program.tables[*table_id];
                Ok(ResolvedType::Table { 
                    table_name: table.name.name.clone() 
                })
            }
            Some(IdentifierResolution::Type(_)) => {
                Err(format!("Identifier {} refers to a type, not a value", name.name))
            }
            None => {
                Err(format!("Identifier {} is not resolved", name.name))
            }
        }
    }

    /// Resolve binary operation result type
    fn resolve_binary_op_type(&self, op: BinaryOp, left_type: Option<ResolvedType>, right_type: Option<ResolvedType>) -> Result<ResolvedType, String> {
        match (left_type, right_type) {
            (Some(left), Some(right)) => {
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        if matches!(left, ResolvedType::Primitive(PrimitiveType::Int | PrimitiveType::Float)) &&
                           matches!(right, ResolvedType::Primitive(PrimitiveType::Int | PrimitiveType::Float)) {
                            // Promote to float if either operand is float
                            if matches!(left, ResolvedType::Primitive(PrimitiveType::Float)) || 
                               matches!(right, ResolvedType::Primitive(PrimitiveType::Float)) {
                                Ok(ResolvedType::Primitive(PrimitiveType::Float))
                            } else {
                                Ok(ResolvedType::Primitive(PrimitiveType::Int))
                            }
                        } else {
                            Err(format!("Arithmetic operation {:?} not supported for types {:?} and {:?}", op, left, right))
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        if self.types_comparable(&left, &right) {
                            Ok(ResolvedType::Primitive(PrimitiveType::Bool))
                        } else {
                            Err(format!("Comparison operation {:?} not supported for types {:?} and {:?}", op, left, right))
                        }
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if matches!(left, ResolvedType::Primitive(PrimitiveType::Bool)) &&
                           matches!(right, ResolvedType::Primitive(PrimitiveType::Bool)) {
                            Ok(ResolvedType::Primitive(PrimitiveType::Bool))
                        } else {
                            Err(format!("Logical operation {:?} requires boolean operands, got {:?} and {:?}", op, left, right))
                        }
                    }
                }
            }
            _ => Err("Binary operation operands have unresolved types".to_string()),
        }
    }

    /// Resolve unary operation result type
    fn resolve_unary_op_type(&self, op: UnaryOp, operand_type: Option<ResolvedType>) -> Result<ResolvedType, String> {
        match operand_type {
            Some(operand) => {
                match op {
                    UnaryOp::Plus | UnaryOp::Minus => {
                        if matches!(operand, ResolvedType::Primitive(PrimitiveType::Int | PrimitiveType::Float)) {
                            Ok(operand)
                        } else {
                            Err(format!("Unary arithmetic operation {:?} not supported for type {:?}", op, operand))
                        }
                    }
                    UnaryOp::Not => {
                        if matches!(operand, ResolvedType::Primitive(PrimitiveType::Bool)) {
                            Ok(ResolvedType::Primitive(PrimitiveType::Bool))
                        } else {
                            Err(format!("Logical not operation requires boolean operand, got {:?}", operand))
                        }
                    }
                }
            }
            None => Err("Unary operation operand has unresolved type".to_string()),
        }
    }

    /// Resolve function call result type
    fn resolve_call_type(&self, callable_type: Option<ResolvedType>, args: &[ExprId]) -> Result<ResolvedType, String> {
        match callable_type {
            Some(ResolvedType::Function { params, return_type }) => {
                // Check argument count
                if args.len() != params.len() {
                    return Err(format!("Function call expects {} arguments, got {}", params.len(), args.len()));
                }

                // Check argument types
                for (i, (&arg_id, expected_type)) in args.iter().zip(params.iter()).enumerate() {
                    let arg_expr = &self.program.expressions[arg_id];
                    if let Some(arg_type) = arg_expr.resolved_type() {
                        if !self.types_compatible(expected_type, arg_type) {
                            return Err(format!("Argument {} type mismatch: expected {:?}, got {:?}", 
                                i + 1, expected_type, arg_type));
                        }
                    }
                }

                Ok(*return_type.clone())
            }
            Some(other) => Err(format!("Cannot call non-function type {:?}", other)),
            None => Err("Cannot determine callable type".to_string()),
        }
    }

    /// Resolve member access type
    fn resolve_member_access_type(&self, object_type: Option<ResolvedType>, member: &Identifier) -> Result<ResolvedType, String> {
        match object_type {
            Some(ResolvedType::Table { table_name }) => {
                // Look up the table and find the field
                for (_, table) in self.program.tables.iter() {
                    if table.name.name == table_name {
                        for element in &table.elements {
                            if let TableElement::Field(field) = element {
                                if field.name.name == member.name {
                                    return field.resolved_type.clone()
                                        .ok_or_else(|| format!("Table field {} type not resolved", member.name));
                                }
                            }
                        }
                        return Err(format!("Table {} has no field {}", table_name, member.name));
                    }
                }
                Err(format!("Table {} not found", table_name))
            }
            Some(ResolvedType::UserDefined { name, .. }) => {
                // For user-defined types, we'd need to look up their structure
                // For now, return unknown
                Ok(ResolvedType::Unknown)
            }
            Some(other) => Err(format!("Member access not supported for type {:?}", other)),
            None => Err("Cannot determine object type for member access".to_string()),
        }
    }

    /// Resolve table row access type  
    fn resolve_table_access_type(&self, table_type: Option<ResolvedType>) -> Result<ResolvedType, String> {
        match table_type {
            Some(ResolvedType::Table { table_name }) => {
                // Table[key] returns a row type - for now, return the table type itself
                // In a more complete implementation, this would return a row type
                Ok(ResolvedType::Table { table_name })
            }
            Some(other) => Err(format!("Cannot index non-table type {:?}", other)),
            None => Err("Cannot determine table type for indexing".to_string()),
        }
    }

    /// Check if two types are compatible for assignment
    fn types_compatible(&self, target: &ResolvedType, source: &ResolvedType) -> bool {
        match (target, source) {
            (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) => a == b,
            (ResolvedType::UserDefined { name: name1, .. }, ResolvedType::UserDefined { name: name2, .. }) => {
                name1 == name2 // Simplified check
            }
            (ResolvedType::Table { table_name: name1 }, ResolvedType::Table { table_name: name2 }) => {
                name1 == name2
            }
            (ResolvedType::List { element_type: elem1 }, ResolvedType::List { element_type: elem2 }) => {
                self.types_compatible(elem1, elem2)
            }
            (ResolvedType::Function { params: p1, return_type: r1 }, 
             ResolvedType::Function { params: p2, return_type: r2 }) => {
                p1.len() == p2.len() && 
                p1.iter().zip(p2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2)) &&
                self.types_compatible(r1, r2)
            }
            (ResolvedType::TypeVariable { id: id1 }, ResolvedType::TypeVariable { id: id2 }) => {
                id1 == id2
            }
            _ => false,
        }
    }

    /// Check if two types can be compared
    fn types_comparable(&self, left: &ResolvedType, right: &ResolvedType) -> bool {
        match (left, right) {
            (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) => {
                // All primitives can be compared with themselves, numbers can be compared with each other
                a == b || (matches!(a, PrimitiveType::Int | PrimitiveType::Float) && 
                          matches!(b, PrimitiveType::Int | PrimitiveType::Float))
            }
            (ResolvedType::UserDefined { name: name1, .. }, ResolvedType::UserDefined { name: name2, .. }) => {
                name1 == name2
            }
            _ => self.types_compatible(left, right), // Compatible types are comparable
        }
    }
}

// Extension trait to add resolved_type accessors to Expression
trait ExpressionTypeExt {
    fn resolved_type(&self) -> Option<&ResolvedType>;
    fn set_resolved_type(&mut self, resolved_type: Option<ResolvedType>);
}

impl ExpressionTypeExt for Expression {
    fn resolved_type(&self) -> Option<&ResolvedType> {
        match self {
            Expression::Literal { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Identifier { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Binary { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Unary { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Call { resolved_type, .. } => resolved_type.as_ref(),
            Expression::MemberAccess { resolved_type, .. } => resolved_type.as_ref(),
            Expression::TableRowAccess { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Assignment { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Lambda { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Grouped { resolved_type, .. } => resolved_type.as_ref(),
        }
    }

    fn set_resolved_type(&mut self, new_resolved_type: Option<ResolvedType>) {
        match self {
            Expression::Literal { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Identifier { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Binary { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Unary { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Call { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::MemberAccess { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::TableRowAccess { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Assignment { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Lambda { resolved_type, .. } => *resolved_type = new_resolved_type,
            Expression::Grouped { resolved_type, .. } => *resolved_type = new_resolved_type,
        }
    }
}
