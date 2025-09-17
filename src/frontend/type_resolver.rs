//! src/frontend/type_resolver.rs
//!
//! Implements the type resolution and checking phase of the compiler.
//! This visitor traverses the AST and resolves the types of expressions,
//! statements, and declarations. It also handles overload resolution for
//! functions and operators.

use crate::ast::item::ConstId;
use crate::ast::visit_mut::VisitorMut;
use crate::ast::*;
use crate::frontend::errors::FrontEndErrorKind;
use crate::util::{CompilerError, Span};
use std::collections::HashMap;

pub fn resolve_types(program: &mut Program) -> Result<(), Vec<CompilerError>> {
    let mut resolver = TypeResolver::new();
    // We explicitly walk the program declarations to control the order.
    // This isn't a typical visitor pattern, but it ensures function signatures are resolved before bodies.
    if let Err(errors) = resolver.resolve_signatures(program) {
        return Err(errors);
    }
    if let Err(errors) = resolver.resolve_bodies(program) {
        return Err(errors);
    }

    if resolver.errors.is_empty() {
        Ok(())
    } else {
        Err(resolver.errors)
    }
}

struct TypeResolver {
    errors: Vec<CompilerError>,
    type_vars: u32,
    substitution: Substitution,
    // A cache for resolved AST types to avoid re-computation.
    ast_type_cache: HashMap<AstTypeId, ResolvedType>,
    // Current generic parameter bindings (GenericParamId -> TypeVarId)
    generic_bindings: HashMap<GenericParamId, TypeVarId>,
}

impl TypeResolver {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            type_vars: 0,
            substitution: Substitution::new(),
            ast_type_cache: HashMap::new(),
            generic_bindings: HashMap::new(),
        }
    }

    /// Find a primitive type by name in the program's type declarations
    fn find_primitive_type(&self, prog: &Program, type_name: &str) -> Option<TypeDeclId> {
        for (type_id, type_decl) in prog.type_decls.iter() {
            if type_decl.name.name == type_name {
                // Check if it's an intrinsic type (has @intrinsic decorator)
                if type_decl
                    .decorators
                    .iter()
                    .any(|d| d.name.name == "intrinsic")
                {
                    return Some(type_id);
                }
            }
        }
        None
    }

    /// Find generic parameter by name in the current function's scope
    fn find_generic_param_in_scope(&self, prog: &Program, name: &str) -> Option<GenericParamId> {
        // Only look at generic parameters that are in our current bindings
        for (&generic_param_id, _) in &self.generic_bindings {
            if let Some(generic_param) = prog.generic_params.get(generic_param_id) {
                if generic_param.name.name == name {
                    return Some(generic_param_id);
                }
            }
        }
        None
    }

    /// Find table by name
    fn find_table_by_name(&self, prog: &Program, name: &str) -> Option<TableId> {
        for (table_id, table_decl) in prog.table_decls.iter() {
            if table_decl.name.name == name {
                return Some(table_id);
            }
        }
        None
    }

    /// Create a primitive type with fallback to type variable
    fn resolve_primitive_type(&mut self, prog: &Program, type_name: &str) -> ResolvedType {
        if let Some(type_id) = self.find_primitive_type(prog, type_name) {
            ResolvedType::Primitive {
                type_id,
                type_args: vec![],
                bound_vars: vec![],
            }
        } else {
            self.new_type_var()
        }
    }

    /// Resolve literal types systematically
    fn resolve_literal_type(&mut self, prog: &Program, literal: &Literal) -> ResolvedType {
        match literal {
            Literal::Integer(_) => self.resolve_primitive_type(prog, "int"),
            Literal::Float(_) => self.resolve_primitive_type(prog, "float"),
            Literal::String(_) => self.resolve_primitive_type(prog, "string"),
            Literal::Bool(_) => self.resolve_primitive_type(prog, "bool"),
            Literal::List(_items) => {
                // For now, treat as a type variable since we don't have the List type declaration here
                self.new_type_var()
            }
            Literal::RowLiteral(_) => self.new_type_var(),
        }
    }

    /// Generic operator resolution for both binary and unary operators
    fn resolve_operator_overloads(
        &mut self,
        prog: &Program,
        operator_name: &str,
        operand_types: &[ResolvedType],
        resolved_callables: &mut Vec<FunctionId>,
        span: Span,
    ) -> Result<ResolvedType, ResolvedType> {
        let mut matching_overloads = Vec::new();

        for func_id in resolved_callables.iter() {
            let func = &prog.functions[*func_id];
            if func.params.len() == operand_types.len() {
                let mut all_match = true;
                for (i, operand_type) in operand_types.iter().enumerate() {
                    if let Some(param_type) = prog.params[func.params[i]].resolved_type.as_ref() {
                        if !self.try_unify(operand_type, param_type) {
                            all_match = false;
                            break;
                        }
                    } else {
                        all_match = false;
                        break;
                    }
                }
                if all_match {
                    matching_overloads.push(*func_id);
                }
            }
        }

        if matching_overloads.len() == 1 {
            let func_id = matching_overloads[0];
            let func = &prog.functions[func_id];
            *resolved_callables = vec![func_id];
            let return_type = func
                .resolved_return_type
                .clone()
                .unwrap_or_else(|| ResolvedType::Unknown);
            Ok(return_type)
        } else {
            let details = if matching_overloads.is_empty() {
                "No matching operator overload found".to_string()
            } else {
                "Ambiguous operator overload".to_string()
            };
            self.errors.push(CompilerError::new(
                FrontEndErrorKind::InvalidOperation {
                    op: operator_name.to_string(),
                    details,
                },
                span,
            ));
            Err(ResolvedType::Unknown)
        }
    }

    /// Stage 1: Resolve all function signatures and explicit types first.
    fn resolve_signatures(&mut self, program: &mut Program) -> Result<(), Vec<CompilerError>> {
        let function_ids: Vec<FunctionId> = program
            .declarations
            .iter()
            .filter_map(|item| match item {
                Item::Callable(id) => Some(*id),
                _ => None,
            })
            .collect();

        for id in function_ids {
            self.visit_callable_decl(program, id).unwrap();
        }

        // Also resolve const declarations in the first pass
        let const_ids: Vec<ConstId> = program
            .declarations
            .iter()
            .filter_map(|item| match item {
                Item::Const(id) => Some(*id),
                _ => None,
            })
            .collect();

        for id in const_ids {
            self.visit_const_decl(program, id).unwrap();
        }

        // Also resolve table declarations to handle field types
        let table_ids: Vec<TableId> = program
            .declarations
            .iter()
            .filter_map(|item| match item {
                Item::Table(id) => Some(*id),
                _ => None,
            })
            .collect();

        for id in table_ids {
            self.visit_table_decl(program, id).unwrap();
        }

        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }
        Ok(())
    }

    /// Stage 2: Resolve function bodies and expressions.
    fn resolve_bodies(&mut self, program: &mut Program) -> Result<(), Vec<CompilerError>> {
        let function_bodies: Vec<(FunctionId, BlockId)> = program
            .declarations
            .iter()
            .filter_map(|item| match item {
                Item::Callable(id) => {
                    if let Some(body_id) = program.functions[*id].body {
                        Some((*id, body_id))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect();

        for (_, body_id) in function_bodies {
            self.visit_block(program, body_id).unwrap();
        }
        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }
        Ok(())
    }

    fn new_type_var(&mut self) -> ResolvedType {
        let var_id = self.type_vars;
        self.type_vars += 1;
        ResolvedType::TypeVariable {
            var_id,
            name: format!("T{}", var_id),
            bound_to: None,
        }
    }

    /// Resolves an `AstTypeId` into a `ResolvedType`, using a cache to avoid re-computation.
    fn resolve_ast_type(&mut self, prog: &Program, id: AstTypeId) -> ResolvedType {
        if let Some(cached) = self.ast_type_cache.get(&id) {
            return cached.clone();
        }

        let ast_type = &prog.types[id];
        let resolved_type = match ast_type {
            AstType::Named { name, .. } => {
                // First, try to find a primitive/intrinsic type declaration
                if let Some(primitive_type_id) = self.find_primitive_type(prog, &name.name) {
                    ResolvedType::Primitive {
                        type_id: primitive_type_id,
                        type_args: vec![],
                        bound_vars: vec![],
                    }
                }
                // Check if this is a table name
                else if let Some(table_id) = self.find_table_by_name(prog, &name.name) {
                    ResolvedType::Table { table_id }
                }
                // Check if this is a generic parameter name
                else if let Some(generic_param_id) =
                    self.find_generic_param_in_scope(prog, &name.name)
                {
                    if let Some(&type_var_id) = self.generic_bindings.get(&generic_param_id) {
                        ResolvedType::TypeVariable {
                            var_id: type_var_id,
                            name: name.name.clone(),
                            bound_to: Some(generic_param_id),
                        }
                    } else {
                        // Generic parameter not in current scope, create unbound type variable
                        self.new_type_var()
                    }
                } else {
                    // For user-defined types or unknown identifiers
                    self.new_type_var()
                }
            }
            AstType::Generic { base, args, .. } => {
                let arg_types: Vec<ResolvedType> = args
                    .iter()
                    .map(|arg_id| self.resolve_ast_type(prog, *arg_id))
                    .collect();

                // Handle generic types as primitive types with type arguments
                // Find the type declaration for the base type
                if let Some(type_id) = self.find_primitive_type(prog, &base.name) {
                    // Check if any type arguments are type variables to determine bound_vars
                    let mut bound_vars = vec![];
                    for arg_type in arg_types.iter() {
                        if let ResolvedType::TypeVariable { var_id, .. } = arg_type {
                            bound_vars.push(*var_id);
                        }
                    }
                    ResolvedType::Primitive {
                        type_id,
                        type_args: arg_types,
                        bound_vars,
                    }
                } else {
                    // Unknown generic type, use type variable
                    self.new_type_var()
                }
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|p_id| self.resolve_ast_type(prog, *p_id))
                    .collect();
                let ret_type = self.resolve_ast_type(prog, *return_type);
                ResolvedType::Function {
                    param_types,
                    return_type: Box::new(ret_type),
                    bound_vars: vec![],
                }
            }
        };

        self.ast_type_cache.insert(id, resolved_type.clone());
        resolved_type
    }

    fn unify(&mut self, t1: &ResolvedType, t2: &ResolvedType, span: Span) {
        let t1 = self.substitution.apply(t1);
        let t2 = self.substitution.apply(t2);

        match (t1.clone(), t2.clone()) {
            (ResolvedType::TypeVariable { var_id, .. }, t)
            | (t, ResolvedType::TypeVariable { var_id, .. }) => {
                if let ResolvedType::TypeVariable {
                    var_id: other_id, ..
                } = t
                {
                    if var_id == other_id {
                        return;
                    }
                }
                self.substitution.extend(var_id, t);
            }
            (
                ResolvedType::Primitive {
                    type_id: id1,
                    type_args: args1,
                    ..
                },
                ResolvedType::Primitive {
                    type_id: id2,
                    type_args: args2,
                    ..
                },
            ) if id1 == id2 && args1.len() == args2.len() => {
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2, span);
                }
            }
            (
                ResolvedType::Function {
                    param_types: params1,
                    return_type: ret1,
                    ..
                },
                ResolvedType::Function {
                    param_types: params2,
                    return_type: ret2,
                    ..
                },
            ) if params1.len() == params2.len() => {
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2, span);
                }
                self.unify(&ret1, &ret2, span);
            }
            (ResolvedType::Void, ResolvedType::Void) => {}
            (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => {}
            (ResolvedType::Unresolved(_), _) | (_, ResolvedType::Unresolved(_)) => {
                self.errors.push(CompilerError::new(
                    FrontEndErrorKind::SyntaxError {
                        message: "Cannot unify unresolved types".to_string(),
                    },
                    span,
                ));
            }
            (t1, t2) if t1 == t2 => {}
            (t1, t2) => {
                self.errors.push(CompilerError::new(
                    FrontEndErrorKind::TypeMismatch {
                        expected: format!("{:?}", t1),
                        found: format!("{:?}", t2),
                    },
                    span,
                ));
            }
        }
    }
}

impl<'ast> VisitorMut<'ast> for TypeResolver {
    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) -> Result<(), ()> {
        // Save the current generic bindings to restore later
        let saved_bindings = self.generic_bindings.clone();

        // Create type variables for this function's generic parameters
        let generic_param_ids = prog.functions[id].generic_params.clone();
        for generic_param_id in generic_param_ids {
            let type_var_id = self.type_vars;
            self.type_vars += 1;
            self.generic_bindings.insert(generic_param_id, type_var_id);
        }

        // First, collect the type info we need
        let params_info: Vec<(ParamId, AstTypeId)> = prog.functions[id]
            .params
            .iter()
            .map(|&param_id| (param_id, prog.params[param_id].ty))
            .collect();

        let return_type_id = prog.functions[id].return_type;

        // Resolve parameter types
        for (param_id, ast_type_id) in params_info {
            let resolved_type = self.resolve_ast_type(prog, ast_type_id);
            prog.params[param_id].resolved_type = Some(resolved_type);
        }

        // Resolve return type
        let resolved_return_type = if let Some(ret_type_id) = return_type_id {
            self.resolve_ast_type(prog, ret_type_id)
        } else {
            ResolvedType::Void
        };

        // Collect bound type variables for this function
        let _bound_vars: Vec<TypeVarId> = self.generic_bindings.values().cloned().collect();

        prog.functions[id].resolved_return_type = Some(resolved_return_type);

        // Restore previous generic bindings
        self.generic_bindings = saved_bindings;
        Ok(())
    }

    fn visit_var_decl(&mut self, prog: &mut Program, id: VarId) -> Result<(), ()> {
        // Collect the data we need first to avoid borrowing issues
        let ty = prog.var_decls[id].ty;
        let init = prog.var_decls[id].init;

        // If the variable has an explicit type, resolve it
        if let Some(ast_type_id) = ty {
            let resolved_type = self.resolve_ast_type(prog, ast_type_id);
            prog.var_decls[id].resolved_type = Some(resolved_type);
        } else if let Some(init_expr_id) = init {
            // If no explicit type but has initializer, resolve the initializer first
            self.visit_expr(prog, init_expr_id)?;
            let init_type = prog.expressions[init_expr_id]
                .resolved_type()
                .cloned()
                .unwrap_or_else(|| self.new_type_var());
            prog.var_decls[id].resolved_type = Some(init_type);
        } else {
            // No type or initializer, create a type variable
            prog.var_decls[id].resolved_type = Some(self.new_type_var());
        }

        // Visit the initializer expression if present
        if let Some(init_expr_id) = init {
            self.visit_expr(prog, init_expr_id)?;
        }

        Ok(())
    }

    fn visit_const_decl(&mut self, prog: &mut Program, id: ConstId) -> Result<(), ()> {
        // Get the const declaration info
        let ast_type_id = prog.const_decls[id].ty;
        let value_expr_id = prog.const_decls[id].value;

        // Resolve the explicit type
        let resolved_type = self.resolve_ast_type(prog, ast_type_id);
        prog.const_decls[id].resolved_type = Some(resolved_type.clone());

        // Visit the value expression to type check it
        self.visit_expr(prog, value_expr_id)?;

        // Ensure the value expression type matches the declared type
        if let Some(value_type) = prog.expressions[value_expr_id].resolved_type() {
            let span = prog.const_decls[id]
                .span
                .unwrap_or_else(|| Span::new(0, 0, "unknown"));
            self.unify(&resolved_type, value_type, span);
        }

        Ok(())
    }

    fn visit_table_decl(&mut self, prog: &mut Program, id: TableId) -> Result<(), ()> {
        // Resolve types for all table fields
        for element in &prog.table_decls[id].elements.clone() {
            if let TableElement::Field(table_field) = element {
                // Resolve the field type
                let resolved_type = self.resolve_ast_type(prog, table_field.ty);

                // Find and update the field in the table declaration
                for element_mut in &mut prog.table_decls[id].elements {
                    if let TableElement::Field(field_mut) = element_mut {
                        if field_mut.name.name == table_field.name.name {
                            field_mut.resolved_type = Some(resolved_type.clone());
                            break;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<(), ()> {
        let mut expr = prog.expressions[id].clone();
        let span = expr.span().unwrap_or_else(|| Span::new(0, 0, "unknown"));

        // Resolve the type of this expression based on its kind
        let _resolved_type = match &mut expr {
            Expression::Literal {
                value,
                resolved_type,
                ..
            } => self.resolve_literal_expression(prog, value, resolved_type, span),
            Expression::Identifier {
                resolved_declarations,
                resolved_type,
                ..
            } => self.resolve_identifier_expression(prog, resolved_declarations, resolved_type),
            Expression::Binary {
                left,
                right,
                op,
                resolved_callables,
                resolved_type,
                ..
            } => self.resolve_binary_expression(
                prog,
                *left,
                *right,
                op,
                resolved_callables,
                resolved_type,
                span,
            )?,
            Expression::Call {
                callee,
                args,
                resolved_type,
                ..
            } => self.resolve_call_expression(prog, *callee, args, resolved_type, span)?,
            Expression::MemberAccess {
                object,
                member,
                resolved_table,
                resolved_field,
                resolved_type,
                ..
            } => self.resolve_member_access_expression(
                prog,
                *object,
                member,
                resolved_table,
                resolved_field,
                resolved_type,
            )?,
            Expression::Assignment {
                lhs,
                rhs,
                resolved_type,
                ..
            } => self.resolve_assignment_expression(prog, *lhs, *rhs, resolved_type)?,
            Expression::Unary {
                expr: inner_expr,
                op,
                resolved_callables,
                resolved_type,
                ..
            } => self.resolve_unary_expression(
                prog,
                *inner_expr,
                op,
                resolved_callables,
                resolved_type,
                span,
            )?,
            Expression::TableRowAccess {
                key_values,
                resolved_table,
                resolved_type,
                ..
            } => self.resolve_table_row_access_expression(
                prog,
                key_values,
                resolved_table,
                resolved_type,
            )?,
            Expression::Lambda {
                params,
                body,
                resolved_type,
                ..
            } => self.resolve_lambda_expression(prog, params, *body, resolved_type)?,
            _ => {
                // Handle other expression types with a type variable
                let ty = self.new_type_var();
                match &mut expr {
                    Expression::Literal { resolved_type, .. } => *resolved_type = Some(ty.clone()),
                    Expression::Identifier { resolved_type, .. } => {
                        *resolved_type = Some(ty.clone())
                    }
                    Expression::Binary { resolved_type, .. } => *resolved_type = Some(ty.clone()),
                    Expression::Unary { resolved_type, .. } => *resolved_type = Some(ty.clone()),
                    Expression::Assignment { resolved_type, .. } => {
                        *resolved_type = Some(ty.clone())
                    }
                    Expression::Call { resolved_type, .. } => *resolved_type = Some(ty.clone()),
                    Expression::MemberAccess { resolved_type, .. } => {
                        *resolved_type = Some(ty.clone())
                    }
                    Expression::TableRowAccess { resolved_type, .. } => {
                        *resolved_type = Some(ty.clone())
                    }
                    Expression::Grouped { resolved_type, .. } => *resolved_type = Some(ty.clone()),
                    Expression::Lambda { resolved_type, .. } => *resolved_type = Some(ty.clone()),
                }
                ty
            }
        };

        // Update the expression in the program
        prog.expressions[id] = expr;
        Ok(())
    }
}

impl TypeResolver {
    fn resolve_literal_expression(
        &mut self,
        prog: &mut Program,
        value: &Literal,
        resolved_type: &mut Option<ResolvedType>,
        span: Span,
    ) -> ResolvedType {
        let literal_type = if let Literal::List(items) = value {
            let element_type = self.new_type_var();
            for item_id in items {
                if let Err(_) = self.visit_expr(prog, *item_id) {
                    continue;
                }
                let item_expr = &prog.expressions[*item_id];
                if let Some(item_type) = item_expr.resolved_type() {
                    self.unify(&element_type, item_type, span);
                }
            }
            // For now, treat list literals as type variables
            // In a full implementation, this would be a List<T> primitive type
            self.new_type_var()
        } else {
            self.resolve_literal_type(prog, value)
        };
        *resolved_type = Some(literal_type.clone());
        literal_type
    }

    fn resolve_identifier_expression(
        &mut self,
        prog: &Program,
        resolved_declarations: &[IdentifierResolution],
        resolved_type: &mut Option<ResolvedType>,
    ) -> ResolvedType {
        if resolved_declarations.len() == 1 {
            let decl = resolved_declarations[0];
            let ty = match decl {
                IdentifierResolution::Var(id) => prog.var_decls[id].resolved_type.clone(),
                IdentifierResolution::Param(id) => prog.params[id].resolved_type.clone(),
                IdentifierResolution::Const(id) => prog.const_decls[id].resolved_type.clone(),
                IdentifierResolution::Function(function_id) => {
                    // Functions have function types based on their signatures
                    let function = &prog.functions[function_id];

                    // Get parameter types
                    let param_types: Vec<ResolvedType> = function
                        .params
                        .iter()
                        .map(|&param_id| {
                            prog.params[param_id]
                                .resolved_type
                                .clone()
                                .unwrap_or_else(|| self.new_type_var())
                        })
                        .collect();

                    // Get return type
                    let return_type = function
                        .resolved_return_type
                        .clone()
                        .unwrap_or_else(|| ResolvedType::Void);

                    Some(ResolvedType::Function {
                        param_types,
                        return_type: Box::new(return_type),
                        bound_vars: vec![],
                    })
                }
                IdentifierResolution::Table(table_id) => {
                    // Table names have type Table<TableName>
                    if let Some(table_type_id) = self.find_primitive_type(prog, "Table") {
                        Some(ResolvedType::Primitive {
                            type_id: table_type_id,
                            type_args: vec![ResolvedType::Table { table_id }],
                            bound_vars: vec![],
                        })
                    } else {
                        Some(self.new_type_var())
                    }
                }
                _ => Some(self.new_type_var()),
            };
            let ty = ty.unwrap_or_else(|| self.new_type_var());
            *resolved_type = Some(ty.clone());
            ty
        } else {
            let ty = self.new_type_var();
            *resolved_type = Some(ty.clone());
            ty
        }
    }

    fn resolve_binary_expression(
        &mut self,
        prog: &mut Program,
        left: ExprId,
        right: ExprId,
        op: &Identifier,
        resolved_callables: &mut Vec<FunctionId>,
        resolved_type: &mut Option<ResolvedType>,
        span: Span,
    ) -> Result<ResolvedType, ()> {
        self.visit_expr(prog, left)?;
        self.visit_expr(prog, right)?;

        let left_type = prog.expressions[left]
            .resolved_type()
            .cloned()
            .unwrap_or(ResolvedType::Unknown);
        let right_type = prog.expressions[right]
            .resolved_type()
            .cloned()
            .unwrap_or(ResolvedType::Unknown);

        let operand_types = vec![left_type, right_type];
        let result_type = match self.resolve_operator_overloads(
            prog,
            &op.name,
            &operand_types,
            resolved_callables,
            span,
        ) {
            Ok(return_type) => return_type,
            Err(fallback_type) => fallback_type,
        };

        *resolved_type = Some(result_type.clone());
        Ok(result_type)
    }

    fn resolve_call_expression(
        &mut self,
        prog: &mut Program,
        callee: ExprId,
        args: &[ExprId],
        resolved_type: &mut Option<ResolvedType>,
        span: Span,
    ) -> Result<ResolvedType, ()> {
        self.visit_expr(prog, callee)?;
        let callee_type = prog.expressions[callee]
            .resolved_type()
            .cloned()
            .unwrap_or_else(|| self.new_type_var());

        let mut arg_types = Vec::new();
        for &arg_id in args {
            self.visit_expr(prog, arg_id)?;
            let arg_type = prog.expressions[arg_id]
                .resolved_type()
                .cloned()
                .unwrap_or_else(|| self.new_type_var());
            arg_types.push(arg_type);
        }

        // If the callee is already a function type, extract its return type
        let return_type = match &callee_type {
            ResolvedType::Function {
                return_type: ret_type,
                param_types,
                ..
            } => {
                // Verify parameter count matches
                if param_types.len() == arg_types.len() {
                    // Unify argument types with parameter types
                    for (arg_type, param_type) in arg_types.iter().zip(param_types.iter()) {
                        self.unify(arg_type, param_type, span);
                    }
                    *ret_type.clone()
                } else {
                    // Parameter count mismatch, create type variable
                    self.new_type_var()
                }
            }
            _ => {
                // Unknown callee type, create expected function type and unify
                let return_type = self.new_type_var();
                let func_type = ResolvedType::Function {
                    param_types: arg_types,
                    return_type: Box::new(return_type.clone()),
                    bound_vars: vec![],
                };
                self.unify(&callee_type, &func_type, span);
                return_type
            }
        };

        *resolved_type = Some(return_type.clone());
        Ok(return_type)
    }

    fn resolve_member_access_expression(
        &mut self,
        prog: &mut Program,
        object: ExprId,
        member: &Identifier,
        resolved_table: &mut Option<TableId>,
        resolved_field: &mut Option<TableField>,
        resolved_type: &mut Option<ResolvedType>,
    ) -> Result<ResolvedType, ()> {
        // First resolve the object type
        self.visit_expr(prog, object)?;
        let object_type = prog.expressions[object]
            .resolved_type()
            .cloned()
            .unwrap_or_else(|| self.new_type_var());

        // Try to resolve based on the object type
        if let ResolvedType::Table { table_id } = &object_type {
            if let Some(table_decl) = prog.table_decls.get(*table_id) {
                for element in &table_decl.elements {
                    if let TableElement::Field(table_field) = element {
                        if table_field.name.name == member.name {
                            let field_type = self.resolve_ast_type(prog, table_field.ty);
                            *resolved_type = Some(field_type.clone());
                            *resolved_table = Some(*table_id);
                            *resolved_field = Some(table_field.clone());
                            return Ok(field_type);
                        }
                    }
                }
            }
        }

        // If we know the table and field from name resolution, use that
        if let (Some(table_id), Some(_field)) = (resolved_table, resolved_field) {
            if let Some(table_decl) = prog.table_decls.get(*table_id) {
                for element in &table_decl.elements {
                    if let TableElement::Field(table_field) = element {
                        if table_field.name.name == member.name {
                            let field_type = self.resolve_ast_type(prog, table_field.ty);
                            *resolved_type = Some(field_type.clone());
                            return Ok(field_type);
                        }
                    }
                }
            }
        }

        // Fallback: create a type variable
        let ty = self.new_type_var();
        *resolved_type = Some(ty.clone());
        Ok(ty)
    }

    fn resolve_assignment_expression(
        &mut self,
        prog: &mut Program,
        lhs: ExprId,
        rhs: ExprId,
        resolved_type: &mut Option<ResolvedType>,
    ) -> Result<ResolvedType, ()> {
        self.visit_expr(prog, lhs)?;
        self.visit_expr(prog, rhs)?;

        // Assignment returns the type of the RHS
        let rhs_type = prog.expressions[rhs]
            .resolved_type()
            .cloned()
            .unwrap_or_else(|| self.new_type_var());

        *resolved_type = Some(rhs_type.clone());
        Ok(rhs_type)
    }

    fn resolve_unary_expression(
        &mut self,
        prog: &mut Program,
        inner_expr: ExprId,
        op: &Identifier,
        resolved_callables: &mut Vec<FunctionId>,
        resolved_type: &mut Option<ResolvedType>,
        span: Span,
    ) -> Result<ResolvedType, ()> {
        self.visit_expr(prog, inner_expr)?;

        let operand_type = prog.expressions[inner_expr]
            .resolved_type()
            .cloned()
            .unwrap_or(ResolvedType::Unknown);

        let operand_types = vec![operand_type];
        let result_type = match self.resolve_operator_overloads(
            prog,
            &op.name,
            &operand_types,
            resolved_callables,
            span,
        ) {
            Ok(return_type) => return_type,
            Err(fallback_type) => fallback_type,
        };

        *resolved_type = Some(result_type.clone());
        Ok(result_type)
    }

    fn resolve_table_row_access_expression(
        &mut self,
        prog: &mut Program,
        key_values: &[KeyValue],
        resolved_table: &Option<TableId>,
        resolved_type: &mut Option<ResolvedType>,
    ) -> Result<ResolvedType, ()> {
        // Visit all key expressions first
        for key_value in key_values {
            self.visit_expr(prog, key_value.value)?;
        }

        // If we know which table this accesses, create the table row type
        if let Some(table_id) = resolved_table {
            let table_type = ResolvedType::Table {
                table_id: *table_id,
            };
            *resolved_type = Some(table_type.clone());
            Ok(table_type)
        } else {
            // Fallback: create a type variable
            let ty = self.new_type_var();
            *resolved_type = Some(ty.clone());
            Ok(ty)
        }
    }
}

trait ExpressionExt {
    fn resolved_type(&self) -> Option<&ResolvedType>;
    fn span(&self) -> Option<Span>;
}

impl ExpressionExt for Expression {
    fn resolved_type(&self) -> Option<&ResolvedType> {
        match self {
            Expression::Literal { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Identifier { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Binary { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Unary { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Assignment { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Call { resolved_type, .. } => resolved_type.as_ref(),
            Expression::MemberAccess { resolved_type, .. } => resolved_type.as_ref(),
            Expression::TableRowAccess { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Grouped { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Lambda { resolved_type, .. } => resolved_type.as_ref(),
        }
    }

    fn span(&self) -> Option<Span> {
        match self {
            Expression::Literal { span, .. } => *span,
            Expression::Identifier { span, .. } => *span,
            Expression::Binary { span, .. } => *span,
            Expression::Unary { span, .. } => *span,
            Expression::Assignment { span, .. } => *span,
            Expression::Call { span, .. } => *span,
            Expression::MemberAccess { span, .. } => *span,
            Expression::TableRowAccess { span, .. } => *span,
            Expression::Grouped { span, .. } => *span,
            Expression::Lambda { span, .. } => *span,
        }
    }
}

impl TypeResolver {
    fn resolve_lambda_expression(
        &mut self,
        prog: &mut Program,
        params: &[ParamId],
        body: BlockId,
        resolved_type: &mut Option<ResolvedType>,
    ) -> Result<ResolvedType, ()> {
        // First resolve parameter types
        for &param_id in params {
            let ast_type_id = prog.params[param_id].ty;
            let resolved_param_type = self.resolve_ast_type(prog, ast_type_id);
            prog.params[param_id].resolved_type = Some(resolved_param_type);
        }

        // Then resolve the body
        self.visit_block(prog, body)?;

        // For lambdas, we'll use a type variable for the return type for now
        let return_type = self.new_type_var();

        // Build the lambda function type
        let param_types: Vec<ResolvedType> = params
            .iter()
            .map(|&param_id| {
                prog.params[param_id]
                    .resolved_type
                    .clone()
                    .unwrap_or_else(|| self.new_type_var())
            })
            .collect();

        let lambda_type = ResolvedType::Function {
            param_types,
            return_type: Box::new(return_type),
            bound_vars: vec![],
        };

        *resolved_type = Some(lambda_type.clone());
        Ok(lambda_type)
    }

    fn try_unify(&self, t1: &ResolvedType, t2: &ResolvedType) -> bool {
        let t1 = self.substitution.apply(t1);
        let t2 = self.substitution.apply(t2);

        match (t1, t2) {
            (ResolvedType::TypeVariable { .. }, _) | (_, ResolvedType::TypeVariable { .. }) => true,
            (
                ResolvedType::Primitive { type_id: id1, .. },
                ResolvedType::Primitive { type_id: id2, .. },
            ) => id1 == id2,
            (
                ResolvedType::Function {
                    param_types: params1,
                    ..
                },
                ResolvedType::Function {
                    param_types: params2,
                    ..
                },
            ) => params1.len() == params2.len(),
            (t1, t2) => t1 == t2,
        }
    }
}
