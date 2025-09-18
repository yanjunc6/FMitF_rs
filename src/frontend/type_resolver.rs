//! src/frontend/type_resolver.rs
//!
//! Complete type inference system for the FMitF compiler.

use crate::ast::visit::{self, Visitor};
use crate::ast::visit_mut::{self, VisitorMut};
use crate::ast::*;
use crate::frontend::errors::FrontEndErrorKind;
use crate::util::{CompilerError, Span};
use std::collections::HashMap;

// ----------------------------------------------------------------
// Main resolve function
// ----------------------------------------------------------------

pub fn resolve_types(program: &mut Program) -> Result<(), Vec<CompilerError>> {
    // Pass 1: Collect top-level type and table names.
    let mut type_collector = TypeCollector::new();
    let _ = visit::walk_program(&mut type_collector, program);
    let (type_name_to_id, table_name_to_id) = type_collector.get_maps();

    // Pass 2: Resolve function signatures.
    let mut signature_resolver = FunctionSignatureResolver {
        type_name_to_id: &type_name_to_id,
        table_name_to_id: &table_name_to_id,
        type_var_counter: 0,
    };
    let _ = signature_resolver.visit_program(program);

    // Pass 3: Resolve types in function bodies, variables, and expressions.
    let mut resolver = TypeResolver::new(type_name_to_id, table_name_to_id);
    resolver.resolve(program)
}

// ----------------------------------------------------------------
// Pass 1: Type Collector
// ----------------------------------------------------------------

struct TypeCollector {
    type_name_to_id: HashMap<String, TypeDeclId>,
    table_name_to_id: HashMap<String, TableId>,
}

impl TypeCollector {
    fn new() -> Self {
        Self {
            type_name_to_id: HashMap::new(),
            table_name_to_id: HashMap::new(),
        }
    }

    fn get_maps(self) -> (HashMap<String, TypeDeclId>, HashMap<String, TableId>) {
        (self.type_name_to_id, self.table_name_to_id)
    }
}

impl<'ast> Visitor<'ast, (), ()> for TypeCollector {
    fn visit_type_decl(
        &mut self,
        _prog: &'ast Program,
        id: TypeDeclId,
        decl: &'ast TypeDecl,
    ) -> Result<(), ()> {
        self.type_name_to_id.insert(decl.name.name.clone(), id);
        Ok(())
    }

    fn visit_table_decl(
        &mut self,
        _prog: &'ast Program,
        id: TableId,
        decl: &'ast TableDecl,
    ) -> Result<(), ()> {
        self.table_name_to_id.insert(decl.name.name.clone(), id);
        Ok(())
    }
}

// ----------------------------------------------------------------
// Pass 2: Function Signature Resolver
// ----------------------------------------------------------------

struct FunctionSignatureResolver<'a> {
    type_name_to_id: &'a HashMap<String, TypeDeclId>,
    table_name_to_id: &'a HashMap<String, TableId>,
    type_var_counter: u32,
}

impl<'a> FunctionSignatureResolver<'a> {
    fn fresh_type_var(&mut self) -> u32 {
        let var_id = self.type_var_counter;
        self.type_var_counter += 1;
        var_id
    }

    fn resolve_ast_type(&mut self, program: &Program, ast_type_id: AstTypeId) -> ResolvedType {
        let ast_type = &program.types[ast_type_id];

        match ast_type {
            AstType::Named { name, .. } => {
                if let Some(&type_id) = self.type_name_to_id.get(&name.name) {
                    ResolvedType::Primitive {
                        type_id,
                        type_args: vec![],
                        bound_vars: vec![],
                    }
                } else if let Some(&table_id) = self.table_name_to_id.get(&name.name) {
                    ResolvedType::Table { table_id }
                } else {
                    ResolvedType::TypeVariable {
                        var_id: self.fresh_type_var(),
                        name: name.name.clone(),
                        bound_to: None,
                    }
                }
            }
            AstType::Generic { base, args, .. } => {
                if let Some(&type_id) = self.type_name_to_id.get(&base.name) {
                    let type_args = args
                        .iter()
                        .map(|&arg_id| self.resolve_ast_type(program, arg_id))
                        .collect();
                    ResolvedType::Primitive {
                        type_id,
                        type_args,
                        bound_vars: vec![],
                    }
                } else {
                    ResolvedType::Unknown
                }
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|&param_id| self.resolve_ast_type(program, param_id))
                    .collect();
                let ret_type = self.resolve_ast_type(program, *return_type);
                ResolvedType::Function {
                    param_types,
                    return_type: Box::new(ret_type),
                    bound_vars: vec![],
                }
            }
        }
    }
}

impl<'ast, 'a> VisitorMut<'ast, (), ()> for FunctionSignatureResolver<'a> {
    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) -> Result<(), ()> {
        let (param_ids, return_type) = {
            let function = &prog.functions[id];
            (function.params.clone(), function.return_type)
        };

        let mut resolved_param_types = Vec::new();
        for param_id in &param_ids {
            let ast_type_id = prog.params[*param_id].ty;
            let resolved_type = self.resolve_ast_type(prog, ast_type_id);
            prog.params[*param_id].resolved_type = Some(resolved_type.clone());
            resolved_param_types.push(resolved_type);
        }

        let resolved_return_type = if let Some(return_type_id) = return_type {
            self.resolve_ast_type(prog, return_type_id)
        } else {
            ResolvedType::Void
        };

        let function = &mut prog.functions[id];
        function.resolved_param_types = Some(resolved_param_types);
        function.resolved_return_type = Some(resolved_return_type);

        Ok(())
    }
}

// ----------------------------------------------------------------
// Pass 3: Type Resolver
// ----------------------------------------------------------------

#[allow(dead_code)]
struct TypeResolver {
    errors: Vec<CompilerError>,
    type_var_counter: u32,
    substitutions: HashMap<u32, ResolvedType>,
    type_name_to_id: HashMap<String, TypeDeclId>,
    table_name_to_id: HashMap<String, TableId>,
    current_generic_bindings: HashMap<GenericParamId, u32>,
}

impl TypeResolver {
    fn new(
        type_name_to_id: HashMap<String, TypeDeclId>,
        table_name_to_id: HashMap<String, TableId>,
    ) -> Self {
        Self {
            errors: Vec::new(),
            type_var_counter: 0,
            substitutions: HashMap::new(),
            type_name_to_id,
            table_name_to_id,
            current_generic_bindings: HashMap::new(),
        }
    }

    fn display_type(&self, program: &Program, ty: &ResolvedType) -> String {
        match ty {
            ResolvedType::Primitive {
                type_id, type_args, ..
            } => {
                let type_decl = &program.type_decls[*type_id];
                let base_name = &type_decl.name.name;
                if type_args.is_empty() {
                    base_name.clone()
                } else {
                    let args_str = type_args
                        .iter()
                        .map(|arg| self.display_type(program, arg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", base_name, args_str)
                }
            }
            ResolvedType::Table { table_id } => {
                let table_decl = &program.table_decls[*table_id];
                table_decl.name.name.clone()
            }
            ResolvedType::TypeVariable { name, .. } => name.clone(),
            ResolvedType::Function {
                param_types,
                return_type,
                ..
            } => {
                let params_str = param_types
                    .iter()
                    .map(|p| self.display_type(program, p))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_str = self.display_type(program, return_type);
                format!("({}) -> {}", params_str, ret_str)
            }
            ResolvedType::Void => "void".to_string(),
            ResolvedType::Unknown => "unknown".to_string(),
            ResolvedType::Unresolved(ast_type_id) => format!("unresolved_{:?}", ast_type_id),
        }
    }

    fn resolve(&mut self, program: &mut Program) -> Result<(), Vec<CompilerError>> {
        self.visit_program(program)
            .map_err(|_| std::mem::take(&mut self.errors))?;
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn fresh_type_var(&mut self) -> u32 {
        let var_id = self.type_var_counter;
        self.type_var_counter += 1;
        var_id
    }

    fn resolve_ast_type(&mut self, program: &Program, ast_type_id: AstTypeId) -> ResolvedType {
        let ast_type = &program.types[ast_type_id];

        match ast_type {
            AstType::Named { name, .. } => {
                if let Some(&type_id) = self.type_name_to_id.get(&name.name) {
                    ResolvedType::Primitive {
                        type_id,
                        type_args: vec![],
                        bound_vars: vec![],
                    }
                } else if let Some(&table_id) = self.table_name_to_id.get(&name.name) {
                    ResolvedType::Table { table_id }
                } else {
                    ResolvedType::TypeVariable {
                        var_id: self.fresh_type_var(),
                        name: name.name.clone(),
                        bound_to: None,
                    }
                }
            }
            AstType::Generic { base, args, .. } => {
                if let Some(&type_id) = self.type_name_to_id.get(&base.name) {
                    let type_args = args
                        .iter()
                        .map(|&arg_id| self.resolve_ast_type(program, arg_id))
                        .collect();
                    ResolvedType::Primitive {
                        type_id,
                        type_args,
                        bound_vars: vec![],
                    }
                } else {
                    ResolvedType::Unknown
                }
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|&param_id| self.resolve_ast_type(program, param_id))
                    .collect();
                let ret_type = self.resolve_ast_type(program, *return_type);
                ResolvedType::Function {
                    param_types,
                    return_type: Box::new(ret_type),
                    bound_vars: vec![],
                }
            }
        }
    }

    fn resolve_literal_type(&self, literal: &Literal) -> ResolvedType {
        let type_name = match literal {
            Literal::Integer(_) => "int",
            Literal::Float(_) => "float",
            Literal::String(_) => "string",
            Literal::Bool(_) => "bool",
            _ => return ResolvedType::Unknown,
        };

        if let Some(&type_id) = self.type_name_to_id.get(type_name) {
            ResolvedType::Primitive {
                type_id,
                type_args: vec![],
                bound_vars: vec![],
            }
        } else {
            ResolvedType::Unknown
        }
    }

    fn resolve_identifier_type(
        &mut self,
        program: &Program,
        resolutions: &[IdentifierResolution],
    ) -> ResolvedType {
        if resolutions.len() != 1 {
            return ResolvedType::TypeVariable {
                var_id: self.fresh_type_var(),
                name: "unknown".to_string(),
                bound_to: None,
            };
        }

        match resolutions[0] {
            IdentifierResolution::Var(var_id) => program.var_decls[var_id]
                .resolved_type
                .clone()
                .unwrap_or_else(|| ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "var_type".to_string(),
                    bound_to: None,
                }),
            IdentifierResolution::Param(param_id) => program.params[param_id]
                .resolved_type
                .clone()
                .unwrap_or_else(|| ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "param_type".to_string(),
                    bound_to: None,
                }),
            IdentifierResolution::Table(table_id) => ResolvedType::Table { table_id },
            IdentifierResolution::Function(func_id) => {
                if let Some(func) = program.functions.get(func_id) {
                    if let (Some(params), Some(ret)) = (
                        func.resolved_param_types.clone(),
                        func.resolved_return_type.clone(),
                    ) {
                        return ResolvedType::Function {
                            param_types: params,
                            return_type: Box::new(ret),
                            bound_vars: vec![],
                        };
                    }
                }
                ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "func_type".to_string(),
                    bound_to: None,
                }
            }
            _ => ResolvedType::TypeVariable {
                var_id: self.fresh_type_var(),
                name: "other_type".to_string(),
                bound_to: None,
            },
        }
    }

    fn set_expression_type(&self, prog: &mut Program, expr_id: ExprId, ty: ResolvedType) {
        match &mut prog.expressions[expr_id] {
            Expression::Literal { resolved_type, .. }
            | Expression::Identifier { resolved_type, .. }
            | Expression::Binary { resolved_type, .. }
            | Expression::Unary { resolved_type, .. }
            | Expression::Assignment { resolved_type, .. }
            | Expression::Call { resolved_type, .. }
            | Expression::MemberAccess { resolved_type, .. }
            | Expression::TableRowAccess { resolved_type, .. }
            | Expression::Grouped { resolved_type, .. }
            | Expression::Lambda { resolved_type, .. } => *resolved_type = Some(ty),
        }
    }

    fn unify(&mut self, a: &ResolvedType, b: &ResolvedType, program: &Program, span: Span) {
        let a = self.apply_substitutions(a);
        let b = self.apply_substitutions(b);

        match (a, b) {
            (
                ResolvedType::TypeVariable { var_id: id_a, .. },
                ResolvedType::TypeVariable { var_id: id_b, .. },
            ) if id_a == id_b => {}
            (ResolvedType::TypeVariable { var_id, .. }, other)
            | (other, ResolvedType::TypeVariable { var_id, .. }) => {
                // TODO: Check for recursive types
                self.substitutions.insert(var_id, other.clone());
            }
            (
                ResolvedType::Primitive {
                    type_id: id_a,
                    type_args: args_a,
                    ..
                },
                ResolvedType::Primitive {
                    type_id: id_b,
                    type_args: args_b,
                    ..
                },
            ) if id_a == id_b => {
                for (arg_a, arg_b) in args_a.iter().zip(args_b.iter()) {
                    self.unify(arg_a, arg_b, program, span);
                }
            }
            (ResolvedType::Table { table_id: id_a }, ResolvedType::Table { table_id: id_b })
                if id_a == id_b => {}
            (
                ResolvedType::Function {
                    param_types: params_a,
                    return_type: ret_a,
                    ..
                },
                ResolvedType::Function {
                    param_types: params_b,
                    return_type: ret_b,
                    ..
                },
            ) => {
                if params_a.len() == params_b.len() {
                    for (p_a, p_b) in params_a.iter().zip(params_b.iter()) {
                        self.unify(p_a, p_b, program, span);
                    }
                    self.unify(&*ret_a, &*ret_b, program, span);
                } else {
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::ArgumentCountMismatch {
                            expected: params_a.len(),
                            found: params_b.len(),
                        },
                        span,
                    ));
                }
            }
            (ResolvedType::Void, ResolvedType::Void) => {}
            (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => {} // Don't propagate errors from unknowns
            (type_a, type_b) => {
                self.errors.push(CompilerError::new(
                    FrontEndErrorKind::TypeMismatch {
                        expected: self.display_type(program, &type_a),
                        found: self.display_type(program, &type_b),
                        context: "unification".to_string(),
                    },
                    span,
                ));
            }
        }
    }

    fn apply_substitutions(&self, ty: &ResolvedType) -> ResolvedType {
        if let ResolvedType::TypeVariable { var_id, .. } = ty {
            if let Some(subst_ty) = self.substitutions.get(&var_id) {
                return self.apply_substitutions(subst_ty);
            }
        }
        ty.clone()
    }
}

impl<'ast> VisitorMut<'ast, (), ()> for TypeResolver {
    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) -> Result<(), ()> {
        let (generic_params, body) = {
            let function = &prog.functions[id];
            (function.generic_params.clone(), function.body)
        };

        self.current_generic_bindings.clear();
        for generic_param_id in generic_params {
            let type_var = self.fresh_type_var();
            self.current_generic_bindings
                .insert(generic_param_id, type_var);
        }

        if let Some(body_id) = body {
            self.visit_block(prog, body_id)?;
        }

        Ok(())
    }

    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<(), ()> {
        visit_mut::walk_expr_mut(self, prog, id)?;

        let expr = prog.expressions[id].clone();
        let error_span = expr.span().unwrap_or_else(|| Span::new(0, 0, ""));
        let resolved_type = match &expr {
            Expression::Literal { value, .. } => self.resolve_literal_type(value),
            Expression::Identifier {
                resolved_declarations,
                ..
            } => self.resolve_identifier_type(prog, resolved_declarations),
            Expression::Binary { left, right, .. } => {
                let left_type = prog.expressions[*left]
                    .resolved_type()
                    .cloned()
                    .unwrap_or(ResolvedType::Unknown);
                let right_type = prog.expressions[*right]
                    .resolved_type()
                    .cloned()
                    .unwrap_or(ResolvedType::Unknown);

                // TODO: Implement proper operator type resolution.
                // For now, we assume the result is unknown if we can't figure it out simply.
                self.unify(&left_type, &right_type, prog, error_span);
                self.apply_substitutions(&left_type)
            }
            Expression::Call { callee, args, .. } => {
                if let Some(callee_type) = prog.expressions[*callee].resolved_type().cloned() {
                    if let ResolvedType::Function {
                        param_types,
                        return_type,
                        ..
                    } = callee_type
                    {
                        let arg_types: Vec<ResolvedType> = args
                            .iter()
                            .map(|arg_id| {
                                prog.expressions[*arg_id]
                                    .resolved_type()
                                    .cloned()
                                    .unwrap_or(ResolvedType::Unknown)
                            })
                            .collect();
                        if param_types.len() == arg_types.len() {
                            for (param_ty, arg_ty) in param_types.iter().zip(arg_types.iter()) {
                                self.unify(param_ty, arg_ty, prog, error_span);
                            }
                            self.set_expression_type(prog, id, *return_type.clone());
                            return Ok(());
                        }
                    }
                }
                ResolvedType::Unknown
            }
            Expression::Assignment { .. } => ResolvedType::Void,
            Expression::TableRowAccess { resolved_table, .. } => {
                if let Some(table_id) = resolved_table {
                    if let Some(&row_type_id) = self.type_name_to_id.get("Row") {
                        ResolvedType::Primitive {
                            type_id: row_type_id,
                            type_args: vec![ResolvedType::Table {
                                table_id: *table_id,
                            }],
                            bound_vars: vec![],
                        }
                    } else {
                        ResolvedType::Unknown
                    }
                } else {
                    ResolvedType::Unknown
                }
            }
            Expression::Grouped { expr, .. } => {
                if let Some(inner_type) = prog.expressions[*expr].resolved_type() {
                    inner_type.clone()
                } else {
                    ResolvedType::Unknown
                }
            }
            _ => ResolvedType::TypeVariable {
                var_id: self.fresh_type_var(),
                name: "expr_type".to_string(),
                bound_to: None,
            },
        };

        self.set_expression_type(prog, id, resolved_type);
        Ok(())
    }

    fn visit_var_decl(&mut self, prog: &mut Program, id: VarId) -> Result<(), ()> {
        let (ty, init_id, span, name) = {
            let var_decl = &prog.var_decls[id];
            (
                var_decl.ty,
                var_decl.init,
                var_decl.span.clone(),
                var_decl.name.name.clone(),
            )
        };

        let error_span = span.unwrap_or_else(|| Span::new(0, 0, ""));

        if let Some(init_id) = init_id {
            self.visit_expr(prog, init_id)?;
        }

        let var_type = if let Some(ty_id) = ty {
            self.resolve_ast_type(prog, ty_id)
        } else if let Some(init_id) = init_id {
            prog.expressions[init_id]
                .resolved_type()
                .cloned()
                .unwrap_or(ResolvedType::Unknown)
        } else {
            // Error: cannot infer type
            self.errors.push(CompilerError::new(
                FrontEndErrorKind::CannotInferType { item: name },
                error_span,
            ));
            ResolvedType::Unknown
        };

        if let Some(init_id) = init_id {
            if let Some(init_type) = prog.expressions[init_id].resolved_type() {
                self.unify(&var_type, init_type, prog, error_span);
            }
        }

        prog.var_decls[id].resolved_type = Some(self.apply_substitutions(&var_type));
        Ok(())
    }
}

// Add a helper method to Expression for getting resolved type
impl Expression {
    pub fn resolved_type(&self) -> Option<&ResolvedType> {
        match self {
            Expression::Literal { resolved_type, .. }
            | Expression::Identifier { resolved_type, .. }
            | Expression::Binary { resolved_type, .. }
            | Expression::Unary { resolved_type, .. }
            | Expression::Assignment { resolved_type, .. }
            | Expression::Call { resolved_type, .. }
            | Expression::MemberAccess { resolved_type, .. }
            | Expression::TableRowAccess { resolved_type, .. }
            | Expression::Grouped { resolved_type, .. }
            | Expression::Lambda { resolved_type, .. } => resolved_type.as_ref(),
        }
    }
}
