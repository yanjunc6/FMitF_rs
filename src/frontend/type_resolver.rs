//! src/frontend/type_resolver.rs
//!
//! Hindley-Milner style type resolver with TypeSchemes and proper inference.
//!
//! This implements a two-pass algorithm:
//! 1. First pass: Collect all declarations and build an environment mapping IDs to TypeSchemes.
//! 2. Second pass: Type check function bodies using unification and inference.

use crate::ast::visit::{self, Visitor};
use crate::ast::visit_mut::{self, VisitorMut};
use crate::ast::*;
use crate::frontend::errors::FrontEndErrorKind;
use crate::util::{CompilerError, Span};
use std::collections::HashMap;

// ============================================================================
// --- Core Types for Hindley-Milner System
// ============================================================================

/// A substitution maps inference variables to the types they have been unified with.
type Substitution = HashMap<InferVarId, ResolvedType>;

/// Environment that maps declaration IDs to their type schemes.
#[derive(Debug, Clone)]
struct Environment {
    functions: HashMap<FunctionId, TypeScheme>,
    types: HashMap<TypeDeclId, TypeScheme>,
    tables: HashMap<TableId, TypeScheme>,
    type_name_to_id: HashMap<String, TypeDeclId>,
    table_name_to_id: HashMap<String, TableId>,
}

impl Environment {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            types: HashMap::new(),
            tables: HashMap::new(),
            type_name_to_id: HashMap::new(),
            table_name_to_id: HashMap::new(),
        }
    }
}

// ============================================================================
// --- Main Entry Point
// ============================================================================

pub fn resolve_types(program: &mut Program) -> Result<(), Vec<CompilerError>> {
    // Pass 1: Build environment of TypeSchemes.
    let mut collector = TypeSchemeCollector::new();
    let result = collector.visit_program(program);
    let (environment, mut errors) = collector.into_env_and_errors();

    if let Err(e) = result {
        errors.push(e);
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    // Pass 2: Type check function bodies.
    let mut type_checker = TypeChecker::new(environment);
    let result = type_checker.visit_program(program);
    let mut checker_errors = type_checker.into_errors();

    if let Err(e) = result {
        checker_errors.push(e);
    }
    if !checker_errors.is_empty() {
        Err(checker_errors)
    } else {
        Ok(())
    }
}

// ============================================================================
// --- Pass 1: TypeScheme Collection
// ============================================================================

struct TypeSchemeCollector {
    env: Environment,
    errors: Vec<CompilerError>,
    // For resolving types during collection, we need a map of generic param names to their IDs.
    current_generic_params: HashMap<String, GenericParamId>,
}

impl TypeSchemeCollector {
    fn new() -> Self {
        Self {
            env: Environment::new(),
            errors: Vec::new(),
            current_generic_params: HashMap::new(),
        }
    }

    fn into_env_and_errors(self) -> (Environment, Vec<CompilerError>) {
        (self.env, self.errors)
    }

    fn ast_to_resolved(&self, program: &Program, ast_type_id: AstTypeId) -> ResolvedType {
        let ast_type = &program.types[ast_type_id];
        match ast_type {
            AstType::Named { name, .. } => {
                if let Some(&param_id) = self.current_generic_params.get(&name.name) {
                    ResolvedType::GenericParam(param_id)
                } else if let Some(&type_id) = self.env.type_name_to_id.get(&name.name) {
                    ResolvedType::Declared {
                        decl_id: type_id,
                        args: vec![],
                    }
                } else if let Some(&table_id) = self.env.table_name_to_id.get(&name.name) {
                    ResolvedType::Table { table_id }
                } else {
                    // TODO: This should not happen after name resolution.
                    // We'll create an error and return a placeholder.
                    // TODO: push an error here, identifier has a span
                    ResolvedType::Void // Placeholder
                }
            }
            AstType::Generic { base, args, .. } => {
                if let Some(&type_id) = self.env.type_name_to_id.get(&base.name) {
                    let resolved_args = args
                        .iter()
                        .map(|&arg_id| self.ast_to_resolved(program, arg_id))
                        .collect();
                    ResolvedType::Declared {
                        decl_id: type_id,
                        args: resolved_args,
                    }
                } else {
                    ResolvedType::Void // Placeholder
                }
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|&p| self.ast_to_resolved(program, p))
                    .collect();
                let return_type = Box::new(self.ast_to_resolved(program, *return_type));
                ResolvedType::Function {
                    param_types,
                    return_type,
                }
            }
        }
    }
}

impl<'ast> Visitor<'ast, (), CompilerError> for TypeSchemeCollector {
    fn visit_program(&mut self, prog: &'ast Program) -> Result<(), CompilerError> {
        // First, collect all top-level names so we can resolve types within declarations.
        for (id, decl) in &prog.type_decls {
            self.env.type_name_to_id.insert(decl.name.name.clone(), id);
        }
        for (id, decl) in &prog.table_decls {
            self.env.table_name_to_id.insert(decl.name.name.clone(), id);
        }

        // Now visit each declaration to build its TypeScheme.
        visit::walk_program(self, prog)
    }

    fn visit_type_decl(
        &mut self,
        prog: &'ast Program,
        id: TypeDeclId,
        decl: &'ast TypeDecl,
    ) -> Result<(), CompilerError> {
        self.current_generic_params.clear();
        for &param_id in &decl.generic_params {
            let param = &prog.generic_params[param_id];
            self.current_generic_params
                .insert(param.name.name.clone(), param_id);
        }

        let args = decl
            .generic_params
            .iter()
            .map(|&p| ResolvedType::GenericParam(p))
            .collect();

        let scheme = TypeScheme {
            quantified_params: decl.generic_params.clone(),
            ty: ResolvedType::Declared { decl_id: id, args },
        };
        self.env.types.insert(id, scheme);
        Ok(())
    }

    fn visit_table_decl(
        &mut self,
        _prog: &'ast Program,
        id: TableId,
        _decl: &'ast TableDecl,
    ) -> Result<(), CompilerError> {
        // Tables are currently monomorphic.
        let scheme = TypeScheme {
            quantified_params: vec![],
            ty: ResolvedType::Table { table_id: id },
        };
        self.env.tables.insert(id, scheme);
        Ok(())
    }

    fn visit_callable_decl(
        &mut self,
        prog: &'ast Program,
        id: FunctionId,
        decl: &'ast CallableDecl,
    ) -> Result<(), CompilerError> {
        self.current_generic_params.clear();
        for &param_id in &decl.generic_params {
            let param = &prog.generic_params[param_id];
            self.current_generic_params
                .insert(param.name.name.clone(), param_id);
        }

        let param_types = decl
            .params
            .iter()
            .map(|&p_id| self.ast_to_resolved(prog, prog.params[p_id].ty))
            .collect();

        let return_type = decl
            .return_type
            .map(|rt_id| self.ast_to_resolved(prog, rt_id))
            .unwrap_or(ResolvedType::Void);

        let func_type = ResolvedType::Function {
            param_types,
            return_type: Box::new(return_type),
        };

        let scheme = TypeScheme {
            quantified_params: decl.generic_params.clone(),
            ty: func_type,
        };

        self.env.functions.insert(id, scheme);
        Ok(())
    }

    fn visit_const_decl(
        &mut self,
        _prog: &'ast Program,
        _id: ConstId,
        _decl: &'ast ConstDecl,
    ) -> Result<(), CompilerError> {
        // Constants are not yet handled in this pass.
        Ok(())
    }
}

// ============================================================================
// --- Pass 2: Type Checking with Unification
// ============================================================================

struct TypeChecker {
    env: Environment,
    errors: Vec<CompilerError>,
    infer_var_counter: u32,
    // This substitution is local to the function being checked.
    substitution: Substitution,
    // The rigid generic parameters for the current function scope.
    current_generic_params: HashMap<String, GenericParamId>,
}

impl TypeChecker {
    fn new(env: Environment) -> Self {
        Self {
            env,
            errors: Vec::new(),
            infer_var_counter: 0,
            substitution: HashMap::new(),
            current_generic_params: HashMap::new(),
        }
    }

    fn into_errors(self) -> Vec<CompilerError> {
        self.errors
    }

    fn fresh_infer_var(&mut self) -> ResolvedType {
        let var_id = self.infer_var_counter;
        self.infer_var_counter += 1;
        ResolvedType::InferVar(var_id)
    }

    fn instantiate(&mut self, scheme: &TypeScheme) -> ResolvedType {
        let mut generic_subst = HashMap::new();
        for &param_id in &scheme.quantified_params {
            generic_subst.insert(param_id, self.fresh_infer_var());
        }
        self.subst_generic_params(&scheme.ty, &generic_subst)
    }

    /// Find a type declaration by name, returning a ResolvedType or an error
    fn find_type_by_name(
        &mut self,
        prog: &Program,
        type_name: &str,
        span: Span,
    ) -> Result<ResolvedType, CompilerError> {
        if let Some(&type_id) = self.env.type_name_to_id.get(type_name) {
            Ok(ResolvedType::Declared {
                decl_id: type_id,
                args: vec![],
            })
        } else {
            let error = CompilerError::new(
                FrontEndErrorKind::UndefinedIdentifier {
                    name: type_name.to_string(),
                },
                span,
            );
            self.errors.push(error);
            Ok(self.fresh_infer_var()) // Return fresh var to continue type checking
        }
    }

    /// Find a type declaration by name for generic types like List<T>
    fn find_generic_type_by_name(
        &mut self,
        prog: &Program,
        type_name: &str,
        args: Vec<ResolvedType>,
        span: Span,
    ) -> Result<ResolvedType, CompilerError> {
        if let Some(&type_id) = self.env.type_name_to_id.get(type_name) {
            Ok(ResolvedType::Declared {
                decl_id: type_id,
                args,
            })
        } else {
            let error = CompilerError::new(
                FrontEndErrorKind::UndefinedIdentifier {
                    name: type_name.to_string(),
                },
                span,
            );
            self.errors.push(error);
            Ok(self.fresh_infer_var()) // Return fresh var to continue type checking
        }
    }

    /// Recursively replace GenericParam types with types from the substitution map.
    fn subst_generic_params(
        &self,
        ty: &ResolvedType,
        subst: &HashMap<GenericParamId, ResolvedType>,
    ) -> ResolvedType {
        match ty {
            ResolvedType::GenericParam(p_id) => {
                subst.get(p_id).cloned().unwrap_or_else(|| ty.clone())
            }
            ResolvedType::Declared { decl_id, args } => ResolvedType::Declared {
                decl_id: *decl_id,
                args: args
                    .iter()
                    .map(|arg| self.subst_generic_params(arg, subst))
                    .collect(),
            },
            ResolvedType::Function {
                param_types,
                return_type,
            } => ResolvedType::Function {
                param_types: param_types
                    .iter()
                    .map(|p| self.subst_generic_params(p, subst))
                    .collect(),
                return_type: Box::new(self.subst_generic_params(return_type, subst)),
            },
            _ => ty.clone(),
        }
    }

    /// Follow the substitution chain for a type.
    fn apply_substitution(&self, ty: &ResolvedType) -> ResolvedType {
        let mut ty = ty.clone();
        loop {
            match ty {
                ResolvedType::InferVar(var_id) => {
                    if let Some(t) = self.substitution.get(&var_id) {
                        ty = t.clone();
                    } else {
                        break;
                    }
                }
                ResolvedType::Declared { decl_id, args } => {
                    return ResolvedType::Declared {
                        decl_id,
                        args: args
                            .iter()
                            .map(|arg| self.apply_substitution(arg))
                            .collect(),
                    }
                }
                ResolvedType::Function {
                    param_types,
                    return_type,
                } => {
                    return ResolvedType::Function {
                        param_types: param_types
                            .iter()
                            .map(|p| self.apply_substitution(p))
                            .collect(),
                        return_type: Box::new(self.apply_substitution(&return_type)),
                    }
                }
                _ => break,
            }
        }
        ty
    }

    /// Check if an inference variable occurs within a type.
    fn occurs_check(&self, var_id: InferVarId, ty: &ResolvedType) -> bool {
        let ty = self.apply_substitution(ty);
        match ty {
            ResolvedType::InferVar(id) => id == var_id,
            ResolvedType::Declared { args, .. } => {
                args.iter().any(|arg| self.occurs_check(var_id, arg))
            }
            ResolvedType::Function {
                param_types,
                return_type,
            } => {
                param_types.iter().any(|p| self.occurs_check(var_id, p))
                    || self.occurs_check(var_id, &return_type)
            }
            _ => false,
        }
    }

    /// The core unification algorithm.
    fn unify(&mut self, t1: &ResolvedType, t2: &ResolvedType, span: Span) {
        let t1 = self.apply_substitution(t1);
        let t2 = self.apply_substitution(t2);

        match (t1, t2) {
            (ResolvedType::InferVar(id1), ResolvedType::InferVar(id2)) if id1 == id2 => {}
            (ResolvedType::InferVar(id), ty) | (ty, ResolvedType::InferVar(id)) => {
                if self.occurs_check(id, &ty) {
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::InvalidOperation {
                            op: "unify".to_string(),
                            details: "recursive type".to_string(),
                        },
                        span,
                    ));
                } else {
                    self.substitution.insert(id, ty);
                }
            }
            (ResolvedType::GenericParam(id1), ResolvedType::GenericParam(id2)) if id1 == id2 => {}
            (
                ResolvedType::Declared {
                    decl_id: id1,
                    args: args1,
                },
                ResolvedType::Declared {
                    decl_id: id2,
                    args: args2,
                },
            ) if id1 == id2 && args1.len() == args2.len() => {
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2, span);
                }
            }
            (ResolvedType::Table { table_id: id1 }, ResolvedType::Table { table_id: id2 })
                if id1 == id2 => {}
            (
                ResolvedType::Function {
                    param_types: p1,
                    return_type: r1,
                },
                ResolvedType::Function {
                    param_types: p2,
                    return_type: r2,
                },
            ) if p1.len() == p2.len() => {
                for (a1, a2) in p1.iter().zip(p2.iter()) {
                    self.unify(a1, a2, span);
                }
                self.unify(&r1, &r2, span);
            }
            (ResolvedType::Void, ResolvedType::Void) => {}
            (t1, t2) => {
                // Before reporting an error, check if one is a placeholder.
                if t1 != ResolvedType::Void && t2 != ResolvedType::Void {
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::TypeMismatch {
                            expected: t1.to_string(),
                            found: t2.to_string(),
                            context: "unification".to_string(),
                        },
                        span,
                    ));
                }
            }
        }
    }

    fn ast_to_resolved(
        &mut self,
        program: &Program,
        ast_type_id: AstTypeId,
        span: Span,
    ) -> ResolvedType {
        let ast_type = &program.types[ast_type_id];
        match ast_type {
            AstType::Named { name, .. } => {
                if let Some(&param_id) = self.current_generic_params.get(&name.name) {
                    ResolvedType::GenericParam(param_id)
                } else if let Some(&type_id) = self.env.type_name_to_id.get(&name.name) {
                    ResolvedType::Declared {
                        decl_id: type_id,
                        args: vec![],
                    }
                } else if let Some(&table_id) = self.env.table_name_to_id.get(&name.name) {
                    ResolvedType::Table { table_id }
                } else {
                    // Generate error for unresolved type
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::UndefinedIdentifier {
                            name: name.name.clone(),
                        },
                        span,
                    ));
                    self.fresh_infer_var()
                }
            }
            AstType::Generic { base, args, .. } => {
                if let Some(&type_id) = self.env.type_name_to_id.get(&base.name) {
                    let resolved_args = args
                        .iter()
                        .map(|&arg_id| self.ast_to_resolved(program, arg_id, span))
                        .collect();
                    ResolvedType::Declared {
                        decl_id: type_id,
                        args: resolved_args,
                    }
                } else {
                    // Generate error for unresolved generic type
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::UndefinedIdentifier {
                            name: base.name.clone(),
                        },
                        span,
                    ));
                    self.fresh_infer_var()
                }
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|&p| self.ast_to_resolved(program, p, span))
                    .collect();
                let return_type = Box::new(self.ast_to_resolved(program, *return_type, span));
                ResolvedType::Function {
                    param_types,
                    return_type,
                }
            }
        }
    }

    fn infer_expr_type(
        &mut self,
        prog: &mut Program,
        id: ExprId,
    ) -> Result<ResolvedType, CompilerError> {
        visit_mut::walk_expr_mut(self, prog, id)?;
        let expr_clone = prog.expressions[id].clone();
        let expr_span = expr_clone
            .span()
            .unwrap_or(Span::new(0, 0, "<type_resolver>"));
        let ty = match expr_clone {
            Expression::Literal { value: lit, .. } => match lit {
                Literal::Integer(_) => self.find_type_by_name(prog, "int", expr_span)?,
                Literal::Bool(_) => self.find_type_by_name(prog, "bool", expr_span)?,
                Literal::String(_) => self.find_type_by_name(prog, "string", expr_span)?,
                Literal::Float(_) => self.find_type_by_name(prog, "float", expr_span)?,
                Literal::List(elements) => {
                    if elements.is_empty() {
                        // For empty lists, we can't infer the element type, use a fresh type variable
                        let element_type = self.fresh_infer_var();
                        self.find_generic_type_by_name(prog, "List", vec![element_type], expr_span)?
                    } else {
                        // Infer type from first element and unify with others
                        let first_element_type = self.infer_expr_type(prog, elements[0])?;
                        for &element_id in &elements[1..] {
                            let element_type = self.infer_expr_type(prog, element_id)?;
                            let element_span =
                                prog.expressions[element_id].span().unwrap_or(expr_span);
                            self.unify(&first_element_type, &element_type, element_span);
                        }
                        self.find_generic_type_by_name(
                            prog,
                            "List",
                            vec![first_element_type],
                            expr_span,
                        )?
                    }
                }
                Literal::RowLiteral(_) => {
                    // For row literals, we need to determine the row type based on the fields
                    // For now, create a fresh inference variable - proper row type inference would be complex
                    let row_type = self.fresh_infer_var();
                    self.find_generic_type_by_name(prog, "Row", vec![row_type], expr_span)?
                }
            },
            Expression::Identifier {
                resolved_declarations,
                ..
            } => {
                let resolution = resolved_declarations.get(0).cloned(); // Assuming single resolution for now
                match resolution {
                    Some(IdentifierResolution::Var(var_id)) => prog.var_decls[var_id]
                        .resolved_type
                        .clone()
                        .unwrap_or_else(|| self.fresh_infer_var()),
                    Some(IdentifierResolution::Param(param_id)) => prog.params[param_id]
                        .resolved_type
                        .clone()
                        .unwrap_or_else(|| self.fresh_infer_var()),
                    Some(IdentifierResolution::Const(const_id)) => prog.const_decls[const_id]
                        .resolved_type
                        .clone()
                        .unwrap_or_else(|| self.fresh_infer_var()),
                    Some(IdentifierResolution::Function(func_id)) => {
                        if let Some(scheme) = self.env.functions.get(&func_id).cloned() {
                            self.instantiate(&scheme)
                        } else {
                            self.fresh_infer_var()
                        }
                    }
                    _ => self.fresh_infer_var(), // Other cases not handled yet
                }
            }
            Expression::Call { callee, args, .. } => {
                let callee_type = self.infer_expr_type(prog, callee)?;
                let arg_types: Vec<ResolvedType> = args
                    .iter()
                    .map(|arg_id| self.infer_expr_type(prog, *arg_id))
                    .collect::<Result<_, _>>()?;

                let return_type = self.fresh_infer_var();
                let expected_func_type = ResolvedType::Function {
                    param_types: arg_types,
                    return_type: Box::new(return_type.clone()),
                };

                self.unify(&callee_type, &expected_func_type, expr_span);
                return_type
            }
            Expression::Binary {
                op, left, right, ..
            } => {
                let left_type = self.infer_expr_type(prog, left)?;
                let right_type = self.infer_expr_type(prog, right)?;

                self.unify(&left_type, &right_type, expr_span);

                // For comparison ops, return proper bool type
                if op.name == "=="
                    || op.name == "!="
                    || op.name == "<"
                    || op.name == "<="
                    || op.name == ">"
                    || op.name == ">="
                {
                    self.find_type_by_name(prog, "bool", expr_span)?
                } else {
                    left_type
                }
            }
            Expression::Unary { expr: operand, .. } => self.infer_expr_type(prog, operand)?,
            Expression::MemberAccess { object, .. } => self.infer_expr_type(prog, object)?,
            _ => self.fresh_infer_var(),
        };

        let final_type = self.apply_substitution(&ty);

        // Update the expression's resolved type
        match &mut prog.expressions[id] {
            Expression::Literal { resolved_type, .. }
            | Expression::Identifier { resolved_type, .. }
            | Expression::Binary { resolved_type, .. }
            | Expression::Unary { resolved_type, .. }
            | Expression::Assignment { resolved_type, .. }
            | Expression::Call { resolved_type, .. }
            | Expression::MemberAccess { resolved_type, .. }
            | Expression::TableRowAccess { resolved_type, .. }
            | Expression::Grouped { resolved_type, .. }
            | Expression::Lambda { resolved_type, .. } => {
                *resolved_type = Some(final_type.clone());
            }
        }

        Ok(final_type)
    }
}

impl<'ast> VisitorMut<'ast, (), CompilerError> for TypeChecker {
    fn visit_program(&mut self, prog: &mut Program) -> Result<(), CompilerError> {
        visit_mut::walk_program_mut(self, prog)
    }

    fn visit_callable_decl(
        &mut self,
        prog: &mut Program,
        id: FunctionId,
    ) -> Result<(), CompilerError> {
        self.substitution.clear();
        self.infer_var_counter = 0;
        self.current_generic_params.clear();

        // Get the function declaration without cloning
        let decl_generic_params = prog.functions[id].generic_params.clone();
        let decl_params = prog.functions[id].params.clone();
        let decl_return_type = prog.functions[id].return_type;
        let decl_body = prog.functions[id].body;
        let decl_name_span = prog.functions[id].name.span;

        // Set up generic parameters for this function scope
        for &param_id in &decl_generic_params {
            let param = &prog.generic_params[param_id];
            self.current_generic_params
                .insert(param.name.name.clone(), param_id);
        }

        // Get the function scheme and instantiate it
        let func_scheme = self.env.functions.get(&id).cloned().unwrap();
        let func_type = self.instantiate(&func_scheme);

        // Fill the resolved_function_type field
        prog.functions[id].resolved_function_type = Some(func_scheme.clone());

        if let ResolvedType::Function {
            param_types: expected_param_types,
            return_type: expected_return_type,
        } = func_type
        {
            // Process parameters
            for (param_id, expected_ty) in decl_params.iter().zip(expected_param_types.iter()) {
                let param = &prog.params[*param_id];
                let span = param
                    .name
                    .span
                    .unwrap_or(Span::new(0, 0, "<type_resolver>"));
                let declared_ty = self.ast_to_resolved(prog, param.ty, span);

                self.unify(&declared_ty, expected_ty, span);

                // Update parameter resolved type
                prog.params[*param_id].resolved_type = Some(self.apply_substitution(expected_ty));
            }

            // Process return type
            if let Some(return_type_id) = decl_return_type {
                let span = decl_name_span.unwrap_or(Span::new(0, 0, "<type_resolver>"));
                let declared_return_ty = self.ast_to_resolved(prog, return_type_id, span);
                self.unify(&declared_return_ty, &expected_return_type, span);
                prog.functions[id].resolved_return_type =
                    Some(self.apply_substitution(&expected_return_type));
            } else {
                self.unify(
                    &ResolvedType::Void,
                    &expected_return_type,
                    decl_name_span.unwrap_or(Span::new(0, 0, "<type_resolver>")),
                );
                prog.functions[id].resolved_return_type = Some(ResolvedType::Void);
            }

            // Visit the function body to infer all expression types
            if let Some(body_id) = decl_body {
                self.visit_block(prog, body_id)?;
            }

            // Visit assumptions (preconditions)
            let assumptions = prog.functions[id].assumptions.clone();
            for assumption_id in assumptions {
                self.visit_expr(prog, assumption_id)?;
            }

            // Apply final substitutions to all types
            for param_id in &decl_params {
                if let Some(ref mut ty) = prog.params[*param_id].resolved_type {
                    *ty = self.apply_substitution(ty);
                }
            }
            if let Some(ref mut ty) = prog.functions[id].resolved_return_type {
                *ty = self.apply_substitution(ty);
            }
        }

        Ok(())
    }

    fn visit_stmt(&mut self, prog: &mut Program, id: StmtId) -> Result<(), CompilerError> {
        // Clone the statement to avoid borrowing conflicts
        let stmt = prog.statements[id].clone();

        match stmt {
            Statement::VarDecl(var_id) => {
                // Get the necessary data
                let var_name_span =
                    prog.var_decls[var_id]
                        .name
                        .span
                        .unwrap_or(Span::new(0, 0, "<type_resolver>"));
                let init_expr_id = prog.var_decls[var_id].init;
                let type_id = prog.var_decls[var_id].ty;

                // Create fresh type variable
                let var_type = self.fresh_infer_var();

                // Handle init expression if present
                if let Some(init_expr_id) = init_expr_id {
                    let init_type = self.infer_expr_type(prog, init_expr_id)?;
                    self.unify(&var_type, &init_type, var_name_span);
                }

                // Handle type annotation if present
                if let Some(type_id) = type_id {
                    let annotated_type = self.ast_to_resolved(prog, type_id, var_name_span);
                    self.unify(&var_type, &annotated_type, var_name_span);
                }

                // Update the variable declaration
                prog.var_decls[var_id].resolved_type = Some(self.apply_substitution(&var_type));
            }
            Statement::Expression { expr, .. } => {
                self.visit_expr(prog, expr)?;
            }
            Statement::Return { value, .. } => {
                if let Some(expr_id) = value {
                    self.visit_expr(prog, expr_id)?;
                }
            }
            Statement::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.visit_expr(prog, condition)?;
                self.visit_block(prog, then_block)?;
                if let Some(else_block) = else_block {
                    self.visit_block(prog, else_block)?;
                }
            }
            Statement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                // Visit for loop initialization
                if let Some(for_init) = init {
                    match for_init {
                        ForInit::VarDecl(var_id) => {
                            // Process variable declaration directly
                            let var_name_span = prog.var_decls[var_id]
                                .name
                                .span
                                .unwrap_or(Span::new(0, 0, "<type_resolver>"));
                            let init_expr_id = prog.var_decls[var_id].init;
                            let type_id = prog.var_decls[var_id].ty;

                            let var_type = self.fresh_infer_var();

                            if let Some(init_expr_id) = init_expr_id {
                                let init_type = self.infer_expr_type(prog, init_expr_id)?;
                                self.unify(&var_type, &init_type, var_name_span);
                            }

                            if let Some(type_id) = type_id {
                                let annotated_type =
                                    self.ast_to_resolved(prog, type_id, var_name_span);
                                self.unify(&var_type, &annotated_type, var_name_span);
                            }

                            prog.var_decls[var_id].resolved_type =
                                Some(self.apply_substitution(&var_type));
                        }
                        ForInit::Expression(expr_id) => {
                            self.visit_expr(prog, expr_id)?;
                        }
                    }
                }

                // Visit condition
                if let Some(cond_id) = condition {
                    self.visit_expr(prog, cond_id)?;
                }

                // Visit update expression
                if let Some(update_id) = update {
                    self.visit_expr(prog, update_id)?;
                }

                // Visit body
                self.visit_block(prog, body)?;
            }
            Statement::Assert { expr, .. } => {
                self.visit_expr(prog, expr)?;
            }
            Statement::Hop { body, .. } => {
                self.visit_block(prog, body)?;
            }
            Statement::HopsFor {
                var,
                start,
                end,
                body,
                ..
            } => {
                // Handle the loop variable
                let var_name_span =
                    prog.var_decls[var]
                        .name
                        .span
                        .unwrap_or(Span::new(0, 0, "<type_resolver>"));
                let init_expr_id = prog.var_decls[var].init;
                let type_id = prog.var_decls[var].ty;

                let var_type = self.fresh_infer_var();

                if let Some(init_expr_id) = init_expr_id {
                    let init_type = self.infer_expr_type(prog, init_expr_id)?;
                    self.unify(&var_type, &init_type, var_name_span);
                }

                if let Some(type_id) = type_id {
                    let annotated_type = self.ast_to_resolved(prog, type_id, var_name_span);
                    self.unify(&var_type, &annotated_type, var_name_span);
                }

                prog.var_decls[var].resolved_type = Some(self.apply_substitution(&var_type));

                // Visit start and end expressions
                self.visit_expr(prog, start)?;
                self.visit_expr(prog, end)?;

                // Visit body
                self.visit_block(prog, body)?;
            }
            Statement::Block(block_id) => {
                self.visit_block(prog, block_id)?;
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<(), CompilerError> {
        self.infer_expr_type(prog, id)?;
        Ok(())
    }
}
