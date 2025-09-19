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

    // Pass 2: Type check (including table field types resolution, then function bodies).
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

    fn ast_to_resolved(
        &mut self,
        program: &Program,
        ast_type_id: AstTypeId,
    ) -> Result<ResolvedType, CompilerError> {
        let ast_type = &program.types[ast_type_id];
        match ast_type {
            AstType::Named { name, .. } => {
                if let Some(&param_id) = self.current_generic_params.get(&name.name) {
                    Ok(ResolvedType::GenericParam(param_id))
                } else if let Some(&type_id) = self.env.type_name_to_id.get(&name.name) {
                    Ok(ResolvedType::Declared {
                        decl_id: type_id,
                        args: vec![],
                    })
                } else if let Some(&table_id) = self.env.table_name_to_id.get(&name.name) {
                    Ok(ResolvedType::Table { table_id })
                } else {
                    // Report error for undefined type with proper span
                    let span = name.span.unwrap_or(Span::new(0, 0, "<type_resolver>"));
                    Err(CompilerError::new(
                        FrontEndErrorKind::UndefinedIdentifier {
                            name: name.name.clone(),
                        },
                        span,
                    ))
                }
            }
            AstType::Generic {
                base, args, span, ..
            } => {
                if let Some(&type_id) = self.env.type_name_to_id.get(&base.name) {
                    let resolved_args: Result<Vec<_>, _> = args
                        .iter()
                        .map(|&arg_id| self.ast_to_resolved(program, arg_id))
                        .collect();
                    let resolved_args = resolved_args?;
                    Ok(ResolvedType::Declared {
                        decl_id: type_id,
                        args: resolved_args,
                    })
                } else {
                    // Report error for undefined generic type with proper span
                    let error_span = span
                        .unwrap_or_else(|| base.span.unwrap_or(Span::new(0, 0, "<type_resolver>")));
                    Err(CompilerError::new(
                        FrontEndErrorKind::UndefinedIdentifier {
                            name: base.name.clone(),
                        },
                        error_span,
                    ))
                }
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types: Result<Vec<_>, _> = params
                    .iter()
                    .map(|&p| self.ast_to_resolved(program, p))
                    .collect();
                let param_types = param_types?;
                let return_type = Box::new(self.ast_to_resolved(program, *return_type)?);
                Ok(ResolvedType::Function {
                    param_types,
                    return_type,
                })
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

        let param_types: Result<Vec<_>, _> = decl
            .params
            .iter()
            .map(|&p_id| self.ast_to_resolved(prog, prog.params[p_id].ty))
            .collect();
        let param_types = param_types.map_err(|e| {
            self.errors.push(e);
            CompilerError::new(
                FrontEndErrorKind::InvalidOperation {
                    op: "parameter type resolution".to_string(),
                    details: "failed to resolve parameter types".to_string(),
                },
                decl.name.span.unwrap_or(Span::new(0, 0, "<type_resolver>")),
            )
        })?;

        let return_type = if let Some(rt_id) = decl.return_type {
            self.ast_to_resolved(prog, rt_id).map_err(|e| {
                self.errors.push(e);
                CompilerError::new(
                    FrontEndErrorKind::InvalidOperation {
                        op: "return type resolution".to_string(),
                        details: "failed to resolve return type".to_string(),
                    },
                    decl.name.span.unwrap_or(Span::new(0, 0, "<type_resolver>")),
                )
            })?
        } else {
            // No explicit return type - look up "void" as a user-defined type
            if let Some(&type_id) = self.env.type_name_to_id.get("void") {
                ResolvedType::Declared {
                    decl_id: type_id,
                    args: vec![],
                }
            } else {
                // "void" not found as user-defined type - report error
                let span = decl.name.span.unwrap_or(Span::new(0, 0, "<type_resolver>"));
                return Err(CompilerError::new(
                    FrontEndErrorKind::UndefinedIdentifier {
                        name: "void".to_string(),
                    },
                    span,
                ));
            }
        };

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

    /// Resolve function overloading by checking argument types against candidates
    /// Returns the best matching function ID and its instantiated type
    fn resolve_overloading(
        &mut self,
        candidates: &[FunctionId],
        arg_types: &[ResolvedType],
        expr_span: Span,
    ) -> Option<(FunctionId, ResolvedType)> {
        for &func_id in candidates {
            if let Some(scheme) = self.env.functions.get(&func_id).cloned() {
                let func_type = self.instantiate(&scheme);

                if let ResolvedType::Function {
                    param_types,
                    return_type,
                } = func_type
                {
                    // Check if parameter count matches
                    if param_types.len() == arg_types.len() {
                        // Check if all argument types can unify with parameter types
                        let mut can_unify = true;
                        let old_substitution = self.substitution.clone();
                        let old_error_count = self.errors.len();

                        for (param_ty, arg_ty) in param_types.iter().zip(arg_types.iter()) {
                            self.unify(param_ty, arg_ty, expr_span);
                        }

                        // Check if unification introduced new errors
                        if self.errors.len() > old_error_count {
                            can_unify = false;
                            // Remove the errors we just added for this tentative unification
                            self.errors.truncate(old_error_count);
                            // Restore substitution
                            self.substitution = old_substitution;
                        }

                        if can_unify {
                            // We found a match - return the function and its return type
                            let instantiated_return_type = self.apply_substitution(&return_type);
                            return Some((func_id, instantiated_return_type));
                        }
                    }
                }
            }
        }

        None
    }

    /// Find a type declaration by name, returning a ResolvedType or an error
    fn find_type_by_name(
        &mut self,
        _prog: &Program,
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
        _prog: &Program,
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
            (t1, t2) => {
                // Before reporting an error, check if one is a placeholder.
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
                    // When a table name is used as a type, it should be a Table type
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
}

// ============================================================================
// --- Pass 2: Type Checking with Unification
// ============================================================================

impl<'ast> VisitorMut<'ast, (), CompilerError> for TypeChecker {
    fn visit_program(&mut self, prog: &mut Program) -> Result<(), CompilerError> {
        visit_mut::walk_program_mut(self, prog)
    }

    fn visit_table_decl(&mut self, prog: &mut Program, id: TableId) -> Result<(), CompilerError> {
        let table_decl = prog.table_decls[id].clone();

        // First pass: Resolve types for all fields in this table
        for element in &table_decl.elements {
            if let TableElement::Field(field_id) = element {
                let field = prog.fields[*field_id].clone();
                let field_span = field.span.unwrap_or(Span::new(0, 0, "<field>"));

                // Resolve the field's type
                let resolved_type = self.ast_to_resolved(prog, field.ty, field_span);

                // Update the field's resolved type
                prog.fields[*field_id].resolved_type = Some(resolved_type);
            }
        }

        // Second pass: Process nodes and invariants with fields in scope
        for element in &table_decl.elements {
            match element {
                TableElement::Node(node) => {
                    let node_span = node.name.span.unwrap_or(Span::new(0, 0, "<table_node>"));

                    // Type check all arguments in the node first
                    for &arg_expr_id in &node.args {
                        self.visit_expr(prog, arg_expr_id)?;
                    }

                    // Collect argument types for overloading resolution
                    let arg_types: Vec<ResolvedType> = node
                        .args
                        .iter()
                        .map(|&arg_id| {
                            prog.expressions[arg_id]
                                .resolved_type()
                                .cloned()
                                .unwrap_or_else(|| self.fresh_infer_var())
                        })
                        .collect();

                    // Resolve function overloading for the partition function
                    if !node.resolved_partitions.is_empty() {
                        if let Some((resolved_func, return_type)) = self.resolve_overloading(
                            &node.resolved_partitions,
                            &arg_types,
                            node_span,
                        ) {
                            // Update the table element to contain only the selected partition function
                            // We need to find the correct element in the table to update
                            let table_decl = &mut prog.table_decls[id];
                            for element in &mut table_decl.elements {
                                if let TableElement::Node(ref mut node_mut) = element {
                                    if node_mut.name.name == node.name.name
                                        && node_mut.args.len() == node.args.len()
                                    {
                                        node_mut.resolved_partitions = vec![resolved_func];
                                        break;
                                    }
                                }
                            }

                            // Enforce that partition function returns int
                            let int_type = self.find_type_by_name(prog, "int", node_span)?;
                            self.unify(&return_type, &int_type, node_span);
                        } else {
                            // No suitable overload found for partition function
                            self.errors.push(CompilerError::new(
                                FrontEndErrorKind::TypeMismatch {
                                    expected: format!(
                                        "partition function with {} arguments",
                                        arg_types.len()
                                    ),
                                    found: "no matching overload".to_string(),
                                    context: "table node partition function".to_string(),
                                },
                                node_span,
                            ));
                        }
                    } else {
                        // No partition functions resolved by name resolver
                        self.errors.push(CompilerError::new(
                            FrontEndErrorKind::UndefinedIdentifier {
                                name: node.name.name.clone(),
                            },
                            node_span,
                        ));
                    }
                }
                TableElement::Invariant(expr_id) => {
                    let expr_span = prog.expressions[*expr_id].span().unwrap_or(Span::new(
                        0,
                        0,
                        "<table_invariant>",
                    ));

                    // Type check the invariant expression
                    self.visit_expr(prog, *expr_id)?;

                    // Enforce that invariant returns bool
                    let expr_type = prog.expressions[*expr_id]
                        .resolved_type()
                        .cloned()
                        .unwrap_or_else(|| self.fresh_infer_var());
                    let bool_type = self.find_type_by_name(prog, "bool", expr_span)?;
                    self.unify(&expr_type, &bool_type, expr_span);
                }
                TableElement::Field(_) => {
                    // Already handled in first pass
                }
            }
        }

        Ok(())
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
                // No explicit return type - look up "void" as user-defined type
                let void_type = self.find_type_by_name(
                    prog,
                    "void",
                    decl_name_span.unwrap_or(Span::new(0, 0, "<type_resolver>")),
                )?;
                self.unify(
                    &void_type,
                    &expected_return_type,
                    decl_name_span.unwrap_or(Span::new(0, 0, "<type_resolver>")),
                );
                prog.functions[id].resolved_return_type = Some(self.apply_substitution(&void_type));
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
                    self.visit_expr(prog, init_expr_id)?;
                    let init_type = prog.expressions[init_expr_id]
                        .resolved_type()
                        .cloned()
                        .unwrap_or_else(|| self.fresh_infer_var());
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
                                self.visit_expr(prog, init_expr_id)?;
                                let init_type = prog.expressions[init_expr_id]
                                    .resolved_type()
                                    .cloned()
                                    .unwrap_or_else(|| self.fresh_infer_var());
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
                    self.visit_expr(prog, init_expr_id)?;
                    let init_type = prog.expressions[init_expr_id]
                        .resolved_type()
                        .cloned()
                        .unwrap_or_else(|| self.fresh_infer_var());
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
        // First, visit children expressions using the visitor pattern
        visit_mut::walk_expr_mut(self, prog, id)?;

        // Now infer the type for this expression after children are processed
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
                        // Get type from first element and unify with others
                        let first_element_type = prog.expressions[elements[0]]
                            .resolved_type()
                            .cloned()
                            .unwrap_or_else(|| self.fresh_infer_var());
                        for &element_id in &elements[1..] {
                            let element_type = prog.expressions[element_id]
                                .resolved_type()
                                .cloned()
                                .unwrap_or_else(|| self.fresh_infer_var());
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
                            // Function not found in environment - this should not happen after name resolution
                            self.errors.push(CompilerError::new(
                                FrontEndErrorKind::UndefinedIdentifier {
                                    name: format!("function with id {:?}", func_id),
                                },
                                expr_span,
                            ));
                            self.fresh_infer_var()
                        }
                    }
                    Some(IdentifierResolution::Table(table_id)) => {
                        // Table identifiers have type Table<table_name>
                        self.find_generic_type_by_name(
                            prog,
                            "Table",
                            vec![ResolvedType::Table { table_id }],
                            expr_span,
                        )?
                    }
                    Some(IdentifierResolution::Field(field_id)) => {
                        // Field identifiers have the type of the field
                        prog.fields[field_id]
                            .resolved_type
                            .clone()
                            .unwrap_or_else(|| self.fresh_infer_var())
                    }
                    Some(IdentifierResolution::Type(_)) => {
                        // Type identifiers should not appear in expression context
                        self.errors.push(CompilerError::new(
                            FrontEndErrorKind::InvalidOperation {
                                op: "type resolution".to_string(),
                                details: "type identifier in expression context".to_string(),
                            },
                            expr_span,
                        ));
                        self.fresh_infer_var()
                    }
                    Some(IdentifierResolution::GenericParam(param_id)) => {
                        // Generic parameters in expression context
                        ResolvedType::GenericParam(param_id)
                    }
                    None => {
                        // No resolution found for identifier - this should not happen after name resolution
                        self.errors.push(CompilerError::new(
                            FrontEndErrorKind::UndefinedIdentifier {
                                name: "unresolved identifier".to_string(),
                            },
                            expr_span,
                        ));
                        self.fresh_infer_var()
                    }
                }
            }
            Expression::Call {
                args,
                resolved_callables,
                ..
            } => {
                let arg_types: Vec<ResolvedType> = args
                    .iter()
                    .map(|&arg_id| {
                        prog.expressions[arg_id]
                            .resolved_type()
                            .cloned()
                            .unwrap_or_else(|| self.fresh_infer_var())
                    })
                    .collect();

                // Only check resolved_callables
                if !resolved_callables.is_empty() {
                    if let Some((resolved_func, return_type)) =
                        self.resolve_overloading(&resolved_callables, &arg_types, expr_span)
                    {
                        // Update the resolved_callables to contain only the selected function
                        if let Expression::Call {
                            resolved_callables: ref mut callables,
                            ..
                        } = &mut prog.expressions[id]
                        {
                            *callables = vec![resolved_func];
                        }
                        return_type
                    } else {
                        // No suitable overload found
                        self.errors.push(CompilerError::new(
                            FrontEndErrorKind::TypeMismatch {
                                expected: format!("function taking arguments"),
                                found: "call expression".to_string(),
                                context: "function call".to_string(),
                            },
                            expr_span,
                        ));
                        self.fresh_infer_var()
                    }
                } else {
                    // No resolved callables - report error
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::InvalidOperation {
                            op: "function call".to_string(),
                            details: "no candidates found for function call".to_string(),
                        },
                        expr_span,
                    ));
                    self.fresh_infer_var()
                }
            }
            Expression::Binary {
                left,
                right,
                resolved_callables,
                ..
            } => {
                let left_type = prog.expressions[left]
                    .resolved_type()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_infer_var());
                let right_type = prog.expressions[right]
                    .resolved_type()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_infer_var());
                let arg_types = vec![left_type, right_type];

                // Only check resolved_callables
                if !resolved_callables.is_empty() {
                    if let Some((resolved_func, return_type)) =
                        self.resolve_overloading(&resolved_callables, &arg_types, expr_span)
                    {
                        // Update the resolved_callables to contain only the selected function
                        if let Expression::Binary {
                            resolved_callables: ref mut callables,
                            ..
                        } = &mut prog.expressions[id]
                        {
                            *callables = vec![resolved_func];
                        }
                        return_type
                    } else {
                        // No suitable overload found
                        self.errors.push(CompilerError::new(
                            FrontEndErrorKind::TypeMismatch {
                                expected: "binary operator overload".to_string(),
                                found: "binary expression".to_string(),
                                context: "operator overloading".to_string(),
                            },
                            expr_span,
                        ));
                        self.fresh_infer_var()
                    }
                } else {
                    // No resolved callables - report error
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::InvalidOperation {
                            op: "binary operator".to_string(),
                            details: "no candidates found for binary operator".to_string(),
                        },
                        expr_span,
                    ));
                    self.fresh_infer_var()
                }
            }
            Expression::Unary {
                expr: operand,
                resolved_callables,
                ..
            } => {
                let operand_type = prog.expressions[operand]
                    .resolved_type()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_infer_var());
                let arg_types = vec![operand_type];

                // Only check resolved_callables
                if !resolved_callables.is_empty() {
                    if let Some((resolved_func, return_type)) =
                        self.resolve_overloading(&resolved_callables, &arg_types, expr_span)
                    {
                        // Update the resolved_callables to contain only the selected function
                        if let Expression::Unary {
                            resolved_callables: ref mut callables,
                            ..
                        } = &mut prog.expressions[id]
                        {
                            *callables = vec![resolved_func];
                        }
                        return_type
                    } else {
                        // No suitable overload found
                        self.errors.push(CompilerError::new(
                            FrontEndErrorKind::TypeMismatch {
                                expected: "unary operator overload".to_string(),
                                found: "unary expression".to_string(),
                                context: "operator overloading".to_string(),
                            },
                            expr_span,
                        ));
                        self.fresh_infer_var()
                    }
                } else {
                    // No resolved callables - report error
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::InvalidOperation {
                            op: "unary operator".to_string(),
                            details: "no candidates found for unary operator".to_string(),
                        },
                        expr_span,
                    ));
                    self.fresh_infer_var()
                }
            }
            Expression::MemberAccess { .. } => {
                // Member access is not supported yet - report error
                // self.errors.push(CompilerError::new(
                //     FrontEndErrorKind::InvalidOperation {
                //         op: "member access".to_string(),
                //         details: "member access is not yet implemented".to_string(),
                //     },
                //     expr_span,
                // ));
                self.fresh_infer_var()
            }
            Expression::TableRowAccess {
                table,
                key_values,
                resolved_table,
                ..
            } => {
                // First, ensure the table expression is processed
                self.visit_expr(prog, table)?;

                // Also get the key value expression types (already processed by visitor)
                for key_value in &key_values {
                    let _value_type = prog.expressions[key_value.value]
                        .resolved_type()
                        .cloned()
                        .unwrap_or_else(|| self.fresh_infer_var());
                }

                // Table row access returns a Row<Table<table_name>>
                if let Some(table_id) = resolved_table {
                    // Create a Row type with the specific table type as the content
                    let table_content_type = ResolvedType::Table { table_id };
                    self.find_generic_type_by_name(
                        prog,
                        "Row",
                        vec![table_content_type],
                        expr_span,
                    )?
                } else {
                    // Table not resolved - this should not happen after name resolution
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::InvalidOperation {
                            op: "table row access".to_string(),
                            details: "table not resolved".to_string(),
                        },
                        expr_span,
                    ));
                    self.fresh_infer_var()
                }
            }
            Expression::Assignment { lhs, rhs, .. } => {
                // Assignment expressions: get both sides and unify them (already processed by visitor)
                let left_type = prog.expressions[lhs]
                    .resolved_type()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_infer_var());
                let right_type = prog.expressions[rhs]
                    .resolved_type()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_infer_var());

                self.unify(&left_type, &right_type, expr_span);

                // Assignment expression returns void/unit type
                self.find_type_by_name(prog, "void", expr_span)?
            }
            Expression::Grouped { expr, .. } => {
                // Grouped expressions have the same type as the inner expression (already processed by visitor)
                prog.expressions[expr]
                    .resolved_type()
                    .cloned()
                    .unwrap_or_else(|| self.fresh_infer_var())
            }
            Expression::Lambda {
                params,
                return_type,
                body,
                ..
            } => {
                // Lambda expressions are function types
                // Infer parameter types (they might have type annotations)
                let param_types: Vec<ResolvedType> = params
                    .iter()
                    .map(|&param_id| {
                        let param = &prog.params[param_id];
                        // Parameters always have type annotations in the AST
                        self.ast_to_resolved(prog, param.ty, expr_span)
                    })
                    .collect();
                self.visit_block(prog, body)?;

                // Infer the return type by analyzing all return statements in the lambda body
                let return_type = self.ast_to_resolved(prog, return_type, expr_span);

                // Lambda type is a function type
                ResolvedType::Function {
                    param_types,
                    return_type: Box::new(return_type),
                }
            }
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

        Ok(())
    }
}
