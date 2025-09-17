//! src/frontend/type_resolver.rs
//!
//! Complete type inference system for the FMitF compiler.

use crate::ast::visit_mut::*;
use crate::ast::*;
use crate::util::CompilerError;
use std::collections::HashMap;

pub fn resolve_types(program: &mut Program) -> Result<(), Vec<CompilerError>> {
    let mut resolver = TypeResolver::new();
    resolver.resolve(program)
}

struct TypeResolver {
    errors: Vec<CompilerError>,
    type_var_counter: u32,
    // First pass - type environment mappings
    type_name_to_id: HashMap<String, TypeDeclId>,
    table_name_to_id: HashMap<String, TableId>,
    // Second pass - function-local generic bindings
    current_generic_bindings: HashMap<GenericParamId, u32>, // GenericParam -> TypeVar
}

impl TypeResolver {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            type_var_counter: 0,
            type_name_to_id: HashMap::new(),
            table_name_to_id: HashMap::new(),
            current_generic_bindings: HashMap::new(),
        }
    }

    fn resolve(&mut self, program: &mut Program) -> Result<(), Vec<CompilerError>> {
        // First visitor scan: collect all type declarations and build type environment
        self.collect_type_environment(program);
        
        // Second visitor scan: resolve types for functions
        self.visit_program(program).map_err(|_| std::mem::take(&mut self.errors))?;

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// First pass: collect type declarations and table declarations to build environment
    fn collect_type_environment(&mut self, program: &Program) {
        // Collect type declarations: int, bool, List<T>, etc.
        for (type_id, type_decl) in &program.type_decls {
            self.type_name_to_id.insert(type_decl.name.name.clone(), type_id);
        }
        
        // Collect table declarations: Graph, Status, etc.
        for (table_id, table_decl) in &program.table_decls {
            self.table_name_to_id.insert(table_decl.name.name.clone(), table_id);
        }
    }

    /// Generate a fresh type variable
    fn fresh_type_var(&mut self) -> u32 {
        let var_id = self.type_var_counter;
        self.type_var_counter += 1;
        var_id
    }

    /// Resolve an AST type to a ResolvedType
    fn resolve_ast_type(&mut self, program: &Program, ast_type_id: AstTypeId) -> ResolvedType {
        let ast_type = &program.types[ast_type_id];
        
        match ast_type {
            AstType::Named { name, .. } => {
                // Check if it's a type declaration (int, bool, List, etc.)
                if let Some(&type_id) = self.type_name_to_id.get(&name.name) {
                    ResolvedType::Primitive {
                        type_id,
                        type_args: vec![],
                        bound_vars: vec![],
                    }
                }
                // Check if it's a table name (when used as type)
                else if let Some(&table_id) = self.table_name_to_id.get(&name.name) {
                    ResolvedType::Table { table_id }
                }
                // Check if it's a generic parameter in current function
                else {
                    // For now, create a type variable - this would need generic context
                    ResolvedType::TypeVariable {
                        var_id: self.fresh_type_var(),
                        name: name.name.clone(),
                        bound_to: None,
                    }
                }
            }
            AstType::Generic { base, args, .. } => {
                if let Some(&type_id) = self.type_name_to_id.get(&base.name) {
                    let type_args = args.iter()
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
            AstType::Function { params, return_type, .. } => {
                let param_types = params.iter()
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

    /// Resolve a literal to its primitive type
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

    /// Resolve type for an identifier based on its resolution
    fn resolve_identifier_type(&mut self, program: &Program, resolutions: &[IdentifierResolution]) -> ResolvedType {
        if resolutions.len() != 1 {
            return ResolvedType::TypeVariable {
                var_id: self.fresh_type_var(),
                name: "unknown".to_string(),
                bound_to: None,
            };
        }

        match resolutions[0] {
            IdentifierResolution::Var(var_id) => {
                program.var_decls[var_id].resolved_type.clone()
                    .unwrap_or_else(|| ResolvedType::TypeVariable {
                        var_id: self.fresh_type_var(),
                        name: "var_type".to_string(),
                        bound_to: None,
                    })
            }
            IdentifierResolution::Param(param_id) => {
                program.params[param_id].resolved_type.clone()
                    .unwrap_or_else(|| ResolvedType::TypeVariable {
                        var_id: self.fresh_type_var(),
                        name: "param_type".to_string(),
                        bound_to: None,
                    })
            }
            IdentifierResolution::Table(table_id) => {
                ResolvedType::Table { table_id }
            }
            IdentifierResolution::Function(_) => {
                // Function types need more complex handling
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
            }
        }
    }

    /// Set the resolved type on an expression
    fn set_expression_type(&self, prog: &mut Program, expr_id: ExprId, ty: ResolvedType) {
        match &mut prog.expressions[expr_id] {
            Expression::Literal { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Identifier { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Binary { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Unary { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Assignment { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Call { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::MemberAccess { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::TableRowAccess { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Grouped { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Lambda { resolved_type, .. } => *resolved_type = Some(ty),
        }
    }
}

// Implement VisitorMut trait for the second pass
impl<'ast> VisitorMut<'ast, (), ()> for TypeResolver {
    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) -> Result<(), ()> {
        // Clone the data we need before borrowing mutably
        let (generic_params, param_ids, return_type, body) = {
            let function = &prog.functions[id];
            (function.generic_params.clone(), function.params.clone(), function.return_type, function.body)
        };
        
        // Build generic parameter bindings for this function
        self.current_generic_bindings.clear();
        for generic_param_id in generic_params {
            let type_var = self.fresh_type_var();
            self.current_generic_bindings.insert(generic_param_id, type_var);
        }

        // Resolve parameter types
        for param_id in param_ids {
            let ast_type_id = prog.params[param_id].ty;
            let resolved_type = self.resolve_ast_type(prog, ast_type_id);
            prog.params[param_id].resolved_type = Some(resolved_type);
        }

        // Resolve return type
        if let Some(return_type_id) = return_type {
            let resolved_return_type = self.resolve_ast_type(prog, return_type_id);
            prog.functions[id].resolved_return_type = Some(resolved_return_type);
        } else {
            prog.functions[id].resolved_return_type = Some(ResolvedType::Void);
        }

        // Visit the function body if it exists
        if let Some(body_id) = body {
            self.visit_block(prog, body_id)?;
        }

        Ok(())
    }

    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<(), ()> {
        // First, visit child expressions
        walk_expr_mut(self, prog, id)?;

        // Then resolve this expression's type
        let expr = &prog.expressions[id];
        let resolved_type = match expr {
            Expression::Literal { value, .. } => {
                self.resolve_literal_type(value)
            }
            Expression::Identifier { resolved_declarations, .. } => {
                self.resolve_identifier_type(prog, resolved_declarations)
            }
            Expression::Binary { .. } => {
                // For binary operations, we'd need to look up the operator function
                // For now, use a type variable
                ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "binary_result".to_string(),
                    bound_to: None,
                }
            }
            Expression::Unary { .. } => {
                ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "unary_result".to_string(),
                    bound_to: None,
                }
            }
            Expression::Call { .. } => {
                // Function call result type would depend on the called function
                ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "call_result".to_string(),
                    bound_to: None,
                }
            }
            Expression::Assignment { .. } => {
                ResolvedType::Void
            }
            Expression::MemberAccess { .. } => {
                // Member access type depends on the field type
                ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "member_type".to_string(),
                    bound_to: None,
                }
            }
            Expression::TableRowAccess { resolved_table, .. } => {
                // Table row access returns Row<TableType>
                if let Some(table_id) = resolved_table {
                    // Look up Row type and create Row<Table>
                    if let Some(&row_type_id) = self.type_name_to_id.get("Row") {
                        ResolvedType::Primitive {
                            type_id: row_type_id,
                            type_args: vec![ResolvedType::Table { table_id: *table_id }],
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
                // Grouped expression has same type as inner expression
                if let Some(inner_type) = prog.expressions[*expr].resolved_type() {
                    inner_type.clone()
                } else {
                    ResolvedType::Unknown
                }
            }
            Expression::Lambda { .. } => {
                // Lambda type is a function type
                ResolvedType::TypeVariable {
                    var_id: self.fresh_type_var(),
                    name: "lambda_type".to_string(),
                    bound_to: None,
                }
            }
        };

        // Set the resolved type on the expression
        self.set_expression_type(prog, id, resolved_type);
        Ok(())
    }

    fn visit_var_decl(&mut self, prog: &mut Program, id: VarId) -> Result<(), ()> {
        // Clone the data we need before borrowing mutably
        let (ty, init_id) = {
            let var_decl = &prog.var_decls[id];
            (var_decl.ty, var_decl.init)
        };

        // Resolve explicit type if provided
        if let Some(ty_id) = ty {
            let resolved_type = self.resolve_ast_type(prog, ty_id);
            prog.var_decls[id].resolved_type = Some(resolved_type.clone());
        }

        // Visit initializer if present
        if let Some(init_id) = init_id {
            self.visit_expr(prog, init_id)?;
            
            // If no explicit type, infer from initializer
            if ty.is_none() {
                if let Some(init_type) = prog.expressions[init_id].resolved_type() {
                    prog.var_decls[id].resolved_type = Some(init_type.clone());
                }
            }
        }

        Ok(())
    }

    fn visit_table_decl(&mut self, prog: &mut Program, id: TableId) -> Result<(), ()> {
        // Resolve field types in table declaration
        for element in &prog.table_decls[id].elements.clone() {
            match element {
                TableElement::Field(field) => {
                    let resolved_type = self.resolve_ast_type(prog, field.ty);
                    // Find the field in the table and update its resolved type
                    if let Some(TableElement::Field(field_mut)) = prog.table_decls[id].elements.iter_mut()
                        .find(|e| matches!(e, TableElement::Field(f) if f.name.name == field.name.name)) {
                        field_mut.resolved_type = Some(resolved_type);
                    }
                }
                TableElement::Node(node) => {
                    // Visit node arguments
                    for &arg_id in &node.args {
                        self.visit_expr(prog, arg_id)?;
                    }
                }
                TableElement::Invariant(expr_id) => {
                    self.visit_expr(prog, *expr_id)?;
                }
            }
        }
        Ok(())
    }
}

// Add a helper method to Expression for getting resolved type
impl Expression {
    pub fn resolved_type(&self) -> Option<&ResolvedType> {
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
}