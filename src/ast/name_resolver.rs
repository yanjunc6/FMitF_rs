//! Name Resolution
//!
//! This module implements name resolution for the AST. It performs:
//! - Symbol table construction from declarations
//! - Identifier resolution to declarations
//! - Scope checking and validation
//! - Circular dependency detection
//! - Forward reference validation

use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::*;
use crate::ast::errors::*;

// ============================================================================
// --- Name Resolution Context
// ============================================================================

/// Main name resolver that tracks symbols and performs resolution
pub struct NameResolver {
    /// Global symbol table mapping names to declarations
    global_symbols: HashMap<String, DeclRef>,
    
    /// Stack of local scopes (for function parameters, local variables)
    scope_stack: Vec<HashMap<String, DeclRef>>,
    
    /// Current function being processed (for return type checking)
    current_function: Option<FunctionId>,
    
    /// Error collector
    errors: ErrorCollector,
}

/// Represents different scopes where names can be declared
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Global,
    Function,
    Block,
}

// ============================================================================
// --- Name Resolution Implementation
// ============================================================================

impl NameResolver {
    pub fn new() -> Self {
        Self {
            global_symbols: HashMap::new(),
            scope_stack: Vec::new(),
            current_function: None,
            errors: ErrorCollector::new(),
        }
    }

    /// Perform name resolution on a program
    pub fn resolve_names(mut program: Program) -> Results<Program> {
        let mut resolver = Self::new();
        
        // First pass: collect all global declarations
        resolver.collect_global_symbols(&program);
        
        // Second pass: resolve all references
        resolver.resolve_program_references(&mut program);
        
        resolver.errors.into_result(Some(program))
    }

    // ========================================================================
    // --- Symbol Collection Phase
    // ========================================================================

    fn collect_global_symbols(&mut self, program: &Program) {
        // Collect all global declarations first
        for declaration in &program.declarations {
            match declaration {
                Declaration::Callable(id) => {
                    let func = &program.functions[*id];
                    self.add_global_symbol(func.name.value.clone(), DeclRef::Function(*id), func.span);
                }
                Declaration::Type(id) => {
                    let type_decl = &program.type_decls[*id];
                    self.add_global_symbol(type_decl.name.value.clone(), DeclRef::Type(*id), type_decl.span);
                }
                Declaration::Const(id) => {
                    let const_decl = &program.const_decls[*id];
                    self.add_global_symbol(const_decl.name.value.clone(), DeclRef::Const(*id), const_decl.span);
                }
                Declaration::Table(id) => {
                    let table_decl = &program.table_decls[*id];
                    self.add_global_symbol(table_decl.name.value.clone(), DeclRef::Table(*id), table_decl.span);
                }
            }
        }
    }

    fn add_global_symbol(&mut self, name: String, decl_ref: DeclRef, span: Option<Span>) {
        if let Some(existing) = self.global_symbols.get(&name) {
            let error = NameResolutionError::DuplicateDeclaration {
                name,
                original_span: None, // Would need to track original spans
            };
            self.errors.add_error(AstError::NameResolution(error), span);
        } else {
            self.global_symbols.insert(name, decl_ref);
        }
    }

    // ========================================================================
    // --- Reference Resolution Phase
    // ========================================================================

    fn resolve_program_references(&mut self, program: &mut Program) {
        // Resolve references in all declarations
        for declaration in &program.declarations {
            match declaration {
                Declaration::Callable(id) => {
                    self.resolve_callable_references(*id, program);
                }
                Declaration::Type(id) => {
                    self.resolve_type_declaration_references(*id, program);
                }
                Declaration::Const(id) => {
                    self.resolve_const_declaration_references(*id, program);
                }
                Declaration::Table(id) => {
                    self.resolve_table_declaration_references(*id, program);
                }
            }
        }
    }

    fn resolve_callable_references(&mut self, func_id: FunctionId, program: &mut Program) {
        self.current_function = Some(func_id);
        self.push_scope();

        // Check operator decorators for additional validation
        let func = &program.functions[func_id];
        if func.kind == CallableKind::Operator {
            self.validate_operator_decorators(&func.decorators, func.name.span);
        }

        // Add parameters to scope
        let param_ids = program.functions[func_id].params.clone();
        for param_id in param_ids {
            let param = &program.params[param_id];
            self.add_local_symbol(param.name.value.clone(), DeclRef::Param(param_id), param.span);
            
            // Resolve parameter type
            if let Some(type_id) = param.ty {
                self.resolve_type_references(type_id, program);
            }
        }

        // Add generic parameters to scope
        let generic_param_ids = program.functions[func_id].generic_params.clone();
        for generic_param_id in generic_param_ids {
            let generic_param = &program.generic_params[generic_param_id];
            self.add_local_symbol(
                generic_param.name.value.clone(), 
                DeclRef::GenericParam(generic_param_id), 
                generic_param.span
            );
        }

        // Resolve return type
        if let Some(return_type) = program.functions[func_id].return_type {
            self.resolve_type_references(return_type, program);
        }

        // Resolve assumption expressions
        let assumption_ids = program.functions[func_id].assumptions.clone();
        for assumption_id in assumption_ids {
            self.resolve_expression_references(assumption_id, program);
        }

        // Resolve function body
        if let Some(body_id) = program.functions[func_id].body {
            self.resolve_block_references(body_id, program);
        }

        self.pop_scope();
        self.current_function = None;
    }

    /// Validate operator decorators (@infix, @prefix, etc.)
    fn validate_operator_decorators(&mut self, decorators: &[Spanned<String>], span: Option<Span>) {
        let mut has_infix = false;
        let mut has_prefix = false;
        let mut has_postfix = false;

        for decorator in decorators {
            match decorator.value.as_str() {
                "infix" => has_infix = true,
                "prefix" => has_prefix = true,
                "postfix" => has_postfix = true,
                "intrinsic" => {
                    // @intrinsic is valid for operators
                }
                _ => {
                    // Unknown decorator for operators
                    self.errors.add_error(
                        AstError::Semantic(SemanticError::InvalidDecorator {
                            decorator: decorator.value.clone(),
                            context: "operator".to_string(),
                        }),
                        decorator.span
                    );
                }
            }
        }

        // Validate decorator combinations
        let decorator_count = [has_infix, has_prefix, has_postfix].iter().filter(|&&x| x).count();
        if decorator_count > 1 {
            self.errors.add_error(
                AstError::Semantic(SemanticError::ConflictingDecorators {
                    decorators: vec!["infix".to_string(), "prefix".to_string(), "postfix".to_string()],
                }),
                span
            );
        }

        // Default to infix if no fixity decorator is specified
        // This is just a note for future implementation
    }

    fn resolve_type_declaration_references(&mut self, type_id: TypeDeclId, program: &mut Program) {
        self.push_scope();

        // Add generic parameters to scope
        let generic_param_ids = program.type_decls[type_id].generic_params.clone();
        for generic_param_id in generic_param_ids {
            let generic_param = &program.generic_params[generic_param_id];
            self.add_local_symbol(
                generic_param.name.value.clone(), 
                DeclRef::GenericParam(generic_param_id), 
                generic_param.span
            );
        }

        self.pop_scope();
    }

    fn resolve_const_declaration_references(&mut self, const_id: ConstId, program: &mut Program) {
        let const_decl = &program.const_decls[const_id];
        
        // Resolve type
        self.resolve_type_references(const_decl.ty, program);
        
        // Resolve value expression
        self.resolve_expression_references(const_decl.value, program);
    }

    fn resolve_table_declaration_references(&mut self, table_id: TableId, program: &mut Program) {
        let table_decl = &program.table_decls[table_id];
        
        // Resolve field types
        for field in &table_decl.fields {
            self.resolve_type_references(field.ty, program);
        }
        
        // Resolve node partition references
        for node in &table_decl.nodes {
            // Try to resolve the partition function
            if let Some(decl_ref) = self.lookup_symbol(&node.name.value) {
                if let DeclRef::Function(func_id) = decl_ref {
                    // Verify it's a partition function
                    let func = &program.functions[func_id];
                    if func.kind == CallableKind::Partition {
                        // Update the table node with resolved partition
                        // Note: We need mutable access, but we have immutable ref
                        // This would require restructuring or unsafe code
                    } else {
                        let error = NameResolutionError::InvalidReference {
                            name: node.name.value.clone(),
                            expected_kind: "partition function".to_string(),
                            found_kind: format!("{:?}", func.kind),
                        };
                        self.errors.add_error(AstError::NameResolution(error), node.span);
                    }
                } else {
                    let error = NameResolutionError::InvalidReference {
                        name: node.name.value.clone(),
                        expected_kind: "partition function".to_string(),
                        found_kind: "other declaration".to_string(),
                    };
                    self.errors.add_error(AstError::NameResolution(error), node.span);
                }
            } else {
                let error = NameResolutionError::UndefinedIdentifier {
                    name: node.name.value.clone(),
                };
                self.errors.add_error(AstError::NameResolution(error), node.span);
            }
            
            // Resolve node arguments
            for arg_id in &node.args {
                self.resolve_expression_references(*arg_id, program);
            }
        }
        
        // Resolve invariant expressions
        for invariant_id in &table_decl.invariants {
            self.resolve_expression_references(*invariant_id, program);
        }
    }

    // ========================================================================
    // --- Type Resolution
    // ========================================================================

    fn resolve_type_references(&mut self, type_id: TypeId, program: &mut Program) {
        let type_ref = &mut program.types[type_id];
        
        match type_ref {
            Type::Named(identifier) => {
                self.resolve_identifier_reference(identifier);
            }
            Type::Generic { base, args, .. } => {
                self.resolve_identifier_reference(base);
                for arg_type_id in args {
                    self.resolve_type_references(*arg_type_id, program);
                }
            }
            Type::Function { params, return_type, .. } => {
                for param_type_id in params {
                    self.resolve_type_references(*param_type_id, program);
                }
                self.resolve_type_references(*return_type, program);
            }
        }
    }

    // ========================================================================
    // --- Statement Resolution
    // ========================================================================

    fn resolve_block_references(&mut self, block_id: BlockId, program: &mut Program) {
        self.push_scope();
        
        let statement_ids = program.blocks[block_id].statements.clone();
        for stmt_id in statement_ids {
            self.resolve_statement_references(stmt_id, program);
        }
        
        self.pop_scope();
    }

    fn resolve_statement_references(&mut self, stmt_id: StmtId, program: &mut Program) {
        let stmt = &program.statements[stmt_id];
        
        match stmt {
            Statement::VarDecl(var_id) => {
                let var_decl = &program.var_decls[*var_id];
                
                // Add variable to current scope
                self.add_local_symbol(var_decl.name.value.clone(), DeclRef::Var(*var_id), var_decl.span);
                
                // Resolve type if present
                if let Some(type_id) = var_decl.ty {
                    self.resolve_type_references(type_id, program);
                }
                
                // Resolve initializer expression if present
                if let Some(init_id) = var_decl.init {
                    self.resolve_expression_references(init_id, program);
                }
            }
            
            Statement::If { condition, then_block, else_block, .. } => {
                self.resolve_expression_references(*condition, program);
                self.resolve_block_references(*then_block, program);
                if let Some(else_block_id) = else_block {
                    self.resolve_block_references(*else_block_id, program);
                }
            }
            
            Statement::For { init, condition, update, body, .. } => {
                self.push_scope(); // For loop has its own scope
                
                if let Some(for_init) = init {
                    match for_init {
                        ForInit::VarDecl(var_id) => {
                            let var_decl = &program.var_decls[*var_id];
                            self.add_local_symbol(var_decl.name.value.clone(), DeclRef::Var(*var_id), var_decl.span);
                            if let Some(type_id) = var_decl.ty {
                                self.resolve_type_references(type_id, program);
                            }
                            if let Some(init_id) = var_decl.init {
                                self.resolve_expression_references(init_id, program);
                            }
                        }
                        ForInit::Expression(expr_id) => {
                            self.resolve_expression_references(*expr_id, program);
                        }
                    }
                }
                
                if let Some(condition_id) = condition {
                    self.resolve_expression_references(*condition_id, program);
                }
                
                if let Some(update_id) = update {
                    self.resolve_expression_references(*update_id, program);
                }
                
                self.resolve_block_references(*body, program);
                self.pop_scope();
            }
            
            Statement::Return { value, .. } => {
                if let Some(value_id) = value {
                    self.resolve_expression_references(*value_id, program);
                }
            }
            
            Statement::Assert { expr, .. } => {
                self.resolve_expression_references(*expr, program);
            }
            
            Statement::Hop { body, .. } => {
                self.resolve_block_references(*body, program);
            }
            
            Statement::HopsFor { var, start, end, body, .. } => {
                self.push_scope();
                
                // Add loop variable to scope
                let var_decl = &program.var_decls[*var];
                self.add_local_symbol(var_decl.name.value.clone(), DeclRef::Var(*var), var_decl.span);
                
                // Resolve type if present
                if let Some(type_id) = var_decl.ty {
                    self.resolve_type_references(type_id, program);
                }
                
                self.resolve_expression_references(*start, program);
                self.resolve_expression_references(*end, program);
                self.resolve_block_references(*body, program);
                
                self.pop_scope();
            }
            
            Statement::Expression { expr, .. } => {
                self.resolve_expression_references(*expr, program);
            }
            
            Statement::Block(block_id) => {
                self.resolve_block_references(*block_id, program);
            }
        }
    }

    // ========================================================================
    // --- Expression Resolution
    // ========================================================================

    fn resolve_expression_references(&mut self, expr_id: ExprId, program: &mut Program) {
        let expr = &mut program.expressions[expr_id];
        
        match expr {
            Expression::Literal { .. } => {
                // Literals don't need resolution
            }
            
            Expression::Identifier(identifier) => {
                self.resolve_identifier_reference(identifier);
            }
            
            Expression::Binary { left, right, .. } => {
                self.resolve_expression_references(*left, program);
                self.resolve_expression_references(*right, program);
            }
            
            Expression::Unary { expr, .. } => {
                self.resolve_expression_references(*expr, program);
            }
            
            Expression::Assignment { lhs, rhs, .. } => {
                self.resolve_expression_references(*lhs, program);
                self.resolve_expression_references(*rhs, program);
            }
            
            Expression::Call { callee, args, resolved_callable, .. } => {
                self.resolve_expression_references(*callee, program);
                
                // Try to resolve the callable if it's an identifier
                if let Expression::Identifier(identifier) = &program.expressions[*callee] {
                    if let Some(DeclRef::Function(func_id)) = identifier.resolved {
                        *resolved_callable = Some(func_id);
                    }
                }
                
                for arg_id in args {
                    self.resolve_expression_references(*arg_id, program);
                }
            }
            
            Expression::MemberAccess { object, .. } => {
                self.resolve_expression_references(*object, program);
                // Field resolution would happen in type checking phase
            }
            
            Expression::TableRowAccess { table, key_values, .. } => {
                self.resolve_expression_references(*table, program);
                for key_value in key_values {
                    self.resolve_expression_references(key_value.value, program);
                }
            }
            
            Expression::Grouped { expr, .. } => {
                self.resolve_expression_references(*expr, program);
            }
        }
    }

    // ========================================================================
    // --- Identifier Resolution
    // ========================================================================

    fn resolve_identifier_reference(&mut self, identifier: &mut Identifier) {
        if let Some(decl_ref) = self.lookup_symbol(&identifier.name) {
            identifier.resolved = Some(decl_ref);
        } else {
            let error = NameResolutionError::UndefinedIdentifier {
                name: identifier.name.clone(),
            };
            self.errors.add_error(AstError::NameResolution(error), identifier.span);
        }
    }

    // ========================================================================
    // --- Scope Management
    // ========================================================================

    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn add_local_symbol(&mut self, name: String, decl_ref: DeclRef, span: Option<Span>) {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            if current_scope.contains_key(&name) {
                let error = NameResolutionError::DuplicateDeclaration {
                    name,
                    original_span: None,
                };
                self.errors.add_error(AstError::NameResolution(error), span);
            } else {
                current_scope.insert(name, decl_ref);
            }
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<DeclRef> {
        // Search local scopes first (from innermost to outermost)
        for scope in self.scope_stack.iter().rev() {
            if let Some(decl_ref) = scope.get(name) {
                return Some(*decl_ref);
            }
        }
        
        // Then search global scope
        self.global_symbols.get(name).copied()
    }
}

// ============================================================================
// --- Dependency Analysis
// ============================================================================

/// Analyzes dependencies between declarations to detect cycles
pub struct DependencyAnalyzer {
    visited: HashSet<DeclRef>,
    visiting: HashSet<DeclRef>,
    errors: ErrorCollector,
}

impl DependencyAnalyzer {
    pub fn new() -> Self {
        Self {
            visited: HashSet::new(),
            visiting: HashSet::new(),
            errors: ErrorCollector::new(),
        }
    }

    pub fn check_dependencies(program: &Program) -> Results<()> {
        let mut analyzer = Self::new();
        
        // Check all constant declarations for circular dependencies
        for declaration in &program.declarations {
            if let Declaration::Const(const_id) = declaration {
                analyzer.check_const_dependencies(*const_id, program);
            }
        }
        
        analyzer.errors.into_result(Some(()))
    }

    fn check_const_dependencies(&mut self, const_id: ConstId, program: &Program) {
        let decl_ref = DeclRef::Const(const_id);
        
        if self.visited.contains(&decl_ref) {
            return;
        }
        
        if self.visiting.contains(&decl_ref) {
            // Circular dependency detected
            let error = NameResolutionError::CircularDependency {
                cycle: vec![format!("const {}", program.const_decls[const_id].name.value)],
            };
            self.errors.add_error(AstError::NameResolution(error), program.const_decls[const_id].span);
            return;
        }
        
        self.visiting.insert(decl_ref);
        
        // Analyze dependencies in the constant expression
        self.analyze_expression_dependencies(program.const_decls[const_id].value, program);
        
        self.visiting.remove(&decl_ref);
        self.visited.insert(decl_ref);
    }

    fn analyze_expression_dependencies(&mut self, expr_id: ExprId, program: &Program) {
        let expr = &program.expressions[expr_id];
        
        match expr {
            Expression::Identifier(identifier) => {
                if let Some(DeclRef::Const(const_id)) = identifier.resolved {
                    self.check_const_dependencies(const_id, program);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.analyze_expression_dependencies(*left, program);
                self.analyze_expression_dependencies(*right, program);
            }
            Expression::Unary { expr, .. } => {
                self.analyze_expression_dependencies(*expr, program);
            }
            Expression::Call { callee, args, .. } => {
                self.analyze_expression_dependencies(*callee, program);
                for arg_id in args {
                    self.analyze_expression_dependencies(*arg_id, program);
                }
            }
            Expression::Grouped { expr, .. } => {
                self.analyze_expression_dependencies(*expr, program);
            }
            // Other expression types...
            _ => {}
        }
    }
}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Perform complete name resolution on a program
pub fn resolve_names(program: Program) -> Results<Program> {
    // First resolve names
    let result = NameResolver::resolve_names(program);
    
    if let Some(program) = result.result {
        // Then check for circular dependencies
        let dep_result = DependencyAnalyzer::check_dependencies(&program);
        
        if dep_result.has_errors() {
            // Combine errors
            let mut all_errors = result.errors;
            all_errors.extend(dep_result.errors);
            Results {
                result: Some(program),
                errors: all_errors,
            }
        } else {
            result
        }
    } else {
        result
    }
}
