//! Name Resolution for AST
//!
//! This module implements symbol table based name resolution for the FMitF DSL.
//! It resolves all identifier references to their declarations and populates
//! the AST resolved fields accordingly.
//!
//! Key features:
//! - Traditional symbol table with scope stack
//! - Special handling for `hop` (no new scope) and `hops_for` (creates scope with loop var)
//! - Operator resolution with infix/prefix classification
//! - Populates AST resolved fields: Identifier.resolved, Call.resolved_callable, etc.
//! - Validates identifier existence and reports undefined identifier errors

use crate::ast::errors::{AstError, AstErrorKind};
use crate::ast::*;
use std::collections::HashMap;

// ============================================================================
// --- Symbol Table
// ============================================================================

/// Symbol table for name resolution using a scope stack
#[derive(Debug)]
struct SymbolTable {
    /// Stack of scopes, with global scope at index 0
    scopes: Vec<Scope>,
}

/// A single scope containing symbol bindings
#[derive(Debug)]
struct Scope {
    /// Map from symbol name to declaration reference(s)
    /// For functions/operators, we can have multiple with same name (overloading)
    symbols: HashMap<String, Vec<IdentifierResolution>>,
    /// Whether this scope is a function scope (for return statements)
    is_function_scope: bool,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: vec![Scope {
                symbols: HashMap::new(),
                is_function_scope: false,
            }],
        }
    }

    /// Enter a new scope
    fn enter_scope(&mut self, is_function_scope: bool) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
            is_function_scope,
        });
    }

    /// Exit the current scope
    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a symbol in the current scope
    fn define(&mut self, name: String, decl_ref: IdentifierResolution) -> Result<(), AstError> {
        let current_scope = self.scopes.last_mut().unwrap();

        // For functions, allow multiple definitions (overloading)
        // For other types, check for duplicates
        match decl_ref {
            IdentifierResolution::Function(_) => {
                // Allow multiple function definitions with same name (overloading)
                current_scope
                    .symbols
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(decl_ref);
            }
            _ => {
                // For non-functions, don't allow duplicates
                if let Some(existing) = current_scope.symbols.get(&name) {
                    if !existing.is_empty() {
                        return Err(AstError::duplicate_definition(name));
                    }
                }
                current_scope.symbols.insert(name, vec![decl_ref]);
            }
        }
        Ok(())
    }

    /// Look up a symbol, searching from current scope to global scope
    /// Returns the first match found (for overloaded functions, you may get any overload)
    fn lookup(&self, name: &str) -> Option<IdentifierResolution> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl_refs) = scope.symbols.get(name) {
                if let Some(&first_decl) = decl_refs.first() {
                    return Some(first_decl);
                }
            }
        }
        None
    }

    /// Check if we're currently in a function scope
    fn is_in_function(&self) -> bool {
        self.scopes.iter().any(|scope| scope.is_function_scope)
    }
}

// ============================================================================
// --- Name Resolver
// ============================================================================

/// Main name resolver struct
pub struct NameResolver {
    symbol_table: SymbolTable,
    errors: Vec<AstError>,
}

impl NameResolver {
    fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    /// Resolve all names in the program
    pub fn resolve(program: &mut Program) -> Result<(), Vec<AstError>> {
        let mut resolver = Self::new();

        // Phase 1: Collect all top-level declarations
        resolver.collect_global_declarations(program);

        // Phase 2: Resolve names within each declaration
        resolver.resolve_all_declarations(program);

        if resolver.errors.is_empty() {
            Ok(())
        } else {
            Err(resolver.errors)
        }
    }

    /// Collect all global declarations and add them to the symbol table
    fn collect_global_declarations(&mut self, program: &Program) {
        for declaration in &program.declarations {
            match declaration {
                Declaration::Callable(func_id) => {
                    if let Some(func) = program.functions.get(*func_id) {
                        let name = Self::get_callable_name(func);
                        if let Err(e) = self
                            .symbol_table
                            .define(name, IdentifierResolution::Function(*func_id))
                        {
                            self.errors.push(e);
                        }
                    }
                }
                Declaration::Type(type_id) => {
                    if let Some(type_decl) = program.type_decls.get(*type_id) {
                        let name = type_decl.name.name.clone();
                        if let Err(e) = self
                            .symbol_table
                            .define(name, IdentifierResolution::Type(*type_id))
                        {
                            self.errors.push(e);
                        }
                    }
                }
                Declaration::Const(const_id) => {
                    if let Some(const_decl) = program.const_decls.get(*const_id) {
                        let name = const_decl.name.name.clone();
                        if let Err(e) = self
                            .symbol_table
                            .define(name, IdentifierResolution::Const(*const_id))
                        {
                            self.errors.push(e);
                        }
                    }
                }
                Declaration::Table(table_id) => {
                    if let Some(table_decl) = program.table_decls.get(*table_id) {
                        let name = table_decl.name.name.clone();
                        if let Err(e) = self
                            .symbol_table
                            .define(name, IdentifierResolution::Table(*table_id))
                        {
                            self.errors.push(e);
                        }
                    }
                }
            }
        }
    }

    /// Resolve names in all declarations
    fn resolve_all_declarations(&mut self, program: &mut Program) {
        let declarations: Vec<_> = program.declarations.clone();
        for declaration in declarations {
            self.resolve_declaration(program, declaration);
        }
    }

    /// Resolve names in a declaration
    fn resolve_declaration(&mut self, program: &mut Program, declaration: Declaration) {
        match declaration {
            Declaration::Callable(func_id) => {
                self.resolve_callable(program, func_id);
            }
            Declaration::Type(type_id) => {
                self.resolve_type_declaration(program, type_id);
            }
            Declaration::Const(const_id) => {
                self.resolve_const(program, const_id);
            }
            Declaration::Table(table_id) => {
                self.resolve_table(program, table_id);
            }
        }
    }

    /// Get the name of a callable declaration
    fn get_callable_name(func: &CallableDecl) -> String {
        match &func.name {
            CallableName::Identifier(id) => id.name.clone(),
            CallableName::Operator(op) => op.value.clone(),
        }
    }

    /// Resolve names in a callable declaration
    fn resolve_callable(&mut self, program: &mut Program, func_id: FunctionId) {
        // Enter function scope
        self.symbol_table.enter_scope(true);

        // Clone the function to avoid borrowing conflicts
        let func = if let Some(func) = program.functions.get(func_id) {
            func.clone()
        } else {
            return;
        };

        // Add generic parameters to scope first
        for &generic_param_id in &func.generic_params {
            if let Some(generic_param) = program.generic_params.get(generic_param_id) {
                let name = generic_param.name.name.clone();
                if let Err(e) = self
                    .symbol_table
                    .define(name, IdentifierResolution::GenericParam(generic_param_id))
                {
                    self.errors.push(e);
                }
            }
        }

        // Add parameters to scope
        for &param_id in &func.params {
            if let Some(param) = program.params.get(param_id) {
                let name = param.name.name.clone();
                if let Err(e) = self
                    .symbol_table
                    .define(name, IdentifierResolution::Param(param_id))
                {
                    self.errors.push(e);
                }
                // Resolve parameter type
                self.resolve_type(program, param.ty);
            }
        }

        // Resolve return type
        if let Some(return_type_id) = func.return_type {
            self.resolve_type(program, return_type_id);
        }

        // Resolve assumptions
        for &assumption_id in &func.assumptions {
            self.resolve_expression(program, assumption_id);
        }

        // Resolve body
        if let Some(body_id) = func.body {
            self.resolve_block(program, body_id);
        }

        // Exit function scope
        self.symbol_table.exit_scope();
    }

    /// Resolve names in a const declaration
    fn resolve_const(&mut self, program: &mut Program, const_id: ConstId) {
        if let Some(const_decl) = program.const_decls.get(const_id) {
            let const_decl = const_decl.clone();

            // Resolve type
            self.resolve_type(program, const_decl.ty);

            // Resolve value expression
            self.resolve_expression(program, const_decl.value);
        }
    }

    /// Resolve names in a type declaration
    fn resolve_type_declaration(&mut self, program: &mut Program, type_id: TypeDeclId) {
        let type_decl = if let Some(type_decl) = program.type_decls.get(type_id) {
            type_decl.clone()
        } else {
            return;
        };

        // Enter scope for generic parameters
        self.symbol_table.enter_scope(false);

        // Add generic parameters to scope
        for &generic_param_id in &type_decl.generic_params {
            if let Some(generic_param) = program.generic_params.get(generic_param_id) {
                let name = generic_param.name.name.clone();
                if let Err(e) = self
                    .symbol_table
                    .define(name, IdentifierResolution::GenericParam(generic_param_id))
                {
                    self.errors.push(e);
                }
            }
        }

        // Exit scope
        self.symbol_table.exit_scope();
    }

    /// Resolve names in a table declaration
    fn resolve_table(&mut self, program: &mut Program, table_id: TableId) {
        let table_decl = if let Some(table_decl) = program.table_decls.get(table_id) {
            table_decl.clone()
        } else {
            return;
        };

        // Enter scope for table field resolution
        self.symbol_table.enter_scope(false);

        // Collect table fields for field reference resolution
        let mut table_fields = HashMap::new();

        // Add table fields to scope and collect field info
        for element in &table_decl.elements {
            if let TableElement::Field(field) = element {
                table_fields.insert(field.name.name.clone(), field.clone());

                // Resolve field type
                self.resolve_type(program, field.ty);
            }
        }

        // Resolve partition function call
        for element in &table_decl.elements {
            if let TableElement::Node(table_node) = element {
                // Resolve partition function reference
                if let Some(decl_ref) = self.symbol_table.lookup(&table_node.name.name) {
                    if let IdentifierResolution::Function(func_id) = decl_ref {
                        // Populate the resolved_partition field
                        if let Some(mut_table_decl) = program.table_decls.get_mut(table_id) {
                            for element in &mut mut_table_decl.elements {
                                if let TableElement::Node(node) = element {
                                    node.resolved_partition = Some(func_id);
                                    break;
                                }
                            }
                        }
                    }
                } else {
                    self.errors
                        .push(AstError::undefined_identifier(table_node.name.name.clone()));
                }

                // Resolve arguments with table field context
                for &arg_id in &table_node.args {
                    self.resolve_expression_with_table_fields(program, arg_id, &table_fields);
                }
            }
        }

        // Exit table scope
        self.symbol_table.exit_scope();
    }

    /// Resolve an expression with table field context for validation
    fn resolve_expression_with_table_fields(
        &mut self,
        program: &mut Program,
        expr_id: ExprId,
        table_fields: &HashMap<String, TableField>,
    ) {
        if let Some(expression) = program.expressions.get(expr_id) {
            let expression = expression.clone();
            match expression {
                Expression::Identifier {
                    name: identifier, ..
                } => {
                    // Check if this identifier refers to a table field or a normal identifier
                    if table_fields.contains_key(&identifier.name) {
                        // This is a table field reference - valid
                    } else {
                        // Normal identifier - resolve it
                        self.resolve_identifier(program, expr_id, &identifier.name);
                    }
                }
                Expression::Binary {
                    left, right, op, ..
                } => {
                    self.resolve_expression_with_table_fields(program, left, table_fields);
                    self.resolve_expression_with_table_fields(program, right, table_fields);
                    self.resolve_operator(program, &op.value, 2);
                }
                Expression::Unary { expr, op, .. } => {
                    self.resolve_expression_with_table_fields(program, expr, table_fields);
                    self.resolve_operator(program, &op.value, 1);
                }
                Expression::Call { callee, args, .. } => {
                    self.resolve_expression_with_table_fields(program, callee, table_fields);
                    for &arg_id in &args {
                        self.resolve_expression_with_table_fields(program, arg_id, table_fields);
                    }
                }
                _ => {
                    // For other expression types, delegate to normal resolution
                    self.resolve_expression(program, expr_id);
                }
            }
        }
    }

    /// Resolve names in a type
    fn resolve_type(&mut self, program: &mut Program, type_id: TypeId) {
        if let Some(type_decl) = program.types.get(type_id) {
            let type_decl = type_decl.clone();
            match type_decl {
                Type::Named {
                    name: identifier, ..
                } => {
                    self.resolve_identifier_name(&identifier.name);
                }
                Type::Generic { base, args, .. } => {
                    self.resolve_identifier_name(&base.name);
                    for &arg_id in &args {
                        self.resolve_type(program, arg_id);
                    }
                }
                Type::Function {
                    params,
                    return_type,
                    ..
                } => {
                    for &param_id in &params {
                        self.resolve_type(program, param_id);
                    }
                    self.resolve_type(program, return_type);
                }
            }
        }
    }

    /// Resolve names in a block
    fn resolve_block(&mut self, program: &mut Program, block_id: BlockId) {
        if let Some(block) = program.blocks.get(block_id) {
            let statements = block.statements.clone();
            for &stmt_id in &statements {
                self.resolve_statement(program, stmt_id);
            }
        }
    }

    /// Resolve names in a statement
    fn resolve_statement(&mut self, program: &mut Program, stmt_id: StmtId) {
        if let Some(statement) = program.statements.get(stmt_id) {
            let statement = statement.clone();
            match statement {
                Statement::VarDecl(var_id) => {
                    if let Some(var_decl) = program.var_decls.get(var_id) {
                        let var_decl = var_decl.clone();

                        // Define the variable in current scope
                        let name = var_decl.name.name.clone();
                        if let Err(e) = self
                            .symbol_table
                            .define(name, IdentifierResolution::Var(var_id))
                        {
                            self.errors.push(e);
                        }

                        // Resolve type if present
                        if let Some(type_id) = var_decl.ty {
                            self.resolve_type(program, type_id);
                        }

                        // Resolve initializer if present
                        if let Some(init_id) = var_decl.init {
                            self.resolve_expression(program, init_id);
                        }
                    }
                }
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                    ..
                } => {
                    self.resolve_expression(program, condition);

                    // Enter scope for then block
                    self.symbol_table.enter_scope(false);
                    self.resolve_block(program, then_block);
                    self.symbol_table.exit_scope();

                    if let Some(else_block_id) = else_block {
                        // Enter scope for else block
                        self.symbol_table.enter_scope(false);
                        self.resolve_block(program, else_block_id);
                        self.symbol_table.exit_scope();
                    }
                }
                Statement::For {
                    init,
                    condition,
                    update,
                    body,
                    ..
                } => {
                    // Enter scope for the entire for loop
                    self.symbol_table.enter_scope(false);

                    // Resolve init
                    if let Some(for_init) = init {
                        match for_init {
                            ForInit::VarDecl(var_id) => {
                                if let Some(var_decl) = program.var_decls.get(var_id) {
                                    let var_decl = var_decl.clone();
                                    let name = var_decl.name.name.clone();
                                    if let Err(e) = self
                                        .symbol_table
                                        .define(name, IdentifierResolution::Var(var_id))
                                    {
                                        self.errors.push(e);
                                    }

                                    if let Some(type_id) = var_decl.ty {
                                        self.resolve_type(program, type_id);
                                    }

                                    if let Some(init_id) = var_decl.init {
                                        self.resolve_expression(program, init_id);
                                    }
                                }
                            }
                            ForInit::Expression(expr_id) => {
                                self.resolve_expression(program, expr_id);
                            }
                        }
                    }

                    // Resolve condition
                    if let Some(condition_id) = condition {
                        self.resolve_expression(program, condition_id);
                    }

                    // Resolve update
                    if let Some(update_id) = update {
                        self.resolve_expression(program, update_id);
                    }

                    // Resolve body
                    self.resolve_block(program, body);

                    // Exit for loop scope
                    self.symbol_table.exit_scope();
                }
                Statement::Return { value, .. } => {
                    if !self.symbol_table.is_in_function() {
                        self.errors.push(AstError::return_outside_function());
                    }

                    if let Some(value_id) = value {
                        self.resolve_expression(program, value_id);
                    }
                }
                Statement::Assert { expr, .. } => {
                    self.resolve_expression(program, expr);
                }
                Statement::Hop { body, .. } => {
                    // IMPORTANT: Hop does NOT create a new scope
                    self.resolve_block(program, body);
                }
                Statement::HopsFor {
                    var,
                    start,
                    end,
                    body,
                    ..
                } => {
                    // IMPORTANT: HopsFor DOES create a new scope with the loop variable
                    self.symbol_table.enter_scope(false);

                    // Define the loop variable
                    if let Some(var_decl) = program.var_decls.get(var) {
                        let var_decl = var_decl.clone();
                        let name = var_decl.name.name.clone();
                        if let Err(e) = self
                            .symbol_table
                            .define(name, IdentifierResolution::Var(var))
                        {
                            self.errors.push(e);
                        }

                        if let Some(type_id) = var_decl.ty {
                            self.resolve_type(program, type_id);
                        }
                    }

                    // Resolve start and end expressions
                    self.resolve_expression(program, start);
                    self.resolve_expression(program, end);

                    // Resolve body
                    self.resolve_block(program, body);

                    // Exit hops_for scope
                    self.symbol_table.exit_scope();
                }
                Statement::Expression { expr, .. } => {
                    self.resolve_expression(program, expr);
                }
                Statement::Block(block_id) => {
                    // Enter scope for nested block
                    self.symbol_table.enter_scope(false);
                    self.resolve_block(program, block_id);
                    self.symbol_table.exit_scope();
                }
            }
        }
    }

    /// Resolve names in an expression
    fn resolve_expression(&mut self, program: &mut Program, expr_id: ExprId) {
        if let Some(expression) = program.expressions.get(expr_id) {
            let expression = expression.clone();
            match expression {
                Expression::Literal { .. } => {
                    // Literals don't need name resolution
                }
                Expression::Identifier {
                    name: identifier, ..
                } => {
                    self.resolve_identifier(program, expr_id, &identifier.name);
                }
                Expression::Binary {
                    left, right, op, ..
                } => {
                    self.resolve_expression(program, left);
                    self.resolve_expression(program, right);

                    // Resolve binary operator (2 parameters)
                    self.resolve_operator(program, &op.value, 2);
                }
                Expression::Unary { expr, op, .. } => {
                    self.resolve_expression(program, expr);

                    // Resolve unary operator (1 parameter)
                    self.resolve_operator(program, &op.value, 1);
                }
                Expression::Assignment { lhs, rhs, .. } => {
                    self.resolve_expression(program, lhs);
                    self.resolve_expression(program, rhs);
                }
                Expression::Call { callee, args, .. } => {
                    self.resolve_expression(program, callee);

                    // Resolve call - populate resolved_callable if callee is an identifier
                    if let Some(Expression::Identifier { name: ident, .. }) =
                        program.expressions.get(callee)
                    {
                        if let Some(decl_ref) = self.symbol_table.lookup(&ident.name) {
                            if let IdentifierResolution::Function(func_id) = decl_ref {
                                // Populate the resolved_callable field
                                if let Some(Expression::Call {
                                    resolved_callable, ..
                                }) = program.expressions.get_mut(expr_id)
                                {
                                    *resolved_callable = Some(func_id);
                                }
                            }
                        }
                    }

                    for &arg_id in &args {
                        self.resolve_expression(program, arg_id);
                    }
                }
                Expression::MemberAccess { object, member, .. } => {
                    self.resolve_expression(program, object);

                    // Resolve member access - populate resolved_field if object is a table
                    self.resolve_member_access(program, expr_id, object, &member.name);
                }
                Expression::TableRowAccess {
                    table, key_values, ..
                } => {
                    self.resolve_expression(program, table);
                    for key_value in &key_values {
                        self.resolve_expression(program, key_value.value);

                        // Resolve key field - populate resolved_field
                        self.resolve_key_value_field(program, key_value, table);
                    }
                }
                Expression::Grouped { expr, .. } => {
                    self.resolve_expression(program, expr);
                }
                Expression::Lambda {
                    params,
                    return_type,
                    body,
                    ..
                } => {
                    // Enter scope for lambda
                    self.symbol_table.enter_scope(true); // Lambda is a function scope

                    // Add parameters to scope
                    for &param_id in &params {
                        if let Some(param) = program.params.get(param_id) {
                            let name = param.name.name.clone();
                            if let Err(e) = self
                                .symbol_table
                                .define(name, IdentifierResolution::Param(param_id))
                            {
                                self.errors.push(e);
                            }
                            // Resolve parameter type
                            self.resolve_type(program, param.ty);
                        }
                    }

                    // Resolve return type
                    self.resolve_type(program, return_type);

                    // Resolve body
                    self.resolve_block(program, body);

                    // Exit lambda scope
                    self.symbol_table.exit_scope();
                }
            }
        }
    }

    /// Resolve an identifier and populate the resolved_declaration field
    fn resolve_identifier(&mut self, program: &mut Program, expr_id: ExprId, name: &str) {
        if let Some(decl_ref) = self.symbol_table.lookup(name) {
            // Populate the resolved_declaration field
            if let Some(Expression::Identifier {
                resolved_declaration,
                ..
            }) = program.expressions.get_mut(expr_id)
            {
                *resolved_declaration = Some(decl_ref);
            }
        } else {
            self.errors
                .push(AstError::undefined_identifier(name.to_string()));
        }
    }

    /// Resolve an identifier name (without AST mutation)
    fn resolve_identifier_name(&mut self, name: &str) {
        if self.symbol_table.lookup(name).is_none() {
            self.errors
                .push(AstError::undefined_identifier(name.to_string()));
        }
    }

    /// Resolve member access and populate resolved_field
    fn resolve_member_access(
        &mut self,
        program: &mut Program,
        expr_id: ExprId,
        _object_id: ExprId,
        member_name: &str,
    ) {
        // Get the type of the object to determine if it's a table
        // For now, we'll implement basic validation
        // In a full implementation, this would use type information

        // Try to find the field in any table that matches
        let mut found_field = None;
        for table_decl in program.table_decls.iter() {
            for element in &table_decl.1.elements {
                if let TableElement::Field(field) = element {
                    if field.name.name == member_name {
                        found_field = Some(field.clone());
                        break;
                    }
                }
            }
            if found_field.is_some() {
                break;
            }
        }

        if let Some(field) = found_field {
            // Populate the resolved_field
            if let Some(Expression::MemberAccess { resolved_field, .. }) =
                program.expressions.get_mut(expr_id)
            {
                *resolved_field = Some(field);
            }
        } else {
            self.errors.push(AstError::undefined_identifier(format!(
                "field '{}'",
                member_name
            )));
        }
    }

    /// Resolve key-value field and populate resolved_field
    fn resolve_key_value_field(
        &mut self,
        program: &mut Program,
        key_value: &KeyValue,
        _table_id: ExprId,
    ) {
        // Find the table and verify the key field exists
        // This is a simplified implementation
        let mut found_field = None;

        for table_decl in program.table_decls.iter() {
            for element in &table_decl.1.elements {
                if let TableElement::Field(field) = element {
                    if field.name.name == key_value.key.name {
                        found_field = Some(field.clone());
                        break;
                    }
                }
            }
            if found_field.is_some() {
                break;
            }
        }

        if found_field.is_none() {
            self.errors.push(AstError::undefined_identifier(format!(
                "table key field '{}'",
                key_value.key.name
            )));
        }

        // Note: We can't mutate key_value here since it's borrowed immutably
        // In a real implementation, we'd need to restructure this to allow mutation
    }

    /// Resolve an operator by looking for its declaration
    fn resolve_operator(&mut self, _program: &mut Program, op: &str, expected_params: usize) {
        // Check if it's a built-in operator first
        if self.is_builtin_operator(op, expected_params) {
            // Built-in operators are always valid
            return;
        }

        // Look for operator declaration in symbol table (custom operators)
        if let Some(decl_ref) = self.symbol_table.lookup(op) {
            if let IdentifierResolution::Function(func_id) = decl_ref {
                // Validate parameter count matches expected usage
                if let Some(func) = _program.functions.get(func_id) {
                    let param_count = func.params.len();
                    if param_count != expected_params {
                        self.errors
                            .push(AstError::new(AstErrorKind::InvalidOperation {
                                op: op.to_string(),
                                details: format!(
                                    "Expected {} parameters, found {}",
                                    expected_params, param_count
                                ),
                            }));
                    }
                }
            }
        } else {
            // Operator not found in symbol table - it's undefined
            self.errors
                .push(AstError::undefined_identifier(op.to_string()));
        }
    }

    /// Check if an operator is a built-in operator with the correct arity
    fn is_builtin_operator(&self, op: &str, expected_params: usize) -> bool {
        match expected_params {
            1 => {
                // Unary operators
                matches!(op, "!" | "-")
            }
            2 => {
                // Binary operators
                matches!(
                    op,
                    "+" | "-"
                        | "*"
                        | "/"
                        | "%"
                        | "=="
                        | "!="
                        | "<"
                        | ">"
                        | "<="
                        | ">="
                        | "&&"
                        | "||"
                )
            }
            _ => false,
        }
    }
}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Resolve all names in the program
pub fn resolve_names(program: &mut Program) -> Result<(), Vec<AstError>> {
    NameResolver::resolve(program)
}
