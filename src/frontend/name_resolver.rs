use crate::ast::common::IdentifierResolution;
use crate::ast::expr::{Expression, KeyValue};
use crate::ast::item::{Item, TableElement};
use crate::ast::stmt::{ForInit, Statement};
use crate::ast::visit_mut::{self, VisitorMut};
use crate::ast::{BlockId, ExprId, FunctionId, Program, StmtId, VarId};
use crate::frontend::errors::FrontEndErrorKind;
use crate::util::CompilerError;
use std::collections::HashMap;

// ============================================================================
// --- Symbol Table
// ============================================================================

/// Represents a single scope in the symbol table.
#[derive(Debug, Clone, Default)]
struct Scope {
    /// Maps a name to a list of declarations. The list handles function/operator overloading.
    symbols: HashMap<String, Vec<IdentifierResolution>>,
}

impl Scope {
    /// Add a new symbol definition to this scope.
    fn define(&mut self, name: String, resolution: IdentifierResolution) {
        self.symbols.entry(name).or_default().push(resolution);
    }

    /// Find a symbol in this scope.
    fn find(&self, name: &str) -> Option<&Vec<IdentifierResolution>> {
        self.symbols.get(name)
    }
}

/// A stack-based symbol table for managing scopes during name resolution.
#[derive(Debug, Default)]
struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    /// Creates a new symbol table with a single global scope.
    fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    /// Pushes a new scope onto the stack.
    fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    /// Pops the current scope from the stack.
    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Defines a new identifier in the current scope.
    fn define(&mut self, name: String, resolution: IdentifierResolution) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, resolution);
        }
    }

    /// Resolves an identifier by searching from the innermost scope outwards.
    fn resolve(&self, name: &str) -> Option<&Vec<IdentifierResolution>> {
        for scope in self.scopes.iter().rev() {
            if let Some(resolutions) = scope.find(name) {
                return Some(resolutions);
            }
        }
        None
    }
}

// ============================================================================
// --- Name Resolver
// ============================================================================

/// The main struct for performing name resolution.
/// It traverses the AST, using a symbol table to resolve identifiers.
pub struct NameResolver {
    symbols: SymbolTable,
    errors: Vec<CompilerError>,
    // This tracks if we are inside a loop, useful for `break` or `continue` validation later.
    loop_depth: u32,
}

/// Public function to start the name resolution process.
pub fn resolve_names(prog: &mut Program) -> Result<(), Vec<CompilerError>> {
    let mut resolver = NameResolver {
        symbols: SymbolTable::new(),
        errors: Vec::new(),
        loop_depth: 0,
    };

    // First Pass: Collect all top-level (global) declarations.
    // This allows functions/types to be used before they are declared in the source file.
    for item in &prog.declarations {
        match item {
            Item::Callable(id) => {
                let decl = &prog.functions[*id];
                resolver
                    .symbols
                    .define(decl.name.name.clone(), IdentifierResolution::Function(*id));
            }
            Item::Type(id) => {
                let decl = &prog.type_decls[*id];
                resolver
                    .symbols
                    .define(decl.name.name.clone(), IdentifierResolution::Type(*id));
            }
            Item::Const(id) => {
                let decl = &prog.const_decls[*id];
                resolver
                    .symbols
                    .define(decl.name.name.clone(), IdentifierResolution::Const(*id));
            }
            Item::Table(id) => {
                let decl = &prog.table_decls[*id];
                resolver
                    .symbols
                    .define(decl.name.name.clone(), IdentifierResolution::Table(*id));
            }
        }
    }

    // Second Pass: Visit and resolve all nodes in the AST.
    let _ = resolver.visit_program(prog);

    if resolver.errors.is_empty() {
        Ok(())
    } else {
        Err(resolver.errors)
    }
}

impl<'ast> VisitorMut<'ast, (), ()> for NameResolver {
    // We only need to implement visit methods for nodes that define or reference names,
    // or that create new scopes.

    fn visit_program(&mut self, prog: &mut Program) -> Result<(), ()> {
        // First visit all the top-level items using the default walker
        visit_mut::walk_program_mut(self, prog)?;

        // Collect table IDs to avoid borrowing issues
        let table_ids: Vec<_> = prog
            .declarations
            .iter()
            .filter_map(|item| {
                if let Item::Table(table_id) = item {
                    Some(*table_id)
                } else {
                    None
                }
            })
            .collect();

        // Then explicitly visit table declarations to resolve partition references
        for table_id in table_ids {
            self.visit_table_decl(prog, table_id)?;
        }

        Ok(())
    }

    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) -> Result<(), ()> {
        self.symbols.enter_scope();
        let decl = prog.functions[id].clone();

        for param_id in &decl.generic_params {
            let param = &prog.generic_params[*param_id];
            self.symbols.define(
                param.name.name.clone(),
                IdentifierResolution::GenericParam(*param_id),
            );
        }

        for param_id in &decl.params {
            let param = &prog.params[*param_id];
            self.symbols.define(
                param.name.name.clone(),
                IdentifierResolution::Param(*param_id),
            );
        }

        // Use the default walker to visit the body.
        visit_mut::walk_callable_decl_mut(self, prog, id)?;

        self.symbols.exit_scope();
        Ok(())
    }

    fn visit_block(&mut self, prog: &mut Program, id: BlockId) -> Result<(), ()> {
        self.symbols.enter_scope();
        visit_mut::walk_block_mut(self, prog, id)?;
        self.symbols.exit_scope();
        Ok(())
    }

    fn visit_var_decl(&mut self, prog: &mut Program, id: VarId) -> Result<(), ()> {
        // First, visit the initializer expression if it exists.
        // This prevents `let x = x;` from being valid.
        visit_mut::walk_var_decl_mut(self, prog, id)?;

        // Then, define the variable in the current scope.
        let decl = &prog.var_decls[id];
        self.symbols
            .define(decl.name.name.clone(), IdentifierResolution::Var(id));

        Ok(())
    }

    fn visit_stmt(&mut self, prog: &mut Program, id: StmtId) -> Result<(), ()> {
        let stmt = prog.statements[id].clone();
        match stmt {
            // `hop` does not create a new scope.
            Statement::Hop { body, .. } => {
                let _ = self.visit_block(prog, body);
            }
            // `HopsFor` creates a new scope for its loop variable.
            Statement::HopsFor {
                var,
                body,
                start,
                end,
                ..
            } => {
                self.loop_depth += 1;
                self.symbols.enter_scope();
                let _ = self.visit_expr(prog, start);
                let _ = self.visit_expr(prog, end);
                let var_decl = &prog.var_decls[var];
                self.symbols
                    .define(var_decl.name.name.clone(), IdentifierResolution::Var(var));
                let _ = self.visit_block(prog, body);
                self.symbols.exit_scope();
                self.loop_depth -= 1;
            }
            // `For` also creates a scope if it has a variable declaration.
            Statement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                self.loop_depth += 1;
                self.symbols.enter_scope();
                if let Some(ForInit::VarDecl(var_id)) = init {
                    let _ = self.visit_var_decl(prog, var_id);
                } else if let Some(ForInit::Expression(expr_id)) = init {
                    let _ = self.visit_expr(prog, expr_id);
                }

                if let Some(cond_id) = condition {
                    let _ = self.visit_expr(prog, cond_id);
                }
                if let Some(update_id) = update {
                    let _ = self.visit_expr(prog, update_id);
                }
                let _ = self.visit_block(prog, body);
                self.symbols.exit_scope();
                self.loop_depth -= 1;
            }
            // For all other statements, use the default walker.
            _ => {
                visit_mut::walk_stmt_mut(self, prog, id)?;
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<(), ()> {
        // First, handle the specific cases that need resolution before recursing
        match &prog.expressions[id] {
            Expression::Identifier { name, span, .. } => {
                if let Some(resolutions) = self.symbols.resolve(&name.name) {
                    // In case of overloading, we just take the first one.
                    // The type checker will be responsible for picking the correct overload.
                    if let Expression::Identifier {
                        resolved_declaration,
                        ..
                    } = &mut prog.expressions[id]
                    {
                        *resolved_declaration = Some(resolutions[0]);
                    }
                } else {
                    let err = FrontEndErrorKind::UndefinedIdentifier {
                        name: name.name.clone(),
                    };
                    let default_span =
                        span.unwrap_or_else(|| crate::util::Span::new(0, 0, "<unknown>"));
                    self.errors.push(CompilerError::new(err, default_span));
                }
            }
            Expression::MemberAccess { object, member, .. } => {
                // Collect information before modifying
                let object_id = *object;
                let member_name = member.name.clone();

                // Check if object is an identifier
                if let Expression::Identifier {
                    name: table_name, ..
                } = &prog.expressions[object_id]
                {
                    let table_name = table_name.name.clone();
                    if let Some(resolutions) = self.symbols.resolve(&table_name) {
                        if let IdentifierResolution::Table(table_id) = resolutions[0] {
                            // Look up the field in the table
                            let mut resolved_field = None;
                            if let Some(table_item) = prog.table_decls.get(table_id) {
                                for element in &table_item.elements {
                                    if let TableElement::Field(field) = element {
                                        if field.name.name == member_name {
                                            resolved_field = Some(field.clone());
                                            break;
                                        }
                                    }
                                }
                            }

                            // Now update the expression
                            if let Expression::MemberAccess {
                                resolved_table,
                                resolved_field: rf,
                                ..
                            } = &mut prog.expressions[id]
                            {
                                *resolved_table = Some(table_id);
                                *rf = resolved_field;
                            }
                        }
                    }
                }
            }
            Expression::TableRowAccess { table, .. } => {
                let table_id = *table;

                // Check if table is an identifier
                if let Expression::Identifier {
                    name: table_name, ..
                } = &prog.expressions[table_id]
                {
                    let table_name = table_name.name.clone();
                    if let Some(resolutions) = self.symbols.resolve(&table_name) {
                        if let IdentifierResolution::Table(table_id) = resolutions[0] {
                            // Update the expression
                            if let Expression::TableRowAccess { resolved_table, .. } =
                                &mut prog.expressions[id]
                            {
                                *resolved_table = Some(table_id);
                            }
                        }
                    }
                }
            }
            Expression::Call { callee, .. } => {
                let callee_id = *callee;

                // If callee is an identifier, try to resolve it as a function
                if let Expression::Identifier { name, .. } = &prog.expressions[callee_id] {
                    let func_name = name.name.clone();
                    if let Some(resolutions) = self.symbols.resolve(&func_name) {
                        if let IdentifierResolution::Function(func_id) = resolutions[0] {
                            if let Expression::Call {
                                resolved_callable, ..
                            } = &mut prog.expressions[id]
                            {
                                *resolved_callable = Some(func_id);
                            }
                        }
                    } else {
                        let err = FrontEndErrorKind::UndefinedIdentifier { name: func_name };
                        let default_span = name
                            .span
                            .unwrap_or_else(|| crate::util::Span::new(0, 0, "<unknown>"));
                        self.errors.push(CompilerError::new(err, default_span));
                    }
                }
            }
            Expression::Binary { op, .. } => {
                // Try to resolve the operator as a function
                if let Some(resolutions) = self.symbols.resolve(&op.name) {
                    if let IdentifierResolution::Function(func_id) = resolutions[0] {
                        if let Expression::Binary {
                            resolved_callable, ..
                        } = &mut prog.expressions[id]
                        {
                            *resolved_callable = Some(func_id);
                        }
                    }
                }
                // For binary operators, this might be a built-in operator, so we don't error
            }
            Expression::Unary { op, .. } => {
                // Try to resolve the operator as a function
                if let Some(resolutions) = self.symbols.resolve(&op.name) {
                    if let IdentifierResolution::Function(func_id) = resolutions[0] {
                        if let Expression::Unary {
                            resolved_callable, ..
                        } = &mut prog.expressions[id]
                        {
                            *resolved_callable = Some(func_id);
                        }
                    }
                }
                // For unary operators, this might be a built-in operator, so we don't error
            }
            Expression::Lambda { params, body, .. } => {
                // Enter a new scope for lambda parameters
                self.symbols.enter_scope();

                // Add lambda parameters to symbol table
                for param_id in params {
                    let param = &prog.params[*param_id];
                    self.symbols.define(
                        param.name.name.clone(),
                        IdentifierResolution::Param(*param_id),
                    );
                }

                // Visit the body
                self.visit_block(prog, *body)?;

                // Exit lambda scope
                self.symbols.exit_scope();

                // Return early since we handled the recursion manually
                return Ok(());
            }
            Expression::Literal { value, .. } => {
                // Handle RowLiteral which contains KeyValue structures
                if let crate::ast::expr::Literal::RowLiteral(key_values) = value {
                    // For RowLiteral, we need to process KeyValues specially
                    // Collect KeyValue data to avoid borrowing issues
                    let kv_data: Vec<(String, ExprId)> = key_values
                        .iter()
                        .map(|kv| (kv.key.name.clone(), kv.value))
                        .collect();

                    // Visit each value expression
                    for (_, value_id) in &kv_data {
                        self.visit_expr(prog, *value_id)?;
                    }

                    // KeyValue resolution would be handled by context (table operations)
                    return Ok(());
                }
            }
            _ => {
                // For other expressions, just use the default walker
            }
        }

        // Use the default walker for all other cases
        visit_mut::walk_expr_mut(self, prog, id)
    }

    fn visit_key_value(&mut self, prog: &mut Program, kv: &mut KeyValue) -> Result<(), ()> {
        // First visit the value expression
        self.visit_expr(prog, kv.value)?;

        // The KeyValue key might need resolution in table contexts
        // The resolved_table and resolved_field will be set by the containing expression
        // (like TableRowAccess or RowLiteral when used in table contexts)

        Ok(())
    }

    fn visit_table_decl(
        &mut self,
        prog: &mut Program,
        id: crate::ast::item::TableId,
    ) -> Result<(), ()> {
        // Visit table elements to resolve partition references
        let table = &prog.table_decls[id];
        let mut elements_to_visit = Vec::new();

        // Collect elements that need visiting to avoid borrowing issues
        for (idx, element) in table.elements.iter().enumerate() {
            match element {
                TableElement::Node(node) => {
                    elements_to_visit.push((idx, node.name.name.clone(), node.args.clone()));
                }
                TableElement::Invariant(expr_id) => {
                    elements_to_visit.push((idx, String::new(), vec![*expr_id]));
                }
                _ => {}
            }
        }

        // Now process the elements
        for (idx, name, args) in elements_to_visit {
            match &mut prog.table_decls[id].elements[idx] {
                TableElement::Node(node) => {
                    // Try to resolve the partition function
                    if let Some(resolutions) = self.symbols.resolve(&name) {
                        if let IdentifierResolution::Function(func_id) = resolutions[0] {
                            node.resolved_partition = Some(func_id);
                        }
                    } else {
                        let err = FrontEndErrorKind::UndefinedIdentifier { name: name.clone() };
                        let default_span = node
                            .name
                            .span
                            .unwrap_or_else(|| crate::util::Span::new(0, 0, "<unknown>"));
                        self.errors.push(CompilerError::new(err, default_span));
                    }

                    // Visit the arguments - these should resolve to table fields
                    for arg_id in &args {
                        // For expressions in table node arguments, check if they are valid field references
                        if let Expression::Identifier { name: arg_name, .. } =
                            &prog.expressions[*arg_id]
                        {
                            // Look for this identifier as a field in the current table
                            let table = &prog.table_decls[id];
                            let mut found_field = false;

                            for element in &table.elements {
                                if let TableElement::Field(field) = element {
                                    if field.name.name == arg_name.name {
                                        found_field = true;
                                        break;
                                    }
                                }
                            }

                            if !found_field {
                                // This identifier is not a field in this table
                                let err = FrontEndErrorKind::UndefinedIdentifier {
                                    name: arg_name.name.clone(),
                                };
                                let default_span = arg_name
                                    .span
                                    .unwrap_or_else(|| crate::util::Span::new(0, 0, "<unknown>"));
                                self.errors.push(CompilerError::new(err, default_span));
                            }
                        } else {
                            // For other expression types, use regular resolution
                            self.visit_expr(prog, *arg_id)?;
                        }
                    }
                }
                TableElement::Invariant(_) => {
                    // Visit the invariant expression
                    for arg_id in &args {
                        self.visit_expr(prog, *arg_id)?;
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}
