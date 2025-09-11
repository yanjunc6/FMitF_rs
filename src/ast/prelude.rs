use crate::ast::ast_builder::AstBuilder;
use crate::ast::*;
use id_arena::Arena;

// ============================================================================
// --- Default Program
// ============================================================================

impl Default for Program {
    fn default() -> Self {
        // Create empty program
        Program {
            declarations: Vec::new(),
            functions: Arena::new(),
            type_decls: Arena::new(),
            const_decls: Arena::new(),
            table_decls: Arena::new(),
            var_decls: Arena::new(),
            params: Arena::new(),
            generic_params: Arena::new(),
            types: Arena::new(),
            statements: Arena::new(),
            expressions: Arena::new(),
            blocks: Arena::new(),
        }
    }
}

impl Program {
    /// Create a new program with built-in prelude loaded
    pub fn with_prelude() -> Result<Self, Box<dyn std::error::Error>> {
        // Read the prelude.transact file
        let prelude_content = include_str!("prelude.transact");

        // Parse the prelude directly into a fresh program
        let mut program = AstBuilder::parse_raw(prelude_content)
            .map_err(|errs| format!("Failed to parse prelude: {:?}", errs))?;

        // Clear spans from all prelude elements
        clear_all_spans(&mut program);

        Ok(program)
    }
}

fn clear_all_spans(program: &mut Program) {
    // Clear spans from all declarations
    for declaration in &program.declarations {
        match declaration {
            Declaration::Callable(func_id) => {
                if let Some(func) = program.functions.get_mut(*func_id) {
                    clear_callable_spans(func);
                }
            }
            Declaration::Type(type_id) => {
                if let Some(type_decl) = program.type_decls.get_mut(*type_id) {
                    clear_type_decl_spans(type_decl);
                }
            }
            Declaration::Const(const_id) => {
                if let Some(const_decl) = program.const_decls.get_mut(*const_id) {
                    clear_const_decl_spans(const_decl);
                }
            }
            Declaration::Table(table_id) => {
                if let Some(table_decl) = program.table_decls.get_mut(*table_id) {
                    clear_table_decl_spans(table_decl);
                }
            }
        }
    }

    // Clear spans from all other items in arenas
    for (_, var_decl) in program.var_decls.iter_mut() {
        clear_var_decl_spans(var_decl);
    }

    for (_, param) in program.params.iter_mut() {
        clear_param_spans(param);
    }

    for (_, generic_param) in program.generic_params.iter_mut() {
        clear_generic_param_spans(generic_param);
    }

    for (_, type_) in program.types.iter_mut() {
        clear_type_spans(type_);
    }

    for (_, statement) in program.statements.iter_mut() {
        clear_statement_spans(statement);
    }

    for (_, expression) in program.expressions.iter_mut() {
        clear_expression_spans(expression);
    }

    for (_, block) in program.blocks.iter_mut() {
        clear_block_spans(block);
    }
}

fn clear_callable_spans(func: &mut CallableDecl) {
    func.span = None;
    for decorator in &mut func.decorators {
        clear_decorator_spans(decorator);
    }
    clear_callable_name_spans(&mut func.name);
}

fn clear_type_decl_spans(type_decl: &mut TypeDecl) {
    type_decl.span = None;
    for decorator in &mut type_decl.decorators {
        clear_decorator_spans(decorator);
    }
    clear_identifier_spans(&mut type_decl.name);
}

fn clear_const_decl_spans(const_decl: &mut ConstDecl) {
    const_decl.span = None;
    clear_identifier_spans(&mut const_decl.name);
}

fn clear_table_decl_spans(table_decl: &mut TableDecl) {
    table_decl.span = None;
    clear_identifier_spans(&mut table_decl.name);
}

fn clear_var_decl_spans(var_decl: &mut VarDecl) {
    var_decl.span = None;
    clear_identifier_spans(&mut var_decl.name);
}

fn clear_param_spans(param: &mut Parameter) {
    param.span = None;
    clear_identifier_spans(&mut param.name);
}

fn clear_generic_param_spans(generic_param: &mut GenericParam) {
    generic_param.span = None;
    clear_identifier_spans(&mut generic_param.name);
}

fn clear_type_spans(type_: &mut Type) {
    match type_ {
        Type::Named { name: id, .. } => {
            clear_identifier_spans(id);
        }
        Type::Generic {
            base,
            args: _,
            span,
            ..
        } => {
            *span = None;
            clear_identifier_spans(base);
            // Args will be handled recursively
        }
        Type::Function {
            params: _,
            return_type: _,
            span,
        } => {
            *span = None;
            // Params and return type will be handled recursively
        }
    }
}

fn clear_statement_spans(statement: &mut Statement) {
    match statement {
        Statement::VarDecl(_) => {
            // VarDecl spans handled separately
        }
        Statement::If { span, .. } => {
            *span = None;
        }
        Statement::For { span, .. } => {
            *span = None;
        }
        Statement::Return { span, .. } => {
            *span = None;
        }
        Statement::Assert { span, .. } => {
            *span = None;
        }
        Statement::Hop {
            decorators, span, ..
        } => {
            *span = None;
            for decorator in decorators {
                clear_decorator_spans(decorator);
            }
        }
        Statement::HopsFor {
            decorators, span, ..
        } => {
            *span = None;
            for decorator in decorators {
                clear_decorator_spans(decorator);
            }
        }
        Statement::Expression { span, .. } => {
            *span = None;
        }
        Statement::Block(_) => {
            // Block spans handled separately
        }
    }
}

fn clear_expression_spans(expression: &mut Expression) {
    match expression {
        Expression::Literal { span, .. } => {
            *span = None;
        }
        Expression::Identifier { .. } => {
            // Identifier spans handled separately
        }
        Expression::Binary { span, .. } => {
            *span = None;
        }
        Expression::Unary { span, .. } => {
            *span = None;
        }
        Expression::Assignment { span, .. } => {
            *span = None;
        }
        Expression::Call { span, .. } => {
            *span = None;
        }
        Expression::MemberAccess { span, .. } => {
            *span = None;
        }
        Expression::TableRowAccess { span, .. } => {
            *span = None;
        }
        Expression::Lambda { span, .. } => {
            *span = None;
        }
        Expression::Grouped { span, .. } => {
            *span = None;
        }
    }
}

fn clear_block_spans(block: &mut Block) {
    block.span = None;
}

fn clear_decorator_spans(decorator: &mut Decorator) {
    decorator.span = None;
    clear_identifier_spans(&mut decorator.name);
}

fn clear_identifier_spans(identifier: &mut Identifier) {
    identifier.span = None;
}

fn clear_callable_name_spans(name: &mut CallableName) {
    match name {
        CallableName::Identifier(id) => {
            clear_identifier_spans(id);
        }
        CallableName::Operator(op) => {
            op.span = None;
        }
    }
}
