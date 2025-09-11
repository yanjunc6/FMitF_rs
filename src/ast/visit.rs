//! ast/visit.rs
//!
//! Defines the `Visitor` trait for read-only traversal of the AST.
//!
//! This pattern uses default methods, so you only need to implement `visit_*`
//! methods for the nodes you care about. The `walk_*` functions provide the
//! default recursive traversal logic.

use super::*;

#[allow(unused_variables)]
pub trait Visitor<'ast>: Sized {
    fn visit_program(&mut self, prog: &'ast Program) {
        walk_program(self, prog);
    }

    // Items
    fn visit_item(&mut self, prog: &'ast Program, item: &'ast Item) {
        walk_item(self, prog, item);
    }
    fn visit_callable_decl(&mut self, prog: &'ast Program, id: FunctionId, decl: &'ast CallableDecl) {
        walk_callable_decl(self, prog, id, decl);
    }
    fn visit_type_decl(&mut self, prog: &'ast Program, id: TypeDeclId, decl: &'ast TypeDecl) {}
    fn visit_const_decl(&mut self, prog: &'ast Program, id: ConstId, decl: &'ast ConstDecl) {
        walk_const_decl(self, prog, id, decl);
    }
    fn visit_table_decl(&mut self, prog: &'ast Program, id: TableId, decl: &'ast TableDecl) {
        walk_table_decl(self, prog, id, decl);
    }
    fn visit_var_decl(&mut self, prog: &'ast Program, id: VarId, decl: &'ast VarDecl) {
        walk_var_decl(self, prog, id, decl);
    }
    fn visit_param(&mut self, prog: &'ast Program, id: ParamId, param: &'ast Parameter) {
        walk_param(self, prog, id, param);
    }
    fn visit_generic_param(&mut self, prog: &'ast Program, id: GenericParamId, param: &'ast GenericParam) {}

    // Statements
    fn visit_stmt(&mut self, prog: &'ast Program, id: StmtId) {
        walk_stmt(self, prog, id);
    }
    fn visit_block(&mut self, prog: &'ast Program, id: BlockId) {
        walk_block(self, prog, id);
    }

    // Expressions
    fn visit_expr(&mut self, prog: &'ast Program, id: ExprId) {
        walk_expr(self, prog, id);
    }

    // Types
    fn visit_ast_type(&mut self, prog: &'ast Program, id: AstTypeId) {
        walk_ast_type(self, prog, id);
    }
}

// Default walkers
pub fn walk_program<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program) {
    for item in &prog.declarations {
        visitor.visit_item(prog, item);
    }
}

pub fn walk_item<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, item: &'ast Item) {
    match item {
        Item::Callable(id) => visitor.visit_callable_decl(prog, *id, &prog.functions[*id]),
        Item::Type(id) => visitor.visit_type_decl(prog, *id, &prog.type_decls[*id]),
        Item::Const(id) => visitor.visit_const_decl(prog, *id, &prog.const_decls[*id]),
        Item::Table(id) => visitor.visit_table_decl(prog, *id, &prog.table_decls[*id]),
    }
}

pub fn walk_callable_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, _id: FunctionId, decl: &'ast CallableDecl) {
    for param_id in &decl.generic_params {
        visitor.visit_generic_param(prog, *param_id, &prog.generic_params[*param_id]);
    }
    for param_id in &decl.params {
        visitor.visit_param(prog, *param_id, &prog.params[*param_id]);
    }
    if let Some(type_id) = decl.return_type {
        visitor.visit_ast_type(prog, type_id);
    }
    for expr_id in &decl.assumptions {
        visitor.visit_expr(prog, *expr_id);
    }
    if let Some(block_id) = decl.body {
        visitor.visit_block(prog, block_id);
    }
}

pub fn walk_const_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, _id: ConstId, decl: &'ast ConstDecl) {
    visitor.visit_ast_type(prog, decl.ty);
    visitor.visit_expr(prog, decl.value);
}

pub fn walk_table_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, _id: TableId, decl: &'ast TableDecl) {
    for element in &decl.elements {
        match element {
            TableElement::Field(field) => visitor.visit_ast_type(prog, field.ty),
            TableElement::Node(node) => {
                for arg_id in &node.args {
                    visitor.visit_expr(prog, *arg_id);
                }
            }
            TableElement::Invariant(expr_id) => visitor.visit_expr(prog, *expr_id),
        }
    }
}

pub fn walk_var_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, _id: VarId, decl: &'ast VarDecl) {
    if let Some(type_id) = decl.ty {
        visitor.visit_ast_type(prog, type_id);
    }
    if let Some(expr_id) = decl.init {
        visitor.visit_expr(prog, expr_id);
    }
}

pub fn walk_param<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, _id: ParamId, param: &'ast Parameter) {
    visitor.visit_ast_type(prog, param.ty);
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, id: StmtId) {
    match &prog.statements[id] {
        Statement::VarDecl(var_id) => visitor.visit_var_decl(prog, *var_id, &prog.var_decls[*var_id]),
        Statement::If { condition, then_block, else_block, .. } => {
            visitor.visit_expr(prog, *condition);
            visitor.visit_block(prog, *then_block);
            if let Some(else_id) = else_block {
                visitor.visit_block(prog, *else_id);
            }
        }
        Statement::For { init, condition, update, body, .. } => {
            if let Some(init) = init {
                match init {
                    ForInit::VarDecl(id) => visitor.visit_var_decl(prog, *id, &prog.var_decls[*id]),
                    ForInit::Expression(id) => visitor.visit_expr(prog, *id),
                }
            }
            if let Some(cond_id) = condition {
                visitor.visit_expr(prog, *cond_id);
            }
            if let Some(update_id) = update {
                visitor.visit_expr(prog, *update_id);
            }
            visitor.visit_block(prog, *body);
        }
        Statement::Return { value, .. } => {
            if let Some(expr_id) = value {
                visitor.visit_expr(prog, *expr_id);
            }
        }
        Statement::Assert { expr, .. } => visitor.visit_expr(prog, *expr),
        Statement::Hop { body, .. } => visitor.visit_block(prog, *body),
        Statement::HopsFor { var, start, end, body, .. } => {
            visitor.visit_var_decl(prog, *var, &prog.var_decls[*var]);
            visitor.visit_expr(prog, *start);
            visitor.visit_expr(prog, *end);
            visitor.visit_block(prog, *body);
        }
        Statement::Expression { expr, .. } => visitor.visit_expr(prog, *expr),
        Statement::Block(block_id) => visitor.visit_block(prog, *block_id),
    }
}

pub fn walk_block<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, id: BlockId) {
    for stmt_id in &prog.blocks[id].statements {
        visitor.visit_stmt(prog, *stmt_id);
    }
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, id: ExprId) {
    match &prog.expressions[id] {
        Expression::Literal { value, .. } => {
            if let Literal::List(items) = value {
                for item_id in items {
                    visitor.visit_expr(prog, *item_id);
                }
            }
        }
        Expression::Identifier { .. } => {}
        Expression::Binary { left, right, .. } => {
            visitor.visit_expr(prog, *left);
            visitor.visit_expr(prog, *right);
        }
        Expression::Unary { expr, .. } => visitor.visit_expr(prog, *expr),
        Expression::Assignment { lhs, rhs, .. } => {
            visitor.visit_expr(prog, *lhs);
            visitor.visit_expr(prog, *rhs);
        }
        Expression::Call { callee, args, .. } => {
            visitor.visit_expr(prog, *callee);
            for arg_id in args {
                visitor.visit_expr(prog, *arg_id);
            }
        }
        Expression::MemberAccess { object, .. } => visitor.visit_expr(prog, *object),
        Expression::TableRowAccess { table, key_values, .. } => {
            visitor.visit_expr(prog, *table);
            for kv in key_values {
                visitor.visit_expr(prog, kv.value);
            }
        }
        Expression::Grouped { expr, .. } => visitor.visit_expr(prog, *expr),
        Expression::Lambda { params, return_type, body, .. } => {
            for param_id in params {
                visitor.visit_param(prog, *param_id, &prog.params[*param_id]);
            }
            visitor.visit_ast_type(prog, *return_type);
            visitor.visit_block(prog, *body);
        }
    }
}

pub fn walk_ast_type<'ast, V: Visitor<'ast>>(visitor: &mut V, prog: &'ast Program, id: AstTypeId) {
    match &prog.types[id] {
        AstType::Named { .. } => {}
        AstType::Generic { args, .. } => {
            for arg_id in args {
                visitor.visit_ast_type(prog, *arg_id);
            }
        }
        AstType::Function { params, return_type, .. } => {
            for param_id in params {
                visitor.visit_ast_type(prog, *param_id);
            }
            visitor.visit_ast_type(prog, *return_type);
        }
    }
}