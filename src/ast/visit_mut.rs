//! ast/visit_mut.rs
//!
//! Defines the `VisitorMut` trait for mutable traversal of the AST.
//! This allows for in-place modifications of AST nodes.

use super::*;

#[allow(unused_variables)]
pub trait VisitorMut: Sized {
    fn visit_program(&mut self, prog: &mut Program) {
        walk_program_mut(self, prog);
    }

    // Items
    fn visit_item(&mut self, prog: &mut Program, item: Item) {
        walk_item_mut(self, prog, item);
    }
    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) {
        walk_callable_decl_mut(self, prog, id);
    }
    fn visit_type_decl(&mut self, _prog: &mut Program, _id: TypeDeclId) {}
    fn visit_const_decl(&mut self, prog: &mut Program, id: ConstId) {
        walk_const_decl_mut(self, prog, id);
    }
    fn visit_table_decl(&mut self, prog: &mut Program, id: TableId) {
        walk_table_decl_mut(self, prog, id);
    }
    fn visit_var_decl(&mut self, prog: &mut Program, id: VarId) {
        walk_var_decl_mut(self, prog, id);
    }
    fn visit_param(&mut self, prog: &mut Program, id: ParamId) {
        walk_param_mut(self, prog, id);
    }
    fn visit_generic_param(&mut self, _prog: &mut Program, _id: GenericParamId) {}

    // Statements
    fn visit_stmt(&mut self, prog: &mut Program, id: StmtId) {
        walk_stmt_mut(self, prog, id);
    }
    fn visit_block(&mut self, prog: &mut Program, id: BlockId) {
        walk_block_mut(self, prog, id);
    }

    // Expressions
    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) {
        walk_expr_mut(self, prog, id);
    }

    // Types
    fn visit_ast_type(&mut self, prog: &mut Program, id: AstTypeId) {
        walk_ast_type_mut(self, prog, id);
    }
}

// Walkers for mutable visitor
// The `clone()` calls are important to avoid borrowing `prog` mutably multiple times.
// We clone the IDs or nodes, recurse, and then use the IDs to access potentially modified data.

pub fn walk_program_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program) {
    for item in prog.declarations.clone() {
        visitor.visit_item(prog, item);
    }
}

pub fn walk_item_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, item: Item) {
    match item {
        Item::Callable(id) => visitor.visit_callable_decl(prog, id),
        Item::Type(id) => visitor.visit_type_decl(prog, id),
        Item::Const(id) => visitor.visit_const_decl(prog, id),
        Item::Table(id) => visitor.visit_table_decl(prog, id),
    }
}

pub fn walk_callable_decl_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: FunctionId) {
    let decl = prog.functions[id].clone(); // Clone to work around borrow checker
    
    for param_id in &decl.generic_params {
        visitor.visit_generic_param(prog, *param_id);
    }
    for param_id in &decl.params {
        visitor.visit_param(prog, *param_id);
    }
    if let Some(type_id) = decl.return_type {
        visitor.visit_ast_type(prog, type_id);
    }
    for expr_id in &decl.assumptions {
        visitor.visit_expr(prog, *expr_id);
    }
    if let Some(body_id) = decl.body {
        visitor.visit_block(prog, body_id);
    }
}

pub fn walk_const_decl_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: ConstId) {
    let decl = prog.const_decls[id].clone();
    visitor.visit_ast_type(prog, decl.ty);
    visitor.visit_expr(prog, decl.value);
}

pub fn walk_table_decl_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: TableId) {
    let decl = prog.table_decls[id].clone();
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

pub fn walk_var_decl_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: VarId) {
    let decl = prog.var_decls[id].clone();
    if let Some(type_id) = decl.ty {
        visitor.visit_ast_type(prog, type_id);
    }
    if let Some(expr_id) = decl.init {
        visitor.visit_expr(prog, expr_id);
    }
}

pub fn walk_param_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: ParamId) {
    let param = prog.params[id].clone();
    visitor.visit_ast_type(prog, param.ty);
}

pub fn walk_stmt_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: StmtId) {
    let stmt = prog.statements[id].clone();
    match stmt {
        Statement::VarDecl(var_id) => visitor.visit_var_decl(prog, var_id),
        Statement::If { condition, then_block, else_block, .. } => {
            visitor.visit_expr(prog, condition);
            visitor.visit_block(prog, then_block);
            if let Some(else_id) = else_block {
                visitor.visit_block(prog, else_id);
            }
        }
        Statement::For { init, condition, update, body, .. } => {
            if let Some(init) = init {
                match init {
                    ForInit::VarDecl(id) => visitor.visit_var_decl(prog, id),
                    ForInit::Expression(id) => visitor.visit_expr(prog, id),
                }
            }
            if let Some(cond_id) = condition {
                visitor.visit_expr(prog, cond_id);
            }
            if let Some(update_id) = update {
                visitor.visit_expr(prog, update_id);
            }
            visitor.visit_block(prog, body);
        }
        Statement::Return { value, .. } => {
            if let Some(expr_id) = value {
                visitor.visit_expr(prog, expr_id);
            }
        }
        Statement::Assert { expr, .. } => visitor.visit_expr(prog, expr),
        Statement::Hop { body, .. } => visitor.visit_block(prog, body),
        Statement::HopsFor { var, start, end, body, .. } => {
            visitor.visit_var_decl(prog, var);
            visitor.visit_expr(prog, start);
            visitor.visit_expr(prog, end);
            visitor.visit_block(prog, body);
        }
        Statement::Expression { expr, .. } => visitor.visit_expr(prog, expr),
        Statement::Block(block_id) => visitor.visit_block(prog, block_id),
    }
}

pub fn walk_block_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: BlockId) {
    let block = prog.blocks[id].clone();
    for stmt_id in block.statements {
        visitor.visit_stmt(prog, stmt_id);
    }
}

pub fn walk_expr_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: ExprId) {
    let expr = prog.expressions[id].clone();
    match expr {
        Expression::Literal { value, .. } => {
            if let Literal::List(items) = value {
                for item_id in items {
                    visitor.visit_expr(prog, item_id);
                }
            }
        }
        Expression::Identifier { .. } => {}
        Expression::Binary { left, right, .. } => {
            visitor.visit_expr(prog, left);
            visitor.visit_expr(prog, right);
        }
        Expression::Unary { expr, .. } => visitor.visit_expr(prog, expr),
        Expression::Assignment { lhs, rhs, .. } => {
            visitor.visit_expr(prog, lhs);
            visitor.visit_expr(prog, rhs);
        }
        Expression::Call { callee, args, .. } => {
            visitor.visit_expr(prog, callee);
            for arg_id in args {
                visitor.visit_expr(prog, arg_id);
            }
        }
        Expression::MemberAccess { object, .. } => visitor.visit_expr(prog, object),
        Expression::TableRowAccess { table, key_values, .. } => {
            visitor.visit_expr(prog, table);
            for kv in key_values {
                visitor.visit_expr(prog, kv.value);
            }
        }
        Expression::Grouped { expr, .. } => visitor.visit_expr(prog, expr),
        Expression::Lambda { params, return_type, body, .. } => {
            for param_id in params {
                visitor.visit_param(prog, param_id);
            }
            visitor.visit_ast_type(prog, return_type);
            visitor.visit_block(prog, body);
        }
    }
}

pub fn walk_ast_type_mut<V: VisitorMut>(visitor: &mut V, prog: &mut Program, id: AstTypeId) {
    let ast_type = prog.types[id].clone();
    match ast_type {
        AstType::Named { .. } => {}
        AstType::Generic { args, .. } => {
            for arg_id in args {
                visitor.visit_ast_type(prog, arg_id);
            }
        }
        AstType::Function { params, return_type, .. } => {
            for param_id in params {
                visitor.visit_ast_type(prog, param_id);
            }
            visitor.visit_ast_type(prog, return_type);
        }
    }
}