//! ast/visit.rs
//!
//! Defines the `Visitor` trait for read-only traversal of the AST.
//!
//! This pattern uses default methods, so you only need to implement `visit_*`
//! methods for the nodes you care about. The `walk_*` functions provide the
//! default recursive traversal logic.

use super::*;

#[allow(unused_variables)]
pub trait Visitor<'ast, R: Default = (), E = ()>: Sized {
    fn visit_program(&mut self, prog: &'ast Program) -> Result<R, E> {
        walk_program(self, prog)
    }

    // Items
    fn visit_item(&mut self, prog: &'ast Program, item: &'ast Item) -> Result<R, E> {
        walk_item(self, prog, item)
    }
    fn visit_callable_decl(
        &mut self,
        prog: &'ast Program,
        id: FunctionId,
        decl: &'ast CallableDecl,
    ) -> Result<R, E> {
        walk_callable_decl(self, prog, id, decl)
    }
    fn visit_type_decl(
        &mut self,
        prog: &'ast Program,
        id: TypeDeclId,
        decl: &'ast TypeDecl,
    ) -> Result<R, E> {
        Ok(R::default())
    }
    fn visit_const_decl(
        &mut self,
        prog: &'ast Program,
        id: ConstId,
        decl: &'ast ConstDecl,
    ) -> Result<R, E> {
        walk_const_decl(self, prog, id, decl)
    }
    fn visit_table_decl(
        &mut self,
        prog: &'ast Program,
        id: TableId,
        decl: &'ast TableDecl,
    ) -> Result<R, E> {
        walk_table_decl(self, prog, id, decl)
    }
    fn visit_var_decl(
        &mut self,
        prog: &'ast Program,
        id: VarId,
        decl: &'ast VarDecl,
    ) -> Result<R, E> {
        walk_var_decl(self, prog, id, decl)
    }
    fn visit_param(
        &mut self,
        prog: &'ast Program,
        id: ParamId,
        param: &'ast Parameter,
    ) -> Result<R, E> {
        walk_param(self, prog, id, param)
    }
    fn visit_generic_param(
        &mut self,
        prog: &'ast Program,
        id: GenericParamId,
        param: &'ast GenericParam,
    ) -> Result<R, E> {
        Ok(R::default())
    }

    // Statements
    fn visit_stmt(&mut self, prog: &'ast Program, id: StmtId) -> Result<R, E> {
        walk_stmt(self, prog, id)
    }
    fn visit_block(&mut self, prog: &'ast Program, id: BlockId) -> Result<R, E> {
        walk_block(self, prog, id)
    }

    // Expressions
    fn visit_expr(&mut self, prog: &'ast Program, id: ExprId) -> Result<R, E> {
        walk_expr(self, prog, id)
    }

    // Types
    fn visit_ast_type(&mut self, prog: &'ast Program, id: AstTypeId) -> Result<R, E> {
        walk_ast_type(self, prog, id)
    }
}

// Default walkers
pub fn walk_program<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
) -> Result<R, E> {
    for item in &prog.declarations {
        let _ = visitor.visit_item(prog, item)?;
    }
    Ok(R::default())
}

pub fn walk_item<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    item: &'ast Item,
) -> Result<R, E> {
    match item {
        Item::Callable(id) => {
            let _ = visitor.visit_callable_decl(prog, *id, &prog.functions[*id])?;
        }
        Item::Type(id) => {
            let _ = visitor.visit_type_decl(prog, *id, &prog.type_decls[*id])?;
        }
        Item::Const(id) => {
            let _ = visitor.visit_const_decl(prog, *id, &prog.const_decls[*id])?;
        }
        Item::Table(id) => {
            let _ = visitor.visit_table_decl(prog, *id, &prog.table_decls[*id])?;
        }
    }
    Ok(R::default())
}

pub fn walk_callable_decl<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    _id: FunctionId,
    decl: &'ast CallableDecl,
) -> Result<R, E> {
    for param_id in &decl.generic_params {
        let _ = visitor.visit_generic_param(prog, *param_id, &prog.generic_params[*param_id])?;
    }
    for param_id in &decl.params {
        let _ = visitor.visit_param(prog, *param_id, &prog.params[*param_id])?;
    }
    if let Some(type_id) = decl.return_type {
        let _ = visitor.visit_ast_type(prog, type_id)?;
    }
    for expr_id in &decl.assumptions {
        let _ = visitor.visit_expr(prog, *expr_id)?;
    }
    if let Some(block_id) = decl.body {
        let _ = visitor.visit_block(prog, block_id)?;
    }
    Ok(R::default())
}

pub fn walk_const_decl<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    _id: ConstId,
    decl: &'ast ConstDecl,
) -> Result<R, E> {
    let _ = visitor.visit_ast_type(prog, decl.ty)?;
    let _ = visitor.visit_expr(prog, decl.value)?;
    Ok(R::default())
}

pub fn walk_table_decl<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    _id: TableId,
    decl: &'ast TableDecl,
) -> Result<R, E> {
    for element in &decl.elements {
        match element {
            TableElement::Field(field) => {
                let _ = visitor.visit_ast_type(prog, field.ty)?;
            }
            TableElement::Node(node) => {
                for arg_id in &node.args {
                    let _ = visitor.visit_expr(prog, *arg_id)?;
                }
            }
            TableElement::Invariant(expr_id) => {
                let _ = visitor.visit_expr(prog, *expr_id)?;
            }
        }
    }
    Ok(R::default())
}

pub fn walk_var_decl<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    _id: VarId,
    decl: &'ast VarDecl,
) -> Result<R, E> {
    if let Some(type_id) = decl.ty {
        let _ = visitor.visit_ast_type(prog, type_id)?;
    }
    if let Some(expr_id) = decl.init {
        let _ = visitor.visit_expr(prog, expr_id)?;
    }
    Ok(R::default())
}

pub fn walk_param<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    _id: ParamId,
    param: &'ast Parameter,
) -> Result<R, E> {
    let _ = visitor.visit_ast_type(prog, param.ty)?;
    Ok(R::default())
}

pub fn walk_stmt<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    id: StmtId,
) -> Result<R, E> {
    match &prog.statements[id] {
        Statement::VarDecl(var_id) => {
            let _ = visitor.visit_var_decl(prog, *var_id, &prog.var_decls[*var_id])?;
        }
        Statement::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            let _ = visitor.visit_expr(prog, *condition)?;
            let _ = visitor.visit_block(prog, *then_block)?;
            if let Some(else_id) = else_block {
                let _ = visitor.visit_block(prog, *else_id)?;
            }
        }
        Statement::For {
            init,
            condition,
            update,
            body,
            ..
        } => {
            if let Some(init) = init {
                match init {
                    ForInit::VarDecl(id) => {
                        let _ = visitor.visit_var_decl(prog, *id, &prog.var_decls[*id])?;
                    }
                    ForInit::Expression(id) => {
                        let _ = visitor.visit_expr(prog, *id)?;
                    }
                }
            }
            if let Some(cond_id) = condition {
                let _ = visitor.visit_expr(prog, *cond_id)?;
            }
            if let Some(update_id) = update {
                let _ = visitor.visit_expr(prog, *update_id)?;
            }
            let _ = visitor.visit_block(prog, *body)?;
        }
        Statement::Return { value, .. } => {
            if let Some(expr_id) = value {
                let _ = visitor.visit_expr(prog, *expr_id)?;
            }
        }
        Statement::Assert { expr, .. } => {
            let _ = visitor.visit_expr(prog, *expr)?;
        }
        Statement::Hop { body, .. } => {
            let _ = visitor.visit_block(prog, *body)?;
        }
        Statement::HopsFor {
            var,
            start,
            end,
            body,
            ..
        } => {
            let _ = visitor.visit_var_decl(prog, *var, &prog.var_decls[*var])?;
            let _ = visitor.visit_expr(prog, *start)?;
            let _ = visitor.visit_expr(prog, *end)?;
            let _ = visitor.visit_block(prog, *body)?;
        }
        Statement::Expression { expr, .. } => {
            let _ = visitor.visit_expr(prog, *expr)?;
        }
        Statement::Block(block_id) => {
            let _ = visitor.visit_block(prog, *block_id)?;
        }
    }
    Ok(R::default())
}

pub fn walk_block<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    id: BlockId,
) -> Result<R, E> {
    for stmt_id in &prog.blocks[id].statements {
        let _ = visitor.visit_stmt(prog, *stmt_id)?;
    }
    Ok(R::default())
}

pub fn walk_expr<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    id: ExprId,
) -> Result<R, E> {
    match &prog.expressions[id] {
        Expression::Literal { value, .. } => {
            if let Literal::List(items) = value {
                for item_id in items {
                    let _ = visitor.visit_expr(prog, *item_id)?;
                }
            }
        }
        Expression::Identifier { .. } => {}
        Expression::Binary { left, right, .. } => {
            let _ = visitor.visit_expr(prog, *left)?;
            let _ = visitor.visit_expr(prog, *right)?;
        }
        Expression::Unary { expr, .. } => {
            let _ = visitor.visit_expr(prog, *expr)?;
        }
        Expression::Assignment { lhs, rhs, .. } => {
            let _ = visitor.visit_expr(prog, *lhs)?;
            let _ = visitor.visit_expr(prog, *rhs)?;
        }
        Expression::Call { callee, args, .. } => {
            let _ = visitor.visit_expr(prog, *callee)?;
            for arg_id in args {
                let _ = visitor.visit_expr(prog, *arg_id)?;
            }
        }
        Expression::MemberAccess { object, .. } => {
            let _ = visitor.visit_expr(prog, *object)?;
        }
        Expression::TableRowAccess {
            table, key_values, ..
        } => {
            let _ = visitor.visit_expr(prog, *table)?;
            for kv in key_values {
                let _ = visitor.visit_expr(prog, kv.value)?;
            }
        }
        Expression::Grouped { expr, .. } => {
            let _ = visitor.visit_expr(prog, *expr)?;
        }
        Expression::Lambda {
            params,
            return_type,
            body,
            ..
        } => {
            for param_id in params {
                let _ = visitor.visit_param(prog, *param_id, &prog.params[*param_id])?;
            }
            let _ = visitor.visit_ast_type(prog, *return_type)?;
            let _ = visitor.visit_block(prog, *body)?;
        }
    }
    Ok(R::default())
}

pub fn walk_ast_type<'ast, R: Default, E, V: Visitor<'ast, R, E>>(
    visitor: &mut V,
    prog: &'ast Program,
    id: AstTypeId,
) -> Result<R, E> {
    match &prog.types[id] {
        AstType::Named { .. } => {}
        AstType::Generic { args, .. } => {
            for arg_id in args {
                let _ = visitor.visit_ast_type(prog, *arg_id)?;
            }
        }
        AstType::Function {
            params,
            return_type,
            ..
        } => {
            for param_id in params {
                let _ = visitor.visit_ast_type(prog, *param_id)?;
            }
            let _ = visitor.visit_ast_type(prog, *return_type)?;
        }
    }
    Ok(R::default())
}
