//! ast/visit_mut.rs
//!
//! Defines the `VisitorMut` trait for mutable traversal of the AST.
//! This allows for in-place modifications of AST nodes.

use super::*;

#[allow(unused_variables)]
pub trait VisitorMut<'ast, R: Default = (), E = ()>: Sized {
    fn visit_program(&mut self, prog: &mut Program) -> Result<R, E> {
        walk_program_mut(self, prog)
    }

    // Items
    fn visit_item(&mut self, prog: &mut Program, item: Item) -> Result<R, E> {
        walk_item_mut(self, prog, item)
    }
    fn visit_callable_decl(&mut self, prog: &mut Program, id: FunctionId) -> Result<R, E> {
        walk_callable_decl_mut(self, prog, id)
    }
    fn visit_type_decl(&mut self, _prog: &mut Program, _id: TypeDeclId) -> Result<R, E> {
        Ok(R::default())
    }
    fn visit_const_decl(&mut self, prog: &mut Program, id: ConstId) -> Result<R, E> {
        walk_const_decl_mut(self, prog, id)
    }
    fn visit_table_decl(&mut self, prog: &mut Program, id: TableId) -> Result<R, E> {
        walk_table_decl_mut(self, prog, id)
    }
    fn visit_var_decl(&mut self, prog: &mut Program, id: VarId) -> Result<R, E> {
        walk_var_decl_mut(self, prog, id)
    }
    fn visit_param(&mut self, prog: &mut Program, id: ParamId) -> Result<R, E> {
        walk_param_mut(self, prog, id)
    }
    fn visit_generic_param(&mut self, _prog: &mut Program, _id: GenericParamId) -> Result<R, E> {
        Ok(R::default())
    }

    // Statements
    fn visit_stmt(&mut self, prog: &mut Program, id: StmtId) -> Result<R, E> {
        walk_stmt_mut(self, prog, id)
    }
    fn visit_block(&mut self, prog: &mut Program, id: BlockId) -> Result<R, E> {
        walk_block_mut(self, prog, id)
    }

    // Expressions
    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<R, E> {
        walk_expr_mut(self, prog, id)
    }

    // KeyValue (used in TableRowAccess and RowLiteral)
    // This method is called for KeyValue resolution, not for recursive traversal
    fn visit_key_value(&mut self, prog: &mut Program, kv: &mut KeyValue) -> Result<R, E> {
        walk_key_value_mut(self, prog, kv)
    }

    // Types
    fn visit_ast_type(&mut self, prog: &mut Program, id: AstTypeId) -> Result<R, E> {
        walk_ast_type_mut(self, prog, id)
    }
}

// Walkers for mutable visitor
// The `clone()` calls are important to avoid borrowing `prog` mutably multiple times.
// We clone the IDs or nodes, recurse, and then use the IDs to access potentially modified data.

pub fn walk_program_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
) -> Result<R, E> {
    for item in prog.declarations.clone() {
        let _ = visitor.visit_item(prog, item)?;
    }
    Ok(R::default())
}

pub fn walk_item_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    item: Item,
) -> Result<R, E> {
    match item {
        Item::Callable(id) => {
            let _ = visitor.visit_callable_decl(prog, id)?;
        }
        Item::Type(id) => {
            let _ = visitor.visit_type_decl(prog, id)?;
        }
        Item::Const(id) => {
            let _ = visitor.visit_const_decl(prog, id)?;
        }
        Item::Table(id) => {
            let _ = visitor.visit_table_decl(prog, id)?;
        }
    }
    Ok(R::default())
}

pub fn walk_callable_decl_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: FunctionId,
) -> Result<R, E> {
    let decl = prog.functions[id].clone(); // Clone to work around borrow checker

    for param_id in &decl.generic_params {
        let _ = visitor.visit_generic_param(prog, *param_id)?;
    }
    for param_id in &decl.params {
        let _ = visitor.visit_param(prog, *param_id)?;
    }
    if let Some(type_id) = decl.return_type {
        let _ = visitor.visit_ast_type(prog, type_id)?;
    }
    for expr_id in &decl.assumptions {
        let _ = visitor.visit_expr(prog, *expr_id)?;
    }
    if let Some(body_id) = decl.body {
        let _ = visitor.visit_block(prog, body_id)?;
    }
    Ok(R::default())
}

pub fn walk_const_decl_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: ConstId,
) -> Result<R, E> {
    let decl = prog.const_decls[id].clone();
    let _ = visitor.visit_ast_type(prog, decl.ty)?;
    let _ = visitor.visit_expr(prog, decl.value)?;
    Ok(R::default())
}

pub fn walk_table_decl_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: TableId,
) -> Result<R, E> {
    let decl = prog.table_decls[id].clone();
    for element in &decl.elements {
        match element {
            TableElement::Field(field_id) => {
                let field = &prog.fields[*field_id];
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

pub fn walk_var_decl_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: VarId,
) -> Result<R, E> {
    let decl = prog.var_decls[id].clone();
    if let Some(type_id) = decl.ty {
        let _ = visitor.visit_ast_type(prog, type_id)?;
    }
    if let Some(expr_id) = decl.init {
        let _ = visitor.visit_expr(prog, expr_id)?;
    }
    Ok(R::default())
}

pub fn walk_param_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: ParamId,
) -> Result<R, E> {
    let param = prog.params[id].clone();
    let _ = visitor.visit_ast_type(prog, param.ty)?;
    Ok(R::default())
}

pub fn walk_stmt_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: StmtId,
) -> Result<R, E> {
    let stmt = prog.statements[id].clone();
    match stmt {
        Statement::VarDecl(var_id) => {
            let _ = visitor.visit_var_decl(prog, var_id)?;
        }
        Statement::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            let _ = visitor.visit_expr(prog, condition)?;
            let _ = visitor.visit_block(prog, then_block)?;
            if let Some(else_id) = else_block {
                let _ = visitor.visit_block(prog, else_id)?;
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
                        let _ = visitor.visit_var_decl(prog, id)?;
                    }
                    ForInit::Expression(id) => {
                        let _ = visitor.visit_expr(prog, id)?;
                    }
                }
            }
            if let Some(cond_id) = condition {
                let _ = visitor.visit_expr(prog, cond_id)?;
            }
            if let Some(update_id) = update {
                let _ = visitor.visit_expr(prog, update_id)?;
            }
            let _ = visitor.visit_block(prog, body)?;
        }
        Statement::Return { value, .. } => {
            if let Some(expr_id) = value {
                let _ = visitor.visit_expr(prog, expr_id)?;
            }
        }
        Statement::Assert { expr, .. } => {
            let _ = visitor.visit_expr(prog, expr)?;
        }
        Statement::Hop { body, .. } => {
            let _ = visitor.visit_block(prog, body)?;
        }
        Statement::HopsFor {
            var,
            start,
            end,
            body,
            ..
        } => {
            let _ = visitor.visit_var_decl(prog, var)?;
            let _ = visitor.visit_expr(prog, start)?;
            let _ = visitor.visit_expr(prog, end)?;
            let _ = visitor.visit_block(prog, body)?;
        }
        Statement::Expression { expr, .. } => {
            let _ = visitor.visit_expr(prog, expr)?;
        }
        Statement::Block(block_id) => {
            let _ = visitor.visit_block(prog, block_id)?;
        }
    }
    Ok(R::default())
}

pub fn walk_block_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: BlockId,
) -> Result<R, E> {
    let block = prog.blocks[id].clone();
    for stmt_id in block.statements {
        let _ = visitor.visit_stmt(prog, stmt_id)?;
    }
    Ok(R::default())
}

pub fn walk_expr_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: ExprId,
) -> Result<R, E> {
    let expr = prog.expressions[id].clone();
    match expr {
        Expression::Literal { value, .. } => {
            match value {
                Literal::List(items) => {
                    for item_id in items {
                        let _ = visitor.visit_expr(prog, item_id)?;
                    }
                }
                Literal::RowLiteral(_) => {
                    // For RowLiteral, collect the value IDs first to avoid borrowing issues
                    let value_ids: Vec<ExprId> = if let Expression::Literal {
                        value: Literal::RowLiteral(key_values),
                        ..
                    } = &prog.expressions[id]
                    {
                        key_values.iter().map(|kv| kv.value).collect()
                    } else {
                        Vec::new()
                    };

                    // Visit each value expression
                    for value_id in value_ids {
                        let _ = visitor.visit_expr(prog, value_id)?;
                    }
                }
                _ => {} // Other literals don't contain expressions
            }
        }
        Expression::Identifier { .. } => {}
        Expression::Binary { left, right, .. } => {
            let _ = visitor.visit_expr(prog, left)?;
            let _ = visitor.visit_expr(prog, right)?;
        }
        Expression::Unary { expr, .. } => {
            let _ = visitor.visit_expr(prog, expr)?;
        }
        Expression::Assignment { lhs, rhs, .. } => {
            let _ = visitor.visit_expr(prog, lhs)?;
            let _ = visitor.visit_expr(prog, rhs)?;
        }
        Expression::Call { callee, args, .. } => {
            let _ = visitor.visit_expr(prog, callee)?;
            for arg_id in args {
                let _ = visitor.visit_expr(prog, arg_id)?;
            }
        }
        Expression::MemberAccess { object, .. } => {
            let _ = visitor.visit_expr(prog, object)?;
        }
        Expression::TableRowAccess {
            table, key_values, ..
        } => {
            let _ = visitor.visit_expr(prog, table)?;
            // Collect value IDs first to avoid borrowing issues
            let value_ids: Vec<ExprId> = key_values.iter().map(|kv| kv.value).collect();
            for value_id in value_ids {
                let _ = visitor.visit_expr(prog, value_id)?;
            }
        }
        Expression::Grouped { expr, .. } => {
            let _ = visitor.visit_expr(prog, expr)?;
        }
        Expression::Lambda {
            params,
            return_type,
            body,
            ..
        } => {
            for param_id in params {
                let _ = visitor.visit_param(prog, param_id)?;
            }
            let _ = visitor.visit_ast_type(prog, return_type)?;
            let _ = visitor.visit_block(prog, body)?;
        }
    }
    Ok(R::default())
}

pub fn walk_ast_type_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    id: AstTypeId,
) -> Result<R, E> {
    let ast_type = prog.types[id].clone();
    match ast_type {
        AstType::Named { .. } => {}
        AstType::Generic { args, .. } => {
            for arg_id in args {
                let _ = visitor.visit_ast_type(prog, arg_id)?;
            }
        }
        AstType::Function {
            params,
            return_type,
            ..
        } => {
            for param_id in params {
                let _ = visitor.visit_ast_type(prog, param_id)?;
            }
            let _ = visitor.visit_ast_type(prog, return_type)?;
        }
    }
    Ok(R::default())
}

pub fn walk_key_value_mut<'ast, R: Default, E, V: VisitorMut<'ast, R, E>>(
    visitor: &mut V,
    prog: &mut Program,
    kv: &mut KeyValue,
) -> Result<R, E> {
    let _ = visitor.visit_expr(prog, kv.value)?;
    Ok(R::default())
}
