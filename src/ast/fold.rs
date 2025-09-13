//! ast/fold.rs
//!
//! Defines the `Fold` trait for transforming the AST.
//! This allows for creating new AST nodes by transforming existing ones.
//!
//! The fold pattern is useful for AST transformations where you want to
//! create a new AST structure based on an existing one, potentially
//! modifying some nodes while keeping others unchanged.

use super::*;

#[allow(unused_variables)]
pub trait Fold: Sized {
    fn fold_program(&mut self, prog: Program) -> Program {
        foldwalk_program(self, prog)
    }

    // Items
    fn fold_item(&mut self, item: Item) -> Item {
        foldwalk_item(self, item)
    }
    fn fold_callable_decl(
        &mut self,
        prog: &Program,
        id: FunctionId,
        decl: CallableDecl,
    ) -> CallableDecl {
        foldwalk_callable_decl(self, prog, id, decl)
    }
    fn fold_type_decl(&mut self, prog: &Program, id: TypeDeclId, decl: TypeDecl) -> TypeDecl {
        decl
    }
    fn fold_const_decl(&mut self, prog: &Program, id: ConstId, decl: ConstDecl) -> ConstDecl {
        foldwalk_const_decl(self, prog, id, decl)
    }
    fn fold_table_decl(&mut self, prog: &Program, id: TableId, decl: TableDecl) -> TableDecl {
        foldwalk_table_decl(self, prog, id, decl)
    }
    fn fold_var_decl(&mut self, prog: &Program, id: VarId, decl: VarDecl) -> VarDecl {
        foldwalk_var_decl(self, prog, id, decl)
    }
    fn fold_param(&mut self, prog: &Program, id: ParamId, param: Parameter) -> Parameter {
        foldwalk_param(self, prog, id, param)
    }
    fn fold_generic_param(
        &mut self,
        prog: &Program,
        id: GenericParamId,
        param: GenericParam,
    ) -> GenericParam {
        param
    }

    // Statements
    fn fold_stmt(&mut self, prog: &Program, id: StmtId, stmt: Statement) -> Statement {
        foldwalk_stmt(self, prog, id, stmt)
    }
    fn fold_block(&mut self, prog: &Program, id: BlockId, block: Block) -> Block {
        foldwalk_block(self, prog, id, block)
    }

    // Expressions
    fn fold_expr(&mut self, prog: &Program, id: ExprId, expr: Expression) -> Expression {
        foldwalk_expr(self, prog, id, expr)
    }

    // Types
    fn fold_ast_type(&mut self, prog: &Program, id: AstTypeId, ast_type: AstType) -> AstType {
        foldwalk_ast_type(self, prog, id, ast_type)
    }
}

// Default fold implementations
pub fn foldwalk_program<F: Fold>(folder: &mut F, prog: Program) -> Program {
    // Note: In a real implementation, you would need to carefully handle the arenas
    // and potentially create new ones. This is a simplified version.
    for item in &prog.declarations.clone() {
        folder.fold_item(item.clone());
    }
    prog
}

pub fn foldwalk_item<F: Fold>(_folder: &mut F, item: Item) -> Item {
    item // In a full implementation, you would transform the item based on its type
}

pub fn foldwalk_callable_decl<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: FunctionId,
    decl: CallableDecl,
) -> CallableDecl {
    // Transform generic parameters
    for param_id in &decl.generic_params.clone() {
        let param = prog.generic_params[*param_id].clone();
        folder.fold_generic_param(prog, *param_id, param);
    }

    // Transform parameters
    for param_id in &decl.params.clone() {
        let param = prog.params[*param_id].clone();
        folder.fold_param(prog, *param_id, param);
    }

    // Transform return type
    if let Some(type_id) = decl.return_type {
        let ast_type = prog.types[type_id].clone();
        folder.fold_ast_type(prog, type_id, ast_type);
    }

    // Transform assumptions
    for expr_id in &decl.assumptions.clone() {
        let expr = prog.expressions[*expr_id].clone();
        folder.fold_expr(prog, *expr_id, expr);
    }

    // Transform body
    if let Some(block_id) = decl.body {
        let block = prog.blocks[block_id].clone();
        folder.fold_block(prog, block_id, block);
    }

    decl
}

pub fn foldwalk_const_decl<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: ConstId,
    decl: ConstDecl,
) -> ConstDecl {
    let ast_type = prog.types[decl.ty].clone();
    folder.fold_ast_type(prog, decl.ty, ast_type);

    let expr = prog.expressions[decl.value].clone();
    folder.fold_expr(prog, decl.value, expr);

    decl
}

pub fn foldwalk_table_decl<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: TableId,
    decl: TableDecl,
) -> TableDecl {
    for element in &decl.elements.clone() {
        match element {
            TableElement::Field(field) => {
                let ast_type = prog.types[field.ty].clone();
                folder.fold_ast_type(prog, field.ty, ast_type);
            }
            TableElement::Node(node) => {
                for arg_id in &node.args {
                    let expr = prog.expressions[*arg_id].clone();
                    folder.fold_expr(prog, *arg_id, expr);
                }
            }
            TableElement::Invariant(expr_id) => {
                let expr = prog.expressions[*expr_id].clone();
                folder.fold_expr(prog, *expr_id, expr);
            }
        }
    }
    decl
}

pub fn foldwalk_var_decl<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: VarId,
    decl: VarDecl,
) -> VarDecl {
    if let Some(type_id) = decl.ty {
        let ast_type = prog.types[type_id].clone();
        folder.fold_ast_type(prog, type_id, ast_type);
    }

    if let Some(expr_id) = decl.init {
        let expr = prog.expressions[expr_id].clone();
        folder.fold_expr(prog, expr_id, expr);
    }

    decl
}

pub fn foldwalk_param<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: ParamId,
    param: Parameter,
) -> Parameter {
    let ast_type = prog.types[param.ty].clone();
    folder.fold_ast_type(prog, param.ty, ast_type);
    param
}

pub fn foldwalk_stmt<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: StmtId,
    stmt: Statement,
) -> Statement {
    match stmt {
        Statement::VarDecl(var_id) => {
            let decl = prog.var_decls[var_id].clone();
            folder.fold_var_decl(prog, var_id, decl);
            stmt
        }
        Statement::If {
            condition,
            then_block,
            else_block,
            span,
        } => {
            let cond_expr = prog.expressions[condition].clone();
            folder.fold_expr(prog, condition, cond_expr);

            let then_block_data = prog.blocks[then_block].clone();
            folder.fold_block(prog, then_block, then_block_data);

            if let Some(else_id) = else_block {
                let else_block_data = prog.blocks[else_id].clone();
                folder.fold_block(prog, else_id, else_block_data);
            }

            Statement::If {
                condition,
                then_block,
                else_block,
                span,
            }
        }
        Statement::For {
            init,
            condition,
            update,
            body,
            span,
        } => {
            if let Some(init) = &init {
                match init {
                    ForInit::VarDecl(id) => {
                        let decl = prog.var_decls[*id].clone();
                        folder.fold_var_decl(prog, *id, decl);
                    }
                    ForInit::Expression(id) => {
                        let expr = prog.expressions[*id].clone();
                        folder.fold_expr(prog, *id, expr);
                    }
                }
            }

            if let Some(cond_id) = condition {
                let expr = prog.expressions[cond_id].clone();
                folder.fold_expr(prog, cond_id, expr);
            }

            if let Some(update_id) = update {
                let expr = prog.expressions[update_id].clone();
                folder.fold_expr(prog, update_id, expr);
            }

            let body_block = prog.blocks[body].clone();
            folder.fold_block(prog, body, body_block);

            Statement::For {
                init,
                condition,
                update,
                body,
                span,
            }
        }
        Statement::Return { value, span } => {
            if let Some(expr_id) = value {
                let expr = prog.expressions[expr_id].clone();
                folder.fold_expr(prog, expr_id, expr);
            }
            Statement::Return { value, span }
        }
        Statement::Assert { expr, span } => {
            let expression = prog.expressions[expr].clone();
            folder.fold_expr(prog, expr, expression);
            Statement::Assert { expr, span }
        }
        Statement::Hop {
            decorators,
            body,
            span,
        } => {
            let body_block = prog.blocks[body].clone();
            folder.fold_block(prog, body, body_block);
            Statement::Hop {
                decorators,
                body,
                span,
            }
        }
        Statement::HopsFor {
            decorators,
            var,
            start,
            end,
            body,
            span,
        } => {
            let var_decl = prog.var_decls[var].clone();
            folder.fold_var_decl(prog, var, var_decl);

            let start_expr = prog.expressions[start].clone();
            folder.fold_expr(prog, start, start_expr);

            let end_expr = prog.expressions[end].clone();
            folder.fold_expr(prog, end, end_expr);

            let body_block = prog.blocks[body].clone();
            folder.fold_block(prog, body, body_block);

            Statement::HopsFor {
                decorators,
                var,
                start,
                end,
                body,
                span,
            }
        }
        Statement::Expression { expr, span } => {
            let expression = prog.expressions[expr].clone();
            folder.fold_expr(prog, expr, expression);
            Statement::Expression { expr, span }
        }
        Statement::Block(block_id) => {
            let block = prog.blocks[block_id].clone();
            folder.fold_block(prog, block_id, block);
            Statement::Block(block_id)
        }
    }
}

pub fn foldwalk_block<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: BlockId,
    block: Block,
) -> Block {
    for stmt_id in &block.statements.clone() {
        let stmt = prog.statements[*stmt_id].clone();
        folder.fold_stmt(prog, *stmt_id, stmt);
    }
    block
}

pub fn foldwalk_expr<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: ExprId,
    expr: Expression,
) -> Expression {
    match expr {
        Expression::Literal {
            value,
            resolved_type,
            span,
        } => {
            match &value {
                Literal::List(items) => {
                    for item_id in items {
                        let item_expr = prog.expressions[*item_id].clone();
                        folder.fold_expr(prog, *item_id, item_expr);
                    }
                }
                Literal::RowLiteral(key_values) => {
                    for kv in key_values {
                        let value_expr = prog.expressions[kv.value].clone();
                        folder.fold_expr(prog, kv.value, value_expr);
                    }
                }
                _ => {} // Other literals don't contain expressions
            }
            Expression::Literal {
                value,
                resolved_type,
                span,
            }
        }
        Expression::Identifier {
            name,
            resolved_declaration,
            resolved_type,
            span,
        } => Expression::Identifier {
            name,
            resolved_declaration,
            resolved_type,
            span,
        },
        Expression::Binary {
            left,
            op,
            right,
            resolved_callable,
            resolved_type,
            span,
        } => {
            let left_expr = prog.expressions[left].clone();
            folder.fold_expr(prog, left, left_expr);

            let right_expr = prog.expressions[right].clone();
            folder.fold_expr(prog, right, right_expr);

            Expression::Binary {
                left,
                op,
                right,
                resolved_callable,
                resolved_type,
                span,
            }
        }
        Expression::Unary {
            op,
            expr,
            resolved_callable,
            resolved_type,
            span,
        } => {
            let expression = prog.expressions[expr].clone();
            folder.fold_expr(prog, expr, expression);
            Expression::Unary {
                op,
                expr,
                resolved_callable,
                resolved_type,
                span,
            }
        }
        Expression::Assignment {
            lhs,
            rhs,
            resolved_type,
            span,
        } => {
            let lhs_expr = prog.expressions[lhs].clone();
            folder.fold_expr(prog, lhs, lhs_expr);

            let rhs_expr = prog.expressions[rhs].clone();
            folder.fold_expr(prog, rhs, rhs_expr);

            Expression::Assignment {
                lhs,
                rhs,
                resolved_type,
                span,
            }
        }
        Expression::Call {
            callee,
            args,
            resolved_callable,
            resolved_type,
            span,
        } => {
            let callee_expr = prog.expressions[callee].clone();
            folder.fold_expr(prog, callee, callee_expr);

            for arg_id in &args {
                let arg_expr = prog.expressions[*arg_id].clone();
                folder.fold_expr(prog, *arg_id, arg_expr);
            }

            Expression::Call {
                callee,
                args,
                resolved_callable,
                resolved_type,
                span,
            }
        }
        Expression::MemberAccess {
            object,
            member,
            resolved_table,
            resolved_field,
            resolved_type,
            span,
        } => {
            let obj_expr = prog.expressions[object].clone();
            folder.fold_expr(prog, object, obj_expr);
            Expression::MemberAccess {
                object,
                member,
                resolved_table,
                resolved_field,
                resolved_type,
                span,
            }
        }
        Expression::TableRowAccess {
            table,
            key_values,
            resolved_table,
            resolved_type,
            span,
        } => {
            let table_expr = prog.expressions[table].clone();
            folder.fold_expr(prog, table, table_expr);

            for kv in &key_values {
                let value_expr = prog.expressions[kv.value].clone();
                folder.fold_expr(prog, kv.value, value_expr);
            }

            Expression::TableRowAccess {
                table,
                key_values,
                resolved_table,
                resolved_type,
                span,
            }
        }
        Expression::Grouped {
            expr,
            resolved_type,
            span,
        } => {
            let expression = prog.expressions[expr].clone();
            folder.fold_expr(prog, expr, expression);
            Expression::Grouped {
                expr,
                resolved_type,
                span,
            }
        }
        Expression::Lambda {
            params,
            return_type,
            body,
            resolved_type,
            span,
        } => {
            for param_id in &params {
                let param = prog.params[*param_id].clone();
                folder.fold_param(prog, *param_id, param);
            }

            let ret_type = prog.types[return_type].clone();
            folder.fold_ast_type(prog, return_type, ret_type);

            let body_block = prog.blocks[body].clone();
            folder.fold_block(prog, body, body_block);

            Expression::Lambda {
                params,
                return_type,
                body,
                resolved_type,
                span,
            }
        }
    }
}

pub fn foldwalk_ast_type<F: Fold>(
    folder: &mut F,
    prog: &Program,
    _id: AstTypeId,
    ast_type: AstType,
) -> AstType {
    match ast_type {
        AstType::Named {
            name,
            resolved_type,
        } => AstType::Named {
            name,
            resolved_type,
        },
        AstType::Generic {
            base,
            args,
            resolved_base_type,
            span,
        } => {
            for arg_id in &args {
                let arg_type = prog.types[*arg_id].clone();
                folder.fold_ast_type(prog, *arg_id, arg_type);
            }
            AstType::Generic {
                base,
                args,
                resolved_base_type,
                span,
            }
        }
        AstType::Function {
            params,
            return_type,
            span,
        } => {
            for param_id in &params {
                let param_type = prog.types[*param_id].clone();
                folder.fold_ast_type(prog, *param_id, param_type);
            }

            let ret_type = prog.types[return_type].clone();
            folder.fold_ast_type(prog, return_type, ret_type);

            AstType::Function {
                params,
                return_type,
                span,
            }
        }
    }
}
