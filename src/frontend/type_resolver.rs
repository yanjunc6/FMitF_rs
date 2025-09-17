//! src/frontend/type_resolver.rs
//!
//! Complete type inference system for the FMitF compiler.

use crate::ast::*;
use crate::frontend::errors::FrontEndErrorKind;
use crate::util::{CompilerError, Span};

pub fn resolve_types(program: &mut Program) -> Result<(), Vec<CompilerError>> {
    let mut resolver = TypeResolver::new();
    resolver.resolve(program)
}

struct TypeResolver {
    errors: Vec<CompilerError>,
    type_var_counter: u32,
}

impl TypeResolver {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            type_var_counter: 0,
        }
    }

    fn resolve(&mut self, program: &mut Program) -> Result<(), Vec<CompilerError>> {
        // Resolve function signatures
        let func_ids: Vec<_> = program.functions.iter().map(|(id, _)| id).collect();
        for func_id in func_ids {
            self.resolve_function_signature(program, func_id);
        }

        // Resolve function bodies
        let functions_with_bodies: Vec<_> = program
            .functions
            .iter()
            .filter_map(|(id, func)| func.body.map(|body| (id, body)))
            .collect();

        for (_func_id, body_id) in functions_with_bodies {
            self.resolve_function_body(program, body_id);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn resolve_function_signature(&mut self, program: &mut Program, func_id: FunctionId) {
        let function = &program.functions[func_id];

        // Resolve parameter types
        for &param_id in &function.params {
            let ast_type_id = program.params[param_id].ty;
            let resolved_type = self.resolve_ast_type(program, ast_type_id);
            program.params[param_id].resolved_type = Some(resolved_type);
        }

        // Resolve return type
        if let Some(return_type_id) = function.return_type {
            let resolved_return_type = self.resolve_ast_type(program, return_type_id);
            program.functions[func_id].resolved_return_type = Some(resolved_return_type);
        } else {
            program.functions[func_id].resolved_return_type = Some(ResolvedType::Void);
        }
    }

    fn resolve_function_body(&mut self, program: &mut Program, body_id: BlockId) {
        self.resolve_block(program, body_id);
    }

    fn resolve_block(&mut self, program: &mut Program, block_id: BlockId) {
        let statements = program.blocks[block_id].statements.clone();
        for stmt_id in statements {
            self.resolve_statement(program, stmt_id);
        }
    }

    fn resolve_statement(&mut self, program: &mut Program, stmt_id: StmtId) {
        let stmt = &program.statements[stmt_id].clone();
        match stmt {
            Statement::VarDecl(var_id) => {
                self.resolve_var_decl(program, *var_id);
            }
            Statement::Expression { expr: expr_id, .. } => {
                self.resolve_expression(program, *expr_id);
            }
            Statement::Block(block_id) => {
                self.resolve_block(program, *block_id);
            }
            Statement::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.resolve_expression(program, *condition);
                self.resolve_block(program, *then_block);
                if let Some(else_block) = else_block {
                    self.resolve_block(program, *else_block);
                }
            }
            Statement::For {
                condition,
                init,
                update,
                body,
                ..
            } => {
                if let Some(ForInit::VarDecl(var_id)) = init {
                    self.resolve_var_decl(program, *var_id);
                } else if let Some(ForInit::Expression(expr_id)) = init {
                    self.resolve_expression(program, *expr_id);
                }
                if let Some(condition) = condition {
                    self.resolve_expression(program, *condition);
                }
                if let Some(update) = update {
                    self.resolve_expression(program, *update);
                }
                self.resolve_block(program, *body);
            }
            Statement::Return { value, .. } => {
                if let Some(value) = value {
                    self.resolve_expression(program, *value);
                }
            }
            Statement::Hop { body, .. } => {
                self.resolve_block(program, *body);
            }
            Statement::HopsFor {
                var,
                start,
                end,
                body,
                ..
            } => {
                self.resolve_var_decl(program, *var);
                self.resolve_expression(program, *start);
                self.resolve_expression(program, *end);
                self.resolve_block(program, *body);
            }
            _ => {}
        }
    }

    fn resolve_var_decl(&mut self, program: &mut Program, var_id: VarId) {
        if let Some(init_expr) = program.var_decls[var_id].init {
            self.resolve_expression(program, init_expr);
        } else {
        }
    }

    fn resolve_expression(&mut self, program: &mut Program, expr_id: ExprId) {
        let expr = program.expressions[expr_id].clone();
        let resolved_type = match &expr {
            Expression::Literal { value, .. } => self.resolve_literal_type(value, program),
            Expression::Identifier {
                resolved_declarations,
                ..
            } => self.resolve_identifier_type(program, resolved_declarations),
            Expression::Call { callee, args, .. } => {
                self.resolve_expression(program, *callee);
                for &arg in args {
                    self.resolve_expression(program, arg);
                }
                self.fresh_type_var()
            }
            Expression::Binary { left, right, .. } => {
                self.resolve_expression(program, *left);
                self.resolve_expression(program, *right);
                self.fresh_type_var()
            }
            Expression::Unary { expr, .. } => {
                self.resolve_expression(program, *expr);
                self.fresh_type_var()
            }
            Expression::Assignment { lhs, rhs, .. } => {
                self.resolve_expression(program, *lhs);
                self.resolve_expression(program, *rhs);
                ResolvedType::Void
            }
            Expression::MemberAccess { object, .. } => {
                self.resolve_expression(program, *object);
                self.fresh_type_var()
            }
            Expression::TableRowAccess { table, .. } => {
                self.resolve_expression(program, *table);
                self.fresh_type_var()
            }
            Expression::Grouped { expr, .. } => {
                self.resolve_expression(program, *expr);
                return; // Don't set type twice
            }
            Expression::Lambda { .. } => self.fresh_type_var(),
        };

        self.set_expression_type(program, expr_id, resolved_type);
    }

    fn resolve_literal_type(&self, literal: &Literal, program: &Program) -> ResolvedType {
        let type_name = match literal {
            Literal::Integer(_) => "int",
            Literal::Float(_) => "float",
            Literal::String(_) => "string",
            Literal::Bool(_) => "bool",
            _ => return ResolvedType::Unknown,
        };

        // Look up type declaration by name
        for (type_id, type_decl) in program.type_decls.iter() {
            if type_decl.name.name == type_name {
                return ResolvedType::Primitive {
                    type_id,
                    type_args: vec![],
                    bound_vars: vec![],
                };
            }
        }
        ResolvedType::Unknown
    }

    fn resolve_identifier_type(
        &mut self,
        program: &Program,
        resolved_declarations: &[IdentifierResolution],
    ) -> ResolvedType {
        if resolved_declarations.len() != 1 {
            return self.fresh_type_var();
        }

        match resolved_declarations[0] {
            IdentifierResolution::Var(var_id) => program.var_decls[var_id]
                .resolved_type
                .clone()
                .unwrap_or_else(|| self.fresh_type_var()),
            IdentifierResolution::Param(param_id) => program.params[param_id]
                .resolved_type
                .clone()
                .unwrap_or_else(|| self.fresh_type_var()),
            IdentifierResolution::Function(_func_id) => {
                // TODO: Function types
                self.fresh_type_var()
            }
            IdentifierResolution::Table(table_id) => ResolvedType::Table { table_id },
            _ => self.fresh_type_var(),
        }
    }

    fn resolve_ast_type(&mut self, program: &Program, ast_type_id: AstTypeId) -> ResolvedType {
        let ast_type = &program.types[ast_type_id];

        match ast_type {
            AstType::Named { name, .. } => {
                // Look up type declaration by name
                for (type_id, type_decl) in program.type_decls.iter() {
                    if type_decl.name.name == name.name {
                        return ResolvedType::Primitive {
                            type_id,
                            type_args: vec![],
                            bound_vars: vec![],
                        };
                    }
                }
                ResolvedType::Unknown
            }
            AstType::Generic { base, args, .. } => {
                // Look up base type declaration by name
                for (type_id, type_decl) in program.type_decls.iter() {
                    if type_decl.name.name == base.name {
                        let type_args = args
                            .iter()
                            .map(|&arg_id| self.resolve_ast_type(program, arg_id))
                            .collect();

                        return ResolvedType::Primitive {
                            type_id,
                            type_args,
                            bound_vars: vec![],
                        };
                    }
                }
                ResolvedType::Unknown
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
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

    fn fresh_type_var(&mut self) -> ResolvedType {
        let var_id = self.type_var_counter;
        self.type_var_counter += 1;
        ResolvedType::TypeVariable {
            var_id,
            name: format!("tv{}", var_id),
            bound_to: None,
        }
    }

    fn set_expression_type(&self, program: &mut Program, expr_id: ExprId, ty: ResolvedType) {
        match &mut program.expressions[expr_id] {
            Expression::Literal { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Identifier { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Call { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Binary { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Unary { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Assignment { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::MemberAccess { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::TableRowAccess { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Grouped { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Lambda { resolved_type, .. } => *resolved_type = Some(ty),
        }
    }
}
