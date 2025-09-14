//! src/frontend/type_resolver.rs
//!
//! Implements the type resolution and checking phase of the compiler.
//! This visitor traverses the AST and resolves the types of expressions,
//! statements, and declarations. It also handles overload resolution for
//! functions and operators.

use crate::ast::visit_mut::VisitorMut;
use crate::ast::*;
use crate::frontend::errors::FrontEndErrorKind;
use crate::util::{CompilerError, Span};
// use std::collections::HashMap;

pub fn resolve_types(program: &mut Program) -> Result<(), Vec<CompilerError>> {
    let mut resolver = TypeResolver::new();
    resolver.visit_program(program).unwrap(); // Assuming no errors from visitor for now

    if resolver.errors.is_empty() {
        Ok(())
    } else {
        Err(resolver.errors)
    }
}

struct TypeResolver {
    errors: Vec<CompilerError>,
    type_vars: u32,
    substitution: Substitution,
    // A map from `AstTypeId` to `ResolvedType` to avoid re-resolving.
    // resolved_ast_types: HashMap<AstTypeId, ResolvedType>,
}

impl TypeResolver {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            type_vars: 0,
            substitution: Substitution::new(),
            // resolved_ast_types: HashMap::new(),
        }
    }

    fn new_type_var(&mut self) -> ResolvedType {
        let var_id = self.type_vars;
        self.type_vars += 1;
        ResolvedType::TypeVariable {
            var_id,
            name: format!("T{}", var_id),
        }
    }

    fn unify(&mut self, t1: &ResolvedType, t2: &ResolvedType, span: Span) {
        let t1 = self.substitution.apply(t1);
        let t2 = self.substitution.apply(t2);

        match (t1.clone(), t2.clone()) {
            (ResolvedType::TypeVariable { var_id, .. }, t)
            | (t, ResolvedType::TypeVariable { var_id, .. }) => {
                if let ResolvedType::TypeVariable {
                    var_id: other_id, ..
                } = t
                {
                    if var_id == other_id {
                        return;
                    }
                }
                self.substitution.extend(var_id, t);
            }
            (
                ResolvedType::Primitive {
                    type_id: id1,
                    type_args: args1,
                },
                ResolvedType::Primitive {
                    type_id: id2,
                    type_args: args2,
                },
            ) if id1 == id2 && args1.len() == args2.len() => {
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2, span);
                }
            }
            (
                ResolvedType::Function {
                    param_types: params1,
                    return_type: ret1,
                },
                ResolvedType::Function {
                    param_types: params2,
                    return_type: ret2,
                },
            ) if params1.len() == params2.len() => {
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2, span);
                }
                self.unify(&ret1, &ret2, span);
            }
            (ResolvedType::List { element_type: e1 }, ResolvedType::List { element_type: e2 }) => {
                self.unify(&e1, &e2, span);
            }
            (ResolvedType::Void, ResolvedType::Void) => {}
            (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => {}
            (ResolvedType::Unresolved(_), _) | (_, ResolvedType::Unresolved(_)) => {
                // Should be resolved before unifying
            }
            (t1, t2) if t1 == t2 => {}
            (t1, t2) => {
                self.errors.push(CompilerError::new(
                    FrontEndErrorKind::TypeMismatch {
                        expected: format!("{:?}", t1),
                        found: format!("{:?}", t2),
                    },
                    span,
                ));
            }
        }
    }
}

impl<'ast> VisitorMut<'ast> for TypeResolver {
    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<(), ()> {
        let mut expr = prog.expressions[id].clone();
        let span = match &expr {
            Expression::Literal { span, .. } => span,
            Expression::Identifier { span, .. } => span,
            Expression::Binary { span, .. } => span,
            Expression::Unary { span, .. } => span,
            Expression::Assignment { span, .. } => span,
            Expression::Call { span, .. } => span,
            Expression::MemberAccess { span, .. } => span,
            Expression::TableRowAccess { span, .. } => span,
            Expression::Grouped { span, .. } => span,
            Expression::Lambda { span, .. } => span,
        }
        .unwrap_or_else(|| Span::new(0, 0, "unknown"));

        let new_type = match &mut expr {
            Expression::Literal {
                value,
                resolved_type,
                ..
            } => {
                let literal_type = match value {
                    Literal::Integer(_) => {
                        ResolvedType::Unresolved(prog.types.alloc(AstType::Named {
                            name: Identifier {
                                name: "int".to_string(),
                                span: None,
                            },
                            resolved_type: None,
                        }))
                    }
                    Literal::Float(_) => {
                        ResolvedType::Unresolved(prog.types.alloc(AstType::Named {
                            name: Identifier {
                                name: "float".to_string(),
                                span: None,
                            },
                            resolved_type: None,
                        }))
                    }
                    Literal::String(_) => {
                        ResolvedType::Unresolved(prog.types.alloc(AstType::Named {
                            name: Identifier {
                                name: "string".to_string(),
                                span: None,
                            },
                            resolved_type: None,
                        }))
                    }
                    Literal::Bool(_) => {
                        ResolvedType::Unresolved(prog.types.alloc(AstType::Named {
                            name: Identifier {
                                name: "bool".to_string(),
                                span: None,
                            },
                            resolved_type: None,
                        }))
                    }
                    Literal::List(items) => {
                        let element_type = self.new_type_var();
                        for item_id in items {
                            self.visit_expr(prog, *item_id)?;
                            let item_expr = &prog.expressions[*item_id];
                            if let Some(item_type) = item_expr.resolved_type() {
                                self.unify(&element_type, item_type, span);
                            }
                        }
                        ResolvedType::List {
                            element_type: Box::new(element_type),
                        }
                    }
                    Literal::RowLiteral(_) => self.new_type_var(),
                };
                *resolved_type = Some(literal_type.clone());
                literal_type
            }
            Expression::Identifier {
                resolved_declarations,
                resolved_type,
                ..
            } => {
                if resolved_declarations.len() == 1 {
                    let decl = resolved_declarations[0];
                    let ty = match decl {
                        IdentifierResolution::Var(id) => prog.var_decls[id].resolved_type.clone(),
                        IdentifierResolution::Param(id) => prog.params[id].resolved_type.clone(),
                        IdentifierResolution::Const(id) => {
                            prog.const_decls[id].resolved_type.clone()
                        }
                        _ => Some(self.new_type_var()),
                    };
                    let ty = ty.unwrap_or_else(|| self.new_type_var());
                    *resolved_type = Some(ty.clone());
                    ty
                } else {
                    let ty = self.new_type_var();
                    *resolved_type = Some(ty.clone());
                    ty
                }
            }
            Expression::Binary {
                left,
                right,
                resolved_callables,
                resolved_type,
                ..
            } => {
                self.visit_expr(prog, *left)?;
                self.visit_expr(prog, *right)?;

                let left_type = prog.expressions[*left].resolved_type().unwrap();
                let right_type = prog.expressions[*right].resolved_type().unwrap();

                let mut matching_overloads = Vec::new();
                for func_id in resolved_callables.iter() {
                    let func = &prog.functions[*func_id];
                    if func.params.len() == 2 {
                        let param1_type =
                            prog.params[func.params[0]].resolved_type.as_ref().unwrap();
                        let param2_type =
                            prog.params[func.params[1]].resolved_type.as_ref().unwrap();
                        if self.try_unify(left_type, param1_type)
                            && self.try_unify(right_type, param2_type)
                        {
                            matching_overloads.push(*func_id);
                        }
                    }
                }

                if matching_overloads.len() == 1 {
                    let func_id = matching_overloads[0];
                    *resolved_callables = vec![func_id];
                    let return_type = prog.functions[func_id]
                        .resolved_return_type
                        .clone()
                        .unwrap();
                    *resolved_type = Some(return_type.clone());
                    return_type
                } else {
                    self.errors.push(CompilerError::new(
                        FrontEndErrorKind::InvalidOperation {
                            op: "binary".to_string(),
                            details: "Cannot resolve overload".to_string(),
                        },
                        span,
                    ));
                    ResolvedType::Unknown
                }
            }
            Expression::Call {
                callee,
                args,
                resolved_type,
                ..
            } => {
                self.visit_expr(prog, *callee)?;
                let callee_type = prog.expressions[*callee].resolved_type().unwrap().clone();

                let mut arg_types = Vec::new();
                for arg_id in args.iter() {
                    self.visit_expr(prog, *arg_id)?;
                    arg_types.push(prog.expressions[*arg_id].resolved_type().unwrap().clone());
                }

                let return_type = self.new_type_var();
                let func_type = ResolvedType::Function {
                    param_types: arg_types,
                    return_type: Box::new(return_type.clone()),
                };

                self.unify(&callee_type, &func_type, span);
                *resolved_type = Some(return_type.clone());
                return_type
            }
            // other expressions...
            _ => self.new_type_var(),
        };
        prog.expressions[id].set_resolved_type(new_type);

        Ok(())
    }
}

trait ExpressionExt {
    fn resolved_type(&self) -> Option<&ResolvedType>;
    fn set_resolved_type(&mut self, ty: ResolvedType);
}

impl ExpressionExt for Expression {
    fn resolved_type(&self) -> Option<&ResolvedType> {
        match self {
            Expression::Literal { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Identifier { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Binary { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Unary { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Assignment { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Call { resolved_type, .. } => resolved_type.as_ref(),
            Expression::MemberAccess { resolved_type, .. } => resolved_type.as_ref(),
            Expression::TableRowAccess { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Grouped { resolved_type, .. } => resolved_type.as_ref(),
            Expression::Lambda { resolved_type, .. } => resolved_type.as_ref(),
        }
    }

    fn set_resolved_type(&mut self, ty: ResolvedType) {
        match self {
            Expression::Literal { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Identifier { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Binary { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Unary { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Assignment { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Call { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::MemberAccess { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::TableRowAccess { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Grouped { resolved_type, .. } => *resolved_type = Some(ty),
            Expression::Lambda { resolved_type, .. } => *resolved_type = Some(ty),
        }
    }
}

impl TypeResolver {
    fn try_unify(&self, t1: &ResolvedType, t2: &ResolvedType) -> bool {
        let t1 = self.substitution.apply(t1);
        let t2 = self.substitution.apply(t2);

        match (t1, t2) {
            (ResolvedType::TypeVariable { .. }, _) | (_, ResolvedType::TypeVariable { .. }) => true,
            (
                ResolvedType::Primitive { type_id: id1, .. },
                ResolvedType::Primitive { type_id: id2, .. },
            ) => id1 == id2,
            (
                ResolvedType::Function {
                    param_types: params1,
                    ..
                },
                ResolvedType::Function {
                    param_types: params2,
                    ..
                },
            ) => params1.len() == params2.len(),
            (ResolvedType::List { .. }, ResolvedType::List { .. }) => true,
            (t1, t2) => t1 == t2,
        }
    }
}
