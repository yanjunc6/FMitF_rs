//! Type Checking
//!
//! This module handles type checking for the AST.

use crate::ast::*;
use crate::ast::errors::{AstError, AstErrorKind};
use std::collections::HashMap;

/// Check types in the AST
pub fn check_types(program: &mut Program) -> Result<(), Vec<AstError>> {
    let mut errors = Vec::new();

    // Basic type checking - ensure all function return types are valid
    for (_func_id, func) in &program.functions {
        if let Some(return_type_id) = func.return_type {
            if program.types.get(return_type_id).is_none() {
                errors.push(AstError::new(AstErrorKind::TypeMismatch {
                    expected: "valid type".to_string(),
                    found: "unknown type".to_string(),
                }));
            }
        }
        
        // Check parameter types
        for param_id in &func.params {
            if let Some(param) = program.params.get(*param_id) {
                if program.types.get(param.ty).is_none() {
                    errors.push(AstError::new(AstErrorKind::TypeMismatch {
                        expected: "valid type".to_string(),
                        found: "unknown parameter type".to_string(),
                    }));
                }
            }
        }
    }

    // Check type declarations are valid
    for (_type_id, type_decl) in &program.type_decls {
        if type_decl.name.value.is_empty() {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "type declaration".to_string(),
                details: "Type name cannot be empty".to_string(),
            }));
        }
    }

    // Check expressions have valid types
    for (_expr_id, expr) in &program.expressions {
        check_expression_types(expr, program, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Check types in an expression
fn check_expression_types(expr: &Expression, program: &Program, errors: &mut Vec<AstError>) {
    match expr {
        Expression::Binary { left, op, right, .. } => {
            // Check if binary operator is valid
            let op_name = &op.value;
            if !is_valid_binary_operator(op_name) {
                errors.push(AstError::new(AstErrorKind::UndefinedIdentifier {
                    name: format!("operator {}", op_name),
                }));
            }
            
            // Check operand types (would need proper type inference)
            check_expression_type_exists(*left, program, errors);
            check_expression_type_exists(*right, program, errors);
        }
        Expression::Unary { op, expr, .. } => {
            let op_name = &op.value;
            if !is_valid_unary_operator(op_name) {
                errors.push(AstError::new(AstErrorKind::UndefinedIdentifier {
                    name: format!("operator {}", op_name),
                }));
            }
            
            check_expression_type_exists(*expr, program, errors);
        }
        Expression::Call { callee, args, .. } => {
            check_expression_type_exists(*callee, program, errors);
            for arg in args {
                check_expression_type_exists(*arg, program, errors);
            }
        }
        Expression::Identifier(ident) => {
            if ident.resolved.is_none() {
                errors.push(AstError::new(AstErrorKind::UndefinedIdentifier {
                    name: ident.name.clone(),
                }));
            }
        }
        _ => {
            // Other expression types are considered valid for now
        }
    }
}

/// Check if an expression ID exists in the program
fn check_expression_type_exists(expr_id: ExprId, program: &Program, errors: &mut Vec<AstError>) {
    if let Some(expr) = program.expressions.get(expr_id) {
        check_expression_types(expr, program, errors);
    } else {
        errors.push(AstError::new(AstErrorKind::InvalidOperation {
            op: "expression reference".to_string(),
            details: "Expression not found in arena".to_string(),
        }));
    }
}

/// Check if a binary operator is valid
fn is_valid_binary_operator(op: &str) -> bool {
    matches!(op, "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||")
}

/// Check if a unary operator is valid
fn is_valid_unary_operator(op: &str) -> bool {
    matches!(op, "!" | "-")
}
