//! Semantic Analysis
//!
//! This module handles semantic analysis for the AST.

use crate::ast::*;
use crate::ast::errors::{AstError, AstErrorKind};

/// Perform semantic analysis on the AST
pub fn analyze_semantics(program: &Program) -> Result<(), Vec<AstError>> {
    let mut errors = Vec::new();

    // Check functions have proper structure
    for (_func_id, func) in &program.functions {
        if func.name.value.is_empty() {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "function definition".to_string(),
                details: "Function name cannot be empty".to_string(),
            }));
        }
        
        // Check if function has a body (unless it's an intrinsic)
        if func.body.is_none() && !has_intrinsic_decorator(&func.decorators) {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "function definition".to_string(),
                details: format!("Function '{}' has no body", func.name.value),
            }));
        }
        
        // Check that operators have proper signatures
        if func.kind == CallableKind::Operator {
            if func.params.len() == 0 {
                errors.push(AstError::new(AstErrorKind::InvalidOperation {
                    op: "operator definition".to_string(),
                    details: format!("Operator '{}' must have at least one parameter", func.name.value),
                }));
            }
            
            if func.return_type.is_none() {
                errors.push(AstError::new(AstErrorKind::InvalidOperation {
                    op: "operator definition".to_string(),
                    details: format!("Operator '{}' must have a return type", func.name.value),
                }));
            }
        }
        
        // Check that transactions have hop statements
        if func.kind == CallableKind::Transaction {
            if let Some(body_id) = func.body {
                if let Some(body) = program.blocks.get(body_id) {
                    check_transaction_body(body, program, &mut errors);
                }
            }
        }
    }

    // Check type declarations
    for (_type_id, type_decl) in &program.type_decls {
        if type_decl.name.value.is_empty() {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "type declaration".to_string(),
                details: "Type name cannot be empty".to_string(),
            }));
        }
    }

    // Check table declarations
    for (_table_id, table_decl) in &program.table_decls {
        if table_decl.name.value.is_empty() {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "table declaration".to_string(),
                details: "Table name cannot be empty".to_string(),
            }));
        }
        
        // Check that table has at least one primary key field
        let has_primary_key = table_decl.fields.iter().any(|field| field.is_primary);
        if !has_primary_key && !table_decl.fields.is_empty() {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "table declaration".to_string(),
                details: format!("Table '{}' must have at least one primary key field", table_decl.name.value),
            }));
        }
    }

    // Check const declarations
    for (_const_id, const_decl) in &program.const_decls {
        if const_decl.name.value.is_empty() {
            errors.push(AstError::new(AstErrorKind::InvalidOperation {
                op: "const declaration".to_string(),
                details: "Const name cannot be empty".to_string(),
            }));
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Check if decorators include @intrinsic
fn has_intrinsic_decorator(decorators: &[Spanned<String>]) -> bool {
    decorators.iter().any(|d| d.value == "intrinsic")
}

/// Check transaction body for hop statements
fn check_transaction_body(body: &Block, program: &Program, errors: &mut Vec<AstError>) {
    let mut has_hop = false;
    
    for stmt_id in &body.statements {
        if let Some(stmt) = program.statements.get(*stmt_id) {
            match stmt {
                Statement::Hop { .. } => {
                    has_hop = true;
                }
                Statement::HopsFor { .. } => {
                    has_hop = true;
                }
                Statement::Block(inner_block_id) => {
                    if let Some(inner_block) = program.blocks.get(*inner_block_id) {
                        check_transaction_body(inner_block, program, errors);
                    }
                }
                _ => {}
            }
        }
    }
    
    if !has_hop {
        errors.push(AstError::new(AstErrorKind::InvalidOperation {
            op: "transaction body".to_string(),
            details: "Transaction must contain at least one hop statement".to_string(),
        }));
    }
}
