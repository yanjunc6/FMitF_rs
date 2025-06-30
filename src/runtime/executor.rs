//! Function Executor - CFG interpreter with proper control flow

use super::{RuntimeError, RuntimeState, RuntimeValue};
use crate::cfg::{BasicBlockId, BinaryOp, Constant, FunctionId, HopId, Operand, Rvalue, Statement, Terminator, UnaryOp};
use std::collections::HashMap;

/// Execute a function by properly following CFG control flow
/// 
/// This executor respects the CFG structure:
/// 1. Starts from the entry hop
/// 2. Follows terminators (branches, gotos, returns) properly
/// 3. Evaluates conditions for branch decisions
/// 4. Handles hop transitions correctly
/// 5. Maintains proper execution state between blocks
pub fn execute_function(
    state: &mut RuntimeState,
    func_id: FunctionId,
    args: Vec<RuntimeValue>,
) -> Result<Option<RuntimeValue>, RuntimeError> {
    let cfg = state
        .cfg_program
        .as_ref()
        .ok_or_else(|| RuntimeError::ExecutionError("No program loaded".to_string()))?;

    let func = &cfg.functions[func_id];
    
    // Set up parameter bindings
    let mut local_vars: HashMap<String, RuntimeValue> = HashMap::new();
    for (i, &param_var_id) in func.parameters.iter().enumerate() {
        if i < args.len() {
            let param_name = &func.variables[param_var_id].name;
            local_vars.insert(param_name.clone(), args[i].clone());
        }
    }

    // Start execution from entry hop
    let entry_hop = func.entry_hop.ok_or_else(|| {
        RuntimeError::ExecutionError("Function has no entry hop".to_string())
    })?;

    execute_hop(state, func_id, entry_hop, &mut local_vars)
}

/// Execute a hop starting from its entry block
fn execute_hop(
    state: &mut RuntimeState,
    func_id: FunctionId,
    hop_id: HopId,
    local_vars: &mut HashMap<String, RuntimeValue>,
) -> Result<Option<RuntimeValue>, RuntimeError> {
    let cfg = state.cfg_program.as_ref().unwrap();
    let func = &cfg.functions[func_id];
    let hop = &func.hops[hop_id];

    let entry_block = hop.entry_block.ok_or_else(|| {
        RuntimeError::ExecutionError("Hop has no entry block".to_string())
    })?;

    execute_block(state, func_id, entry_block, local_vars)
}

/// Execute a basic block and handle its terminator
fn execute_block(
    state: &mut RuntimeState,
    func_id: FunctionId,
    block_id: BasicBlockId,
    local_vars: &mut HashMap<String, RuntimeValue>,
) -> Result<Option<RuntimeValue>, RuntimeError> {
    // Get the statements and terminator first to avoid borrowing issues
    let (statements, terminator) = {
        let cfg = state.cfg_program.as_ref().unwrap();
        let func = &cfg.functions[func_id];
        let block = &func.blocks[block_id];
        (block.statements.clone(), block.terminator.clone())
    };

    // Execute all statements in the block
    for stmt in &statements {
        execute_statement_isolated(state, stmt, local_vars, func_id)?;
    }

    // Handle the terminator
    match terminator {
        Terminator::Goto(next_block) => {
            execute_block(state, func_id, next_block, local_vars)
        }
        
        Terminator::Branch {
            condition,
            then_block,
            else_block,
        } => {
            let condition_value = evaluate_operand_isolated(&condition, local_vars, state, func_id)?;
            let next_block = match condition_value {
                RuntimeValue::Bool(true) => then_block,
                RuntimeValue::Bool(false) => else_block,
                _ => return Err(RuntimeError::ExecutionError(
                    "Branch condition must be boolean".to_string()
                )),
            };
            execute_block(state, func_id, next_block, local_vars)
        }
        
        Terminator::Return(operand) => {
            if let Some(operand) = operand {
                let return_value = evaluate_operand_isolated(&operand, local_vars, state, func_id)?;
                Ok(Some(return_value))
            } else {
                Ok(None)
            }
        }
        
        Terminator::Abort => {
            Err(RuntimeError::ExecutionError("Function aborted".to_string()))
        }
        
        Terminator::HopExit { next_hop } => {
            if let Some(next_hop_id) = next_hop {
                execute_hop(state, func_id, next_hop_id, local_vars)
            } else {
                // End of function
                Ok(None)
            }
        }
    }
}

/// Execute a single statement
fn execute_statement_isolated(
    state: &mut RuntimeState,
    stmt: &Statement,
    local_vars: &mut HashMap<String, RuntimeValue>,
    func_id: FunctionId,
) -> Result<(), RuntimeError> {
    match stmt {
        Statement::Assign {
            var,
            rvalue,
            span: _,
        } => {
            // Get variable name
            let var_name = if let Some(cfg) = &state.cfg_program {
                cfg.functions[func_id].variables[*var].name.clone()
            } else {
                return Err(RuntimeError::ExecutionError(
                    "No program loaded".to_string(),
                ));
            };

            let value = evaluate_rvalue_isolated(rvalue, local_vars, state, func_id)?;
            local_vars.insert(var_name.clone(), value);
            // Removed debug output: println!("    {} = {:?}", var_name, local_vars[&var_name]);
        }

        Statement::TableAssign {
            table,
            pk_fields: _,
            pk_values,
            field,
            value,
            span: _,
        } => {
            // Get primary key values
            let mut pk_vals = Vec::new();
            for operand in pk_values {
                let val = evaluate_operand_isolated(operand, local_vars, state, func_id)?;
                pk_vals.push(val);
            }

            // Get field value
            let field_value = evaluate_operand_isolated(value, local_vars, state, func_id)?;

            // Update table
            let table_data = state.table_data.get_mut(table).ok_or_else(|| {
                RuntimeError::ExecutionError(format!("Table {:?} not found", table))
            })?;

            let record = table_data
                .entry(pk_vals.clone())
                .or_insert_with(HashMap::new);
            record.insert(*field, field_value.clone());

            // Removed debug output: println!("    Table[{:?}].{:?} = {:?}", pk_vals, field, field_value);
        }
    }

    Ok(())
}

/// Evaluate an rvalue expression
fn evaluate_rvalue_isolated(
    rvalue: &Rvalue,
    local_vars: &HashMap<String, RuntimeValue>,
    state: &RuntimeState,
    func_id: FunctionId,
) -> Result<RuntimeValue, RuntimeError> {
    match rvalue {
        Rvalue::Use(operand) => evaluate_operand_isolated(operand, local_vars, state, func_id),

        Rvalue::TableAccess {
            table,
            pk_values, 
            field, 
            ..
        } => {
            // Get primary key values
            let mut pk_vals = Vec::new();
            for operand in pk_values {
                let val = evaluate_operand_isolated(operand, local_vars, state, func_id)?;
                pk_vals.push(val);
            }

            // Read from the specific table
            let table_data = state.table_data.get(table).ok_or_else(|| {
                RuntimeError::ExecutionError(format!("Table {:?} not found", table))
            })?;
            
            let value = table_data
                .get(&pk_vals)
                .and_then(|record| record.get(field))
                .cloned()
                .or_else(|| {
                    // Get default value based on field type
                    if let Some(cfg) = &state.cfg_program {
                        let field_info = &cfg.fields[*field];
                        Some(match field_info.ty {
                            crate::ast::TypeName::Int => RuntimeValue::Int(0),
                            crate::ast::TypeName::Bool => RuntimeValue::Bool(false),
                            crate::ast::TypeName::String => RuntimeValue::String(String::new()),
                            crate::ast::TypeName::Float => RuntimeValue::Int(0), // Convert to int for simplicity
                        })
                    } else {
                        Some(RuntimeValue::Int(0)) // Fallback
                    }
                })
                .unwrap(); // Safe to unwrap because we always provide a default above
            Ok(value)
        }

        Rvalue::BinaryOp { op, left, right } => {
            let left_val = evaluate_operand_isolated(left, local_vars, state, func_id)?;
            let right_val = evaluate_operand_isolated(right, local_vars, state, func_id)?;

            match (op, &left_val, &right_val) {
                (BinaryOp::Add, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Int(a + b))
                }
                (BinaryOp::Sub, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Int(a - b))
                }
                (BinaryOp::Mul, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Int(a * b))
                }
                (BinaryOp::Div, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    if *b == 0 {
                        Err(RuntimeError::ExecutionError("Division by zero".to_string()))
                    } else {
                        Ok(RuntimeValue::Int(a / b))
                    }
                }
                (BinaryOp::Eq, a, b) => Ok(RuntimeValue::Bool(a == b)),
                (BinaryOp::Neq, a, b) => Ok(RuntimeValue::Bool(a != b)),
                (BinaryOp::Lt, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Bool(a < b))
                }
                (BinaryOp::Lte, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Bool(a <= b))
                }
                (BinaryOp::Gt, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Bool(a > b))
                }
                (BinaryOp::Gte, RuntimeValue::Int(a), RuntimeValue::Int(b)) => {
                    Ok(RuntimeValue::Bool(a >= b))
                }
                _ => Err(RuntimeError::ExecutionError(format!(
                    "Unsupported binary operation: {:?} {:?} {:?}",
                    left_val, op, right_val
                ))),
            }
        }

        Rvalue::UnaryOp { op, operand } => {
            let val = evaluate_operand_isolated(operand, local_vars, state, func_id)?;

            match (op, &val) {
                (UnaryOp::Neg, RuntimeValue::Int(a)) => Ok(RuntimeValue::Int(-a)),
                (UnaryOp::Not, RuntimeValue::Bool(a)) => Ok(RuntimeValue::Bool(!a)),
                _ => Err(RuntimeError::ExecutionError(format!(
                    "Unsupported unary operation: {:?} {:?}",
                    op, val
                ))),
            }
        }
    }
}

/// Evaluate an operand
fn evaluate_operand_isolated(
    operand: &Operand,
    local_vars: &HashMap<String, RuntimeValue>,
    state: &RuntimeState,
    func_id: FunctionId,
) -> Result<RuntimeValue, RuntimeError> {
    match operand {
        Operand::Const(constant) => {
            match constant {
                Constant::Int(val) => Ok(RuntimeValue::Int(*val)),
                Constant::String(val) => Ok(RuntimeValue::String(val.clone())),
                Constant::Bool(val) => Ok(RuntimeValue::Bool(*val)),
                Constant::Float(val) => Ok(RuntimeValue::Int(val.into_inner() as i64)), // Convert float to int for simplicity
            }
        }

        Operand::Var(var_id) => {
            // Get variable name from CFG
            let var_name = if let Some(cfg) = &state.cfg_program {
                cfg.functions[func_id].variables[*var_id].name.clone()
            } else {
                return Err(RuntimeError::ExecutionError(
                    "No program loaded".to_string(),
                ));
            };

            local_vars.get(&var_name).cloned().ok_or_else(|| {
                RuntimeError::ExecutionError(format!("Variable '{}' not found", var_name))
            })
        }
    }
}
