//! Function Executor - Simple CFG walker for testing

use super::{RuntimeError, RuntimeState, RuntimeValue};
use crate::cfg::{BinaryOp, Constant, FunctionId, Operand, Rvalue, Statement, UnaryOp};
use std::collections::HashMap;

/// Execute a function by walking CFG blocks
pub fn execute_function(
    state: &mut RuntimeState,
    func_id: FunctionId,
    args: Vec<RuntimeValue>,
) -> Result<(), RuntimeError> {
    // Get function info first
    let (statements_to_execute, parameters) = {
        let cfg = state
            .cfg_program
            .as_ref()
            .ok_or_else(|| RuntimeError::ExecutionError("No program loaded".to_string()))?;

        let func = &cfg.functions[func_id];
        let parameters = func.parameters.clone();

        // Collect all statements first to avoid borrowing issues
        let mut statements_to_execute = Vec::new();
        for (_hop_id, hop) in func.hops.iter() {
            // Execute all blocks in the hop
            for &block_id in &hop.blocks {
                let block = &func.blocks[block_id];

                // Collect statements
                for stmt in &block.statements {
                    statements_to_execute.push(stmt.clone());
                }
            }
        }

        (statements_to_execute, parameters)
    };

    // Set up parameter bindings (very simple)
    let mut local_vars: HashMap<String, RuntimeValue> = HashMap::new();

    // Bind arguments to parameters
    for (i, &param_var_id) in parameters.iter().enumerate() {
        if i < args.len() {
            // Get parameter name
            if let Some(cfg) = &state.cfg_program {
                let param_name = &cfg.functions[func_id].variables[param_var_id].name;
                local_vars.insert(param_name.clone(), args[i].clone());
            }
        }
    }

    // Execute all collected statements
    for stmt in &statements_to_execute {
        execute_statement_isolated(state, &stmt, &mut local_vars, func_id)?;
    }
    Ok(())
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
            pk_values, field, ..
        } => {
            // Get primary key values
            let mut pk_vals = Vec::new();
            for operand in pk_values {
                let val = evaluate_operand_isolated(operand, local_vars, state, func_id)?;
                pk_vals.push(val);
            }

            // Read from table (simplified - assumes first table for now)
            if let Some((_, table_data)) = state.table_data.iter().next() {
                let value = table_data
                    .get(&pk_vals)
                    .and_then(|record| record.get(field))
                    .cloned()
                    .unwrap_or(RuntimeValue::Int(0)); // Default value
                Ok(value)
            } else {
                Ok(RuntimeValue::Int(0)) // Default if no tables
            }
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
