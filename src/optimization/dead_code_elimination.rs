use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{analyze_live_variables, AnalysisLevel};
use crate::optimization::OptimizationPass;
use std::collections::HashSet;

/// Helper function to add used variables from an operand to the live set
fn add_used_var_from_operand(operand: &Operand, live_vars: &mut HashSet<VarId>) {
    if let Operand::Var(var_id) = operand {
        live_vars.insert(*var_id);
    }
}

/// Helper function to add used variables from an rvalue to the live set
fn add_used_vars_from_rvalue(rvalue: &Rvalue, live_vars: &mut HashSet<VarId>) {
    match rvalue {
        Rvalue::Use(operand) => {
            add_used_var_from_operand(operand, live_vars);
        }
        Rvalue::TableAccess { pk_values, .. } => {
            for pk_value in pk_values {
                add_used_var_from_operand(pk_value, live_vars);
            }
        }
        Rvalue::ArrayAccess { array, index } => {
            add_used_var_from_operand(array, live_vars);
            add_used_var_from_operand(index, live_vars);
        }
        Rvalue::UnaryOp { operand, .. } => {
            add_used_var_from_operand(operand, live_vars);
        }
        Rvalue::BinaryOp { left, right, .. } => {
            add_used_var_from_operand(left, live_vars);
            add_used_var_from_operand(right, live_vars);
        }
    }
}

/// Dead Code Elimination optimization pass
///
/// This pass removes assignments to variables that are never used (dead code).
/// It uses backward live variables analysis to determine which variables are live.
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "Dead Code Elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        // Get the function
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        eprintln!("        DEAD CODE ELIMINATION DEBUG:");

        // Run live variables analysis
        let liveness_results = analyze_live_variables(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Process each block
        let function_blocks = func.blocks.clone();
        for block_id in function_blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                eprintln!(
                    "        Block {:?}: {} statements",
                    block_id,
                    block.statements.len()
                );

                // Get live variables at exit of this block
                let mut live_vars = if let Some(lattice) = liveness_results.exit.get(&block_id) {
                    lattice.as_set().unwrap_or(&HashSet::new()).clone()
                } else {
                    HashSet::new()
                };

                eprintln!("        Live vars at exit: {:?}", live_vars);

                let mut statements_to_remove = Vec::new();

                // Process statements in reverse order to simulate backward liveness analysis
                for (stmt_idx, stmt) in block.statements.iter().enumerate().rev() {
                    let Statement::Assign { lvalue, rvalue, .. } = stmt;

                    let should_keep = match lvalue {
                        LValue::Variable { var } => {
                            // Check if this variable is needed (live) at this point
                            if live_vars.contains(var) {
                                eprintln!(
                                    "        [{}] KEEP: {:?} = {:?} (variable is live)",
                                    stmt_idx, lvalue, rvalue
                                );

                                // Update liveness: remove (kill) assigned variable and add (gen) used variables
                                live_vars.remove(var);
                                add_used_vars_from_rvalue(rvalue, &mut live_vars);
                                true
                            } else {
                                // Variable is not live - check if rvalue has side effects
                                let has_side_effects = matches!(rvalue, Rvalue::TableAccess { .. });
                                if has_side_effects {
                                    eprintln!(
                                        "        [{}] KEEP: {:?} = {:?} (has side effects)",
                                        stmt_idx, lvalue, rvalue
                                    );
                                    // Still update liveness for the rvalue
                                    add_used_vars_from_rvalue(rvalue, &mut live_vars);
                                    true
                                } else {
                                    eprintln!(
                                        "        [{}] REMOVE: {:?} = {:?} (dead assignment, no side effects)",
                                        stmt_idx, lvalue, rvalue
                                    );
                                    false
                                }
                            }
                        }
                        LValue::TableField { pk_values, .. } => {
                            eprintln!(
                                "        [{}] KEEP: {:?} = {:?} (table/array write, has side effects)",
                                stmt_idx, lvalue, rvalue
                            );
                            // Table writes have side effects - update liveness for pk values and rvalue
                            for pk_value in pk_values {
                                add_used_var_from_operand(pk_value, &mut live_vars);
                            }
                            add_used_vars_from_rvalue(rvalue, &mut live_vars);
                            true
                        }
                        LValue::ArrayElement { array, index } => {
                            eprintln!(
                                "        [{}] KEEP: {:?} = {:?} (array write, has side effects)",
                                stmt_idx, lvalue, rvalue
                            );
                            // Array writes have side effects - update liveness
                            live_vars.insert(*array);
                            add_used_var_from_operand(index, &mut live_vars);
                            add_used_vars_from_rvalue(rvalue, &mut live_vars);
                            true
                        }
                    };

                    if !should_keep {
                        statements_to_remove.push(stmt_idx);
                        changed = true;
                    }
                }

                // Remove statements (convert to forward order and remove in reverse to preserve indices)
                statements_to_remove.sort();
                statements_to_remove.reverse();

                let before_count = block.statements.len();
                for &idx in &statements_to_remove {
                    program
                        .blocks
                        .get_mut(block_id)
                        .unwrap()
                        .statements
                        .remove(idx);
                }
                let after_count = program.blocks[block_id].statements.len();

                eprintln!(
                    "        Block {:?}: Removed {} statements ({} -> {})",
                    block_id,
                    before_count - after_count,
                    before_count,
                    after_count
                );
            }
        }

        changed
    }
}
