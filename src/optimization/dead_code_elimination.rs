use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{analyze_live_variables, AnalysisLevel};
use crate::optimization::OptimizationPass;
use std::collections::HashSet;

/// Dead Code Elimination optimization pass
///
/// This pass removes assignments to variables that are never used (dead code).
/// It uses backward live variables analysis to determine which variables are live.
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Get all variables used in an rvalue
    fn get_used_vars_from_rvalue(&self, rvalue: &Rvalue) -> HashSet<VarId> {
        let mut used_vars = HashSet::new();

        match rvalue {
            Rvalue::Use(operand) => {
                if let Operand::Var(var_id) = operand {
                    used_vars.insert(*var_id);
                }
            }
            Rvalue::TableAccess { pk_values, .. } => {
                for pk_value in pk_values {
                    if let Operand::Var(var_id) = pk_value {
                        used_vars.insert(*var_id);
                    }
                }
            }
            Rvalue::ArrayAccess { array, index } => {
                if let Operand::Var(var_id) = array {
                    used_vars.insert(*var_id);
                }
                if let Operand::Var(var_id) = index {
                    used_vars.insert(*var_id);
                }
            }
            Rvalue::UnaryOp { operand, .. } => {
                if let Operand::Var(var_id) = operand {
                    used_vars.insert(*var_id);
                }
            }
            Rvalue::BinaryOp { left, right, .. } => {
                if let Operand::Var(var_id) = left {
                    used_vars.insert(*var_id);
                }
                if let Operand::Var(var_id) = right {
                    used_vars.insert(*var_id);
                }
            }
        }

        used_vars
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "Dead Code Elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        // Get function reference for analysis
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

                // Get live variables at entry of this block
                let live_vars_at_entry =
                    if let Some(lattice) = liveness_results.entry.get(&block_id) {
                        lattice.as_set().unwrap_or(&HashSet::new()).clone()
                    } else {
                        HashSet::new()
                    };

                eprintln!("        Live vars at entry: {:?}", live_vars_at_entry);

                // Use actual liveness analysis results - no simulation needed
                // We need to determine which statements to keep by checking if the
                // variable they define is live after the statement
                let mut statements_to_keep = Vec::new();

                // For backward analysis, we need to determine live-after for each statement
                // We'll reconstruct this by walking through the block backwards
                let mut live_after_stmt =
                    if let Some(lattice) = liveness_results.exit.get(&block_id) {
                        lattice.as_set().unwrap_or(&HashSet::new()).clone()
                    } else {
                        HashSet::new()
                    };

                // Process statements in reverse order to compute live-after for each statement
                let mut stmt_liveness: Vec<HashSet<VarId>> = Vec::new();
                for stmt in block.statements.iter().rev() {
                    // Current statement's live-after is the live-after of the next statement
                    stmt_liveness.push(live_after_stmt.clone());

                    // Update live_after_stmt by applying this statement's transfer function
                    let Statement::Assign { lvalue, rvalue, .. } = stmt;
                    match lvalue {
                        LValue::Variable { var } => {
                            // Kill: remove the assigned variable
                            live_after_stmt.remove(var);
                            // Gen: add variables used in rvalue
                            let used_vars = self.get_used_vars_from_rvalue(rvalue);
                            live_after_stmt.extend(used_vars);
                        }
                        LValue::ArrayElement { array, index } => {
                            // Array assignment: uses array and index
                            live_after_stmt.insert(*array);
                            if let Operand::Var(var_id) = index {
                                live_after_stmt.insert(*var_id);
                            }
                            let used_vars = self.get_used_vars_from_rvalue(rvalue);
                            live_after_stmt.extend(used_vars);
                        }
                        LValue::TableField { pk_values, .. } => {
                            // Table assignment: uses primary key values
                            for pk_value in pk_values {
                                if let Operand::Var(var_id) = pk_value {
                                    live_after_stmt.insert(*var_id);
                                }
                            }
                            let used_vars = self.get_used_vars_from_rvalue(rvalue);
                            live_after_stmt.extend(used_vars);
                        }
                    }
                }

                // Reverse the stmt_liveness vector since we built it backwards
                stmt_liveness.reverse();

                // Now decide which statements to keep
                for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                    let Statement::Assign { lvalue, rvalue, .. } = stmt;

                    let should_keep = match lvalue {
                        LValue::Variable { var } => {
                            // Check if the assigned variable is live after this statement
                            let empty_set = HashSet::new();
                            let live_after = stmt_liveness.get(stmt_idx).unwrap_or(&empty_set);
                            if live_after.contains(var) {
                                eprintln!(
                                    "        [{}] KEEP: {:?} = {:?} (var {:?} is live after)",
                                    stmt_idx, lvalue, rvalue, var
                                );
                                true
                            } else {
                                // Check if rvalue has side effects
                                let has_side_effects = matches!(rvalue, Rvalue::TableAccess { .. });
                                if has_side_effects {
                                    eprintln!(
                                        "        [{}] KEEP: {:?} = {:?} (has side effects)",
                                        stmt_idx, lvalue, rvalue
                                    );
                                    true
                                } else {
                                    eprintln!("        [{}] REMOVE: {:?} = {:?} (dead assignment, no side effects)", 
                                             stmt_idx, lvalue, rvalue);
                                    false
                                }
                            }
                        }
                        LValue::ArrayElement { .. } | LValue::TableField { .. } => {
                            // Array and table assignments always have side effects
                            eprintln!("        [{}] KEEP: {:?} = {:?} (array/table write, has side effects)", 
                                     stmt_idx, lvalue, rvalue);
                            true
                        }
                    };

                    if should_keep {
                        statements_to_keep.push(stmt.clone());
                    }
                }

                // Update the block's statements if any were removed
                if statements_to_keep.len() != block.statements.len() {
                    eprintln!(
                        "        Block {:?}: Removed {} statements ({} -> {})",
                        block_id,
                        block.statements.len() - statements_to_keep.len(),
                        block.statements.len(),
                        statements_to_keep.len()
                    );
                    block.statements = statements_to_keep;
                    changed = true;
                } else {
                    eprintln!("      - No change: {} statements", block.statements.len());
                }
            }
        }

        changed
    }
}
