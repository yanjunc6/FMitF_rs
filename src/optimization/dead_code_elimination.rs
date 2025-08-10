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

        // Run live variables analysis
        let liveness_results = analyze_live_variables(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Process each block
        let function_blocks = func.blocks.clone();
        for block_id in function_blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                // Get live variables at entry of this block
                let live_vars_at_entry =
                    if let Some(lattice) = liveness_results.entry.get(&block_id) {
                        lattice.as_set().unwrap_or(&HashSet::new()).clone()
                    } else {
                        HashSet::new()
                    };

                // Simulate the live variables analysis for this block
                // Start with live variables at entry and work through statements
                let mut current_live = live_vars_at_entry;
                let mut statements_to_keep = Vec::new();

                for stmt in &block.statements {
                    let Statement::Assign { lvalue, rvalue, .. } = stmt;

                    match lvalue {
                        LValue::Variable { var } => {
                            // Check if the assigned variable is live
                            if current_live.contains(var) {
                                // Keep this statement
                                statements_to_keep.push(stmt.clone());

                                // Update liveness: remove defined variable, add used variables
                                current_live.remove(var);
                                let used_vars = self.get_used_vars_from_rvalue(rvalue);
                                current_live.extend(used_vars);
                            } else {
                                // This is a dead assignment
                                // But we still need to consider side effects in the rvalue

                                // Check if rvalue has side effects (like table access)
                                let has_side_effects = matches!(rvalue, Rvalue::TableAccess { .. });

                                if has_side_effects {
                                    // Keep statement due to side effects
                                    statements_to_keep.push(stmt.clone());
                                    let used_vars = self.get_used_vars_from_rvalue(rvalue);
                                    current_live.extend(used_vars);
                                } else {
                                    // Remove this dead assignment
                                    changed = true;
                                }
                            }
                        }
                        LValue::TableField { pk_values, .. } => {
                            // Table assignments have side effects, always keep them
                            statements_to_keep.push(stmt.clone());

                            // Add used variables from pk_values and rvalue to live set
                            for pk_value in pk_values {
                                if let Operand::Var(var_id) = pk_value {
                                    current_live.insert(*var_id);
                                }
                            }
                            let used_vars = self.get_used_vars_from_rvalue(rvalue);
                            current_live.extend(used_vars);
                        }
                        LValue::ArrayElement { array, index } => {
                            // Array assignments have side effects, always keep them
                            statements_to_keep.push(stmt.clone());

                            // Add used variables from array, index, and rvalue to live set
                            current_live.insert(*array);
                            if let Operand::Var(var_id) = index {
                                current_live.insert(*var_id);
                            }
                            let used_vars = self.get_used_vars_from_rvalue(rvalue);
                            current_live.extend(used_vars);
                        }
                    }
                }

                // Update the block with the filtered statements
                if statements_to_keep.len() != block.statements.len() {
                    block.statements = statements_to_keep;
                    changed = true;
                }
            }
        }

        changed
    }
}
