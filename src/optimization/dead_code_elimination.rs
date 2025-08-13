use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, LValue, RValue, Statement, VarId};
use crate::dataflow::{analyze_live_variables, StmtLoc};
use std::collections::HashSet;

pub struct DeadCodeElimination;

impl DeadCodeElimination {
    pub fn new() -> Self {
        Self
    }

    /// Check if an assignment is dead (assigned variable is not live after assignment)
    fn is_dead_assignment(&self, stmt: &Statement, live_after: &HashSet<VarId>) -> bool {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Check if this is a variable assignment
                if let LValue::Variable { var } = lvalue {
                    // If the assigned variable is not live after this statement, it's dead
                    let is_dead = !live_after.contains(var);

                    // However, don't eliminate assignments with side effects
                    let has_side_effects = match rvalue {
                        RValue::TableAccess { .. } => true, // Table access might have side effects
                        _ => false,
                    };

                    is_dead && !has_side_effects
                } else {
                    // Array/table assignments might have side effects, don't eliminate
                    false
                }
            }
        }
    }
}

impl OptimizationPass for DeadCodeElimination {
    fn name(&self) -> &'static str {
        "dead-code-elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        let function = &program.functions[func_id];

        // Skip abstract functions
        if matches!(
            function.implementation,
            crate::cfg::FunctionImplementation::Abstract
        ) {
            return false;
        }

        // Run liveness analysis
        let liveness_results = analyze_live_variables(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.blocks {
            let block = &mut program.blocks[block_id];
            let original_len = block.statements.len();

            // First pass: mark statements for deletion
            let mut statements_to_keep = Vec::new();

            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: stmt_idx,
                };

                // Get live variables after this statement
                let should_keep =
                    if let Some(lattice_result) = liveness_results.stmt_exit.get(&stmt_loc) {
                        let live_after = if let Some(live_vars) = lattice_result.as_set() {
                            live_vars.iter().map(|live_var| live_var.0).collect()
                        } else {
                            HashSet::new()
                        };

                        // Keep the statement if it's not dead
                        !self.is_dead_assignment(stmt, &live_after)
                    } else {
                        // If we don't have liveness info, keep the statement
                        true
                    };

                if should_keep {
                    statements_to_keep.push(stmt.clone());
                }
            }

            // Replace the statements
            block.statements = statements_to_keep;

            // Check if we actually removed statements
            if block.statements.len() < original_len {
                changed = true;
            }
        }

        changed
    }
}
