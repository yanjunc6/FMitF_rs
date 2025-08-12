//! Dead Code Elimination Optimization Pass
//!
//! This pass removes assignments to variables that are never used after assignment.

use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, Statement, VarId};
use crate::dataflow::liveness::LiveVar;
use crate::dataflow::{analyze_live_variables, StmtLoc};
use std::collections::HashSet;

pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Extract live variables from lattice result
    fn extract_live_vars(lattice_result: &crate::dataflow::SetLattice<LiveVar>) -> HashSet<VarId> {
        if let Some(live_vars) = lattice_result.as_set() {
            live_vars.iter().map(|live_var| live_var.0).collect()
        } else {
            HashSet::new()
        }
    }

    /// Check if an assignment is dead (assigned variable is not live after assignment)
    fn is_dead_assignment(&self, stmt: &Statement, live_after: &HashSet<VarId>) -> bool {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Check if this is a variable assignment
                if let crate::cfg::LValue::Variable { var } = lvalue {
                    // If the assigned variable is not live after this statement, it's dead
                    let is_dead = !live_after.contains(var);

                    // However, don't eliminate assignments with side effects
                    let has_side_effects = match rvalue {
                        crate::cfg::RValue::TableAccess { .. } => true, // Table access might have side effects
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

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "Dead Code Elimination"
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
            let mut new_statements = Vec::new();

            // Process each statement
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: stmt_idx,
                };

                // Get live variables after this statement
                let should_keep =
                    if let Some(lattice_result) = liveness_results.stmt_exit.get(&stmt_loc) {
                        let live_after = Self::extract_live_vars(lattice_result);
                        !self.is_dead_assignment(stmt, &live_after)
                    } else {
                        // If we don't have liveness info, keep the statement
                        true
                    };

                if should_keep {
                    new_statements.push(stmt.clone());
                } else {
                    changed = true;
                }
            }

            block.statements = new_statements;
        }

        changed
    }
}
