use crate::cfg::{CfgProgram, FunctionId, LValue, Statement};
use crate::dataflow::{analyze_live_variables, AnalysisLevel, StmtLoc};
use crate::optimization::OptimizationPass;

/// Dead Code Elimination optimization pass
///
/// Uses live variable analysis to identify and remove assignments to variables
/// that are never used after the assignment (dead code).
pub struct DeadCodeEliminationPass;

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "Dead Code Elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        // Run liveness analysis
        let liveness = analyze_live_variables(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Process each block
        for &block_id in &func.blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                let mut new_statements = Vec::new();

                for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                    let should_keep = match stmt {
                        Statement::Assign { lvalue, rvalue, .. } => {
                            match lvalue {
                                LValue::Variable { var } => {
                                    // Check if variable is live after this statement
                                    let stmt_loc = StmtLoc {
                                        block: block_id,
                                        index: stmt_idx,
                                    };
                                    let live_after = liveness
                                        .stmt_exit
                                        .get(&stmt_loc)
                                        .and_then(|lattice| lattice.as_set())
                                        .map(|set| set.contains(var))
                                        .unwrap_or(true); // Conservative: keep if unknown

                                    // Keep if live after, or if rvalue has side effects
                                    live_after || Self::has_side_effects(rvalue)
                                }
                                // Array element and table field assignments have side effects
                                LValue::ArrayElement { .. } | LValue::TableField { .. } => true,
                            }
                        }
                    };

                    if should_keep {
                        new_statements.push(stmt.clone());
                    } else {
                        changed = true;
                    }
                }

                block.statements = new_statements;
            }
        }

        changed
    }
}

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Check if an rvalue has side effects (and thus cannot be eliminated)
    fn has_side_effects(rvalue: &crate::cfg::Rvalue) -> bool {
        match rvalue {
            // Simple operations have no side effects
            crate::cfg::Rvalue::Use(_)
            | crate::cfg::Rvalue::BinaryOp { .. }
            | crate::cfg::Rvalue::UnaryOp { .. }
            | crate::cfg::Rvalue::ArrayAccess { .. } => false,

            // Table access might have side effects (database read)
            crate::cfg::Rvalue::TableAccess { .. } => true,
        }
    }
}
