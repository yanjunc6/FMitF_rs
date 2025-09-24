use super::OptimizationPass;
use crate::cfg::{FunctionId, Instruction, InstructionKind, Program, VariableId};
use crate::dataflow::{analyze_live_variables, StmtLoc};
use std::collections::HashSet;

pub struct DeadCodeElimination;

impl DeadCodeElimination {
    pub fn new() -> Self {
        Self
    }

    /// Check if an instruction is dead (assigned variable is not live after assignment)
    fn is_dead_instruction(&self, inst: &Instruction, live_after: &HashSet<VariableId>) -> bool {
        match &inst.kind {
            InstructionKind::Assign { dest, .. } => {
                // If the assigned variable is not live after this instruction, it's dead
                !live_after.contains(dest)
            }
            InstructionKind::BinaryOp { dest, .. } => {
                // If the assigned variable is not live after this instruction, it's dead
                !live_after.contains(dest)
            }
            InstructionKind::UnaryOp { dest, .. } => {
                // If the assigned variable is not live after this instruction, it's dead
                !live_after.contains(dest)
            }
            InstructionKind::Call { dest, .. } => {
                // Function calls might have side effects, but if no one uses the result...
                // For call-by-value, function calls with pure functions can be eliminated if result is unused
                // However, conservatively keep all function calls for now
                if let Some(_dest_var) = dest {
                    // For now, conservatively keep all function calls
                    // In the future, we could mark pure functions and eliminate dead calls to them
                    false // Don't eliminate function calls
                } else {
                    false // Call without return value, might have side effects
                }
            }
            InstructionKind::TableGet { dest, .. } => {
                // Table operations might have side effects, but if it's just a read...
                // For now, conservatively keep table operations
                !live_after.contains(dest) // Could eliminate if result unused
            }
            InstructionKind::TableSet { .. } => {
                // Table writes have side effects, never eliminate
                false
            }
            InstructionKind::Assert { .. } => {
                // Assertions have side effects (they can abort), never eliminate
                false
            }
        }
    }
}

impl OptimizationPass for DeadCodeElimination {
    fn name(&self) -> &'static str {
        "dead-code-elimination"
    }

    fn optimize_function(&self, program: &mut Program, func_id: FunctionId) -> bool {
        let function = &program.functions[func_id];

        // Skip abstract functions (like operators)
        if matches!(function.kind, crate::cfg::FunctionKind::Operator) {
            return false;
        }

        // Run liveness analysis
        let liveness_results = analyze_live_variables(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.all_blocks {
            let block = &mut program.basic_blocks[block_id];
            let original_len = block.instructions.len();

            // First pass: mark instructions for deletion
            let mut instructions_to_keep = Vec::new();

            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: inst_idx,
                };

                // Get live variables after this instruction
                let should_keep =
                    if let Some(lattice_result) = liveness_results.stmt_exit.get(&stmt_loc) {
                        let live_after = if let Some(live_vars) = lattice_result.as_set() {
                            live_vars.iter().map(|live_var| live_var.0).collect()
                        } else {
                            HashSet::new()
                        };

                        // Keep the instruction if it's not dead
                        !self.is_dead_instruction(inst, &live_after)
                    } else {
                        // If we don't have liveness info, keep the instruction
                        true
                    };

                if should_keep {
                    instructions_to_keep.push(inst.clone());
                }
            }

            // Replace the instructions
            block.instructions = instructions_to_keep;

            // Check if we actually removed instructions
            if block.instructions.len() < original_len {
                changed = true;
            }
        }

        changed
    }
}
