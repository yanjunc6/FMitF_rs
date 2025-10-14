//! Copy Propagation Optimization Pass
//!
//! This pass replaces variables with their copied sources when a variable
//! is known to hold the same value as another variable.

use super::OptimizationPass;
use crate::cfg::{FunctionId, Instruction, InstructionKind, Operand, Program, VariableId};
use crate::dataflow::{analyze_copies, CopyTransfer, Flat, MapLattice, StmtLoc, TransferFunction};
use std::collections::HashMap;

pub struct CopyPropagationPass;

impl CopyPropagationPass {
    pub fn new() -> Self {
        Self
    }

    /// Build an inverse map from source variables to destination variables
    /// Given the copy map stores dest -> src, we want to find src -> dest
    fn build_inverse_map(
        copies: &MapLattice<VariableId, VariableId>,
    ) -> HashMap<VariableId, VariableId> {
        let mut inverse = HashMap::new();
        if copies.is_top() {
            return inverse;
        }

        for (dest, flat_src) in copies.iter() {
            if let Flat::Value(src) = flat_src {
                // Map from source to destination
                // Only add if src is not already mapped (keep first occurrence)
                inverse.entry(*src).or_insert(*dest);
            }
        }
        inverse
    }

    fn replace_var_in_operand(
        operand: &mut Operand,
        inverse_copies: &HashMap<VariableId, VariableId>,
    ) -> bool {
        match operand {
            Operand::Variable(var_id) => {
                // Look for this variable in the inverse copy relations
                // If this var is a source that was copied to a dest, replace with dest
                if let Some(&dest_var_id) = inverse_copies.get(var_id) {
                    *operand = Operand::Variable(dest_var_id);
                    true
                } else {
                    false
                }
            }
            Operand::Constant(_) | Operand::Global(_) | Operand::Table(_) => false,
        }
    }

    fn replace_vars_in_instruction(
        instruction: &mut Instruction,
        copies: &MapLattice<VariableId, VariableId>,
    ) -> bool {
        // Build inverse map: src -> dest
        let inverse_copies = Self::build_inverse_map(copies);

        let mut changed = false;
        match instruction {
            Instruction {
                kind: InstructionKind::Assign { src, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(src, &inverse_copies);
            }
            Instruction {
                kind: InstructionKind::BinaryOp { left, right, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(left, &inverse_copies);
                changed |= Self::replace_var_in_operand(right, &inverse_copies);
            }
            Instruction {
                kind: InstructionKind::UnaryOp { operand, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(operand, &inverse_copies);
            }
            Instruction {
                kind: InstructionKind::Call { args, .. },
                ..
            } => {
                for arg in args.iter_mut() {
                    changed |= Self::replace_var_in_operand(arg, &inverse_copies);
                }
            }
            Instruction {
                kind: InstructionKind::TableGet { keys, .. },
                ..
            } => {
                for key in keys.iter_mut() {
                    changed |= Self::replace_var_in_operand(key, &inverse_copies);
                }
            }
            Instruction {
                kind: InstructionKind::TableSet { keys, value, .. },
                ..
            } => {
                for key in keys.iter_mut() {
                    changed |= Self::replace_var_in_operand(key, &inverse_copies);
                }
                changed |= Self::replace_var_in_operand(value, &inverse_copies);
            }
            Instruction {
                kind: InstructionKind::Assert { condition, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(condition, &inverse_copies);
            }
        }
        changed
    }
}

impl OptimizationPass for CopyPropagationPass {
    fn name(&self) -> &'static str {
        "Copy Propagation"
    }

    fn optimize_function(&self, program: &mut Program, func_id: FunctionId) -> bool {
        let function = &program.functions[func_id];

        if matches!(function.kind, crate::cfg::FunctionKind::Operator) {
            return false;
        }

        let analysis_result = analyze_copies(function, program);
        let mut changed = false;
        let transfer = CopyTransfer;

        for &block_id in &function.all_blocks {
            let mut state = analysis_result.block_entry.get(&block_id).cloned().unwrap();

            let block = &mut program.basic_blocks[block_id];

            for (idx, inst) in block.instructions.iter_mut().enumerate() {
                if Self::replace_vars_in_instruction(inst, &state) {
                    changed = true;
                }

                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: idx,
                };
                state = transfer.transfer_instruction(inst, stmt_loc, &state);
            }
        }

        changed
    }
}
