//! Copy Propagation Optimization Pass
//!
//! This pass replaces variables with their copied sources when a variable
//! is known to hold the same value as another variable.

use super::OptimizationPass;
use crate::cfg::{
    FunctionId, Instruction, InstructionKind, Operand, Program, VariableId,
};
use crate::dataflow::{analyze_copies, CopyTransfer, Flat, MapLattice, StmtLoc, TransferFunction};

pub struct CopyPropagationPass;

impl CopyPropagationPass {
    pub fn new() -> Self {
        Self
    }

    fn replace_var_in_operand(
        operand: &mut Operand,
        copies: &MapLattice<VariableId, VariableId>,
    ) -> bool {
        match operand {
            Operand::Variable(var_id) => {
                // Look for this variable in the copy relations
                match copies.get(var_id) {
                    Flat::Value(source_var_id) => {
                        *operand = Operand::Variable(source_var_id);
                        true
                    }
                    _ => false,
                }
            }
            Operand::Constant(_) | Operand::Global(_) | Operand::Table(_) => false, // Non-variables don't need copy propagation
        }
    }

    fn replace_vars_in_instruction(
        instruction: &mut Instruction,
        copies: &MapLattice<VariableId, VariableId>,
    ) -> bool {
        let mut changed = false;
        match instruction {
            Instruction {
                kind: InstructionKind::Assign { src, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(src, copies);
            }
            Instruction {
                kind: InstructionKind::BinaryOp { left, right, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(left, copies);
                changed |= Self::replace_var_in_operand(right, copies);
            }
            Instruction {
                kind: InstructionKind::UnaryOp { operand, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(operand, copies);
            }
            Instruction {
                kind: InstructionKind::Call { args, .. },
                ..
            } => {
                for arg in args.iter_mut() {
                    changed |= Self::replace_var_in_operand(arg, copies);
                }
            }
            Instruction {
                kind: InstructionKind::TableGet { keys, .. },
                ..
            } => {
                for key in keys.iter_mut() {
                    changed |= Self::replace_var_in_operand(key, copies);
                }
            }
            Instruction {
                kind: InstructionKind::TableSet { keys, value, .. },
                ..
            } => {
                for key in keys.iter_mut() {
                    changed |= Self::replace_var_in_operand(key, copies);
                }
                changed |= Self::replace_var_in_operand(value, copies);
            }
            Instruction {
                kind: InstructionKind::Assert { condition, .. },
                ..
            } => {
                changed |= Self::replace_var_in_operand(condition, copies);
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
