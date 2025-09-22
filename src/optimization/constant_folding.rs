//! Constant Folding Optimization Pass
//!
//! This pass evaluates expressions with constant operands at compile time,
//! replacing them with their computed constant values.

use super::OptimizationPass;
use crate::cfg::{ConstantValue, FunctionId, Instruction, Operand, Program, VariableId};
use crate::dataflow::{analyze_constants, Flat, MapLattice, StmtLoc};

pub struct ConstantFoldingPass;

impl ConstantFoldingPass {
    pub fn new() -> Self {
        Self
    }

    fn replace_var_in_operand(
        operand: &mut Operand,
        constants: &MapLattice<VariableId, ConstantValue>,
    ) -> bool {
        match operand {
            Operand::Variable(var_id) => {
                // Look for this variable in the constants
                match constants.get(var_id) {
                    Flat::Value(constant_value) => {
                        *operand = Operand::Constant(constant_value);
                        true
                    }
                    _ => false,
                }
            }
            Operand::Constant(_) | Operand::Global(_) => false, // Already a constant
        }
    }

    fn replace_vars_in_instruction(
        instruction: &mut Instruction,
        constants: &MapLattice<VariableId, ConstantValue>,
    ) -> bool {
        let mut changed = false;
        match instruction {
            Instruction::Assign { src, .. } => {
                changed |= Self::replace_var_in_operand(src, constants);
            }
            Instruction::BinaryOp { left, right, .. } => {
                changed |= Self::replace_var_in_operand(left, constants);
                changed |= Self::replace_var_in_operand(right, constants);
            }
            Instruction::UnaryOp { operand, .. } => {
                changed |= Self::replace_var_in_operand(operand, constants);
            }
            Instruction::Call { args, .. } => {
                for arg in args.iter_mut() {
                    changed |= Self::replace_var_in_operand(arg, constants);
                }
            }
            Instruction::TableGet { keys, .. } => {
                for key in keys.iter_mut() {
                    changed |= Self::replace_var_in_operand(key, constants);
                }
            }
            Instruction::TableSet { keys, value, .. } => {
                for key in keys.iter_mut() {
                    changed |= Self::replace_var_in_operand(key, constants);
                }
                changed |= Self::replace_var_in_operand(value, constants);
            }
            Instruction::Assert { condition, .. } => {
                changed |= Self::replace_var_in_operand(condition, constants);
            }
        }
        changed
    }

    /// After variables are replaced, this checks if an entire instruction can be folded.
    /// It uses the dataflow analysis results to see if the destination of an operation
    /// is a known constant. If so, it replaces the entire instruction with a simple assignment.
    fn fold_instruction(
        instruction: &mut Instruction,
        constants: &MapLattice<VariableId, ConstantValue>,
    ) -> bool {
        let dest_var = match instruction {
            Instruction::BinaryOp { dest, .. } => Some(dest),
            Instruction::UnaryOp { dest, .. } => Some(dest),
            _ => None,
        };

        if let Some(dest) = dest_var {
            // If the analysis determined the destination variable is a constant,
            // we can replace the entire instruction that calculates it.
            if let Flat::Value(const_val) = constants.get(dest) {
                *instruction = Instruction::Assign {
                    dest: *dest,
                    src: Operand::Constant(const_val.clone()),
                };
                return true; // The instruction was changed
            }
        }

        false
    }
}

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &'static str {
        "Constant Folding"
    }

    fn optimize_function(&self, program: &mut Program, func_id: FunctionId) -> bool {
        let function = &program.functions[func_id];

        // Skip abstract functions (like operators)
        if matches!(function.kind, crate::cfg::FunctionKind::Operator) {
            return false;
        }

        // Run constant analysis first
        let analysis_result = analyze_constants(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.all_blocks {
            let block = &mut program.basic_blocks[block_id];
            for (idx, inst) in block.instructions.iter_mut().enumerate() {
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: idx,
                };
                let entry_consts = match analysis_result.stmt_entry.get(&stmt_loc) {
                    Some(l) => l,
                    None => continue,
                };
                // Substitute operands using entry state
                if Self::replace_vars_in_instruction(inst, entry_consts) {
                    changed = true;
                }
                // Use exit state to see the value *after* executing instruction (includes dest)
                if let Some(exit_consts) = analysis_result.stmt_exit.get(&stmt_loc) {
                    if Self::fold_instruction(inst, exit_consts) {
                        changed = true;
                    }
                }
            }
        }

        changed
    }
}
