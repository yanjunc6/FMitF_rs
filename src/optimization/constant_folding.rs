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

            // Process each instruction
            for (idx, inst) in block.instructions.iter_mut().enumerate() {
                // Get constants analysis result for this instruction (use entry state)
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: idx,
                };

                let constants = match analysis_result.stmt_entry.get(&stmt_loc) {
                    Some(lattice) => lattice,
                    None => continue, // Skip if no analysis result
                };

                // Replace variables with constants in this instruction
                changed |= Self::replace_vars_in_instruction(inst, constants);
            }
        }

        changed
    }
}
