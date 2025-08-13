//! Constant Folding Optimization Pass
//!
//! This pass evaluates expressions with constant operands at compile time,
//! replacing them with their computed constant values.

use super::OptimizationPass;
use crate::cfg::{CfgProgram, Constant, FunctionId, LValue, Operand, RValue, Statement, VarId};
use crate::dataflow::{analyze_constants, Flat, MapLattice, StmtLoc};

pub struct ConstantFoldingPass;

impl ConstantFoldingPass {
    pub fn new() -> Self {
        Self
    }

    fn replace_var_in_lvalue(lvalue: &mut LValue, constants: &MapLattice<VarId, Constant>) -> bool {
        match lvalue {
            LValue::Variable { .. } => {
                // For simple variables, we don't replace them in lvalues as they are assignment targets
                false
            }
            LValue::ArrayElement { index, .. } => {
                // Replace variables in the index operand
                Self::replace_var_in_operand(index, constants)
            }
            LValue::TableField { pk_values, .. } => {
                // Replace variables in the primary key values
                let mut changed = false;
                for pk_value in pk_values.iter_mut() {
                    changed |= Self::replace_var_in_operand(pk_value, constants);
                }
                changed
            }
        }
    }

    fn replace_var_in_rvalue(rvalue: &mut RValue, constants: &MapLattice<VarId, Constant>) -> bool {
        match rvalue {
            RValue::Use(operand) => Self::replace_var_in_operand(operand, constants),
            RValue::BinaryOp { left, right, .. } => {
                let mut changed = false;
                changed |= Self::replace_var_in_operand(left, constants);
                changed |= Self::replace_var_in_operand(right, constants);
                changed
            }
            RValue::UnaryOp { operand, .. } => Self::replace_var_in_operand(operand, constants),
            RValue::TableAccess { pk_values, .. } => {
                let mut changed = false;
                for pk_value in pk_values.iter_mut() {
                    changed |= Self::replace_var_in_operand(pk_value, constants);
                }
                changed
            }
            RValue::ArrayAccess { array, index } => {
                let mut changed = false;
                changed |= Self::replace_var_in_operand(array, constants);
                changed |= Self::replace_var_in_operand(index, constants);
                changed
            }
        }
    }

    fn replace_var_in_operand(
        operand: &mut Operand,
        constants: &MapLattice<VarId, Constant>,
    ) -> bool {
        match operand {
            Operand::Var(var_id) => {
                // Look for this variable in the constants
                match constants.get(var_id) {
                    Flat::Value(constant_value) => {
                        *operand = Operand::Const(constant_value);
                        true
                    }
                    _ => false,
                }
            }
            Operand::Const(_) => false, // Already a constant
        }
    }
}

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &'static str {
        "Constant Folding"
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

        // Run constant analysis first
        let analysis_result = analyze_constants(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.blocks {
            let block = &mut program.blocks[block_id];

            // Process each statement
            for (idx, stmt) in block.statements.iter_mut().enumerate() {
                // Get constants analysis result for this statement (use entry state)
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: idx,
                };

                let constants = match analysis_result.stmt_entry.get(&stmt_loc) {
                    Some(lattice) => lattice,
                    None => continue, // Skip if no analysis result
                };

                match stmt {
                    Statement::Assign { lvalue, rvalue, .. } => {
                        // Replace variables in rvalue with constants
                        changed |= Self::replace_var_in_rvalue(rvalue, constants);

                        // Replace variables in lvalue (for complex lvalues like array indices)
                        changed |= Self::replace_var_in_lvalue(lvalue, constants);
                    }
                }
            }
        }

        changed
    }
}
