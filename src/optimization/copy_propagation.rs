//! Copy Propagation Optimization Pass
//!
//! This pass replaces variables with their copied sources when a variable
//! is known to hold the same value as another variable.

use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, RValue, Statement, VarId};
use crate::dataflow::{analyze_copies, Flat, MapLattice, StmtLoc};

pub struct CopyPropagationPass;

impl CopyPropagationPass {
    pub fn new() -> Self {
        Self
    }

    fn replace_var_in_lvalue(lvalue: &mut LValue, copies: &MapLattice<VarId, VarId>) -> bool {
        match lvalue {
            LValue::Variable { .. } => {
                // For simple variables, we don't replace them in lvalues as they are assignment targets
                false
            }
            LValue::ArrayElement { index, .. } => {
                // Replace variables in the index operand
                Self::replace_var_in_operand(index, copies)
            }
            LValue::TableField { pk_values, .. } => {
                // Replace variables in the primary key values
                let mut changed = false;
                for pk_value in pk_values.iter_mut() {
                    changed |= Self::replace_var_in_operand(pk_value, copies);
                }
                changed
            }
        }
    }

    fn replace_var_in_rvalue(rvalue: &mut RValue, copies: &MapLattice<VarId, VarId>) -> bool {
        match rvalue {
            RValue::Use(operand) => Self::replace_var_in_operand(operand, copies),
            RValue::BinaryOp { left, right, .. } => {
                let mut changed = false;
                changed |= Self::replace_var_in_operand(left, copies);
                changed |= Self::replace_var_in_operand(right, copies);
                changed
            }
            RValue::UnaryOp { operand, .. } => Self::replace_var_in_operand(operand, copies),
            RValue::TableAccess { pk_values, .. } => {
                let mut changed = false;
                for pk_value in pk_values.iter_mut() {
                    changed |= Self::replace_var_in_operand(pk_value, copies);
                }
                changed
            }
            RValue::ArrayAccess { array, index } => {
                let mut changed = false;
                changed |= Self::replace_var_in_operand(array, copies);
                changed |= Self::replace_var_in_operand(index, copies);
                changed
            }
        }
    }

    fn replace_var_in_operand(operand: &mut Operand, copies: &MapLattice<VarId, VarId>) -> bool {
        match operand {
            Operand::Var(var_id) => {
                // Look for this variable in the copy relations
                match copies.get(var_id) {
                    Flat::Value(source_var_id) => {
                        *operand = Operand::Var(source_var_id);
                        true
                    }
                    _ => false,
                }
            }
            Operand::Const(_) => false, // Constants don't need copy propagation
        }
    }
}

impl OptimizationPass for CopyPropagationPass {
    fn name(&self) -> &'static str {
        "Copy Propagation"
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

        // Run copy analysis first
        let analysis_result = analyze_copies(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.blocks {
            let block = &mut program.blocks[block_id];

            // Process each statement
            for (idx, stmt) in block.statements.iter_mut().enumerate() {
                // Get copy analysis result for this statement (use entry state)
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: idx,
                };

                let copies = match analysis_result.stmt_entry.get(&stmt_loc) {
                    Some(lattice) => lattice,
                    None => continue, // Skip if no analysis result
                };

                match stmt {
                    Statement::Assign { lvalue, rvalue, .. } => {
                        // Replace variables in rvalue with their copies
                        changed |= Self::replace_var_in_rvalue(rvalue, copies);

                        // Replace variables in lvalue (for complex lvalues like array indices)
                        changed |= Self::replace_var_in_lvalue(lvalue, copies);
                    }
                }
            }
        }

        changed
    }
}
