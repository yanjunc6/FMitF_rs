//! Constant Propagation Optimization Pass
//!
//! This pass uses constant analysis dataflow results to replace variables
//! that are known to have constant values with their constant values.

use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, Operand, Rvalue, Statement};
use crate::dataflow::{
    analyze_constants, ConstantMapData, ConstantMapLattice, StmtLoc,
};
use std::collections::HashMap;

pub struct ConstantPropagationPass;

impl ConstantPropagationPass {
    pub fn new() -> Self {
        Self
    }

    /// Convert constant lattice results to a simpler map format
    fn extract_constants(lattice_result: &ConstantMapLattice) -> ConstantMapData {
        if let Some(bindings) = lattice_result.as_set() {
            bindings
                .iter()
                .map(|binding| (binding.var, binding.value.clone()))
                .collect()
        } else {
            HashMap::new()
        }
    }

    /// Replace operands with constants if available
    fn propagate_in_operand(&self, operand: &Operand, constants: &ConstantMapData) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                if let Some(constant) = constants.get(var_id) {
                    Operand::Const(constant.clone())
                } else {
                    operand.clone()
                }
            }
            Operand::Const(_) => operand.clone(),
        }
    }

    /// Propagate constants in an rvalue
    fn propagate_in_rvalue(&self, rvalue: &Rvalue, constants: &ConstantMapData) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => Rvalue::Use(self.propagate_in_operand(operand, constants)),
            Rvalue::BinaryOp { op, left, right } => Rvalue::BinaryOp {
                op: op.clone(),
                left: self.propagate_in_operand(left, constants),
                right: self.propagate_in_operand(right, constants),
            },
            Rvalue::UnaryOp { op, operand } => Rvalue::UnaryOp {
                op: op.clone(),
                operand: self.propagate_in_operand(operand, constants),
            },
            Rvalue::ArrayAccess { array, index } => Rvalue::ArrayAccess {
                array: self.propagate_in_operand(array, constants),
                index: self.propagate_in_operand(index, constants),
            },
            Rvalue::TableAccess {
                table,
                pk_fields,
                pk_values,
                field,
            } => Rvalue::TableAccess {
                table: *table,
                pk_fields: pk_fields.clone(),
                pk_values: pk_values
                    .iter()
                    .map(|operand| self.propagate_in_operand(operand, constants))
                    .collect(),
                field: *field,
            },
        }
    }

    /// Propagate constants in an lvalue (for array indices and table primary key values)
    fn propagate_in_lvalue(
        &self,
        lvalue: &crate::cfg::LValue,
        constants: &ConstantMapData,
    ) -> crate::cfg::LValue {
        match lvalue {
            crate::cfg::LValue::Variable { var } => crate::cfg::LValue::Variable { var: *var },
            crate::cfg::LValue::ArrayElement { array, index } => {
                crate::cfg::LValue::ArrayElement {
                    array: *array,
                    index: self.propagate_in_operand(index, constants),
                }
            }
            crate::cfg::LValue::TableField {
                table,
                pk_fields,
                pk_values,
                field,
            } => crate::cfg::LValue::TableField {
                table: *table,
                pk_fields: pk_fields.clone(),
                pk_values: pk_values
                    .iter()
                    .map(|operand| self.propagate_in_operand(operand, constants))
                    .collect(),
                field: *field,
            },
        }
    }
}

impl OptimizationPass for ConstantPropagationPass {
    fn name(&self) -> &'static str {
        "Constant Propagation"
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

        // Run constant analysis
        let results = analyze_constants(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.blocks {
            let block = &mut program.blocks[block_id];

            // Process each statement
            for (stmt_idx, stmt) in block.statements.iter_mut().enumerate() {
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: stmt_idx,
                };

                // Get constants available before this statement
                if let Some(lattice_result) = results.stmt_entry.get(&stmt_loc) {
                    let constants = Self::extract_constants(lattice_result);

                    // Apply constant propagation to the statement
                    match stmt {
                        Statement::Assign { lvalue, rvalue, .. } => {
                            let new_lvalue = self.propagate_in_lvalue(lvalue, &constants);
                            let new_rvalue = self.propagate_in_rvalue(rvalue, &constants);

                            if new_lvalue != *lvalue || new_rvalue != *rvalue {
                                *lvalue = new_lvalue;
                                *rvalue = new_rvalue;
                                changed = true;
                            }
                        }
                    }
                }
            }

            // Process conditional edges
            for edge in block.successors.iter_mut() {
                if let crate::cfg::EdgeType::ConditionalTrue { condition }
                | crate::cfg::EdgeType::ConditionalFalse { condition } = &mut edge.edge_type
                {
                    // Get constants available at block exit
                    if let Some(lattice_result) = results.block_exit.get(&block_id) {
                        let constants = Self::extract_constants(lattice_result);
                        let new_condition = self.propagate_in_operand(condition, &constants);

                        if new_condition != *condition {
                            *condition = new_condition;
                            changed = true;
                        }
                    }
                }

                if let crate::cfg::EdgeType::Return { value: Some(return_val) } = &mut edge.edge_type
                {
                    // Get constants available at block exit
                    if let Some(lattice_result) = results.block_exit.get(&block_id) {
                        let constants = Self::extract_constants(lattice_result);
                        let new_return_val = self.propagate_in_operand(return_val, &constants);

                        if new_return_val != *return_val {
                            *return_val = new_return_val;
                            changed = true;
                        }
                    }
                }
            }
        }

        changed
    }
}
