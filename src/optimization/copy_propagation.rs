//! Copy Propagation Optimization Pass
//!
//! This pass replaces uses of variables with their copies when a copy relation exists.
//! For example, if we have `x = y`, subsequent uses of `x` can be replaced with `y`.

use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, Operand, RValue, Statement};
use crate::dataflow::{analyze_copies, CopyMapLattice, CopyRelation, StmtLoc};
use std::collections::HashSet;

pub struct CopyPropagation;

impl CopyPropagation {
    pub fn new() -> Self {
        Self
    }

    /// Extract copy relations from lattice result
    fn extract_copies(lattice_result: &CopyMapLattice) -> HashSet<CopyRelation> {
        if let Some(copies) = lattice_result.as_set() {
            copies.clone()
        } else {
            HashSet::new()
        }
    }

    /// Find the transitive copy for a variable
    fn find_copy_source(
        &self,
        var: crate::cfg::VarId,
        copies: &HashSet<CopyRelation>,
    ) -> crate::cfg::VarId {
        let mut current = var;
        let mut visited = HashSet::new();

        // Follow copy chain until we find the source or detect a cycle
        while visited.insert(current) {
            if let Some(copy_rel) = copies.iter().find(|rel| rel.lhs == current) {
                current = copy_rel.rhs;
            } else {
                break;
            }
        }

        current
    }

    /// Replace operands with their copy sources if available
    fn propagate_in_operand(&self, operand: &Operand, copies: &HashSet<CopyRelation>) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                let source = self.find_copy_source(*var_id, copies);
                if source != *var_id {
                    Operand::Var(source)
                } else {
                    operand.clone()
                }
            }
            Operand::Const(_) => operand.clone(),
        }
    }

    /// Propagate copies in an rvalue
    fn propagate_in_rvalue(&self, rvalue: &RValue, copies: &HashSet<CopyRelation>) -> RValue {
        match rvalue {
            RValue::Use(operand) => RValue::Use(self.propagate_in_operand(operand, copies)),
            RValue::BinaryOp { op, left, right } => RValue::BinaryOp {
                op: op.clone(),
                left: self.propagate_in_operand(left, copies),
                right: self.propagate_in_operand(right, copies),
            },
            RValue::UnaryOp { op, operand } => RValue::UnaryOp {
                op: op.clone(),
                operand: self.propagate_in_operand(operand, copies),
            },
            RValue::ArrayAccess { array, index } => RValue::ArrayAccess {
                array: self.propagate_in_operand(array, copies),
                index: self.propagate_in_operand(index, copies),
            },
            RValue::TableAccess {
                table,
                pk_fields,
                pk_values,
                field,
            } => RValue::TableAccess {
                table: *table,
                pk_fields: pk_fields.clone(),
                pk_values: pk_values
                    .iter()
                    .map(|operand| self.propagate_in_operand(operand, copies))
                    .collect(),
                field: *field,
            },
        }
    }

    /// Propagate copies in an lvalue (for array indices and table primary key values)
    fn propagate_in_lvalue(
        &self,
        lvalue: &crate::cfg::LValue,
        copies: &HashSet<CopyRelation>,
    ) -> crate::cfg::LValue {
        match lvalue {
            crate::cfg::LValue::Variable { var } => crate::cfg::LValue::Variable { var: *var },
            crate::cfg::LValue::ArrayElement { array, index } => crate::cfg::LValue::ArrayElement {
                array: *array,
                index: self.propagate_in_operand(index, copies),
            },
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
                    .map(|operand| self.propagate_in_operand(operand, copies))
                    .collect(),
                field: *field,
            },
        }
    }
}

impl OptimizationPass for CopyPropagation {
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

        // Run copy analysis
        let results = analyze_copies(function, program);
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

                // Get copies available before this statement
                if let Some(lattice_result) = results.stmt_entry.get(&stmt_loc) {
                    let copies = Self::extract_copies(lattice_result);

                    // Apply copy propagation to the statement
                    match stmt {
                        Statement::Assign { lvalue, rvalue, .. } => {
                            let new_lvalue = self.propagate_in_lvalue(lvalue, &copies);
                            let new_rvalue = self.propagate_in_rvalue(rvalue, &copies);

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
                    // Get copies available at block exit
                    if let Some(lattice_result) = results.block_exit.get(&block_id) {
                        let copies = Self::extract_copies(lattice_result);
                        let new_condition = self.propagate_in_operand(condition, &copies);

                        if new_condition != *condition {
                            *condition = new_condition;
                            changed = true;
                        }
                    }
                }

                if let crate::cfg::EdgeType::Return {
                    value: Some(return_val),
                } = &mut edge.edge_type
                {
                    // Get copies available at block exit
                    if let Some(lattice_result) = results.block_exit.get(&block_id) {
                        let copies = Self::extract_copies(lattice_result);
                        let new_return_val = self.propagate_in_operand(return_val, &copies);

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
