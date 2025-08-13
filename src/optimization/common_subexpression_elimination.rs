use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, RValue, Statement, VarId};
use crate::dataflow::{analyze_available_expressions, AvailExpr, StmtLoc};
use std::collections::HashSet;

pub struct CommonSubexpressionElimination;

impl CommonSubexpressionElimination {
    pub fn new() -> Self {
        Self
    }

    /// Find a variable that holds the result of the same computation
    fn find_existing_var(
        &self,
        target_rvalue: &RValue,
        available: &HashSet<AvailExpr>,
    ) -> Option<VarId> {
        for avail_expr in available {
            // Use the new accessor method
            if avail_expr.rvalue == target_rvalue.clone() {
                // Check if the lvalue is a simple variable
                if let LValue::Variable { var } = avail_expr.lvalue {
                    return Some(var);
                }
            }
        }
        None
    }

    /// Check if an RValue is worth optimizing (has computation cost)
    fn is_worth_optimizing(&self, rvalue: &RValue) -> bool {
        match rvalue {
            RValue::BinaryOp { .. } => true,
            RValue::UnaryOp { .. } => true,
            RValue::ArrayAccess { .. } => true,
            RValue::TableAccess { .. } => true,
            RValue::Use(_) => false, // Simple variable use doesn't need CSE
        }
    }
}

impl OptimizationPass for CommonSubexpressionElimination {
    fn name(&self) -> &'static str {
        "common-subexpression-elimination"
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

        // Run available expressions analysis
        let results = analyze_available_expressions(function, program);
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

                // Get available expressions before this statement
                if let Some(lattice_result) = results.stmt_entry.get(&stmt_loc) {
                    // Extract available expressions from lattice
                    let available = if let Some(exprs) = lattice_result.as_set() {
                        exprs.clone()
                    } else {
                        continue; // Top element - can't analyze
                    };

                    // Apply CSE to the statement
                    match stmt {
                        Statement::Assign { rvalue, .. } => {
                            // Only try to optimize computations that are worth it
                            if self.is_worth_optimizing(rvalue) {
                                // Check if this computation is available
                                if let Some(existing_var) =
                                    self.find_existing_var(rvalue, &available)
                                {
                                    // Replace the computation with a use of the existing variable
                                    *rvalue = RValue::Use(Operand::Var(existing_var));
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        changed
    }
}
