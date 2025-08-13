//! Common Subexpression Elimination Optimization Pass
//!
//! This pass identifies and eliminates redundant computations by reusing
//! previously computed values when the same expression is encountered again.

use super::OptimizationPass;
use crate::cfg::{CfgProgram, FunctionId, Operand, RValue, Statement, VarId};
use crate::dataflow::available_expressions::AvailExpr;
use crate::dataflow::{analyze_available_expressions, StmtLoc};
use std::collections::{HashMap, HashSet};

pub struct CommonSubexpressionEliminationPass;

impl CommonSubexpressionEliminationPass {
    /// Extract available expressions from lattice result
    fn extract_available_exprs(
        lattice_result: &crate::dataflow::SetLattice<AvailExpr>,
    ) -> HashSet<AvailExpr> {
        if let Some(exprs) = lattice_result.as_set() {
            exprs.clone()
        } else {
            HashSet::new()
        }
    }

    /// Convert an RValue to an AvailExpr if possible
    fn rvalue_to_avail_expr(&self, rvalue: &RValue) -> Option<AvailExpr> {
        match rvalue {
            RValue::BinaryOp { op, left, right } => Some(AvailExpr::BinaryOp {
                op: op.clone(),
                left: left.clone(),
                right: right.clone(),
            }),
            RValue::UnaryOp { op, operand } => Some(AvailExpr::UnaryOp {
                op: op.clone(),
                operand: operand.clone(),
            }),
            RValue::ArrayAccess { array, index } => Some(AvailExpr::ArrayAccess {
                array: array.clone(),
                index: index.clone(),
            }),
            _ => None, // Use operations and table access don't need CSE
        }
    }

    /// Check if an expression matches an available expression and return the variable that holds it
    fn find_existing_computation(
        &self,
        expr: &AvailExpr,
        available: &HashSet<AvailExpr>,
        expr_to_var: &HashMap<AvailExpr, VarId>,
    ) -> Option<VarId> {
        if available.contains(expr) {
            expr_to_var.get(expr).copied()
        } else {
            None
        }
    }

    /// Create a mapping from expressions to the variables that hold their results
    fn build_expr_to_var_map(
        &self,
        program: &CfgProgram,
        func_id: FunctionId,
    ) -> HashMap<AvailExpr, VarId> {
        let mut expr_to_var = HashMap::new();
        let function = &program.functions[func_id];

        // Scan all statements to build the mapping
        for &block_id in &function.blocks {
            let block = &program.blocks[block_id];
            for stmt in &block.statements {
                match stmt {
                    Statement::Assign { lvalue, rvalue, .. } => {
                        if let crate::cfg::LValue::Variable { var } = lvalue {
                            if let Some(expr) = self.rvalue_to_avail_expr(rvalue) {
                                expr_to_var.insert(expr, *var);
                            }
                        }
                    }
                }
            }
        }

        expr_to_var
    }
}

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn new() -> Self {
        Self
    }

    fn name(&self) -> &'static str {
        "Common Subexpression Elimination"
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

        // Build expression to variable mapping
        let expr_to_var = self.build_expr_to_var_map(program, func_id);

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
                    let available = Self::extract_available_exprs(lattice_result);

                    // Apply CSE to the statement
                    match stmt {
                        Statement::Assign { rvalue, .. } => {
                            if let Some(expr) = self.rvalue_to_avail_expr(rvalue) {
                                // Check if this expression is available
                                if let Some(existing_var) =
                                    self.find_existing_computation(&expr, &available, &expr_to_var)
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
