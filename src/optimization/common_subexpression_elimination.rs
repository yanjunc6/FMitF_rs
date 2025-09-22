use super::OptimizationPass;
use crate::cfg::{FunctionId, Instruction, Operand, Program, VariableId};
use crate::dataflow::{analyze_available_expressions, AvailExpr, ExprKind, StmtLoc};
use std::collections::HashSet;

pub struct CommonSubexpressionElimination;

impl CommonSubexpressionElimination {
    pub fn new() -> Self {
        Self
    }

    /// Find a variable that holds the result of the same computation
    fn find_existing_var(
        &self,
        target_expr: &ExprKind,
        available: &HashSet<AvailExpr>,
    ) -> Option<VariableId> {
        for avail_expr in available {
            if avail_expr.op == *target_expr {
                return Some(avail_expr.dest);
            }
        }
        None
    }

    /// Check if an expression is worth optimizing (has computation cost)
    fn is_worth_optimizing(&self, expr: &ExprKind) -> bool {
        match expr {
            ExprKind::BinaryOp { .. } => true,
            ExprKind::UnaryOp { .. } => true,
            ExprKind::Call { .. } => true,
            ExprKind::Use(_) => false, // Simple variable use doesn't need CSE
        }
    }

    /// Create an expression kind from an instruction for comparison
    fn extract_expr_kind(&self, inst: &Instruction) -> Option<ExprKind> {
        match inst {
            Instruction::BinaryOp {
                op, left, right, ..
            } => Some(ExprKind::BinaryOp {
                op: *op,
                left: left.clone(),
                right: right.clone(),
            }),
            Instruction::UnaryOp { op, operand, .. } => Some(ExprKind::UnaryOp {
                op: *op,
                operand: operand.clone(),
            }),
            Instruction::Call { func, args, .. } => Some(ExprKind::Call {
                func: *func,
                args: args.clone(),
            }),
            Instruction::Assign { src, .. } => Some(ExprKind::Use(src.clone())),
            _ => None, // TableGet, TableSet, Assert don't participate in CSE
        }
    }
}

impl OptimizationPass for CommonSubexpressionElimination {
    fn name(&self) -> &'static str {
        "common-subexpression-elimination"
    }

    fn optimize_function(&self, program: &mut Program, func_id: FunctionId) -> bool {
        let function = &program.functions[func_id];

        // Skip abstract functions (like operators)
        if matches!(function.kind, crate::cfg::FunctionKind::Operator) {
            return false;
        }

        // Run available expressions analysis
        let results = analyze_available_expressions(function, program);
        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.all_blocks {
            let block = &mut program.basic_blocks[block_id];

            // Process each instruction
            for (inst_idx, inst) in block.instructions.iter_mut().enumerate() {
                let stmt_loc = StmtLoc {
                    block: block_id,
                    index: inst_idx,
                };

                // Get available expressions before this instruction
                if let Some(lattice_result) = results.stmt_entry.get(&stmt_loc) {
                    // Extract available expressions from lattice
                    let available = if let Some(exprs) = lattice_result.as_set() {
                        exprs.clone()
                    } else {
                        continue; // Top element - can't analyze
                    };

                    // Apply CSE to the instruction
                    if let Some(expr_kind) = self.extract_expr_kind(inst) {
                        // Only try to optimize computations that are worth it
                        if self.is_worth_optimizing(&expr_kind) {
                            // Check if this computation is available
                            if let Some(existing_var) =
                                self.find_existing_var(&expr_kind, &available)
                            {
                                // Replace the computation with a use of the existing variable
                                match inst {
                                    Instruction::BinaryOp { dest, .. } => {
                                        *inst = Instruction::Assign {
                                            dest: *dest,
                                            src: Operand::Variable(existing_var),
                                        };
                                        changed = true;
                                    }
                                    Instruction::UnaryOp { dest, .. } => {
                                        *inst = Instruction::Assign {
                                            dest: *dest,
                                            src: Operand::Variable(existing_var),
                                        };
                                        changed = true;
                                    }
                                    Instruction::Call {
                                        dest: Some(dest), ..
                                    } => {
                                        let dest_var = *dest;
                                        *inst = Instruction::Assign {
                                            dest: dest_var,
                                            src: Operand::Variable(existing_var),
                                        };
                                        changed = true;
                                    }
                                    _ => {} // Other instructions don't participate in CSE
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
