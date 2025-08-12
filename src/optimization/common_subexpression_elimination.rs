use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{analyze_available_expressions, AnalysisLevel, StmtLoc};
use crate::optimization::OptimizationPass;
use std::collections::HashMap;

/// Common Subexpression Elimination optimization pass
///
/// Uses available expressions analysis to identify redundant computations
/// and replace them with temporaries holding the previously computed values.
pub struct CommonSubexpressionEliminationPass;

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &'static str {
        "Common Subexpression Elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        // Run available expressions analysis
        let available = analyze_available_expressions(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Map expressions to variables that compute them
        let mut expr_to_var: HashMap<Rvalue, VarId> = HashMap::new();

        // Process each block
        for &block_id in &func.blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                let mut new_statements = Vec::new();

                for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                    let stmt_loc = StmtLoc {
                        block: block_id,
                        index: stmt_idx,
                    };

                    match stmt {
                        Statement::Assign {
                            lvalue,
                            rvalue,
                            span,
                        } => {
                            match lvalue {
                                LValue::Variable { var } => {
                                    // Check if this expression is available
                                    if Self::is_cse_candidate(rvalue) {
                                        if let Some(avail_exprs) =
                                            available.stmt_entry.get(&stmt_loc)
                                        {
                                            if let Some(expr_set) = avail_exprs.as_set() {
                                                if expr_set.contains(rvalue) {
                                                    // This expression is available - find the variable that computes it
                                                    if let Some(&existing_var) =
                                                        expr_to_var.get(rvalue)
                                                    {
                                                        // Replace with simple copy
                                                        let new_stmt = Statement::Assign {
                                                            lvalue: lvalue.clone(),
                                                            rvalue: Rvalue::Use(Operand::Var(
                                                                existing_var,
                                                            )),
                                                            span: span.clone(),
                                                        };
                                                        new_statements.push(new_stmt);
                                                        changed = true;
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // Track this expression
                                    if Self::is_cse_candidate(rvalue) {
                                        expr_to_var.insert(rvalue.clone(), *var);
                                    }
                                }
                                _ => {
                                    // Array/table assignments are kept as-is
                                }
                            }
                        }
                    }

                    new_statements.push(stmt.clone());
                }

                block.statements = new_statements;
            }
        }

        changed
    }
}

impl CommonSubexpressionEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Check if an rvalue is a candidate for CSE
    fn is_cse_candidate(rvalue: &Rvalue) -> bool {
        match rvalue {
            // Only complex expressions are worth CSE
            Rvalue::BinaryOp { .. } | Rvalue::UnaryOp { .. } => true,
            Rvalue::TableAccess { .. } | Rvalue::ArrayAccess { .. } => true,
            Rvalue::Use(_) => false, // Simple variable uses don't need CSE
        }
    }
}
