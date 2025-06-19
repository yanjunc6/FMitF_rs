use crate::cfg::{FunctionCfg, Operand, Rvalue, Statement, VarId};
use crate::dataflow::analyze_available_expressions;
use crate::optimization::OptimizationPass;
use std::collections::HashMap;

/// Common Subexpression Elimination optimization pass
pub struct CommonSubexpressionEliminationPass;

// Implement the OptimizationPass trait
impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &'static str {
        "Common Subexpression Elimination"
    }

    fn optimize_function(&self, func: &mut FunctionCfg) -> bool {
        let available_expr_results = analyze_available_expressions(func);
        let mut changed = false;

        // Track expressions to their defining variables
        let mut expr_to_var: HashMap<Rvalue, VarId> = HashMap::new();

        // Process each basic block
        for (block_id, block) in func.blocks.iter_mut() {
            // Get available expressions at entry of this block
            let empty_set = std::collections::HashSet::new();
            let available_at_entry = available_expr_results
                .entry
                .get(&block_id)
                .map(|set| &set.set)
                .unwrap_or(&empty_set);

            // Initialize available expressions for this block
            let mut current_available = available_at_entry.clone();

            // Process statements in the block
            let mut new_statements = Vec::new();

            for stmt in &block.statements {
                match stmt {
                    Statement::Assign { var, rvalue, span } => {
                        // Check if this rvalue is already available
                        if let Some(&existing_var) = expr_to_var.get(rvalue) {
                            // If the expression is available and we have a variable for it,
                            // replace with a simple use
                            if current_available.contains(rvalue) {
                                new_statements.push(Statement::Assign {
                                    var: *var,
                                    rvalue: Rvalue::Use(Operand::Var(existing_var)),
                                    span: span.clone(),
                                });
                                changed = true;
                            } else {
                                // Expression not available, keep original and update tracking
                                new_statements.push(stmt.clone());
                                expr_to_var.insert(rvalue.clone(), *var);
                                self.update_available_after_assign(
                                    &mut current_available,
                                    *var,
                                    rvalue,
                                );
                            }
                        } else {
                            // First time seeing this expression
                            new_statements.push(stmt.clone());

                            // Only track expressions that can be CSE candidates
                            if self.is_cse_candidate(rvalue) {
                                expr_to_var.insert(rvalue.clone(), *var);
                            }

                            self.update_available_after_assign(
                                &mut current_available,
                                *var,
                                rvalue,
                            );
                        }
                    }
                    Statement::TableAssign {
                        table,
                        pk_fields: _,
                        pk_values: _,
                        field,
                        value: _,
                        span: _,
                    } => {
                        // Table assignments can invalidate expressions that read from the same table/field
                        self.kill_table_expressions(&mut current_available, *table, *field);
                        new_statements.push(stmt.clone());
                    }
                }
            }

            // Update the block with optimized statements
            block.statements = new_statements;
        }

        changed
    }
}

impl CommonSubexpressionEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Check if an rvalue is a candidate for CSE
    fn is_cse_candidate(&self, rvalue: &Rvalue) -> bool {
        match rvalue {
            // Binary and unary operations are good CSE candidates
            Rvalue::BinaryOp { .. } | Rvalue::UnaryOp { .. } => true,
            // Table accesses can be CSE candidates too
            Rvalue::TableAccess { .. } => true,
            // Simple uses don't need CSE
            Rvalue::Use(_) => false,
        }
    }

    /// Update available expressions after an assignment
    fn update_available_after_assign(
        &self,
        available: &mut std::collections::HashSet<Rvalue>,
        defined_var: VarId,
        rvalue: &Rvalue,
    ) {
        // Kill expressions that use the defined variable
        available.retain(|expr| !self.expr_uses_var(expr, defined_var));

        // Add the new expression if it's trackable
        if self.is_cse_candidate(rvalue) {
            available.insert(rvalue.clone());
        }
    }

    /// Kill expressions that read from a specific table/field
    fn kill_table_expressions(
        &self,
        available: &mut std::collections::HashSet<Rvalue>,
        table_id: crate::cfg::TableId,
        field_id: crate::cfg::FieldId,
    ) {
        available.retain(|expr| match expr {
            Rvalue::TableAccess { table, field, .. } => !(*table == table_id && *field == field_id),
            _ => true,
        });
    }

    /// Check if an expression uses a specific variable
    fn expr_uses_var(&self, rvalue: &Rvalue, var_id: VarId) -> bool {
        match rvalue {
            Rvalue::Use(operand) => self.operand_uses_var(operand, var_id),
            Rvalue::TableAccess { pk_values, .. } => {
                pk_values.iter().any(|pk_value| self.operand_uses_var(pk_value, var_id))
            }
            Rvalue::UnaryOp { operand, .. } => self.operand_uses_var(operand, var_id),
            Rvalue::BinaryOp { left, right, .. } => {
                self.operand_uses_var(left, var_id) || self.operand_uses_var(right, var_id)
            }
        }
    }

    /// Check if an operand uses a specific variable
    fn operand_uses_var(&self, operand: &Operand, var_id: VarId) -> bool {
        match operand {
            Operand::Var(v) => *v == var_id,
            Operand::Const(_) => false,
        }
    }
}
