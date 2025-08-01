use crate::cfg::{FieldId, FunctionCfg, Operand, Rvalue, Statement, TableId, VarId};
use crate::dataflow::{analyze_available_expressions, AnalysisLevel};
use crate::optimization::OptimizationPass;
use std::collections::{HashMap, HashSet};

/// Common Subexpression Elimination optimization pass
///
/// This pass identifies computations that are performed multiple times
/// and replaces subsequent computations with references to the first result.
pub struct CommonSubexpressionEliminationPass;

impl CommonSubexpressionEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Check if an rvalue is a candidate for CSE
    /// Only complex expressions that are worth optimizing should be candidates
    fn is_cse_candidate(&self, rvalue: &Rvalue) -> bool {
        match rvalue {
            // Binary and unary operations are good CSE candidates
            Rvalue::BinaryOp { .. } | Rvalue::UnaryOp { .. } => true,
            // Table accesses can be CSE candidates too (expensive operations)
            Rvalue::TableAccess { .. } => true,
            // Array accesses could be optimized too
            Rvalue::ArrayAccess { .. } => true,
            // Simple uses don't need CSE
            Rvalue::Use(_) => false,
        }
    }

    /// Check if an expression uses a specific variable
    fn expr_uses_var(&self, rvalue: &Rvalue, var_id: VarId) -> bool {
        match rvalue {
            Rvalue::Use(operand) => self.operand_uses_var(operand, var_id),
            Rvalue::TableAccess { pk_values, .. } => pk_values
                .iter()
                .any(|pk_value| self.operand_uses_var(pk_value, var_id)),
            Rvalue::ArrayAccess { array, index } => {
                self.operand_uses_var(array, var_id) || self.operand_uses_var(index, var_id)
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

    /// Kill expressions that read from a specific table/field
    fn kill_table_expressions(
        &self,
        available: &mut HashSet<Rvalue>,
        table_id: TableId,
        field_id: FieldId,
    ) {
        available.retain(|expr| match expr {
            Rvalue::TableAccess { table, field, .. } => !(*table == table_id && *field == field_id),
            _ => true,
        });
    }

    /// Update available expressions after an assignment
    /// Remove expressions that use the defined variable and add the new expression
    fn update_available_after_assign(
        &self,
        available: &mut HashSet<Rvalue>,
        defined_var: VarId,
        rvalue: &Rvalue,
    ) {
        // Kill expressions that use the defined variable
        available.retain(|expr| !self.expr_uses_var(expr, defined_var));

        // Add the new expression if it's a CSE candidate
        if self.is_cse_candidate(rvalue) {
            available.insert(rvalue.clone());
        }
    }
}

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &'static str {
        "Common Subexpression Elimination"
    }

    fn optimize_function(&self, func: &mut FunctionCfg) -> bool {
        let available_expr_results = analyze_available_expressions(func, AnalysisLevel::Function);
        let mut changed = false;

        // Track expressions to their defining variables across the entire function
        let mut expr_to_var: HashMap<Rvalue, VarId> = HashMap::new();

        // Process each basic block
        for (block_id, block) in func.blocks.iter_mut() {
            // Get available expressions at entry of this block
            let available_at_entry =
                if let Some(lattice) = available_expr_results.entry.get(&block_id) {
                    if let Some(set) = lattice.as_set() {
                        set.clone()
                    } else {
                        HashSet::new()
                    }
                } else {
                    HashSet::new()
                };

            // Track available expressions through this block
            let mut current_available = available_at_entry;
            let mut new_statements = Vec::new();

            for stmt in &block.statements {
                match stmt {
                    Statement::Assign { var, rvalue, span } => {
                        // Check if this expression is already available and we have a variable for it
                        if self.is_cse_candidate(rvalue) && current_available.contains(rvalue) {
                            if let Some(&existing_var) = expr_to_var.get(rvalue) {
                                // Replace with a simple variable use
                                let new_stmt = Statement::Assign {
                                    var: *var,
                                    rvalue: Rvalue::Use(Operand::Var(existing_var)),
                                    span: span.clone(),
                                };
                                new_statements.push(new_stmt);
                                changed = true;

                                // Update available expressions (the assignment itself doesn't change availability)
                                self.update_available_after_assign(
                                    &mut current_available,
                                    *var,
                                    &Rvalue::Use(Operand::Var(existing_var)),
                                );
                                continue;
                            }
                        }

                        // Keep the original statement
                        new_statements.push(stmt.clone());

                        // Track this expression if it's a CSE candidate
                        if self.is_cse_candidate(rvalue) {
                            expr_to_var.insert(rvalue.clone(), *var);
                        }

                        // Update available expressions
                        self.update_available_after_assign(&mut current_available, *var, rvalue);
                    }
                    Statement::TableAssign { table, field, .. } => {
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
