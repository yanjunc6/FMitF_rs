use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{analyze_live_variables, AnalysisLevel};
use crate::optimization::OptimizationPass;
use std::collections::{HashMap, HashSet};

/// Dead Code Elimination optimization pass
///
/// This pass removes assignments to variables that are never used (dead code).
/// It uses backward live variables analysis to determine which variables are live.
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Add all variables used in an rvalue to the live set
    fn add_used_vars_from_rvalue(&self, rvalue: &Rvalue, live_vars: &mut HashSet<VarId>) {
        match rvalue {
            Rvalue::Use(operand) => {
                self.add_used_var_from_operand(operand, live_vars);
            }
            Rvalue::TableAccess { pk_values, .. } => {
                for pk_value in pk_values {
                    self.add_used_var_from_operand(pk_value, live_vars);
                }
            }
            Rvalue::ArrayAccess { array, index } => {
                self.add_used_var_from_operand(array, live_vars);
                self.add_used_var_from_operand(index, live_vars);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.add_used_var_from_operand(operand, live_vars);
            }
            Rvalue::BinaryOp { left, right, .. } => {
                self.add_used_var_from_operand(left, live_vars);
                self.add_used_var_from_operand(right, live_vars);
            }
        }
    }

    /// Add a variable from an operand to the live set
    fn add_used_var_from_operand(&self, operand: &Operand, live_vars: &mut HashSet<VarId>) {
        if let Operand::Var(var_id) = operand {
            live_vars.insert(*var_id);
        }
    }

    /// Substitute removed variables in a statement with their definitions
    fn substitute_variables_in_statement(
        &self,
        stmt: &Statement,
        substitution_map: &HashMap<VarId, Rvalue>,
    ) -> Statement {
        match stmt {
            Statement::Assign {
                lvalue,
                rvalue,
                span,
            } => {
                let new_lvalue = self.substitute_variables_in_lvalue(lvalue, substitution_map);
                let new_rvalue = self.substitute_variables_in_rvalue(rvalue, substitution_map);
                Statement::Assign {
                    lvalue: new_lvalue,
                    rvalue: new_rvalue,
                    span: span.clone(),
                }
            }
        }
    }

    /// Substitute variables in an lvalue
    fn substitute_variables_in_lvalue(
        &self,
        lvalue: &LValue,
        substitution_map: &HashMap<VarId, Rvalue>,
    ) -> LValue {
        match lvalue {
            LValue::Variable { var } => LValue::Variable { var: *var },
            LValue::TableField {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                let new_pk_values = pk_values
                    .iter()
                    .map(|operand| self.substitute_variables_in_operand(operand, substitution_map))
                    .collect();
                LValue::TableField {
                    table: *table,
                    pk_fields: pk_fields.clone(),
                    pk_values: new_pk_values,
                    field: *field,
                }
            }
            LValue::ArrayElement { array, index } => {
                let new_index = self.substitute_variables_in_operand(index, substitution_map);
                LValue::ArrayElement {
                    array: *array,
                    index: new_index,
                }
            }
        }
    }

    /// Substitute variables in an rvalue
    fn substitute_variables_in_rvalue(
        &self,
        rvalue: &Rvalue,
        substitution_map: &HashMap<VarId, Rvalue>,
    ) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => {
                Rvalue::Use(self.substitute_variables_in_operand(operand, substitution_map))
            }
            Rvalue::TableAccess {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                let new_pk_values = pk_values
                    .iter()
                    .map(|operand| self.substitute_variables_in_operand(operand, substitution_map))
                    .collect();
                Rvalue::TableAccess {
                    table: *table,
                    pk_fields: pk_fields.clone(),
                    pk_values: new_pk_values,
                    field: *field,
                }
            }
            Rvalue::ArrayAccess { array, index } => Rvalue::ArrayAccess {
                array: self.substitute_variables_in_operand(array, substitution_map),
                index: self.substitute_variables_in_operand(index, substitution_map),
            },
            Rvalue::UnaryOp { op, operand } => Rvalue::UnaryOp {
                op: op.clone(),
                operand: self.substitute_variables_in_operand(operand, substitution_map),
            },
            Rvalue::BinaryOp { op, left, right } => Rvalue::BinaryOp {
                op: op.clone(),
                left: self.substitute_variables_in_operand(left, substitution_map),
                right: self.substitute_variables_in_operand(right, substitution_map),
            },
        }
    }

    /// Substitute variables in an operand
    fn substitute_variables_in_operand(
        &self,
        operand: &Operand,
        substitution_map: &HashMap<VarId, Rvalue>,
    ) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                // If this variable was removed and we have a substitution, use it
                if let Some(replacement_rvalue) = substitution_map.get(var_id) {
                    // Only substitute if the replacement is a simple constant or variable use
                    match replacement_rvalue {
                        Rvalue::Use(replacement_operand) => replacement_operand.clone(),
                        _ => operand.clone(), // Don't substitute complex expressions for now
                    }
                } else {
                    operand.clone()
                }
            }
            Operand::Const(_) => operand.clone(),
        }
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "Dead Code Elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        // Get function reference for analysis
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        eprintln!("        DEAD CODE ELIMINATION DEBUG:");

        // Run live variables analysis
        let liveness_results = analyze_live_variables(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Process each block
        let function_blocks = func.blocks.clone();
        for block_id in function_blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                eprintln!(
                    "        Block {:?}: {} statements",
                    block_id,
                    block.statements.len()
                );

                // Get live variables at entry of this block
                let live_vars_at_entry =
                    if let Some(lattice) = liveness_results.entry.get(&block_id) {
                        lattice.as_set().unwrap_or(&HashSet::new()).clone()
                    } else {
                        HashSet::new()
                    };

                eprintln!("        Live vars at entry: {:?}", live_vars_at_entry);

                // Build a comprehensive variable definitions map for substitution
                let mut var_definitions: HashMap<VarId, Rvalue> = HashMap::new();
                for stmt in &block.statements {
                    let Statement::Assign { lvalue, rvalue, .. } = stmt;
                    if let LValue::Variable { var } = lvalue {
                        var_definitions.insert(*var, rvalue.clone());
                    }
                }

                // Use liveness analysis to determine which variables are actually live
                // We need to be more precise: simulate the liveness backwards through the block
                let mut current_live = if let Some(lattice) = liveness_results.exit.get(&block_id) {
                    lattice.as_set().unwrap_or(&HashSet::new()).clone()
                } else {
                    HashSet::new()
                };

                let mut statements_to_keep = Vec::new();
                let mut substitution_map: HashMap<VarId, Rvalue> = HashMap::new();

                // Process statements in reverse order to track liveness precisely
                for (stmt_idx, stmt) in block.statements.iter().enumerate().rev() {
                    let Statement::Assign { lvalue, rvalue, .. } = stmt;

                    let should_keep = match lvalue {
                        LValue::Variable { var } => {
                            // Check if this variable is live
                            if current_live.contains(var) {
                                eprintln!(
                                    "        [{}] KEEP: {:?} = {:?} (var {:?} is live)",
                                    stmt_idx, lvalue, rvalue, var
                                );
                                // Remove from live set (kill) and add used variables (gen)
                                current_live.remove(var);
                                self.add_used_vars_from_rvalue(rvalue, &mut current_live);
                                true
                            } else {
                                // Check if rvalue has side effects
                                let has_side_effects = matches!(rvalue, Rvalue::TableAccess { .. });
                                if has_side_effects {
                                    eprintln!(
                                        "        [{}] KEEP: {:?} = {:?} (has side effects)",
                                        stmt_idx, lvalue, rvalue
                                    );
                                    // Still need to add used variables to live set
                                    self.add_used_vars_from_rvalue(rvalue, &mut current_live);
                                    true
                                } else {
                                    eprintln!("        [{}] REMOVE: {:?} = {:?} (dead assignment, no side effects)", 
                                             stmt_idx, lvalue, rvalue);
                                    // Store the definition for substitution if needed later
                                    substitution_map.insert(*var, rvalue.clone());
                                    false
                                }
                            }
                        }
                        LValue::ArrayElement { .. } | LValue::TableField { .. } => {
                            // Array and table assignments always have side effects
                            eprintln!("        [{}] KEEP: {:?} = {:?} (array/table write, has side effects)", 
                                     stmt_idx, lvalue, rvalue);
                            // Add used variables to live set
                            self.add_used_vars_from_rvalue(rvalue, &mut current_live);
                            // For table field assignments, also add pk_values to live set
                            if let LValue::TableField { pk_values, .. } = lvalue {
                                for pk_value in pk_values {
                                    self.add_used_var_from_operand(pk_value, &mut current_live);
                                }
                            }
                            // For array element assignments, add index to live set
                            if let LValue::ArrayElement { index, .. } = lvalue {
                                self.add_used_var_from_operand(index, &mut current_live);
                            }
                            true
                        }
                    };

                    if should_keep {
                        statements_to_keep.push((stmt_idx, stmt.clone()));
                    }
                }

                // Reverse the statements back to original order
                statements_to_keep.reverse();

                // Now substitute any removed variable references in the remaining statements
                let mut final_statements = Vec::new();
                for (_original_idx, stmt) in statements_to_keep {
                    let substituted_stmt =
                        self.substitute_variables_in_statement(&stmt, &substitution_map);
                    final_statements.push(substituted_stmt);
                }

                // Update the block's statements if any were removed
                if final_statements.len() != block.statements.len() {
                    eprintln!(
                        "        Block {:?}: Removed {} statements ({} -> {})",
                        block_id,
                        block.statements.len() - final_statements.len(),
                        block.statements.len(),
                        final_statements.len()
                    );
                    block.statements = final_statements;
                    changed = true;
                } else {
                    eprintln!("        - No change: {} statements", block.statements.len());
                }
            }
        }

        changed
    }
}
