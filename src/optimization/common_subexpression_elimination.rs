use crate::cfg::{
    CfgProgram, FieldId, FunctionId, LValue, Operand, Rvalue, Statement, TableId, VarId,
};
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

    /// Normalize an rvalue by following copy chains
    fn normalize_rvalue(&self, rvalue: &Rvalue, copy_map: &HashMap<VarId, VarId>) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => Rvalue::Use(self.normalize_operand(operand, copy_map)),
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
                    .map(|pk_value| self.normalize_operand(pk_value, copy_map))
                    .collect(),
                field: *field,
            },
            Rvalue::ArrayAccess { array, index } => Rvalue::ArrayAccess {
                array: self.normalize_operand(array, copy_map),
                index: self.normalize_operand(index, copy_map),
            },
            Rvalue::UnaryOp { op, operand } => Rvalue::UnaryOp {
                op: op.clone(),
                operand: self.normalize_operand(operand, copy_map),
            },
            Rvalue::BinaryOp { op, left, right } => Rvalue::BinaryOp {
                op: op.clone(),
                left: self.normalize_operand(left, copy_map),
                right: self.normalize_operand(right, copy_map),
            },
        }
    }

    /// Normalize an operand by following copy chains
    fn normalize_operand(&self, operand: &Operand, copy_map: &HashMap<VarId, VarId>) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                // Follow the copy chain to get the ultimate source
                let mut current_var = *var_id;
                let mut seen = HashSet::new();

                while let Some(&source_var) = copy_map.get(&current_var) {
                    if !seen.insert(current_var) {
                        // Cycle detected, break
                        break;
                    }
                    current_var = source_var;
                }

                Operand::Var(current_var)
            }
            Operand::Const(c) => Operand::Const(c.clone()),
        }
    }
}

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &'static str {
        "Common Subexpression Elimination"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        // Get function reference for analysis
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        eprintln!("        COMMON SUBEXPRESSION ELIMINATION DEBUG:");

        let mut changed = false;

        // First, build a copy propagation map to normalize expressions
        // This maps variables to their simple copy sources (var = other_var assignments)
        let mut copy_map: HashMap<VarId, VarId> = HashMap::new();

        // Process each basic block to find copy assignments
        for &block_id in &func.blocks {
            if let Some(block) = program.blocks.get(block_id) {
                for stmt in &block.statements {
                    if let Statement::Assign {
                        lvalue: LValue::Variable { var },
                        rvalue: Rvalue::Use(Operand::Var(source_var)),
                        ..
                    } = stmt
                    {
                        // This is a simple copy: var = source_var
                        // Follow the copy chain to get the ultimate source
                        let mut ultimate_source = *source_var;
                        while let Some(&next_source) = copy_map.get(&ultimate_source) {
                            if next_source == ultimate_source {
                                break; // Avoid cycles
                            }
                            ultimate_source = next_source;
                        }
                        copy_map.insert(*var, ultimate_source);
                        eprintln!("        Copy: {:?} -> {:?}", var, ultimate_source);
                    }
                }
            }
        }

        eprintln!("        Built copy map with {} entries", copy_map.len());

        // Track normalized expressions to their defining variables
        let mut expr_to_var: HashMap<Rvalue, VarId> = HashMap::new();

        // Process each basic block
        let function_blocks = func.blocks.clone();
        for block_id in function_blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                eprintln!(
                    "        Block {:?}: {} statements",
                    block_id,
                    block.statements.len()
                );

                let mut new_statements = Vec::new();

                for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                    let Statement::Assign {
                        lvalue,
                        rvalue,
                        span,
                    } = stmt;

                    match lvalue {
                        LValue::Variable { var } => {
                            // Normalize the expression using copy propagation
                            let normalized_rvalue = self.normalize_rvalue(rvalue, &copy_map);

                            // Check if this normalized expression is already available
                            if self.is_cse_candidate(&normalized_rvalue) {
                                if let Some(&existing_var) = expr_to_var.get(&normalized_rvalue) {
                                    eprintln!("        [{}] CSE OPTIMIZATION: {:?} = {:?} -> {:?} = Use({:?}) [normalized: {:?}]", 
                                             stmt_idx, lvalue, rvalue, lvalue, existing_var, normalized_rvalue);

                                    // Replace with a simple variable use
                                    let new_stmt = Statement::Assign {
                                        lvalue: lvalue.clone(),
                                        rvalue: Rvalue::Use(Operand::Var(existing_var)),
                                        span: span.clone(),
                                    };
                                    new_statements.push(new_stmt);
                                    changed = true;
                                    continue;
                                }

                                // Track this normalized expression
                                eprintln!(
                                    "        [{}] Tracking CSE candidate: {:?} -> {:?} [normalized: {:?}]",
                                    stmt_idx, rvalue, var, normalized_rvalue
                                );
                                expr_to_var.insert(normalized_rvalue, *var);
                            }

                            eprintln!("        [{}] KEEP: {:?} = {:?}", stmt_idx, lvalue, rvalue);

                            // Keep the original statement (not the normalized one, to preserve semantics)
                            new_statements.push(stmt.clone());
                        }
                        LValue::ArrayElement { .. } | LValue::TableField { .. } => {
                            // Array and table assignments always kept as-is
                            eprintln!(
                                "        [{}] KEEP: {:?} = {:?} (array/table write)",
                                stmt_idx, lvalue, rvalue
                            );
                            new_statements.push(stmt.clone());
                        }
                    }
                }

                // Update the block with the new statements
                block.statements = new_statements;
            }
        }

        changed
    }
}
