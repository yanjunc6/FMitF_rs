use crate::cfg::{BasicBlockId, Constant, FunctionCfg, Operand, Rvalue, Statement};
use crate::dataflow::{analyze_reaching_definitions, AnalysisLevel, ReachingDefinition};
use crate::optimization::OptimizationPass;
use std::collections::{HashMap, HashSet};

/// Constant Propagation optimization pass
pub struct ConstantPropagationPass;

impl OptimizationPass for ConstantPropagationPass {
    fn name(&self) -> &'static str {
        "Constant Propagation"
    }

    fn optimize_function(&self, func: &mut FunctionCfg) -> bool {
        let reaching_defs_results = analyze_reaching_definitions(func, AnalysisLevel::Function);
        let mut changed = false;

        // Build a map from (block_id, stmt_index) to their constant values (if any)
        let mut definition_to_constant: HashMap<(BasicBlockId, usize), Constant> = HashMap::new();

        // First pass: collect constant definitions from all blocks
        for (block_id, block) in func.blocks.iter() {
            for (stmt_index, stmt) in block.statements.iter().enumerate() {
                if let Statement::Assign { rvalue, .. } = stmt {
                    if let Some(constant) = self.extract_constant(rvalue) {
                        definition_to_constant.insert((block_id, stmt_index), constant);
                    }
                }
            }
        }

        // Second pass: propagate constants using reaching definitions
        for (block_id, block) in func.blocks.iter_mut() {
            // Get reaching definitions at entry of this block
            let reaching_defs_at_entry =
                if let Some(lattice) = reaching_defs_results.entry.get(&block_id) {
                    if let Some(set) = lattice.as_set() {
                        set
                    } else {
                        &HashSet::new()
                    }
                } else {
                    &HashSet::new()
                };

            let mut current_reaching_defs = reaching_defs_at_entry.clone();
            let mut new_statements = Vec::new();

            for (stmt_index, stmt) in block.statements.iter().enumerate() {
                match stmt {
                    Statement::Assign { var, rvalue, span } => {
                        let new_rvalue = self.propagate_in_rvalue(
                            rvalue,
                            &current_reaching_defs,
                            &definition_to_constant,
                        );

                        if new_rvalue != *rvalue {
                            changed = true;
                        }

                        let new_stmt = Statement::Assign {
                            var: *var,
                            rvalue: new_rvalue.clone(),
                            span: span.clone(),
                        };
                        new_statements.push(new_stmt);

                        // Update reaching definitions: kill old definitions of var, add new one
                        current_reaching_defs.retain(|def| def.var_id != *var);
                        current_reaching_defs.insert(ReachingDefinition {
                            var_id: *var,
                            block_id,
                            stmt_index,
                        });

                        // If this is a new constant definition, record it
                        if let Some(constant) = self.extract_constant(&new_rvalue) {
                            definition_to_constant.insert((block_id, stmt_index), constant);
                        }
                    }
                    Statement::TableAssign {
                        table,
                        pk_fields,
                        pk_values,
                        field,
                        value,
                        span,
                    } => {
                        let new_pk_values: Vec<Operand> = pk_values
                            .iter()
                            .map(|pk_value| {
                                self.propagate_in_operand(
                                    pk_value,
                                    &current_reaching_defs,
                                    &definition_to_constant,
                                )
                            })
                            .collect();
                        let new_value = self.propagate_in_operand(
                            value,
                            &current_reaching_defs,
                            &definition_to_constant,
                        );

                        let pk_values_changed = new_pk_values
                            .iter()
                            .zip(pk_values.iter())
                            .any(|(new, old)| new != old);

                        if pk_values_changed || new_value != *value {
                            changed = true;
                        }

                        new_statements.push(Statement::TableAssign {
                            table: *table,
                            pk_fields: pk_fields.clone(),
                            pk_values: new_pk_values,
                            field: *field,
                            value: new_value,
                            span: span.clone(),
                        });
                    }
                }
            }

            block.statements = new_statements;
        }

        changed
    }
}

impl ConstantPropagationPass {
    pub fn new() -> Self {
        Self
    }

    /// Extract constant value from an rvalue if it's a simple constant assignment
    fn extract_constant(&self, rvalue: &Rvalue) -> Option<Constant> {
        match rvalue {
            Rvalue::Use(Operand::Const(c)) => Some(c.clone()),
            _ => None,
        }
    }

    /// Propagate constants in an rvalue
    fn propagate_in_rvalue(
        &self,
        rvalue: &Rvalue,
        reaching_defs: &HashSet<ReachingDefinition>,
        definition_to_constant: &HashMap<(BasicBlockId, usize), Constant>,
    ) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => Rvalue::Use(self.propagate_in_operand(
                operand,
                reaching_defs,
                definition_to_constant,
            )),
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
                    .map(|pk_value| {
                        self.propagate_in_operand(pk_value, reaching_defs, definition_to_constant)
                    })
                    .collect(),
                field: *field,
            },
            Rvalue::ArrayAccess { array, index } => {
                let new_array =
                    self.propagate_in_operand(array, reaching_defs, definition_to_constant);
                let new_index =
                    self.propagate_in_operand(index, reaching_defs, definition_to_constant);

                Rvalue::ArrayAccess {
                    array: new_array,
                    index: new_index,
                }
            }
            Rvalue::UnaryOp { op, operand } => {
                let new_operand =
                    self.propagate_in_operand(operand, reaching_defs, definition_to_constant);

                // Try to evaluate the operation if operand is now a constant
                if let Operand::Const(c) = &new_operand {
                    if let Some(result) = self.evaluate_unary_op(op, c) {
                        return Rvalue::Use(Operand::Const(result));
                    }
                }

                Rvalue::UnaryOp {
                    op: op.clone(),
                    operand: new_operand,
                }
            }
            Rvalue::BinaryOp { op, left, right } => {
                let new_left =
                    self.propagate_in_operand(left, reaching_defs, definition_to_constant);
                let new_right =
                    self.propagate_in_operand(right, reaching_defs, definition_to_constant);

                // Try to evaluate the operation if both operands are now constants
                if let (Operand::Const(c1), Operand::Const(c2)) = (&new_left, &new_right) {
                    if let Some(result) = self.evaluate_binary_op(op, c1, c2) {
                        return Rvalue::Use(Operand::Const(result));
                    }
                }

                Rvalue::BinaryOp {
                    op: op.clone(),
                    left: new_left,
                    right: new_right,
                }
            }
        }
    }

    /// Propagate constants in an operand
    fn propagate_in_operand(
        &self,
        operand: &Operand,
        reaching_defs: &HashSet<ReachingDefinition>,
        definition_to_constant: &HashMap<(BasicBlockId, usize), Constant>,
    ) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                // Look for constant definitions of this variable
                let constant_defs: Vec<&Constant> = reaching_defs
                    .iter()
                    .filter(|def| def.var_id == *var_id)
                    .filter_map(|def| definition_to_constant.get(&(def.block_id, def.stmt_index)))
                    .collect();

                // Only propagate if there's exactly one constant definition
                if constant_defs.len() == 1 {
                    Operand::Const(constant_defs[0].clone())
                } else {
                    operand.clone()
                }
            }
            Operand::Const(_) => operand.clone(),
        }
    }

    /// Evaluate a unary operation on a constant
    fn evaluate_unary_op(&self, op: &crate::ast::UnaryOp, operand: &Constant) -> Option<Constant> {
        match (op, operand) {
            (crate::ast::UnaryOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
            (crate::ast::UnaryOp::Neg, Constant::Int(i)) => Some(Constant::Int(-i)),
            (crate::ast::UnaryOp::Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
            _ => None,
        }
    }

    /// Evaluate a binary operation on constants
    fn evaluate_binary_op(
        &self,
        op: &crate::ast::BinaryOp,
        left: &Constant,
        right: &Constant,
    ) -> Option<Constant> {
        match (op, left, right) {
            // Integer arithmetic
            (crate::ast::BinaryOp::Add, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Int(a + b))
            }
            (crate::ast::BinaryOp::Sub, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Int(a - b))
            }
            (crate::ast::BinaryOp::Mul, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Int(a * b))
            }
            (crate::ast::BinaryOp::Div, Constant::Int(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Int(a / b))
            }

            // Float arithmetic
            (crate::ast::BinaryOp::Add, Constant::Float(a), Constant::Float(b)) => {
                Some(Constant::Float(a + b))
            }
            (crate::ast::BinaryOp::Sub, Constant::Float(a), Constant::Float(b)) => {
                Some(Constant::Float(a - b))
            }
            (crate::ast::BinaryOp::Mul, Constant::Float(a), Constant::Float(b)) => {
                Some(Constant::Float(a * b))
            }
            (crate::ast::BinaryOp::Div, Constant::Float(a), Constant::Float(b)) if *b != 0.0 => {
                Some(Constant::Float(a / b))
            }

            // Comparisons for integers
            (crate::ast::BinaryOp::Lt, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Bool(a < b))
            }
            (crate::ast::BinaryOp::Lte, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Bool(a <= b))
            }
            (crate::ast::BinaryOp::Gt, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Bool(a > b))
            }
            (crate::ast::BinaryOp::Gte, Constant::Int(a), Constant::Int(b)) => {
                Some(Constant::Bool(a >= b))
            }

            // Equality comparisons (works for all types)
            (crate::ast::BinaryOp::Eq, a, b) => Some(Constant::Bool(a == b)),
            (crate::ast::BinaryOp::Neq, a, b) => Some(Constant::Bool(a != b)),

            // Boolean operations
            (crate::ast::BinaryOp::And, Constant::Bool(a), Constant::Bool(b)) => {
                Some(Constant::Bool(*a && *b))
            }
            (crate::ast::BinaryOp::Or, Constant::Bool(a), Constant::Bool(b)) => {
                Some(Constant::Bool(*a || *b))
            }

            _ => None,
        }
    }
}
