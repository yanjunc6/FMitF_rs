use crate::cfg::{Constant, FunctionCfg, Operand, Rvalue, Statement};
use crate::dataflow::{analyze_reaching_definitions, ReachingDefinition};
use crate::optimization::OptimizationPass;
use std::collections::HashMap;
use std::ptr::NonNull;

/// Constant Propagation optimization pass
pub struct ConstantPropagationPass;

// Implement the OptimizationPass trait
impl OptimizationPass for ConstantPropagationPass {
    fn name(&self) -> &'static str {
        "Constant Propagation"
    }

    fn optimize_function(&self, func: &mut FunctionCfg) -> bool {
        let reaching_defs_results = analyze_reaching_definitions(func);
        let mut changed = false;

        // Build a map from statement sites to their constant values (if any)
        let mut stmt_to_constant: HashMap<NonNull<Statement>, Constant> = HashMap::new();

        // First pass: collect constant definitions
        for (_, block) in func.blocks.iter() {
            for stmt in &block.statements {
                if let Statement::Assign { rvalue, .. } = stmt {
                    if let Some(constant) = self.extract_constant(rvalue) {
                        let stmt_ptr = NonNull::from(stmt);
                        stmt_to_constant.insert(stmt_ptr, constant);
                    }
                }
            }
        }

        // Second pass: propagate constants
        for (block_id, block) in func.blocks.iter_mut() {
            let empty_set = std::collections::HashSet::new();
            let reaching_defs_at_entry = reaching_defs_results
                .entry
                .get(&block_id)
                .map(|set| &set.set)
                .unwrap_or(&empty_set);

            let mut new_statements = Vec::new();

            for stmt in &block.statements {
                match stmt {
                    Statement::Assign { var, rvalue, span } => {
                        let new_rvalue = self.propagate_in_rvalue(
                            rvalue,
                            reaching_defs_at_entry,
                            &stmt_to_constant,
                        );

                        if new_rvalue != *rvalue {
                            changed = true;
                        }

                        new_statements.push(Statement::Assign {
                            var: *var,
                            rvalue: new_rvalue,
                            span: span.clone(),
                        });
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
                                    reaching_defs_at_entry,
                                    &stmt_to_constant,
                                )
                            })
                            .collect();
                        let new_value = self.propagate_in_operand(
                            value,
                            reaching_defs_at_entry,
                            &stmt_to_constant,
                        );

                        // Check if any primary key values changed
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
        reaching_defs: &std::collections::HashSet<ReachingDefinition>,
        stmt_to_constant: &HashMap<NonNull<Statement>, Constant>,
    ) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => {
                Rvalue::Use(self.propagate_in_operand(operand, reaching_defs, stmt_to_constant))
            }
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
                        self.propagate_in_operand(pk_value, reaching_defs, stmt_to_constant)
                    })
                    .collect(),
                field: *field,
            },
            Rvalue::UnaryOp { op, operand } => {
                let new_operand =
                    self.propagate_in_operand(operand, reaching_defs, stmt_to_constant);

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
                let new_left = self.propagate_in_operand(left, reaching_defs, stmt_to_constant);
                let new_right = self.propagate_in_operand(right, reaching_defs, stmt_to_constant);

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
        reaching_defs: &std::collections::HashSet<ReachingDefinition>,
        stmt_to_constant: &HashMap<NonNull<Statement>, Constant>,
    ) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                // Look for constant definitions of this variable
                let constant_defs: Vec<&Constant> = reaching_defs
                    .iter()
                    .filter(|def| def.var_id == *var_id)
                    .filter_map(|def| stmt_to_constant.get(&def.site))
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
