use crate::cfg::{
    BinaryOp, CfgProgram, Constant, FunctionId, LValue, Operand, Rvalue, Statement, UnaryOp, VarId,
};
use crate::optimization::OptimizationPass;
use std::collections::HashMap;

/// Constant Propagation optimization pass
pub struct ConstantPropagationPass;

impl OptimizationPass for ConstantPropagationPass {
    fn name(&self) -> &'static str {
        "Constant Propagation"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        // Get function reference for analysis
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        eprintln!("        CONSTANT PROPAGATION DEBUG:");

        let mut overall_changed = false;
        let mut iteration = 0;

        // Iterative constant propagation until fixed point
        loop {
            let mut iteration_changed = false;

            // Build a map of variable constants
            let mut var_constants: HashMap<VarId, Constant> = HashMap::new();

            // Multiple passes: collect constant values for variables iteratively within this iteration
            // This handles chains like: tmp_1 = 42; x = tmp_1; tmp_2 = x; y = tmp_2 + 10; etc.
            for _pass in 0..10 {
                let mut pass_changed = false;
                for &block_id in &func.blocks {
                    if let Some(block) = program.blocks.get(block_id) {
                        for stmt in &block.statements {
                            let Statement::Assign { lvalue, rvalue, .. } = stmt;
                            if let LValue::Variable { var } = lvalue {
                                // Only update if we don't already have a constant for this variable
                                if !var_constants.contains_key(var) {
                                    if let Some(constant) =
                                        self.extract_constant_from_rvalue(rvalue, &var_constants)
                                    {
                                        eprintln!(
                                            "        Iteration {}: Found constant: {:?} = {:?}",
                                            iteration, var, constant
                                        );
                                        var_constants.insert(*var, constant);
                                        pass_changed = true;
                                    }
                                }
                            }
                        }
                    }
                }
                if !pass_changed {
                    break; // No more constants found in this iteration
                }
            }

            eprintln!(
                "        Iteration {}: Total constants found: {}",
                iteration,
                var_constants.len()
            );

            // Second pass: propagate constants and fold expressions
            let function_blocks = func.blocks.clone();
            for block_id in function_blocks {
                if let Some(block) = program.blocks.get_mut(block_id) {
                    let mut new_statements = Vec::new();

                    eprintln!(
                        "        Block {:?}: Processing {} statements",
                        block_id,
                        block.statements.len()
                    );

                    for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                        let mut stmt_changed = false;
                        let new_stmt = self.propagate_constants_in_statement(
                            stmt,
                            &var_constants,
                            &mut stmt_changed,
                        );

                        if stmt_changed {
                            eprintln!(
                                "        [{}] CHANGED: {:?} -> {:?}",
                                stmt_idx, stmt, new_stmt
                            );
                            iteration_changed = true;
                        } else {
                            eprintln!("        [{}] UNCHANGED: {:?}", stmt_idx, stmt);
                        }

                        new_statements.push(new_stmt);
                    }

                    block.statements = new_statements;
                }
            }

            if iteration_changed {
                overall_changed = true;
                iteration += 1;
                if iteration >= 10 {
                    // Safety limit
                    eprintln!("        WARNING: Constant propagation iteration limit reached");
                    break;
                }
            } else {
                eprintln!("        Fixed point reached after {} iterations", iteration);
                break;
            }
        }

        overall_changed
    }
}

impl ConstantPropagationPass {
    pub fn new() -> Self {
        Self
    }

    /// Extract constant value from an rvalue, considering known variable constants
    fn extract_constant_from_rvalue(
        &self,
        rvalue: &Rvalue,
        var_constants: &HashMap<VarId, Constant>,
    ) -> Option<Constant> {
        match rvalue {
            Rvalue::Use(Operand::Const(c)) => Some(c.clone()),
            Rvalue::Use(Operand::Var(var_id)) => {
                // Check if this variable has a constant value
                var_constants.get(var_id).cloned()
            }
            Rvalue::BinaryOp { op, left, right } => {
                // Try to evaluate constant binary operations
                let left_const = match left {
                    Operand::Const(c) => Some(c),
                    Operand::Var(var_id) => var_constants.get(var_id),
                };
                let right_const = match right {
                    Operand::Const(c) => Some(c),
                    Operand::Var(var_id) => var_constants.get(var_id),
                };

                if let (Some(left_c), Some(right_c)) = (left_const, right_const) {
                    self.evaluate_binary_op(op, left_c, right_c)
                } else {
                    None
                }
            }
            Rvalue::UnaryOp { op, operand } => {
                // Try to evaluate constant unary operations
                let operand_const = match operand {
                    Operand::Const(c) => Some(c),
                    Operand::Var(var_id) => var_constants.get(var_id),
                };

                if let Some(operand_c) = operand_const {
                    self.evaluate_unary_op(op, operand_c)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Propagate constants in a statement
    fn propagate_constants_in_statement(
        &self,
        stmt: &Statement,
        var_constants: &HashMap<VarId, Constant>,
        changed: &mut bool,
    ) -> Statement {
        match stmt {
            Statement::Assign {
                lvalue,
                rvalue,
                span,
            } => {
                let new_rvalue = self.propagate_constants_in_rvalue(rvalue, var_constants, changed);
                Statement::Assign {
                    lvalue: lvalue.clone(),
                    rvalue: new_rvalue,
                    span: span.clone(),
                }
            }
        }
    }

    /// Propagate constants in an rvalue
    fn propagate_constants_in_rvalue(
        &self,
        rvalue: &Rvalue,
        var_constants: &HashMap<VarId, Constant>,
        changed: &mut bool,
    ) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => {
                Rvalue::Use(self.propagate_constants_in_operand(operand, var_constants, changed))
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
                        self.propagate_constants_in_operand(pk_value, var_constants, changed)
                    })
                    .collect(),
                field: *field,
            },
            Rvalue::ArrayAccess { array, index } => {
                let new_array = self.propagate_constants_in_operand(array, var_constants, changed);
                let new_index = self.propagate_constants_in_operand(index, var_constants, changed);
                Rvalue::ArrayAccess {
                    array: new_array,
                    index: new_index,
                }
            }
            Rvalue::UnaryOp { op, operand } => {
                let new_operand =
                    self.propagate_constants_in_operand(operand, var_constants, changed);

                // Try to evaluate the operation if operand is now a constant
                if let Operand::Const(c) = &new_operand {
                    if let Some(result) = self.evaluate_unary_op(op, c) {
                        *changed = true;
                        return Rvalue::Use(Operand::Const(result));
                    }
                }

                Rvalue::UnaryOp {
                    op: op.clone(),
                    operand: new_operand,
                }
            }
            Rvalue::BinaryOp { op, left, right } => {
                let new_left = self.propagate_constants_in_operand(left, var_constants, changed);
                let new_right = self.propagate_constants_in_operand(right, var_constants, changed);

                // Try to evaluate the operation if both operands are now constants
                if let (Operand::Const(c1), Operand::Const(c2)) = (&new_left, &new_right) {
                    if let Some(result) = self.evaluate_binary_op(op, c1, c2) {
                        *changed = true;
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
    fn propagate_constants_in_operand(
        &self,
        operand: &Operand,
        var_constants: &HashMap<VarId, Constant>,
        changed: &mut bool,
    ) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                // Look for constant value of this variable
                if let Some(constant) = var_constants.get(var_id) {
                    *changed = true;
                    Operand::Const(constant.clone())
                } else {
                    operand.clone()
                }
            }
            Operand::Const(_) => operand.clone(),
        }
    }

    /// Evaluate a unary operation on a constant
    fn evaluate_unary_op(&self, op: &UnaryOp, operand: &Constant) -> Option<Constant> {
        match (op, operand) {
            (UnaryOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
            (UnaryOp::Neg, Constant::Int(i)) => Some(Constant::Int(-i)),
            (UnaryOp::Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
            _ => None,
        }
    }

    /// Evaluate a binary operation on constants
    fn evaluate_binary_op(
        &self,
        op: &BinaryOp,
        left: &Constant,
        right: &Constant,
    ) -> Option<Constant> {
        match (op, left, right) {
            // Integer arithmetic
            (BinaryOp::Add, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a + b)),
            (BinaryOp::Sub, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a - b)),
            (BinaryOp::Mul, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a * b)),
            (BinaryOp::Div, Constant::Int(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Int(a / b))
            }

            // Float arithmetic
            (BinaryOp::Add, Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a + b)),
            (BinaryOp::Sub, Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a - b)),
            (BinaryOp::Mul, Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a * b)),
            (BinaryOp::Div, Constant::Float(a), Constant::Float(b)) if *b != 0.0 => {
                Some(Constant::Float(a / b))
            }

            // Comparisons for integers
            (BinaryOp::Lt, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a < b)),
            (BinaryOp::Lte, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a <= b)),
            (BinaryOp::Gt, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a > b)),
            (BinaryOp::Gte, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a >= b)),

            // Equality comparisons (works for all types)
            (BinaryOp::Eq, a, b) => Some(Constant::Bool(a == b)),
            (BinaryOp::Neq, a, b) => Some(Constant::Bool(a != b)),

            // Boolean operations
            (BinaryOp::And, Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a && *b)),
            (BinaryOp::Or, Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a || *b)),

            _ => None,
        }
    }
}
