//! Constant Folding Optimization Pass
//!
//! This pass evaluates expressions with constant operands at compile time,
//! replacing them with their computed constant values.

use super::OptimizationPass;
use crate::cfg::{BinaryOp, CfgProgram, Constant, FunctionId, Operand, Rvalue, Statement, UnaryOp};

pub struct ConstantFoldingPass;

impl ConstantFoldingPass {
    pub fn new() -> Self {
        Self
    }

    /// Evaluate a binary operation with constant operands
    fn eval_binary_op(&self, op: &BinaryOp, left: &Constant, right: &Constant) -> Option<Constant> {
        use ordered_float::OrderedFloat;

        match (left, right) {
            (Constant::Int(l), Constant::Int(r)) => match op {
                BinaryOp::Add => Some(Constant::Int(l + r)),
                BinaryOp::Sub => Some(Constant::Int(l - r)),
                BinaryOp::Mul => Some(Constant::Int(l * r)),
                BinaryOp::Div => {
                    if *r != 0 {
                        Some(Constant::Int(l / r))
                    } else {
                        None // Division by zero
                    }
                }
                BinaryOp::Eq => Some(Constant::Bool(l == r)),
                BinaryOp::Neq => Some(Constant::Bool(l != r)),
                BinaryOp::Lt => Some(Constant::Bool(l < r)),
                BinaryOp::Lte => Some(Constant::Bool(l <= r)),
                BinaryOp::Gt => Some(Constant::Bool(l > r)),
                BinaryOp::Gte => Some(Constant::Bool(l >= r)),
                BinaryOp::And => Some(Constant::Int(l & r)),
                BinaryOp::Or => Some(Constant::Int(l | r)),
            },
            (Constant::Float(l), Constant::Float(r)) => match op {
                BinaryOp::Add => Some(Constant::Float(OrderedFloat(l.into_inner() + r.into_inner()))),
                BinaryOp::Sub => Some(Constant::Float(OrderedFloat(l.into_inner() - r.into_inner()))),
                BinaryOp::Mul => Some(Constant::Float(OrderedFloat(l.into_inner() * r.into_inner()))),
                BinaryOp::Div => {
                    let r_val = r.into_inner();
                    if r_val != 0.0 {
                        Some(Constant::Float(OrderedFloat(l.into_inner() / r_val)))
                    } else {
                        None // Division by zero
                    }
                }
                BinaryOp::Eq => Some(Constant::Bool(l == r)),
                BinaryOp::Neq => Some(Constant::Bool(l != r)),
                BinaryOp::Lt => Some(Constant::Bool(l < r)),
                BinaryOp::Lte => Some(Constant::Bool(l <= r)),
                BinaryOp::Gt => Some(Constant::Bool(l > r)),
                BinaryOp::Gte => Some(Constant::Bool(l >= r)),
                _ => None, // Bitwise operations not supported for floats
            },
            (Constant::Bool(l), Constant::Bool(r)) => match op {
                BinaryOp::Eq => Some(Constant::Bool(l == r)),
                BinaryOp::Neq => Some(Constant::Bool(l != r)),
                BinaryOp::And => Some(Constant::Bool(*l && *r)),
                BinaryOp::Or => Some(Constant::Bool(*l || *r)),
                _ => None,
            },
            (Constant::String(l), Constant::String(r)) => match op {
                BinaryOp::Add => Some(Constant::String(format!("{}{}", l, r))),
                BinaryOp::Eq => Some(Constant::Bool(l == r)),
                BinaryOp::Neq => Some(Constant::Bool(l != r)),
                _ => None,
            },
            // Mixed type comparisons
            (Constant::Int(l), Constant::Float(r)) => match op {
                BinaryOp::Add => Some(Constant::Float(OrderedFloat(*l as f64 + r.into_inner()))),
                BinaryOp::Sub => Some(Constant::Float(OrderedFloat(*l as f64 - r.into_inner()))),
                BinaryOp::Mul => Some(Constant::Float(OrderedFloat(*l as f64 * r.into_inner()))),
                BinaryOp::Div => {
                    let r_val = r.into_inner();
                    if r_val != 0.0 {
                        Some(Constant::Float(OrderedFloat(*l as f64 / r_val)))
                    } else {
                        None
                    }
                }
                BinaryOp::Eq => Some(Constant::Bool(*l as f64 == r.into_inner())),
                BinaryOp::Neq => Some(Constant::Bool(*l as f64 != r.into_inner())),
                BinaryOp::Lt => Some(Constant::Bool((*l as f64) < r.into_inner())),
                BinaryOp::Lte => Some(Constant::Bool((*l as f64) <= r.into_inner())),
                BinaryOp::Gt => Some(Constant::Bool((*l as f64) > r.into_inner())),
                BinaryOp::Gte => Some(Constant::Bool((*l as f64) >= r.into_inner())),
                _ => None,
            },
            (Constant::Float(l), Constant::Int(r)) => match op {
                BinaryOp::Add => Some(Constant::Float(OrderedFloat(l.into_inner() + *r as f64))),
                BinaryOp::Sub => Some(Constant::Float(OrderedFloat(l.into_inner() - *r as f64))),
                BinaryOp::Mul => Some(Constant::Float(OrderedFloat(l.into_inner() * *r as f64))),
                BinaryOp::Div => {
                    if *r != 0 {
                        Some(Constant::Float(OrderedFloat(l.into_inner() / *r as f64)))
                    } else {
                        None
                    }
                }
                BinaryOp::Eq => Some(Constant::Bool(l.into_inner() == *r as f64)),
                BinaryOp::Neq => Some(Constant::Bool(l.into_inner() != *r as f64)),
                BinaryOp::Lt => Some(Constant::Bool(l.into_inner() < *r as f64)),
                BinaryOp::Lte => Some(Constant::Bool(l.into_inner() <= *r as f64)),
                BinaryOp::Gt => Some(Constant::Bool(l.into_inner() > *r as f64)),
                BinaryOp::Gte => Some(Constant::Bool(l.into_inner() >= *r as f64)),
                _ => None,
            },
            _ => None, // Unsupported type combinations
        }
    }

    /// Evaluate a unary operation with a constant operand
    fn eval_unary_op(&self, op: &UnaryOp, operand: &Constant) -> Option<Constant> {
        match operand {
            Constant::Int(val) => match op {
                UnaryOp::Neg => Some(Constant::Int(-val)),
                _ => None,
            },
            Constant::Float(val) => match op {
                UnaryOp::Neg => Some(Constant::Float(ordered_float::OrderedFloat(-val.into_inner()))),
                _ => None,
            },
            Constant::Bool(val) => match op {
                UnaryOp::Not => Some(Constant::Bool(!val)),
                _ => None,
            },
            _ => None,
        }
    }

    /// Try to fold constants in an rvalue
    fn fold_rvalue(&self, rvalue: &Rvalue) -> Option<Rvalue> {
        match rvalue {
            Rvalue::BinaryOp { op, left, right } => {
                if let (Operand::Const(left_const), Operand::Const(right_const)) = (left, right) {
                    if let Some(result) = self.eval_binary_op(op, left_const, right_const) {
                        Some(Rvalue::Use(Operand::Const(result)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Rvalue::UnaryOp { op, operand } => {
                if let Operand::Const(operand_const) = operand {
                    if let Some(result) = self.eval_unary_op(op, operand_const) {
                        Some(Rvalue::Use(Operand::Const(result)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None, // Other rvalue types don't fold
        }
    }
}

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &'static str {
        "Constant Folding"
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

        let mut changed = false;

        // Process each block in the function
        for &block_id in &function.blocks {
            let block = &mut program.blocks[block_id];

            // Process each statement
            for stmt in block.statements.iter_mut() {
                match stmt {
                    Statement::Assign { rvalue, .. } => {
                        if let Some(folded_rvalue) = self.fold_rvalue(rvalue) {
                            *rvalue = folded_rvalue;
                            changed = true;
                        }
                    }
                }
            }

            // Process conditional edges - could potentially eliminate conditionals
            // but that requires more complex control flow analysis
        }

        changed
    }
}
