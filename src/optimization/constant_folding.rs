use crate::cfg::{BinaryOp, CfgProgram, Constant, FunctionId, Operand, Rvalue, Statement, UnaryOp};
use crate::optimization::OptimizationPass;

/// Constant Folding optimization pass
///
/// Evaluates operations whose operands are all constants at compile time.
/// This is typically done after constant propagation to fold newly exposed constants.
pub struct ConstantFoldingPass;

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &'static str {
        "Constant Folding"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        let mut changed = false;

        // Process each block
        for &block_id in &func.blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                for stmt in block.statements.iter_mut() {
                    if self.fold_constants_in_statement(stmt) {
                        changed = true;
                    }
                }
            }
        }

        changed
    }
}

impl ConstantFoldingPass {
    pub fn new() -> Self {
        Self
    }

    /// Fold constants in a statement, returning true if changed
    fn fold_constants_in_statement(&self, stmt: &mut Statement) -> bool {
        match stmt {
            Statement::Assign { rvalue, .. } => self.fold_constants_in_rvalue(rvalue),
        }
    }

    /// Fold constants in an rvalue, returning true if changed
    fn fold_constants_in_rvalue(&self, rvalue: &mut Rvalue) -> bool {
        match rvalue {
            Rvalue::BinaryOp { op, left, right } => {
                // Check if both operands are constants
                if let (Operand::Const(left_const), Operand::Const(right_const)) = (left, right) {
                    if let Some(result) = Self::eval_binary_op(op, left_const, right_const) {
                        // Replace the entire binary operation with the constant result
                        *rvalue = Rvalue::Use(Operand::Const(result));
                        return true;
                    }
                }
                false
            }
            Rvalue::UnaryOp { op, operand } => {
                // Check if operand is a constant
                if let Operand::Const(const_operand) = operand {
                    if let Some(result) = Self::eval_unary_op(op, const_operand) {
                        // Replace the entire unary operation with the constant result
                        *rvalue = Rvalue::Use(Operand::Const(result));
                        return true;
                    }
                }
                false
            }
            _ => false, // Other rvalue types don't need constant folding
        }
    }

    /// Evaluate binary operations on constants
    fn eval_binary_op(op: &BinaryOp, left: &Constant, right: &Constant) -> Option<Constant> {
        match (op, left, right) {
            (BinaryOp::Add, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a + b)),
            (BinaryOp::Sub, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a - b)),
            (BinaryOp::Mul, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a * b)),
            (BinaryOp::Div, Constant::Int(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Int(a / b))
            }
            (BinaryOp::Add, Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a + b)),
            (BinaryOp::Sub, Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a - b)),
            (BinaryOp::Mul, Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a * b)),
            (BinaryOp::Div, Constant::Float(a), Constant::Float(b)) if *b != 0.0 => {
                Some(Constant::Float(a / b))
            }
            (BinaryOp::Lt, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a < b)),
            (BinaryOp::Lte, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a <= b)),
            (BinaryOp::Gt, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a > b)),
            (BinaryOp::Gte, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a >= b)),
            (BinaryOp::Eq, a, b) => Some(Constant::Bool(a == b)),
            (BinaryOp::Neq, a, b) => Some(Constant::Bool(a != b)),
            (BinaryOp::And, Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a && *b)),
            (BinaryOp::Or, Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a || *b)),
            _ => None,
        }
    }

    /// Evaluate unary operations on constants
    fn eval_unary_op(op: &UnaryOp, operand: &Constant) -> Option<Constant> {
        match (op, operand) {
            (UnaryOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
            (UnaryOp::Neg, Constant::Int(i)) => Some(Constant::Int(-i)),
            (UnaryOp::Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
            _ => None,
        }
    }
}
