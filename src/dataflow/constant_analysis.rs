use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{BasicBlockId, Constant, ControlFlowEdge, FunctionCfg, Operand, Statement, VarId};

/// Constant binding for a variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantBinding {
    pub var: VarId,
    pub value: Constant,
}

/// Constant state - tracks which variables have constant values
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantState {
    Bottom, // No information
    Const(Constant),
    Top, // Not constant (multiple values)
}

/// Map from variables to their constant state
pub type ConstantMapLattice = SetLattice<ConstantBinding>;

/// Simplified constant data for easier use
pub type ConstantMapData = std::collections::HashMap<VarId, Constant>;

/// Transfer function for constant propagation analysis
pub struct ConstantTransfer;

impl TransferFunction<ConstantMapLattice> for ConstantTransfer {
    /// For each statement, update constant information
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &ConstantMapLattice,
    ) -> ConstantMapLattice {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                match lvalue {
                    crate::cfg::LValue::Variable { var } => {
                        // Remove previous constant binding for this variable
                        result_set.retain(|binding| binding.var != *var);

                        // Try to evaluate rvalue to a constant
                        if let Some(constant_val) = self.eval_rvalue(rvalue, &result_set) {
                            result_set.insert(ConstantBinding {
                                var: *var,
                                value: constant_val,
                            });
                        }
                    }
                    crate::cfg::LValue::ArrayElement { array, .. } => {
                        // Array element assignment makes array non-constant
                        result_set.retain(|binding| binding.var != *array);
                    }
                    crate::cfg::LValue::TableField { .. } => {
                        // Table field assignments don't affect local constants
                    }
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &ConstantMapLattice,
    ) -> ConstantMapLattice {
        state.clone()
    }

    fn initial_value(&self) -> ConstantMapLattice {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _blockid: BasicBlockId) -> ConstantMapLattice {
        // Parameters might have constant values, but we don't know them here
        // So start with empty set
        SetLattice::bottom().unwrap()
    }
}

impl ConstantTransfer {
    fn eval_rvalue(
        &self,
        rvalue: &crate::cfg::Rvalue,
        constants: &std::collections::HashSet<ConstantBinding>,
    ) -> Option<Constant> {
        match rvalue {
            crate::cfg::Rvalue::Use(operand) => self.eval_operand(operand, constants),
            crate::cfg::Rvalue::BinaryOp { op, left, right } => {
                let left_val = self.eval_operand(left, constants)?;
                let right_val = self.eval_operand(right, constants)?;
                self.eval_binary_op(op.clone(), &left_val, &right_val)
            }
            crate::cfg::Rvalue::UnaryOp { op, operand } => {
                let val = self.eval_operand(operand, constants)?;
                self.eval_unary_op(op.clone(), &val)
            }
            _ => None, // Table/array access not constant-foldable
        }
    }

    fn eval_operand(
        &self,
        operand: &Operand,
        constants: &std::collections::HashSet<ConstantBinding>,
    ) -> Option<Constant> {
        match operand {
            Operand::Const(c) => Some(c.clone()),
            Operand::Var(var_id) => constants
                .iter()
                .find(|binding| binding.var == *var_id)
                .map(|binding| binding.value.clone()),
        }
    }

    fn eval_binary_op(
        &self,
        op: crate::cfg::BinaryOp,
        left: &Constant,
        right: &Constant,
    ) -> Option<Constant> {
        match (left, right) {
            (Constant::Int(l), Constant::Int(r)) => {
                use crate::cfg::BinaryOp;
                match op {
                    BinaryOp::Add => Some(Constant::Int(l + r)),
                    BinaryOp::Sub => Some(Constant::Int(l - r)),
                    BinaryOp::Mul => Some(Constant::Int(l * r)),
                    BinaryOp::Div => {
                        if *r != 0 {
                            Some(Constant::Int(l / r))
                        } else {
                            None
                        }
                    }
                    BinaryOp::Eq => Some(Constant::Bool(l == r)),
                    BinaryOp::Neq => Some(Constant::Bool(l != r)),
                    BinaryOp::Lt => Some(Constant::Bool(l < r)),
                    BinaryOp::Lte => Some(Constant::Bool(l <= r)),
                    BinaryOp::Gt => Some(Constant::Bool(l > r)),
                    BinaryOp::Gte => Some(Constant::Bool(l >= r)),
                    _ => None,
                }
            }
            (Constant::Bool(l), Constant::Bool(r)) => {
                use crate::cfg::BinaryOp;
                match op {
                    BinaryOp::And => Some(Constant::Bool(*l && *r)),
                    BinaryOp::Or => Some(Constant::Bool(*l || *r)),
                    BinaryOp::Eq => Some(Constant::Bool(l == r)),
                    BinaryOp::Neq => Some(Constant::Bool(l != r)),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn eval_unary_op(&self, op: crate::cfg::UnaryOp, operand: &Constant) -> Option<Constant> {
        match operand {
            Constant::Int(val) => match op {
                crate::cfg::UnaryOp::Neg => Some(Constant::Int(-val)),
                _ => None,
            },
            Constant::Bool(val) => match op {
                crate::cfg::UnaryOp::Not => Some(Constant::Bool(!val)),
                _ => None,
            },
            _ => None,
        }
    }
}

/// Analyze constants in a function (forward, function-level)
pub fn analyze_constants(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<ConstantMapLattice> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::May,
        ConstantTransfer,
    );
    analysis.analyze(func, cfg_program)
}
