use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, StmtLoc,
    TransferFunction,
};
use crate::cfg::{
    BasicBlock, Constant, ControlFlowEdge, FunctionCfg, LValue, Operand, RValue, Statement, VarId,
};

/// Constant state - tracks which variables have constant values
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantState {
    // Bottom, // No information, it should be treat as no key defined in the map
    Const(Constant),
    Top, // Not constant (multiple values)
}

/// Map from variables to their constant state
pub type ConstantMapLattice = std::collections::HashMap<VarId, ConstantState>;

impl Lattice for ConstantMapLattice {
    fn bottom() -> Option<Self> {
        Some(std::collections::HashMap::new())
    }

    fn top() -> Option<Self> {
        None
    }

    fn meet(&self, other: &Self) -> Self {
        let mut result = self.clone();
        for (var, state) in other {
            match state {
                ConstantState::Top => {
                    result.insert(*var, ConstantState::Top);
                }
                ConstantState::Const(value) => {
                    if let Some(existing) = result.get(var) {
                        match existing {
                            ConstantState::Top => continue,
                            ConstantState::Const(existing_value) if existing_value == value => {
                                continue
                            }
                            _ => result.insert(*var, ConstantState::Top),
                        };
                    } else {
                        result.insert(*var, ConstantState::Const(value.clone()));
                    }
                }
            }
        }
        result
    }

    /// Should never been used
    fn join(&self, _other: &Self) -> Self {
        panic!("Join operation is not defined for ConstantMapLattice");
    }

    /// Should never been used
    fn less_equal(&self, other: &Self) -> bool {
        // Check if self is a subset of other
        self.iter().all(|(var, state)| {
            other
                .get(var)
                .map_or(false, |other_state| match (state, other_state) {
                    (ConstantState::Top, _) => true,
                    (ConstantState::Const(value), ConstantState::Const(other_value)) => {
                        value == other_value
                    }
                    _ => false,
                })
        })
    }
}

/// Transfer function for constant propagation analysis
pub struct ConstantTransfer;

impl TransferFunction<ConstantMapLattice> for ConstantTransfer {
    /// For each statement, update constant information
    fn transfer_statement(
        &self,
        stmt: &Statement,
        _stmt_loc: StmtLoc,
        state: &ConstantMapLattice,
    ) -> ConstantMapLattice {
        let mut result_set = state.clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => match lvalue {
                LValue::Variable { var } => {
                    result_set.remove(var);

                    if let Some(constant_val) = self.eval_rvalue(rvalue, &result_set) {
                        result_set.insert(*var, ConstantState::Const(constant_val));
                    } else {
                        result_set.insert(*var, ConstantState::Top);
                    }
                }
                LValue::ArrayElement { .. } => {}
                LValue::TableField { .. } => {}
            },
        }

        result_set
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &ConstantMapLattice,
    ) -> ConstantMapLattice {
        state.clone()
    }

    fn initial_value(&self) -> ConstantMapLattice {
        ConstantMapLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _block: &BasicBlock) -> ConstantMapLattice {
        ConstantMapLattice::bottom().unwrap()
    }
}

impl ConstantTransfer {
    fn eval_rvalue(&self, rvalue: &RValue, constants: &ConstantMapLattice) -> Option<Constant> {
        match rvalue {
            RValue::Use(operand) => self.eval_operand(operand, constants),
            RValue::BinaryOp { op, left, right } => {
                let left_val = self.eval_operand(left, constants)?;
                let right_val = self.eval_operand(right, constants)?;
                self.eval_binary_op(op.clone(), &left_val, &right_val)
            }
            RValue::UnaryOp { op, operand } => {
                let val = self.eval_operand(operand, constants)?;
                self.eval_unary_op(op.clone(), &val)
            }
            _ => None, // Table/array access not constant-foldable
        }
    }

    fn eval_operand(&self, operand: &Operand, constants: &ConstantMapLattice) -> Option<Constant> {
        match operand {
            Operand::Const(c) => {
                match c {
                    Constant::Array(..) => None, // Arrays are not constant-foldable
                    _ => Some(c.clone()),
                }
            }
            Operand::Var(var_id) => constants.get(var_id).and_then(|state| match state {
                ConstantState::Const(value) => Some(value.clone()),
                _ => None,
            }),
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
            (Constant::Float(l), Constant::Float(r)) => {
                use crate::cfg::BinaryOp;
                match op {
                    BinaryOp::Add => Some(Constant::Float(l + r)),
                    BinaryOp::Sub => Some(Constant::Float(l - r)),
                    BinaryOp::Mul => Some(Constant::Float(l * r)),
                    BinaryOp::Div => Some(Constant::Float(l / r)),
                    BinaryOp::Eq => Some(Constant::Bool(l == r)),
                    BinaryOp::Neq => Some(Constant::Bool(l != r)),
                    BinaryOp::Lt => Some(Constant::Bool(l < r)),
                    BinaryOp::Lte => Some(Constant::Bool(l <= r)),
                    BinaryOp::Gt => Some(Constant::Bool(l > r)),
                    BinaryOp::Gte => Some(Constant::Bool(l >= r)),
                    _ => None,
                }
            }
            _ => None, // Unsupported combinations
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
            Constant::Float(val) => match op {
                crate::cfg::UnaryOp::Neg => Some(Constant::Float(-val)),
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
