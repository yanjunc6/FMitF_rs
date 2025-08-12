use crate::cfg::{
    BinaryOp, Constant, ControlFlowEdge, LValue, Operand, Rvalue, Statement, UnaryOp, VarId,
};
use crate::dataflow::{AnalysisLevel, DataflowResults, SetLattice, TransferFunction};
use std::collections::HashMap;

/// Constant data for a single variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantMapData {
    pub var: VarId,
    pub state: ConstantState,
}

/// States for constant analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantState {
    Bottom,             // Undefined
    Constant(Constant), // Known constant value
    Top,                // Unknown (could be any value)
}

impl ConstantMapData {
    pub fn new(var: VarId, state: ConstantState) -> Self {
        Self { var, state }
    }

    pub fn bottom(var: VarId) -> Self {
        Self::new(var, ConstantState::Bottom)
    }

    pub fn constant(var: VarId, value: Constant) -> Self {
        Self::new(var, ConstantState::Constant(value))
    }

    pub fn top(var: VarId) -> Self {
        Self::new(var, ConstantState::Top)
    }
}

/// Constant analysis using SetLattice
pub type ConstantMapLattice = SetLattice<ConstantMapData>;

impl ConstantMapLattice {
    pub fn get(&self, var: VarId) -> ConstantState {
        // Find the entry for this variable
        for entry in &self.set {
            if entry.var == var {
                return entry.state.clone();
            }
        }
        ConstantState::Bottom
    }

    pub fn set(&mut self, var: VarId, state: ConstantState) {
        // Remove any existing entry for this variable
        self.set.retain(|entry| entry.var != var);

        // Add the new entry
        self.set.insert(ConstantMapData::new(var, state));
    }

    pub fn get_constant_value(&self, var: VarId) -> Option<Constant> {
        match self.get(var) {
            ConstantState::Constant(value) => Some(value),
            _ => None,
        }
    }
}

/// Transfer function for constant propagation analysis
pub struct ConstantAnalysisTransfer;

impl ConstantAnalysisTransfer {
    pub fn evaluate_unary(&self, op: &UnaryOp, operand: &Constant) -> Option<Constant> {
        match (op, operand) {
            (UnaryOp::Neg, Constant::Int(n)) => Some(Constant::Int(-n)),
            (UnaryOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
            _ => None,
        }
    }

    fn evaluate_operand(&self, operand: &Operand, state: &ConstantMapLattice) -> ConstantState {
        match operand {
            Operand::Const(c) => ConstantState::Constant(c.clone()),
            Operand::Var(var) => state.get(*var),
        }
    }

    fn evaluate_rvalue(&self, rvalue: &Rvalue, state: &ConstantMapLattice) -> ConstantState {
        match rvalue {
            Rvalue::Use(operand) => self.evaluate_operand(operand, state),
            Rvalue::BinaryOp { op, left, right } => {
                let left_val = self.evaluate_operand(left, state);
                let right_val = self.evaluate_operand(right, state);
                match (left_val, right_val) {
                    (ConstantState::Constant(c1), ConstantState::Constant(c2)) => {
                        if let Some(result) = self.eval_binary_op(op, &c1, &c2) {
                            ConstantState::Constant(result)
                        } else {
                            ConstantState::Top
                        }
                    }
                    (ConstantState::Bottom, _) | (_, ConstantState::Bottom) => {
                        ConstantState::Bottom
                    }
                    _ => ConstantState::Top,
                }
            }
            Rvalue::UnaryOp { op, operand } => {
                let operand_val = self.evaluate_operand(operand, state);
                match operand_val {
                    ConstantState::Constant(c) => {
                        if let Some(result) = self.evaluate_unary(op, &c) {
                            ConstantState::Constant(result)
                        } else {
                            ConstantState::Top
                        }
                    }
                    ConstantState::Bottom => ConstantState::Bottom,
                    ConstantState::Top => ConstantState::Top,
                }
            }
            _ => ConstantState::Top, // Table access, array access are not constant
        }
    }

    fn eval_binary_op(&self, op: &BinaryOp, left: &Constant, right: &Constant) -> Option<Constant> {
        match (op, left, right) {
            (BinaryOp::Add, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a + b)),
            (BinaryOp::Sub, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a - b)),
            (BinaryOp::Mul, Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a * b)),
            (BinaryOp::Div, Constant::Int(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Int(a / b))
            }
            (BinaryOp::Lt, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a < b)),
            (BinaryOp::Lte, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a <= b)),
            (BinaryOp::Gt, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a > b)),
            (BinaryOp::Gte, Constant::Int(a), Constant::Int(b)) => Some(Constant::Bool(a >= b)),
            (BinaryOp::Eq, a, b) => Some(Constant::Bool(a == b)),
            (BinaryOp::Neq, a, b) => Some(Constant::Bool(a != b)),
            (BinaryOp::And, Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a && *b)),
            (BinaryOp::Or, Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a || *b)),
            _ => None, // Division by zero, type mismatch, etc.
        }
    }
}

impl TransferFunction<ConstantMapLattice> for ConstantAnalysisTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &ConstantMapLattice,
    ) -> ConstantMapLattice {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                let mut new_state = state.clone();
                match lvalue {
                    LValue::Variable { var } => {
                        let new_value = self.evaluate_rvalue(rvalue, state);
                        new_state.set(*var, new_value);
                    }
                    _ => {
                        // Array element or table field assignments don't define variables directly
                    }
                }
                new_state
            }
        }
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &ConstantMapLattice,
    ) -> ConstantMapLattice {
        state.clone()
    }

    fn initial_value(&self) -> ConstantMapLattice {
        SetLattice::new(std::collections::HashSet::new())
    }

    fn boundary_value(&self) -> ConstantMapLattice {
        SetLattice::new(std::collections::HashSet::new())
    }
}

/// Run constant analysis on a function
pub fn analyze_constants(
    prog: &crate::cfg::CfgProgram,
    func_id: crate::cfg::FunctionId,
    level: AnalysisLevel,
) -> DataflowResults<ConstantMapLattice> {
    use crate::dataflow::{DataflowAnalysis, Direction};

    if let Some(func) = prog.functions.get(func_id) {
        let analysis = DataflowAnalysis::new(level, Direction::Forward, ConstantAnalysisTransfer);
        analysis.analyze(func, prog)
    } else {
        DataflowResults {
            block_entry: HashMap::new(),
            block_exit: HashMap::new(),
            stmt_entry: HashMap::new(),
            stmt_exit: HashMap::new(),
        }
    }
}
