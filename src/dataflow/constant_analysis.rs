use crate::cfg::{
    BinaryOp, Constant, ControlFlowEdge, LValue, Operand, Rvalue, Statement, UnaryOp, VarId,
};
use crate::dataflow::{AnalysisLevel, DataflowResults, Lattice, TransferFunction};
use std::collections::HashMap;

/// Constant value lattice for constant propagation analysis
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantLattice {
    Bottom,             // Undefined
    Constant(Constant), // Known constant value
    Top,                // Unknown (could be any value)
}

impl Lattice for ConstantLattice {
    fn bottom() -> Option<Self> {
        Some(ConstantLattice::Bottom)
    }

    fn top() -> Option<Self> {
        Some(ConstantLattice::Top)
    }

    fn meet(&self, other: &Self) -> Self {
        match (self, other) {
            (ConstantLattice::Bottom, _) | (_, ConstantLattice::Bottom) => ConstantLattice::Bottom,
            (ConstantLattice::Top, x) | (x, ConstantLattice::Top) => x.clone(),
            (ConstantLattice::Constant(c1), ConstantLattice::Constant(c2)) => {
                if c1 == c2 {
                    ConstantLattice::Constant(c1.clone())
                } else {
                    ConstantLattice::Top
                }
            }
        }
    }

    fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (ConstantLattice::Top, _) | (_, ConstantLattice::Top) => ConstantLattice::Top,
            (ConstantLattice::Bottom, x) | (x, ConstantLattice::Bottom) => x.clone(),
            (ConstantLattice::Constant(c1), ConstantLattice::Constant(c2)) => {
                if c1 == c2 {
                    ConstantLattice::Constant(c1.clone())
                } else {
                    ConstantLattice::Top
                }
            }
        }
    }
}

/// Map lattice for constant analysis (Var -> ConstantLattice)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantMapLattice {
    map: HashMap<VarId, ConstantLattice>,
}

impl ConstantMapLattice {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, var: VarId) -> ConstantLattice {
        self.map
            .get(&var)
            .cloned()
            .unwrap_or(ConstantLattice::Bottom)
    }

    pub fn set(&mut self, var: VarId, value: ConstantLattice) {
        // Always store the value, even if it's Bottom, for consistent representation
        self.map.insert(var, value);
    }
}

impl Lattice for ConstantMapLattice {
    fn bottom() -> Option<Self> {
        Some(ConstantMapLattice::new())
    }

    fn top() -> Option<Self> {
        None // Top is infinite map
    }

    fn meet(&self, other: &Self) -> Self {
        let mut result_map = HashMap::new();

        // Intersection of keys
        for (&var, value1) in &self.map {
            if let Some(value2) = other.map.get(&var) {
                let meet_result = value1.meet(value2);
                // Always insert the result, even if it's Bottom, to maintain consistent representation
                result_map.insert(var, meet_result);
            }
        }

        ConstantMapLattice { map: result_map }
    }

    fn join(&self, other: &Self) -> Self {
        let mut result_map = HashMap::new();

        // Union of keys
        let all_vars: std::collections::HashSet<VarId> =
            self.map.keys().chain(other.map.keys()).cloned().collect();

        for var in all_vars {
            let value1 = self.get(var);
            let value2 = other.get(var);
            let join_result = value1.join(&value2);
            // Always insert the result, even if it's Bottom, to maintain consistent representation
            result_map.insert(var, join_result);
        }

        ConstantMapLattice { map: result_map }
    }
}

/// Transfer function for constant analysis
pub struct ConstantAnalysisTransfer;

impl ConstantAnalysisTransfer {
    fn evaluate_operand(&self, operand: &Operand, state: &ConstantMapLattice) -> ConstantLattice {
        match operand {
            Operand::Const(c) => ConstantLattice::Constant(c.clone()),
            Operand::Var(var_id) => state.get(*var_id),
        }
    }

    fn evaluate_rvalue(&self, rvalue: &Rvalue, state: &ConstantMapLattice) -> ConstantLattice {
        match rvalue {
            Rvalue::Use(operand) => self.evaluate_operand(operand, state),
            Rvalue::BinaryOp { op, left, right } => {
                let left_val = self.evaluate_operand(left, state);
                let right_val = self.evaluate_operand(right, state);
                match (left_val, right_val) {
                    (ConstantLattice::Constant(c1), ConstantLattice::Constant(c2)) => {
                        if let Some(result) = Self::eval_binary_op(op, &c1, &c2) {
                            ConstantLattice::Constant(result)
                        } else {
                            ConstantLattice::Top
                        }
                    }
                    (ConstantLattice::Bottom, _) | (_, ConstantLattice::Bottom) => {
                        ConstantLattice::Bottom
                    }
                    _ => ConstantLattice::Top,
                }
            }
            Rvalue::UnaryOp { op, operand } => {
                let operand_val = self.evaluate_operand(operand, state);
                match operand_val {
                    ConstantLattice::Constant(c) => {
                        if let Some(result) = Self::eval_unary_op(op, &c) {
                            ConstantLattice::Constant(result)
                        } else {
                            ConstantLattice::Top
                        }
                    }
                    ConstantLattice::Bottom => ConstantLattice::Bottom,
                    ConstantLattice::Top => ConstantLattice::Top,
                }
            }
            _ => ConstantLattice::Top, // Table access, array access are not constant
        }
    }

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

    fn eval_unary_op(op: &UnaryOp, operand: &Constant) -> Option<Constant> {
        match (op, operand) {
            (UnaryOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
            (UnaryOp::Neg, Constant::Int(i)) => Some(Constant::Int(-i)),
            (UnaryOp::Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
            _ => None,
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
        ConstantMapLattice::new()
    }

    fn boundary_value(&self) -> ConstantMapLattice {
        ConstantMapLattice::new()
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
