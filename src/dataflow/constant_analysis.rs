use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Flat, Lattice,
    MapLattice, StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, BinaryOp, ConstantValue, Function, Instruction, Operand, Terminator,
    UnaryOp, VariableId,
};

/// Transfer function for constant propagation analysis
pub struct ConstantTransfer;

impl TransferFunction<MapLattice<VariableId, ConstantValue>> for ConstantTransfer {
    fn transfer_instruction(
        &self,
        inst: &Instruction,
        _stmt_loc: StmtLoc,
        state: &MapLattice<VariableId, ConstantValue>,
    ) -> MapLattice<VariableId, ConstantValue> {
        // If the whole lattice element is ⊤ just propagate it unchanged
        if state.is_top() {
            return Lattice::top().unwrap();
        }

        let mut out = state.clone();

        match inst {
            Instruction::Assign { dest, src } => {
                /* KILL: remove any fact mentioning the target variable */
                out.insert(*dest, Flat::Bottom);

                /* GEN: try to fold the operand to a constant */
                if let Some(c) = self.eval_operand(src, state) {
                    out.insert(*dest, Flat::Value(c));
                }
            }
            Instruction::BinaryOp {
                dest,
                op,
                left,
                right,
            } => {
                /* KILL: remove any fact mentioning the target variable */
                out.insert(*dest, Flat::Bottom);

                /* GEN: try to fold the binary operation to a constant */
                if let (Some(l), Some(r)) = (
                    self.eval_operand(left, state),
                    self.eval_operand(right, state),
                ) {
                    if let Some(result) = Self::eval_binary_op(*op, &l, &r) {
                        out.insert(*dest, Flat::Value(result));
                    }
                }
            }
            Instruction::UnaryOp { dest, op, operand } => {
                /* KILL: remove any fact mentioning the target variable */
                out.insert(*dest, Flat::Bottom);

                /* GEN: try to fold the unary operation to a constant */
                if let Some(v) = self.eval_operand(operand, state) {
                    if let Some(result) = Self::eval_unary_op(*op, &v) {
                        out.insert(*dest, Flat::Value(result));
                    }
                }
            }
            Instruction::Call { dest, .. } => {
                // Function calls are not constant-foldable
                if let Some(dest_var) = dest {
                    out.insert(*dest_var, Flat::Bottom);
                }
            }
            Instruction::TableGet { dest, .. } => {
                // Table operations are not constant-foldable
                out.insert(*dest, Flat::Bottom);
            }
            Instruction::TableSet { .. } | Instruction::Assert { .. } => {
                // These instructions don't define variables
            }
        }

        out
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        _block_id: BasicBlockId,
        state: &MapLattice<VariableId, ConstantValue>,
    ) -> MapLattice<VariableId, ConstantValue> {
        // Terminators don't affect constant propagation
        state.clone()
    }

    fn initial_value(&self) -> MapLattice<VariableId, ConstantValue> {
        Lattice::bottom().unwrap() // ⊥ = empty map
    }

    fn boundary_value(
        &self,
        _func: &Function,
        _block: &BasicBlock,
    ) -> MapLattice<VariableId, ConstantValue> {
        Lattice::bottom().unwrap()
    }
}

impl ConstantTransfer {
    fn eval_operand(
        &self,
        op: &Operand,
        facts: &MapLattice<VariableId, ConstantValue>,
    ) -> Option<ConstantValue> {
        match op {
            Operand::Constant(c) => Some(c.clone()),
            Operand::Variable(v) => match facts.get(v) {
                Flat::Value(c) => Some(c),
                _ => None,
            },
            Operand::Global(_) => None, // We don't track global constants here
        }
    }

    /* ---------------- simple constant folding helpers ---------------- */

    fn eval_unary_op(op: UnaryOp, v: &ConstantValue) -> Option<ConstantValue> {
        use UnaryOp::*;
        match (op, v) {
            (NegInt, ConstantValue::Int(i)) => Some(ConstantValue::Int(-i)),
            (NegFloat, ConstantValue::Float(f)) => Some(ConstantValue::Float(-f)),
            (NotBool, ConstantValue::Bool(b)) => Some(ConstantValue::Bool(!b)),
            _ => None,
        }
    }

    fn eval_binary_op(op: BinaryOp, l: &ConstantValue, r: &ConstantValue) -> Option<ConstantValue> {
        use BinaryOp::*;
        match (l, r) {
            (ConstantValue::Int(a), ConstantValue::Int(b)) => match op {
                AddInt => Some(ConstantValue::Int(a + b)),
                SubInt => Some(ConstantValue::Int(a - b)),
                MulInt => Some(ConstantValue::Int(a * b)),
                DivInt if *b != 0 => Some(ConstantValue::Int(a / b)),
                ModInt if *b != 0 => Some(ConstantValue::Int(a % b)),
                EqInt => Some(ConstantValue::Bool(a == b)),
                NeqInt => Some(ConstantValue::Bool(a != b)),
                LtInt => Some(ConstantValue::Bool(a < b)),
                LeqInt => Some(ConstantValue::Bool(a <= b)),
                GtInt => Some(ConstantValue::Bool(a > b)),
                GeqInt => Some(ConstantValue::Bool(a >= b)),
                _ => None,
            },
            (ConstantValue::Float(a), ConstantValue::Float(b)) => match op {
                AddFloat => Some(ConstantValue::Float(a + b)),
                SubFloat => Some(ConstantValue::Float(a - b)),
                MulFloat => Some(ConstantValue::Float(a * b)),
                DivFloat => Some(ConstantValue::Float(a / b)),
                EqFloat => Some(ConstantValue::Bool(a == b)),
                NeqFloat => Some(ConstantValue::Bool(a != b)),
                LtFloat => Some(ConstantValue::Bool(a < b)),
                LeqFloat => Some(ConstantValue::Bool(a <= b)),
                GtFloat => Some(ConstantValue::Bool(a > b)),
                GeqFloat => Some(ConstantValue::Bool(a >= b)),
                _ => None,
            },
            (ConstantValue::Bool(a), ConstantValue::Bool(b)) => match op {
                And => Some(ConstantValue::Bool(*a && *b)),
                Or => Some(ConstantValue::Bool(*a || *b)),
                Eq => Some(ConstantValue::Bool(a == b)),
                Neq => Some(ConstantValue::Bool(a != b)),
                _ => None,
            },
            _ => None,
        }
    }
}

/// Analyze constants in a function (forward, function-level)
pub fn analyze_constants(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<MapLattice<VariableId, ConstantValue>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::May,
        ConstantTransfer,
    );
    analysis.analyze(func, program)
}
