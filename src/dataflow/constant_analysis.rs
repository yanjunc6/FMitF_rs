use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, Constant, ControlFlowEdge, FunctionCfg, LValue, Operand, RValue, Statement, VarId,
};

/// Single definite-constant fact  (x = c).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstRelation {
    pub var: VarId,
    pub value: Constant,
}

/// Transfer function for constant propagation analysis
pub struct ConstantTransfer;

impl TransferFunction<SetLattice<ConstRelation>> for ConstantTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        _stmt_loc: StmtLoc,
        state: &SetLattice<ConstRelation>,
    ) -> SetLattice<ConstRelation> {
        // If the whole lattice element is ⊤ just propagate it unchanged
        if state.is_top() {
            return SetLattice::top_element();
        }

        // Work on an owned copy of the inner HashSet
        let mut out = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                if let LValue::Variable { var } = lvalue {
                    /* KILL: remove any fact mentioning the target variable */
                    out.retain(|rel| rel.var != *var);

                    /* GEN: try to fold the R-value to a constant */
                    if let Some(c) = self.eval_rvalue(rvalue, &out) {
                        out.insert(ConstRelation {
                            var: *var,
                            value: c,
                        });
                    }
                }
            }
        }

        SetLattice::new(out)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<ConstRelation>,
    ) -> SetLattice<ConstRelation> {
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<ConstRelation> {
        SetLattice::bottom().unwrap() // ⊥ = ∅
    }

    fn boundary_value(
        &self,
        _func: &FunctionCfg,
        _block: &BasicBlock,
    ) -> SetLattice<ConstRelation> {
        SetLattice::bottom().unwrap()
    }
}

impl ConstantTransfer {
    fn eval_rvalue(
        &self,
        rv: &RValue,
        facts: &std::collections::HashSet<ConstRelation>,
    ) -> Option<Constant> {
        match rv {
            RValue::Use(op) => self.eval_operand(op, facts),
            RValue::UnaryOp { op, operand } => {
                let v = self.eval_operand(operand, facts)?;
                Self::eval_unary_op(op.clone(), &v)
            }
            RValue::BinaryOp { op, left, right } => {
                let l = self.eval_operand(left, facts)?;
                let r = self.eval_operand(right, facts)?;
                Self::eval_binary_op(op.clone(), &l, &r)
            }
            _ => None,
        }
    }

    fn eval_operand(
        &self,
        op: &Operand,
        facts: &std::collections::HashSet<ConstRelation>,
    ) -> Option<Constant> {
        match op {
            Operand::Const(c) => match c {
                Constant::Array(..) => None,
                _ => Some(c.clone()),
            },
            Operand::Var(v) => facts
                .iter()
                .find(|rel| rel.var == *v)
                .map(|rel| rel.value.clone()),
        }
    }

    /* ---------------- simple constant folding helpers ---------------- */

    fn eval_unary_op(op: crate::cfg::UnaryOp, v: &Constant) -> Option<Constant> {
        use crate::cfg::UnaryOp::*;
        match (op, v) {
            (Neg, Constant::Int(i)) => Some(Constant::Int(-i)),
            (Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
            (Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
            _ => None,
        }
    }

    fn eval_binary_op(op: crate::cfg::BinaryOp, l: &Constant, r: &Constant) -> Option<Constant> {
        use crate::cfg::BinaryOp::*;
        match (l, r) {
            (Constant::Int(a), Constant::Int(b)) => match op {
                Add => Some(Constant::Int(a + b)),
                Sub => Some(Constant::Int(a - b)),
                Mul => Some(Constant::Int(a * b)),
                Div if *b != 0 => Some(Constant::Int(a / b)),
                Eq => Some(Constant::Bool(a == b)),
                Neq => Some(Constant::Bool(a != b)),
                Lt => Some(Constant::Bool(a < b)),
                Lte => Some(Constant::Bool(a <= b)),
                Gt => Some(Constant::Bool(a > b)),
                Gte => Some(Constant::Bool(a >= b)),
                _ => None,
            },
            (Constant::Float(a), Constant::Float(b)) => match op {
                Add => Some(Constant::Float(a + b)),
                Sub => Some(Constant::Float(a - b)),
                Mul => Some(Constant::Float(a * b)),
                Div => Some(Constant::Float(a / b)),
                Eq => Some(Constant::Bool(a == b)),
                Neq => Some(Constant::Bool(a != b)),
                Lt => Some(Constant::Bool(a < b)),
                Lte => Some(Constant::Bool(a <= b)),
                Gt => Some(Constant::Bool(a > b)),
                Gte => Some(Constant::Bool(a >= b)),
                _ => None,
            },
            (Constant::Bool(a), Constant::Bool(b)) => match op {
                And => Some(Constant::Bool(*a && *b)),
                Or => Some(Constant::Bool(*a || *b)),
                Eq => Some(Constant::Bool(a == b)),
                Neq => Some(Constant::Bool(a != b)),
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
) -> DataflowResults<SetLattice<ConstRelation>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::Must, // here, we might lose some information as compromise
        ConstantTransfer,
    );
    analysis.analyze(func, cfg_program)
}
