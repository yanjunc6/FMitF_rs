use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{BasicBlockId, ControlFlowEdge, FunctionCfg, Operand, Rvalue, Statement, VarId};

/// Available expression for tracking computations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AvailExpr {
    BinaryOp {
        op: crate::cfg::BinaryOp,
        left: Operand,
        right: Operand,
    },
    UnaryOp {
        op: crate::cfg::UnaryOp,
        operand: Operand,
    },
    ArrayAccess {
        array: Operand,
        index: Operand,
    },
}

/// Transfer function for available expressions analysis
pub struct AvailExprTransfer;

impl TransferFunction<SetLattice<AvailExpr>> for AvailExprTransfer {
    /// For each statement, add generated expressions and kill expressions using redefined variables
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<AvailExpr>,
    ) -> SetLattice<AvailExpr> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Kill expressions that use the assigned variable
                match lvalue {
                    crate::cfg::LValue::Variable { var } => {
                        result_set.retain(|expr| !self.expr_uses_var(expr, *var));
                    }
                    crate::cfg::LValue::ArrayElement { array, .. } => {
                        result_set.retain(|expr| !self.expr_uses_var(expr, *array));
                    }
                    crate::cfg::LValue::TableField { .. } => {
                        // Table assignments don't kill local expressions
                    }
                }

                // Gen: Add expressions from rvalue
                if let Some(expr) = self.rvalue_to_expr(rvalue) {
                    result_set.insert(expr);
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<AvailExpr>,
    ) -> SetLattice<AvailExpr> {
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<AvailExpr> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _blockid: BasicBlockId) -> SetLattice<AvailExpr> {
        // At function entry, no expressions are available
        SetLattice::bottom().unwrap()
    }
}

impl AvailExprTransfer {
    fn rvalue_to_expr(&self, rvalue: &Rvalue) -> Option<AvailExpr> {
        match rvalue {
            Rvalue::BinaryOp { op, left, right } => Some(AvailExpr::BinaryOp {
                op: op.clone(),
                left: left.clone(),
                right: right.clone(),
            }),
            Rvalue::UnaryOp { op, operand } => Some(AvailExpr::UnaryOp {
                op: op.clone(),
                operand: operand.clone(),
            }),
            Rvalue::ArrayAccess { array, index } => Some(AvailExpr::ArrayAccess {
                array: array.clone(),
                index: index.clone(),
            }),
            _ => None, // Use and table access are not tracked as expressions
        }
    }

    fn expr_uses_var(&self, expr: &AvailExpr, var: VarId) -> bool {
        match expr {
            AvailExpr::BinaryOp { left, right, .. } => {
                self.operand_uses_var(left, var) || self.operand_uses_var(right, var)
            }
            AvailExpr::UnaryOp { operand, .. } => self.operand_uses_var(operand, var),
            AvailExpr::ArrayAccess { array, index } => {
                self.operand_uses_var(array, var) || self.operand_uses_var(index, var)
            }
        }
    }

    fn operand_uses_var(&self, operand: &Operand, var: VarId) -> bool {
        matches!(operand, Operand::Var(v) if *v == var)
    }
}

/// Analyze available expressions in a function (forward, function-level)
pub fn analyze_available_expressions(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<SetLattice<AvailExpr>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::Must,
        AvailExprTransfer,
    );
    analysis.analyze(func, cfg_program)
}
