use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{ControlFlowEdge, FunctionCfg, LValue, RValue, Statement, VarId};

/// Available expression for tracking computations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AvailExpr {
    lvalue: LValue, // variable being assigned, currently no array element/table field recorded here
    rvalue: RValue, // all possible rvalue is recorded here
}

/// Transfer function for available expressions analysis
pub struct AvailExprTransfer;

impl TransferFunction<SetLattice<AvailExpr>> for AvailExprTransfer {
    /// For each statement, add generated expressions and kill expressions using redefined variables
    fn transfer_statement(
        &self,
        stmt: &Statement,
        _stmt_loc: StmtLoc,
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
                    LValue::Variable { var } => {
                        result_set.retain(|expr| !self.expr_uses_var(expr, *var));

                        // Gen: Add expressions from rvalue
                        result_set.insert(AvailExpr {
                            lvalue: lvalue.clone(),
                            rvalue: rvalue.clone(),
                        });
                    }
                    LValue::ArrayElement { array, .. } => {
                        // Kill all expressions containing the array variable
                        result_set.retain(|expr| !self.expr_uses_var(expr, *array));
                    }
                    LValue::TableField { table, .. } => {
                        // Kill all expressions containing the table
                        result_set.retain(|expr| !self.expr_uses_table(expr, *table));
                    }
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

    fn boundary_value(
        &self,
        _func: &FunctionCfg,
        _block: &crate::cfg::BasicBlock,
    ) -> SetLattice<AvailExpr> {
        // At function entry, no expressions are available
        SetLattice::bottom().unwrap()
    }
}

impl AvailExprTransfer {
    fn expr_uses_var(&self, expr: &AvailExpr, var: VarId) -> bool {
        // Check if the variable is used in either lvalue or rvalue
        let lvalue_uses_var = match &expr.lvalue {
            LValue::Variable { var: lvar } => *lvar == var,
            LValue::ArrayElement { array, index } => {
                *array == var || self.operand_uses_var(index, var)
            }
            LValue::TableField { pk_values, .. } => pk_values
                .iter()
                .any(|operand| self.operand_uses_var(operand, var)),
        };

        let rvalue_uses_var = match &expr.rvalue {
            RValue::Use(operand) => self.operand_uses_var(operand, var),
            RValue::ArrayAccess { array, index } => {
                self.operand_uses_var(array, var) || self.operand_uses_var(index, var)
            }
            RValue::TableAccess { pk_values, .. } => pk_values
                .iter()
                .any(|operand| self.operand_uses_var(operand, var)),
            RValue::UnaryOp { operand, .. } => self.operand_uses_var(operand, var),
            RValue::BinaryOp { left, right, .. } => {
                self.operand_uses_var(left, var) || self.operand_uses_var(right, var)
            }
        };

        lvalue_uses_var || rvalue_uses_var
    }

    fn expr_uses_table(&self, expr: &AvailExpr, table: crate::cfg::TableId) -> bool {
        // Check if the table is used in either lvalue or rvalue
        let lvalue_uses_table = match &expr.lvalue {
            LValue::TableField { table: t, .. } => *t == table,
            _ => false,
        };

        let rvalue_uses_table = match &expr.rvalue {
            RValue::TableAccess { table: t, .. } => *t == table,
            _ => false,
        };

        lvalue_uses_table || rvalue_uses_table
    }

    fn operand_uses_var(&self, operand: &crate::cfg::Operand, var: VarId) -> bool {
        matches!(operand, crate::cfg::Operand::Var(v) if *v == var)
    }
}

/// Analyze available expressions in a function (forward, function-level)
/// available expression can be safely re-used (or common-sub-expression–eliminated) without re-computing it.
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
