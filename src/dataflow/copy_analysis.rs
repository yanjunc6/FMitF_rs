use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{ControlFlowEdge, FunctionCfg, LValue, Operand, RValue, Statement, VarId};

/// Copy relation: var1 = var2
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CopyRelation {
    pub lhs: VarId,
    pub rhs: VarId,
}
/// Transfer function for copy propagation analysis
pub struct CopyTransfer;

impl TransferFunction<SetLattice<CopyRelation>> for CopyTransfer {
    /// For each statement, update copy relations
    fn transfer_statement(
        &self,
        stmt: &Statement,
        _stmt_loc: StmtLoc,
        state: &SetLattice<CopyRelation>,
    ) -> SetLattice<CopyRelation> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                match lvalue {
                    LValue::Variable { var } => {
                        // Kill all copy relations involving this variable
                        result_set.retain(|copy| copy.lhs != *var && copy.rhs != *var);

                        // Gen: If rvalue is a simple variable use, add copy relation
                        if let RValue::Use(Operand::Var(source_var)) = rvalue {
                            result_set.insert(CopyRelation {
                                lhs: *var,
                                rhs: *source_var,
                            });
                        }
                    }
                    LValue::ArrayElement { array, .. } => {
                        // Array element assignment kills copy relations involving the array
                        result_set.retain(|copy| copy.lhs != *array && copy.rhs != *array);
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't affect local copy relations
                    }
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<CopyRelation>,
    ) -> SetLattice<CopyRelation> {
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<CopyRelation> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(
        &self,
        _func: &FunctionCfg,
        _block: &crate::cfg::BasicBlock,
    ) -> SetLattice<CopyRelation> {
        SetLattice::bottom().unwrap()
    }
}

/// Analyze copy relations in a function (forward, function-level)
/// whether a variable definitely holds the value of another single variable
pub fn analyze_copies(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<SetLattice<CopyRelation>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::Must,
        CopyTransfer,
    );
    analysis.analyze(func, cfg_program)
}
