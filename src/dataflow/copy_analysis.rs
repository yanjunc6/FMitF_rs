use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{
    BasicBlockId, ControlFlowEdge, FunctionCfg, Operand, Statement, VarId,
};

/// Copy relation: var1 = var2
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CopyRelation {
    pub lhs: VarId,
    pub rhs: VarId,
}

/// Copy analysis lattice
pub type CopyMapLattice = SetLattice<CopyRelation>;

/// Transfer function for copy propagation analysis
pub struct CopyTransfer;

impl TransferFunction<CopyMapLattice> for CopyTransfer {
    /// For each statement, update copy relations
    fn transfer_statement(&self, stmt: &Statement, state: &CopyMapLattice) -> CopyMapLattice {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                match lvalue {
                    crate::cfg::LValue::Variable { var } => {
                        // Kill all copy relations involving this variable
                        result_set.retain(|copy| copy.lhs != *var && copy.rhs != *var);

                        // Gen: If rvalue is a simple variable use, add copy relation
                        if let crate::cfg::Rvalue::Use(Operand::Var(source_var)) = rvalue {
                            result_set.insert(CopyRelation {
                                lhs: *var,
                                rhs: *source_var,
                            });
                        }
                    }
                    crate::cfg::LValue::ArrayElement { array, .. } => {
                        // Array element assignment kills copy relations involving the array
                        result_set.retain(|copy| copy.lhs != *array && copy.rhs != *array);
                    }
                    crate::cfg::LValue::TableField { .. } => {
                        // Table field assignments don't affect local copy relations
                    }
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(&self, _edge: &ControlFlowEdge, state: &CopyMapLattice) -> CopyMapLattice {
        state.clone()
    }

    fn initial_value(&self) -> CopyMapLattice {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _blockid: BasicBlockId) -> CopyMapLattice {
        // At function entry, no copy relations exist
        SetLattice::bottom().unwrap()
    }
}

/// Analyze copy relations in a function (forward, function-level)
pub fn analyze_copies(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<CopyMapLattice> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::May,
        CopyTransfer,
    );
    analysis.analyze(func, cfg_program)
}
