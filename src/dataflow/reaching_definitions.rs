use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{BasicBlock, ControlFlowEdge, FunctionCfg, Statement, VarId};

/// Definition point for a variable (simplified)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Definition {
    pub var: VarId,
    pub loc: StmtLoc,
}

/// Transfer function for reaching definitions analysis
pub struct ReachingDefTransfer;

impl TransferFunction<SetLattice<Definition>> for ReachingDefTransfer {
    /// For each statement, add definitions and kill previous definitions of same variable
    fn transfer_statement(
        &self,
        stmt: &Statement,
        stmt_loc: StmtLoc,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, .. } => {
                match lvalue {
                    crate::cfg::LValue::Variable { var } => {
                        // Kill all previous definitions of this variable
                        result_set.retain(|def| def.var != *var);
                        // Gen: Add new definition (block and stmt_index would be set by caller)
                        result_set.insert(Definition {
                            var: *var,
                            loc: stmt_loc,
                        });
                    }
                    crate::cfg::LValue::ArrayElement { array, .. } => {
                        result_set.retain(|def| def.var != *array);
                        result_set.insert(Definition {
                            var: *array,
                            loc: stmt_loc,
                        });
                    }
                    crate::cfg::LValue::TableField { .. } => {
                        // Table fields don't define local variables
                    }
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<Definition> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _block: &BasicBlock) -> SetLattice<Definition> {
        // At function entry, parameters are defined, but we don't know its definition
        SetLattice::bottom().unwrap()
    }
}

/// Analyze reaching definitions in a function (forward, function-level)
pub fn analyze_reaching_definitions(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<SetLattice<Definition>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::May,
        ReachingDefTransfer,
    );
    analysis.analyze(func, cfg_program)
}
