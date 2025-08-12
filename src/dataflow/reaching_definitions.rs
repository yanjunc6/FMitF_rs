use crate::cfg::{ControlFlowEdge, LValue, Statement, VarId};
use crate::dataflow::{AnalysisLevel, DataflowResults, Lattice, SetLattice, TransferFunction};

/// Transfer function for Reaching Definitions analysis.
/// Reaching definitions is a forward analysis that tracks which variables
/// have been defined.
pub struct ReachingDefinitionsTransfer;

impl TransferFunction<SetLattice<VarId>> for ReachingDefinitionsTransfer {
    fn transfer_statement(&self, stmt: &Statement, state: &SetLattice<VarId>) -> SetLattice<VarId> {
        if state.is_top() {
            return state.clone();
        }

        let mut defined_vars = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, .. } => {
                match lvalue {
                    LValue::Variable { var } => {
                        // GEN: Add this variable as defined
                        defined_vars.insert(*var);
                    }
                    LValue::ArrayElement { array, .. } => {
                        // GEN: Add array variable as defined
                        defined_vars.insert(*array);
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't define local variables
                    }
                }
            }
        }

        SetLattice::new(defined_vars)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<VarId>,
    ) -> SetLattice<VarId> {
        // Control flow edges don't define variables, so pass through unchanged
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<VarId> {
        // Start with empty set for forward analysis
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<VarId> {
        // At function entry, parameters are considered defined
        // For now, start with empty set - parameters will be added by caller if needed
        SetLattice::bottom().unwrap()
    }
}

/// Run reaching definitions analysis on a function
pub fn analyze_reaching_definitions(
    prog: &crate::cfg::CfgProgram,
    func_id: crate::cfg::FunctionId,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<VarId>> {
    use crate::dataflow::{DataflowAnalysis, Direction};

    // Get the function from the program
    if let Some(func) = prog.functions.get(func_id) {
        let analysis =
            DataflowAnalysis::new(level, Direction::Forward, ReachingDefinitionsTransfer);
        analysis.analyze(func, prog)
    } else {
        // Return empty results if function not found
        DataflowResults {
            block_entry: std::collections::HashMap::new(),
            block_exit: std::collections::HashMap::new(),
            stmt_entry: std::collections::HashMap::new(),
            stmt_exit: std::collections::HashMap::new(),
        }
    }
}
