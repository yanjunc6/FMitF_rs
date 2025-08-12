//! Copy propagation analysis using unified SetLattice approach
//! This is a forward analysis that tracks which variables are copies of other variables.

use crate::cfg::{CfgProgram, ControlFlowEdge, FunctionCfg, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{
    AnalysisLevel, CopyData, CopyValue, DataflowAnalysis, DataflowResults, Direction, Lattice,
    SetLattice, TransferFunction,
};

/// Transfer function for copy analysis using SetLattice<CopyData>
pub struct CopyAnalysisSetLatticeTransfer;

impl TransferFunction<SetLattice<CopyData>> for CopyAnalysisSetLatticeTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<CopyData>,
    ) -> SetLattice<CopyData> {
        if state.is_top {
            return state.clone();
        }

        let mut copy_data: std::collections::HashSet<CopyData> = state.set.clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                if let LValue::Variable { var } = lvalue {
                    // Remove any existing copy information for this variable
                    copy_data.retain(|data| data.var_id != *var);

                    // Check if this is a copy assignment
                    if let Some(source_var) = self.is_copy_assignment(rvalue) {
                        // This variable is now a copy of source_var
                        copy_data.insert(CopyData {
                            var_id: *var,
                            copy_info: CopyValue::Copy(source_var),
                        });
                    } else {
                        // This variable is assigned a non-copy value (Top)
                        copy_data.insert(CopyData {
                            var_id: *var,
                            copy_info: CopyValue::Top,
                        });
                    }

                    // Invalidate any copies that were using this variable as source
                    let vars_to_invalidate: Vec<VarId> = copy_data
                        .iter()
                        .filter_map(|data| {
                            if let CopyValue::Copy(source) = data.copy_info {
                                if source == *var {
                                    Some(data.var_id)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect();

                    for invalidated_var in vars_to_invalidate {
                        copy_data.retain(|data| data.var_id != invalidated_var);
                        copy_data.insert(CopyData {
                            var_id: invalidated_var,
                            copy_info: CopyValue::Top,
                        });
                    }
                }
            }
        }

        SetLattice::new(copy_data)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<CopyData>,
    ) -> SetLattice<CopyData> {
        // Control flow edges don't affect copy relationships
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<CopyData> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<CopyData> {
        // At function entry, no copy relationships exist
        SetLattice::bottom().unwrap()
    }
}

impl CopyAnalysisSetLatticeTransfer {
    fn is_copy_assignment(&self, rvalue: &Rvalue) -> Option<VarId> {
        match rvalue {
            Rvalue::Use(Operand::Var(var_id)) => Some(*var_id),
            _ => None,
        }
    }
}

/// Analyze copy propagation for a function using SetLattice approach
pub fn analyze_copies_setlattice(
    func: &FunctionCfg,
    cfg_program: &CfgProgram,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<CopyData>> {
    let analysis = DataflowAnalysis::new(level, Direction::Forward, CopyAnalysisSetLatticeTransfer);
    analysis.analyze(func, cfg_program)
}
