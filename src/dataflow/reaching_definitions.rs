use crate::cfg::{BasicBlockId, ControlFlowEdge, FunctionCfg, LValue, Statement, VarId};
use crate::dataflow::{
    AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};

/// Represents a definition of a variable at a specific program point.
/// Uses block ID and statement index instead of raw pointers for safety.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Definition {
    pub var_id: VarId,
    pub block_id: BasicBlockId,
    pub stmt_index: usize,
}

/// Transfer function for Reaching Definitions analysis.
/// Reaching definitions is a forward analysis that tracks which definitions
/// of variables can reach each program point.
pub struct ReachingDefinitionsTransfer;

impl TransferFunction<SetLattice<Definition>> for ReachingDefinitionsTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        if state.is_top {
            return state.clone();
        }

        let mut reaching_defs = state.set.clone();

        match stmt {
            Statement::Assign { lvalue, .. } => {
                match lvalue {
                    LValue::Variable { var } => {
                        // KILL: Remove all existing definitions of this variable
                        reaching_defs.retain(|def| def.var_id != *var);
                        // GEN: Add new definition (would need location context)
                    }
                    LValue::ArrayElement { array, .. } => {
                        // KILL: Remove all existing definitions of the array variable
                        reaching_defs.retain(|def| def.var_id != *array);
                        // GEN: Add new definition of the array variable
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't define local variables
                    }
                }
            }
        }

        SetLattice::new(reaching_defs)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        // Control flow edges don't define variables, so pass through unchanged
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<Definition> {
        // Start with empty set for forward analysis
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<Definition> {
        // At function entry, we could model parameter definitions
        // For now, start with empty set
        SetLattice::bottom().unwrap()
    }
}

/// Enhanced transfer function that tracks statement locations
pub struct ReachingDefinitionsTransferWithLocation {
    pub current_block: BasicBlockId,
    pub current_stmt_index: usize,
}

impl TransferFunction<SetLattice<Definition>> for ReachingDefinitionsTransferWithLocation {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        if state.is_top {
            return state.clone();
        }

        let mut reaching_defs = state.set.clone();

        match stmt {
            Statement::Assign { lvalue, .. } => {
                match lvalue {
                    LValue::Variable { var } => {
                        // KILL: Remove all existing definitions of this variable
                        reaching_defs.retain(|def| def.var_id != *var);

                        // GEN: Add new definition at current location
                        reaching_defs.insert(Definition {
                            var_id: *var,
                            block_id: self.current_block,
                            stmt_index: self.current_stmt_index,
                        });
                    }
                    LValue::ArrayElement { array, .. } => {
                        // KILL: Remove all existing definitions of the array variable
                        reaching_defs.retain(|def| def.var_id != *array);

                        // GEN: Add new definition of the array variable
                        reaching_defs.insert(Definition {
                            var_id: *array,
                            block_id: self.current_block,
                            stmt_index: self.current_stmt_index,
                        });
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't define local variables
                    }
                }
            }
        }

        SetLattice::new(reaching_defs)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        // Control flow edges don't define variables
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<Definition> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<Definition> {
        // Could model parameter definitions here
        SetLattice::bottom().unwrap()
    }
}

/// Run reaching definitions analysis on a function
pub fn analyze_reaching_definitions(
    func: &FunctionCfg,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<Definition>> {
    let analysis = DataflowAnalysis::new(level, Direction::Forward, ReachingDefinitionsTransfer);
    analysis.analyze(func)
}
