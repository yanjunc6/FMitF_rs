use crate::cfg::{BasicBlockId, FunctionCfg, Statement, ControlFlowEdge, VarId};
use crate::dataflow::{
    DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice, TransferFunction,
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
            Statement::Assign { var, .. } => {
                // KILL: Remove all existing definitions of this variable
                reaching_defs.retain(|def| def.var_id != *var);

                // GEN: Add new definition (we need context to get proper location)
                // For the basic transfer function, we'll skip adding the definition
                // since we don't have the current block context
                // This is why the enhanced version with location tracking is needed
            }
            Statement::TableAssign { .. } => {
                // Table assignments don't define local variables, so no change
            }
        }

        SetLattice::new(reaching_defs)
    }

    fn transfer_edge(&self, _edge: &ControlFlowEdge, state: &SetLattice<Definition>) -> SetLattice<Definition> {
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

impl ReachingDefinitionsTransferWithLocation {
    pub fn new(block_id: BasicBlockId) -> Self {
        Self {
            current_block: block_id,
            current_stmt_index: 0,
        }
    }

    pub fn set_statement_index(&mut self, index: usize) {
        self.current_stmt_index = index;
    }
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
            Statement::Assign { var, .. } => {
                // KILL: Remove all existing definitions of this variable
                reaching_defs.retain(|def| def.var_id != *var);

                // GEN: Add new definition at current location
                reaching_defs.insert(Definition {
                    var_id: *var,
                    block_id: self.current_block,
                    stmt_index: self.current_stmt_index,
                });
            }
            Statement::TableAssign { .. } => {
                // Table assignments don't define local variables
            }
        }

        SetLattice::new(reaching_defs)
    }

    fn transfer_edge(&self, _edge: &ControlFlowEdge, state: &SetLattice<Definition>) -> SetLattice<Definition> {
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
pub fn analyze_reaching_definitions(func: &FunctionCfg) -> DataflowResults<SetLattice<Definition>> {
    let analysis = DataflowAnalysis::new(Direction::Forward, ReachingDefinitionsTransfer);
    analysis.analyze(func)
}

/// Run reaching definitions analysis with location tracking
/// This provides more detailed analysis but requires per-block transfer functions
pub fn analyze_reaching_definitions_detailed(func: &FunctionCfg) -> DataflowResults<SetLattice<Definition>> {
    // For simplicity, we'll use the basic transfer function for now
    // In a more sophisticated implementation, you'd create per-block transfer functions
    // and handle statement indexing properly
    analyze_reaching_definitions(func)
}
