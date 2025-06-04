use crate::cfg::{FunctionCfg, Statement, Terminator, VarId};
use crate::dataflow::{
    DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice, TransferFunction,
};
use std::ptr::NonNull;

/// Represents a definition of a variable at a specific program site.
/// The `site` points to the `Statement` that performs the assignment.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Definition {
    pub var_id: VarId,
    pub site: NonNull<Statement>,
}

/// Transfer function for Reaching Definitions analysis.
pub struct ReachingDefinitionsTransfer;

impl TransferFunction<SetLattice<Definition>> for ReachingDefinitionsTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        let mut new_state = state.clone();

        match stmt {
            Statement::Assign { var, .. } => {
                // KILL: Remove all existing definitions of 'var' from new_state.
                // A new definition of 'var' kills all previous ones for that variable.
                new_state.set.retain(|def| def.var_id != *var);

                // GEN: Add the new definition of 'var'.
                // The site of the definition is the current statement itself.
                new_state.set.insert(Definition {
                    var_id: *var,
                    site: NonNull::from(stmt), // `stmt` is `&Statement`
                });
            }
            // Other statement types (e.g., TableAssign, function calls if they were statements)
            // are assumed not to define local VarIds in this basic version.
            // If they did, similar GEN/KILL logic would be needed.
            _ => {
                // For statements that don't assign to a VarId,
                // the set of reaching definitions doesn't change.
            }
        }
        new_state
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        // Terminators (like Goto, Branch, Return) are generally assumed not to
        // define new variables or kill existing definitions in this context.
        // Thus, the set of reaching definitions passes through them unchanged.
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<Definition> {
        // For a forward analysis like Reaching Definitions, the initial value
        // applies to the entry point of the function.
        // At the very beginning, no definitions (from within the function) have occurred yet.
        SetLattice::bottom() // Represents an empty set of definitions
    }

    fn boundary_value(&self) -> SetLattice<Definition> {
        // This value is not strictly used by the current worklist algorithm's
        // initialization for the main analysis pass but is part of the trait.
        // It could represent, e.g., definitions from parameters if they were modeled.
        SetLattice::bottom()
    }
}

/// Helper function to run Reaching Definitions analysis on a given function CFG.
pub fn analyze_reaching_definitions(func: &FunctionCfg) -> DataflowResults<SetLattice<Definition>> {
    let transfer_function = ReachingDefinitionsTransfer;
    let analysis = DataflowAnalysis::new(Direction::Forward, transfer_function);
    analysis.analyze(func)
}
