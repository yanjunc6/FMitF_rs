use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, Function, Instruction, InstructionKind, Terminator, VariableId,
};

/// Definition point for a variable (simplified)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Definition {
    pub var: VariableId,
    pub loc: StmtLoc,
}

/// Transfer function for reaching definitions analysis
pub struct ReachingDefTransfer;

impl TransferFunction<SetLattice<Definition>> for ReachingDefTransfer {
    /// For each instruction, add definitions and kill previous definitions of same variable
    fn transfer_instruction(
        &self,
        inst: &Instruction,
        stmt_loc: StmtLoc,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match &inst.kind {
            InstructionKind::Assign { dest, .. } => {
                // Kill all previous definitions of this variable
                result_set.retain(|def| def.var != *dest);
                // Gen: Add new definition
                result_set.insert(Definition {
                    var: *dest,
                    loc: stmt_loc,
                });
            }
            InstructionKind::BinaryOp { dest, .. } => {
                // Kill all previous definitions of this variable
                result_set.retain(|def| def.var != *dest);
                // Gen: Add new definition
                result_set.insert(Definition {
                    var: *dest,
                    loc: stmt_loc,
                });
            }
            InstructionKind::UnaryOp { dest, .. } => {
                // Kill all previous definitions of this variable
                result_set.retain(|def| def.var != *dest);
                // Gen: Add new definition
                result_set.insert(Definition {
                    var: *dest,
                    loc: stmt_loc,
                });
            }
            InstructionKind::Call { dest, .. } => {
                // Kill all previous definitions of this variable (if any)
                if let Some(dest_var) = dest {
                    result_set.retain(|def| def.var != *dest_var);
                    // Gen: Add new definition
                    result_set.insert(Definition {
                        var: *dest_var,
                        loc: stmt_loc,
                    });
                }
            }
            InstructionKind::TableGet { dest, .. } => {
                // Kill all previous definitions of this variable
                result_set.retain(|def| def.var != *dest);
                // Gen: Add new definition
                result_set.insert(Definition {
                    var: *dest,
                    loc: stmt_loc,
                });
            }
            InstructionKind::TableSet { .. } | InstructionKind::Assert { .. } => {
                // These instructions don't define any variables
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        _block_id: BasicBlockId,
        state: &SetLattice<Definition>,
    ) -> SetLattice<Definition> {
        // Terminators don't define variables
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<Definition> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &Function, _block: &BasicBlock) -> SetLattice<Definition> {
        // At function entry, parameters are defined, but we don't model their definition points here
        SetLattice::bottom().unwrap()
    }
}

/// Analyze reaching definitions in a function (forward, function-level)
/// Which assignments could possibly supply the current value of this variable?
pub fn analyze_reaching_definitions(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<SetLattice<Definition>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::May,
        ReachingDefTransfer,
    );
    analysis.analyze(func, program)
}

pub fn analyze_reaching_definitions_hop(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<SetLattice<Definition>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Hop,
        Direction::Forward,
        AnalysisKind::May,
        ReachingDefTransfer,
    );
    analysis.analyze(func, program)
}
