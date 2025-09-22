use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Flat, Lattice,
    MapLattice, StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, Function, Instruction, Operand, Terminator, VariableId,
};

/// Transfer function for copy propagation analysis
pub struct CopyTransfer;

impl TransferFunction<MapLattice<VariableId, VariableId>> for CopyTransfer {
    /// For each instruction, update copy relations
    fn transfer_instruction(
        &self,
        inst: &Instruction,
        _stmt_loc: StmtLoc,
        state: &MapLattice<VariableId, VariableId>,
    ) -> MapLattice<VariableId, VariableId> {
        if state.is_top() {
            return Lattice::top().unwrap();
        }

        let mut result = state.clone();

        match inst {
            Instruction::Assign { dest, src } => {
                // Kill: remove any copy relation for this variable
                result.insert(*dest, Flat::Bottom);

                // Gen: If src is a simple variable use, add copy relation
                if let Operand::Variable(source_var) = src {
                    result.insert(*dest, Flat::Value(*source_var));
                }
            }
            Instruction::BinaryOp { dest, .. }
            | Instruction::UnaryOp { dest, .. }
            | Instruction::TableGet { dest, .. } => {
                // These create new values, not copies
                result.insert(*dest, Flat::Bottom);
            }
            Instruction::Call { dest, .. } => {
                // Function calls create new values, not copies
                if let Some(dest_var) = dest {
                    result.insert(*dest_var, Flat::Bottom);
                }
            }
            Instruction::TableSet { .. } | Instruction::Assert { .. } => {
                // These instructions don't affect copy relations
            }
        }

        result
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        _block_id: BasicBlockId,
        state: &MapLattice<VariableId, VariableId>,
    ) -> MapLattice<VariableId, VariableId> {
        // Terminators don't affect copy relations
        state.clone()
    }

    fn initial_value(&self) -> MapLattice<VariableId, VariableId> {
        Lattice::bottom().unwrap()
    }

    fn boundary_value(
        &self,
        _func: &Function,
        _block: &BasicBlock,
    ) -> MapLattice<VariableId, VariableId> {
        Lattice::bottom().unwrap()
    }
}

/// Analyze copy relations in a function (forward, function-level)
/// Maps each variable to the single variable it copies from (if any)
pub fn analyze_copies(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<MapLattice<VariableId, VariableId>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::Must,
        CopyTransfer,
    );
    analysis.analyze(func, program)
}
