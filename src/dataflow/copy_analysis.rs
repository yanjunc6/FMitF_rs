use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Flat, Lattice,
    MapLattice, StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, Function, Instruction, InstructionKind, Operand, Terminator,
    VariableId,
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

        fn kill_stale_aliases(
            result: &mut MapLattice<VariableId, VariableId>,
            stale_source: VariableId,
        ) {
            let stale_destinations: Vec<_> = result
                .iter()
                .filter_map(|(dest, flat_src)| match flat_src {
                    Flat::Value(src) if *src == stale_source => Some(*dest),
                    _ => None,
                })
                .collect();

            for dest in stale_destinations {
                result.insert(dest, Flat::Bottom);
            }
        }

        match &inst.kind {
            InstructionKind::Assign { dest, src } => {
                // Kill: remove any copy relation for this variable
                result.insert(*dest, Flat::Bottom);
                kill_stale_aliases(&mut result, *dest);

                // Gen: If src is a simple variable use, add copy relation
                if let Operand::Variable(source_var) = src {
                    result.insert(*dest, Flat::Value(*source_var));
                }
            }
            InstructionKind::BinaryOp { dest, .. }
            | InstructionKind::UnaryOp { dest, .. }
            | InstructionKind::TableGet { dest, .. } => {
                // These create new values, not copies
                result.insert(*dest, Flat::Bottom);
                kill_stale_aliases(&mut result, *dest);
            }
            InstructionKind::Call { dest, .. } => {
                // Function calls create new values, not copies
                if let Some(dest_var) = dest {
                    result.insert(*dest_var, Flat::Bottom);
                    kill_stale_aliases(&mut result, *dest_var);
                }
            }
            InstructionKind::TableSet { .. } | InstructionKind::Assert { .. } => {
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
