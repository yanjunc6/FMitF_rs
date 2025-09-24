use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::BasicBlock;
use crate::cfg::{
    BasicBlockId, FieldId, Function, Instruction, InstructionKind, TableId, Terminator,
};
use crate::dataflow::StmtLoc;

/// Table access tracking
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableAccess {
    pub table: TableId,
    pub field: FieldId, // only non-primary field is tracked
    pub access_type: AccessType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessType {
    Read,
    Write,
}

/// Transfer function for table mod/ref analysis (hop-level)
pub struct TableModRefTransfer;

impl TransferFunction<SetLattice<TableAccess>> for TableModRefTransfer {
    /// For each instruction, accumulate table accesses
    fn transfer_instruction(
        &self,
        inst: &Instruction,
        _stmt_loc: StmtLoc,
        state: &SetLattice<TableAccess>,
    ) -> SetLattice<TableAccess> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match &inst.kind {
            InstructionKind::TableGet { table, field, .. } => {
                // Read from the table
                if let Some(field_id) = field {
                    result_set.insert(TableAccess {
                        table: *table,
                        field: *field_id,
                        access_type: AccessType::Read,
                    });
                }
            }
            InstructionKind::TableSet { table, field, .. } => {
                // Write to the table
                if let Some(field_id) = field {
                    result_set.insert(TableAccess {
                        table: *table,
                        field: *field_id,
                        access_type: AccessType::Write,
                    });
                }
            }
            _ => {
                // Other instructions don't access tables directly
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        _block_id: BasicBlockId,
        state: &SetLattice<TableAccess>,
    ) -> SetLattice<TableAccess> {
        // Terminators don't access tables
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<TableAccess> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &Function, _block: &BasicBlock) -> SetLattice<TableAccess> {
        SetLattice::bottom().unwrap()
    }
}

/// Analyze table modifications and references in a function (forward, hop-level)
pub fn analyze_table_mod_ref(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<SetLattice<TableAccess>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Hop, // Important: hop-level analysis
        Direction::Forward,
        AnalysisKind::May,
        TableModRefTransfer,
    );
    analysis.analyze(func, program)
}
