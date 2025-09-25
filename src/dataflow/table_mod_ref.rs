use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::BasicBlock;
use crate::cfg::{
    BasicBlockId, FieldId, Function, Instruction, InstructionKind, Operand, Program, TableId,
    Terminator,
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
pub struct TableModRefTransfer<'p> {
    /// Reference to the whole CFG program to query table fields
    program: &'p Program,
}

impl<'p> TableModRefTransfer<'p> {
    #[inline]
    fn insert_read_all_fields(
        &self,
        table: TableId,
        result_set: &mut std::collections::HashSet<TableAccess>,
    ) {
        for &field_id in &self.program.tables[table].other_fields {
            result_set.insert(TableAccess {
                table,
                field: field_id,
                access_type: AccessType::Read,
            });
        }
    }

    #[inline]
    fn insert_write_all_fields(
        &self,
        table: TableId,
        result_set: &mut std::collections::HashSet<TableAccess>,
    ) {
        for &field_id in &self.program.tables[table].other_fields {
            result_set.insert(TableAccess {
                table,
                field: field_id,
                access_type: AccessType::Write,
            });
        }
    }

    #[inline]
    fn note_operand_read(
        &self,
        op: &Operand,
        result_set: &mut std::collections::HashSet<TableAccess>,
    ) {
        if let Operand::Table(table) = op {
            self.insert_read_all_fields(*table, result_set);
        }
    }
}

impl<'p> TransferFunction<SetLattice<TableAccess>> for TableModRefTransfer<'p> {
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
                } else {
                    // Whole-row read => mark all non-PK fields as read
                    self.insert_read_all_fields(*table, &mut result_set);
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
                } else {
                    // Whole-row write => mark all non-PK fields as written
                    self.insert_write_all_fields(*table, &mut result_set);
                }
            }
            InstructionKind::Call { func: _, args, .. } => {
                // Any table operand used as argument => read whole table
                for arg in args {
                    self.note_operand_read(arg, &mut result_set);
                }
            }
            InstructionKind::Assign { src, .. } => {
                // Using a table as a value counts as reading the whole table
                self.note_operand_read(src, &mut result_set);
            }
            InstructionKind::BinaryOp { left, right, .. } => {
                self.note_operand_read(left, &mut result_set);
                self.note_operand_read(right, &mut result_set);
            }
            InstructionKind::UnaryOp { operand, .. } => {
                self.note_operand_read(operand, &mut result_set);
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
        TableModRefTransfer { program },
    );
    analysis.analyze(func, program)
}
