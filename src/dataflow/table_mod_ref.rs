//! Table modification-reference analysis for tracking table reads and writes per hop.
//! This analysis determines which tables are read from and written to by each hop.

use crate::cfg::{FunctionCfg, Rvalue, Statement, TableId, ControlFlowEdge};
use crate::dataflow::{
    DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice, TransferFunction,
};

/// Represents table access information (reads and writes)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableAccess {
    pub table_id: TableId,
    pub access_type: AccessType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccessType {
    Read,
    Write,
}

/// Transfer function for table modification-reference analysis
/// This is a forward analysis that accumulates all table accesses in a function
pub struct TableModRefTransfer;

impl TransferFunction<SetLattice<TableAccess>> for TableModRefTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<TableAccess>,
    ) -> SetLattice<TableAccess> {
        if state.is_top {
            return state.clone();
        }

        let mut table_accesses = state.set.clone();

        match stmt {
            Statement::Assign { rvalue, .. } => {
                // Check if the assignment reads from a table
                self.add_table_reads_from_rvalue(rvalue, &mut table_accesses);
            }
            Statement::TableAssign { table, pk_fields: _, pk_values: _, field: _, value: _, span: _ } => {
                // Table assignment is a write
                table_accesses.insert(TableAccess {
                    table_id: *table,
                    access_type: AccessType::Write,
                });

                // Note: Primary key values and assigned value might read from tables
                // but we don't track variable->table relationships here
            }
        }

        SetLattice::new(table_accesses)
    }

    fn transfer_edge(&self, _edge: &ControlFlowEdge, state: &SetLattice<TableAccess>) -> SetLattice<TableAccess> {
        // Control flow edges don't typically access tables directly
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<TableAccess> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<TableAccess> {
        // At function entry, no table accesses have occurred yet
        SetLattice::bottom().unwrap()
    }
}

impl TableModRefTransfer {
    fn add_table_reads_from_rvalue(&self, rvalue: &Rvalue, table_accesses: &mut std::collections::HashSet<TableAccess>) {
        match rvalue {
            Rvalue::Use(_) => {
                // Simple variable use doesn't access tables
            }
            Rvalue::TableAccess { table, .. } => {
                // Direct table read
                table_accesses.insert(TableAccess {
                    table_id: *table,
                    access_type: AccessType::Read,
                });
            }
            Rvalue::ArrayAccess { array: _, index: _ } => {
                // Array accesses don't directly read tables, but the operands might
                // In a more sophisticated analysis, you might track if array/index
                // come from table reads
            }
            Rvalue::UnaryOp { op: _, operand: _ } => {
                // Unary operations don't directly access tables
            }
            Rvalue::BinaryOp { left: _, right: _, .. } => {
                // Binary operations don't directly access tables
            }
        }
    }
}

/// Analyze table modification and reference patterns for a function
/// Returns the cumulative set of table accesses for each program point
pub fn analyze_table_mod_ref(func: &FunctionCfg) -> DataflowResults<SetLattice<TableAccess>> {
    let analysis = DataflowAnalysis::new(Direction::Forward, TableModRefTransfer);
    analysis.analyze(func)
}
