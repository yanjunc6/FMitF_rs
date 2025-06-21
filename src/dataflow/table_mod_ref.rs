//! Table modification-reference analysis for tracking table reads and writes per hop.
//! This analysis determines which tables are read from and written to by each hop.

use crate::cfg::{Rvalue, Statement, TableId, Terminator, FunctionCfg};
use crate::dataflow::{DataflowAnalysis, DataflowResults, Direction, Lattice, TransferFunction, SetLattice};

/// Represents table access information (reads and writes) using SetLattice
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
pub struct TableModRefTransfer;

impl TransferFunction<SetLattice<TableAccess>> for TableModRefTransfer {
    fn transfer_statement(&self, stmt: &Statement, state: &SetLattice<TableAccess>) -> SetLattice<TableAccess> {
        let mut result = state.clone();

        match stmt {
            Statement::Assign { rvalue, .. } => {
                if let Rvalue::TableAccess { table, .. } = rvalue {
                    result.set.insert(TableAccess {
                        table_id: *table,
                        access_type: AccessType::Read,
                    });
                }
            }
            Statement::TableAssign { table, .. } => {
                result.set.insert(TableAccess {
                    table_id: *table,
                    access_type: AccessType::Write,
                });
            }
        }

        result
    }

    fn transfer_terminator(&self, _term: &Terminator, state: &SetLattice<TableAccess>) -> SetLattice<TableAccess> {
        state.clone() // No table accesses in terminators for now
    }

    fn initial_value(&self) -> SetLattice<TableAccess> {
        SetLattice::bottom()
    }

    fn boundary_value(&self) -> SetLattice<TableAccess> {
        SetLattice::bottom()
    }
}

/// Analyze table modification and reference patterns for a function
pub fn analyze_table_mod_ref(func: &FunctionCfg) -> DataflowResults<SetLattice<TableAccess>> {
    let analysis = DataflowAnalysis::new(Direction::Forward, TableModRefTransfer);
    analysis.analyze(func)
}

