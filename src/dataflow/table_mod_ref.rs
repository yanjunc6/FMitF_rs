use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::BasicBlock;
use crate::cfg::{ControlFlowEdge, FunctionCfg, LValue, RValue, Statement, TableId, FieldId};
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
    /// For each statement, accumulate table accesses
    fn transfer_statement(
        &self,
        stmt: &Statement,
        _stmt_loc: StmtLoc,
        state: &SetLattice<TableAccess>,
    ) -> SetLattice<TableAccess> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Track reads from rvalue
                match rvalue {
                    RValue::TableAccess { table, field, pk_values: _, pk_fields: _, .. } => {
                        // Read from the target field
                        result_set.insert(TableAccess {
                            table: *table,
                            field: *field,
                            access_type: AccessType::Read,
                        });
                    }
                    _ => {}
                }

                // Track writes from lvalue
                match lvalue {
                    LValue::TableField { table, field, pk_values: _, pk_fields: _, .. } => {
                        // Write to the target field
                        result_set.insert(TableAccess {
                            table: *table,
                            field: *field,
                            access_type: AccessType::Write,
                        });
                    }
                    _ => {}
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<TableAccess>,
    ) -> SetLattice<TableAccess> {
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<TableAccess> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _block: &BasicBlock) -> SetLattice<TableAccess> {
        SetLattice::bottom().unwrap()
    }
}

/// Analyze table modifications and references in a function (forward, hop-level)
pub fn analyze_table_mod_ref(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<SetLattice<TableAccess>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Hop, // Important: hop-level analysis
        Direction::Forward,
        AnalysisKind::May,
        TableModRefTransfer,
    );
    analysis.analyze(func, cfg_program)
}
