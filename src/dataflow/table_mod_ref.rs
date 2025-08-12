use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{
    BasicBlockId, ControlFlowEdge, FunctionCfg, Rvalue, Statement, TableId,
};

/// Table access tracking
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableAccess {
    pub table: TableId,
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
    fn transfer_statement(&self, stmt: &Statement, state: &SetLattice<TableAccess>) -> SetLattice<TableAccess> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Track reads from rvalue
                self.add_reads_from_rvalue(&mut result_set, rvalue);

                // Track writes from lvalue
                match lvalue {
                    crate::cfg::LValue::Variable { .. } => {
                        // Variable assignments don't affect tables
                    }
                    crate::cfg::LValue::ArrayElement { .. } => {
                        // Array assignments don't affect tables directly
                    }
                    crate::cfg::LValue::TableField { table, .. } => {
                        result_set.insert(TableAccess {
                            table: *table,
                            access_type: AccessType::Write,
                        });
                    }
                }
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(&self, _edge: &ControlFlowEdge, state: &SetLattice<TableAccess>) -> SetLattice<TableAccess> {
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<TableAccess> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _blockid: BasicBlockId) -> SetLattice<TableAccess> {
        // At hop entry, no table accesses yet
        SetLattice::bottom().unwrap()
    }
}

impl TableModRefTransfer {
    fn add_reads_from_rvalue(&self, accesses: &mut std::collections::HashSet<TableAccess>, rvalue: &Rvalue) {
        match rvalue {
            Rvalue::Use(_) => {
                // Variable use doesn't read from tables
            }
            Rvalue::TableAccess { table, .. } => {
                accesses.insert(TableAccess {
                    table: *table,
                    access_type: AccessType::Read,
                });
            }
            Rvalue::ArrayAccess { array, index } => {
                // Check if array or index operands contain table accesses
                self.add_reads_from_operand(accesses, array);
                self.add_reads_from_operand(accesses, index);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.add_reads_from_operand(accesses, operand);
            }
            Rvalue::BinaryOp { left, right, .. } => {
                self.add_reads_from_operand(accesses, left);
                self.add_reads_from_operand(accesses, right);
            }
        }
    }

    fn add_reads_from_operand(&self, _accesses: &mut std::collections::HashSet<TableAccess>, operand: &crate::cfg::Operand) {
        match operand {
            crate::cfg::Operand::Var(_) => {
                // Variable use doesn't read from tables directly
            }
            crate::cfg::Operand::Const(_) => {
                // Constants don't read from tables
            }
        }
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
