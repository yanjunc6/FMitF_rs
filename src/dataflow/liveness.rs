use crate::cfg::{FunctionCfg, Operand, Rvalue, Statement, Terminator, VarId};
use crate::dataflow::{
    DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice, TransferFunction,
};

pub struct LiveVariablesTransfer;

impl TransferFunction<SetLattice<VarId>> for LiveVariablesTransfer {
    fn transfer_statement(&self, stmt: &Statement, state: &SetLattice<VarId>) -> SetLattice<VarId> {
        let mut result = state.clone();

        match stmt {
            Statement::Assign { var, rvalue, .. } => {
                // Remove defined variable
                result.set.remove(var);

                // Add used variables
                match rvalue {
                    Rvalue::Use(operand) => {
                        if let Operand::Var(v) = operand {
                            result.set.insert(*v);
                        }
                    }
                    Rvalue::TableAccess { pk_value, .. } => {
                        if let Operand::Var(v) = pk_value {
                            result.set.insert(*v);
                        }
                    }
                    Rvalue::UnaryOp { operand, .. } => {
                        if let Operand::Var(v) = operand {
                            result.set.insert(*v);
                        }
                    }
                    Rvalue::BinaryOp { left, right, .. } => {
                        if let Operand::Var(v) = left {
                            result.set.insert(*v);
                        }
                        if let Operand::Var(v) = right {
                            result.set.insert(*v);
                        }
                    }
                }
            }
            Statement::TableAssign {
                pk_value, value, ..
            } => {
                // Add used variables
                if let Operand::Var(v) = pk_value {
                    result.set.insert(*v);
                }
                if let Operand::Var(v) = value {
                    result.set.insert(*v);
                }
            }
        }

        result
    }

    fn transfer_terminator(
        &self,
        term: &Terminator,
        state: &SetLattice<VarId>,
    ) -> SetLattice<VarId> {
        let mut result = state.clone();

        match term {
            Terminator::Branch { condition, .. } => {
                if let Operand::Var(v) = condition {
                    result.set.insert(*v);
                }
            }
            Terminator::Return(Some(operand)) => {
                if let Operand::Var(v) = operand {
                    result.set.insert(*v);
                }
            }
            _ => {}
        }

        result
    }

    fn initial_value(&self) -> SetLattice<VarId> {
        SetLattice::bottom()
    }

    fn boundary_value(&self) -> SetLattice<VarId> {
        SetLattice::bottom()
    }
}

/// Helper function to run live variables analysis
pub fn analyze_live_variables(func: &FunctionCfg) -> DataflowResults<SetLattice<VarId>> {
    let analysis = DataflowAnalysis::new(Direction::Backward, LiveVariablesTransfer);
    analysis.analyze(func)
}
