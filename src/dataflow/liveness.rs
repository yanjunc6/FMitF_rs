use crate::cfg::{ControlFlowEdge, EdgeType, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{AnalysisLevel, DataflowResults, Lattice, SetLattice, TransferFunction};
use std::collections::HashSet;

/// Transfer function for live variables analysis
/// Live variables analysis is a backward analysis that tracks which variables
/// are live (will be used in the future) at each program point
pub struct LiveVariablesTransfer;

impl TransferFunction<SetLattice<VarId>> for LiveVariablesTransfer {
    fn transfer_statement(&self, stmt: &Statement, state: &SetLattice<VarId>) -> SetLattice<VarId> {
        if state.is_top {
            return state.clone();
        }

        let mut live_vars = state.set.clone();
        let initial_live_count = live_vars.len();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Handle the lvalue (what's being assigned to)
                match lvalue {
                    LValue::Variable { var } => {
                        // Kill: Remove the assigned variable (it's defined here)
                        let was_live = live_vars.remove(var);
                        eprintln!(
                            "            LIVENESS TRANSFER: Variable {:?} was_live={}, killed",
                            var, was_live
                        );
                    }
                    LValue::ArrayElement { array, index } => {
                        // Array element assignment: uses the array variable and index
                        live_vars.insert(*array);
                        self.add_used_var_from_operand(index, &mut live_vars);
                        eprintln!(
                            "            LIVENESS TRANSFER: Array assignment uses array {:?}",
                            array
                        );
                    }
                    LValue::TableField { pk_values, .. } => {
                        // Table field assignment: uses primary key values
                        for pk_value in pk_values {
                            let old_len = live_vars.len();
                            self.add_used_var_from_operand(pk_value, &mut live_vars);
                            if live_vars.len() > old_len {
                                eprintln!(
                                    "            LIVENESS TRANSFER: Table assignment uses pk {:?}",
                                    pk_value
                                );
                            }
                        }
                    }
                }

                // Gen: Add all variables used in the rvalue
                let rvalue_before = live_vars.len();
                self.add_used_vars_from_rvalue(rvalue, &mut live_vars);
                let rvalue_added = live_vars.len() - rvalue_before;
                if rvalue_added > 0 {
                    eprintln!(
                        "            LIVENESS TRANSFER: Rvalue {:?} added {} vars",
                        rvalue, rvalue_added
                    );
                }
            }
        }

        let final_live_count = live_vars.len();
        eprintln!(
            "            LIVENESS TRANSFER: {} -> {} live vars",
            initial_live_count, final_live_count
        );

        SetLattice::new(live_vars)
    }

    fn transfer_edge(
        &self,
        edge: &ControlFlowEdge,
        state: &SetLattice<VarId>,
    ) -> SetLattice<VarId> {
        if state.is_top {
            return state.clone();
        }

        let mut live_vars = state.set.clone();

        match &edge.edge_type {
            EdgeType::ConditionalTrue { condition } | EdgeType::ConditionalFalse { condition } => {
                // Gen: Add variables used in the condition
                self.add_used_var_from_operand(condition, &mut live_vars);
            }
            EdgeType::Return {
                value: Some(return_val),
            } => {
                // Gen: Add variable used in return value
                self.add_used_var_from_operand(return_val, &mut live_vars);
            }
            _ => {
                // No variables used in other edge types
            }
        }

        SetLattice::new(live_vars)
    }

    fn initial_value(&self) -> SetLattice<VarId> {
        // Start with empty set for backward analysis
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<VarId> {
        // At function exit points, no variables are live
        SetLattice::bottom().unwrap()
    }
}

impl LiveVariablesTransfer {
    fn add_used_vars_from_rvalue(&self, rvalue: &Rvalue, live_vars: &mut HashSet<VarId>) {
        match rvalue {
            Rvalue::Use(operand) => {
                self.add_used_var_from_operand(operand, live_vars);
            }
            Rvalue::TableAccess { pk_values, .. } => {
                for pk_value in pk_values {
                    self.add_used_var_from_operand(pk_value, live_vars);
                }
            }
            Rvalue::ArrayAccess { array, index } => {
                self.add_used_var_from_operand(array, live_vars);
                self.add_used_var_from_operand(index, live_vars);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.add_used_var_from_operand(operand, live_vars);
            }
            Rvalue::BinaryOp { left, right, .. } => {
                self.add_used_var_from_operand(left, live_vars);
                self.add_used_var_from_operand(right, live_vars);
            }
        }
    }

    fn add_used_var_from_operand(&self, operand: &Operand, live_vars: &mut HashSet<VarId>) {
        if let Operand::Var(var_id) = operand {
            live_vars.insert(*var_id);
        }
    }
}

/// Run live variables analysis on a function using the dataflow framework
pub fn analyze_live_variables(
    prog: &crate::cfg::CfgProgram,
    func_id: crate::cfg::FunctionId,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<VarId>> {
    use crate::dataflow::{DataflowAnalysis, Direction};

    eprintln!("        LIVENESS ANALYSIS DEBUG:");

    // Get the function from the program
    if let Some(func) = prog.functions.get(func_id) {
        eprintln!(
            "        Function has {} blocks: {:?}",
            func.blocks.len(),
            func.blocks
        );

        // Debug: Check boundary blocks
        for &block_id in &func.blocks {
            if let Some(block) = prog.blocks.get(block_id) {
                let has_return_edge = block.successors.iter().any(|edge| {
                    matches!(
                        edge.edge_type,
                        crate::cfg::EdgeType::Return { .. } | crate::cfg::EdgeType::Abort
                    )
                });
                eprintln!(
                    "        Block {:?}: {} successors, has_return_edge: {}",
                    block_id,
                    block.successors.len(),
                    has_return_edge
                );

                for (i, edge) in block.successors.iter().enumerate() {
                    eprintln!(
                        "          Successor[{}]: {:?} -> {:?}, type: {:?}",
                        i, edge.from, edge.to, edge.edge_type
                    );
                }
            }
        }

        let analysis = DataflowAnalysis::new(level, Direction::Backward, LiveVariablesTransfer);
        let results = analysis.analyze(func, prog);

        // Debug results
        eprintln!("        Liveness results:");
        for &block_id in &func.blocks {
            let entry_vars = results
                .entry
                .get(&block_id)
                .and_then(|lattice| lattice.as_set())
                .map(|set| set.len())
                .unwrap_or(0);
            let exit_vars = results
                .exit
                .get(&block_id)
                .and_then(|lattice| lattice.as_set())
                .map(|set| set.len())
                .unwrap_or(0);
            eprintln!(
                "        Block {:?}: entry={} vars, exit={} vars",
                block_id, entry_vars, exit_vars
            );
        }

        results
    } else {
        // Return empty results if function not found
        DataflowResults {
            entry: std::collections::HashMap::new(),
            exit: std::collections::HashMap::new(),
        }
    }
}
