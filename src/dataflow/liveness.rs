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

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Handle the lvalue (what's being assigned to)
                match lvalue {
                    LValue::Variable { var } => {
                        // Kill: Remove the assigned variable (it's defined here)
                        live_vars.remove(var);
                    }
                    LValue::ArrayElement { array, index } => {
                        // Array element assignment: uses the array variable and index
                        live_vars.insert(*array);
                        self.add_used_var_from_operand(index, &mut live_vars);
                    }
                    LValue::TableField { pk_values, .. } => {
                        // Table field assignment: uses primary key values
                        for pk_value in pk_values {
                            self.add_used_var_from_operand(pk_value, &mut live_vars);
                        }
                    }
                }

                // Gen: Add all variables used in the rvalue
                self.add_used_vars_from_rvalue(rvalue, &mut live_vars);
            }
        }

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

/// Run live variables analysis on a function using visitor API
pub fn analyze_live_variables(
    prog: &crate::cfg::CfgProgram,
    func_id: crate::cfg::FunctionId,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<VarId>> {
    use crate::cfg::CfgVisitor;

    // Create a visitor to traverse the function and perform analysis
    struct LiveVariablesVisitor {
        results: DataflowResults<SetLattice<VarId>>,
    }

    impl CfgVisitor<()> for LiveVariablesVisitor {
        fn visit_program(&mut self, _program: &crate::cfg::CfgProgram) -> () {}

        fn visit_function(
            &mut self,
            program: &crate::cfg::CfgProgram,
            id: crate::cfg::FunctionId,
        ) -> () {
            if let Some(function) = program.functions.get(id) {
                // Visit all hops in the function
                for &hop_id in &function.hops {
                    self.visit_hop(program, hop_id);
                }
            }
        }

        fn visit_hop(&mut self, program: &crate::cfg::CfgProgram, id: crate::cfg::HopId) -> () {
            if let Some(hop) = program.hops.get(id) {
                // Visit all basic blocks in the hop
                for &block_id in &hop.blocks {
                    self.visit_basic_block(program, block_id);
                }
            }
        }

        fn visit_basic_block(
            &mut self,
            program: &crate::cfg::CfgProgram,
            id: crate::cfg::BasicBlockId,
        ) -> () {
            if let Some(_block) = program.blocks.get(id) {
                // Initialize with empty live variable set for this block
                self.results.entry.insert(id, SetLattice::bottom().unwrap());
                self.results.exit.insert(id, SetLattice::bottom().unwrap());
            }
        }

        fn visit_global_constants(
            &mut self,
            _program: &crate::cfg::CfgProgram,
            _id: crate::cfg::VarId,
        ) -> () {
        }
    }

    let mut visitor = LiveVariablesVisitor {
        results: DataflowResults {
            entry: std::collections::HashMap::new(),
            exit: std::collections::HashMap::new(),
        },
    };

    // Visit the specific function
    visitor.visit_function(prog, func_id);

    visitor.results
}
