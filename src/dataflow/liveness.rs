use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{
    BasicBlock, ControlFlowEdge, FunctionCfg, Operand, Rvalue, Statement, VarId, EdgeType
};

/// Variable identifier for liveness analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiveVar(pub VarId);

/// Transfer function for live variables analysis
pub struct LivenessTransfer;

impl TransferFunction<SetLattice<LiveVar>> for LivenessTransfer {
    /// For each statement, remove defined variables and add used variables
    fn transfer_statement(&self, stmt: &Statement, state: &SetLattice<LiveVar>) -> SetLattice<LiveVar> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Add used variables (gen)
                self.add_rvalue_vars(&mut result_set, rvalue);

                // Remove defined variables (kill)
                match lvalue {
                    crate::cfg::LValue::Variable { var } => {
                        result_set.remove(&LiveVar(*var));
                    }
                    crate::cfg::LValue::ArrayElement { array, index } => {
                        result_set.insert(LiveVar(*array));
                        self.add_operand_vars(&mut result_set, index);
                    }
                    crate::cfg::LValue::TableField { pk_values, .. } => {
                        for pk_val in pk_values {
                            self.add_operand_vars(&mut result_set, pk_val);
                        }
                    }
                }                
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_edge(&self, edge: &ControlFlowEdge, state: &SetLattice<LiveVar>) -> SetLattice<LiveVar> {
        match &edge.edge_type {
            EdgeType::ConditionalTrue { condition } => {
                let mut result_set = state.as_set().unwrap().clone();
                self.add_operand_vars(&mut result_set, &condition);
                SetLattice::new(result_set)
            }
            EdgeType::ConditionalFalse { condition } => {
                let mut result_set = state.as_set().unwrap().clone();
                self.add_operand_vars(&mut result_set, &condition);
                SetLattice::new(result_set)
            }
            EdgeType::Return { value: Some(return_val) } => {
                let mut result_set = state.as_set().unwrap().clone();
                self.add_operand_vars(&mut result_set, &return_val);
                SetLattice::new(result_set)
            }
            _ => state.clone(),
        }
    }

    fn initial_value(&self) -> SetLattice<LiveVar> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, block: &BasicBlock) -> SetLattice<LiveVar> {
        let mut live_vars = std::collections::HashSet::new();
        for edge in &block.successors {
            if let EdgeType::Return { value: Some(return_val) } = &edge.edge_type {
                self.add_operand_vars(&mut live_vars, return_val);
            }
        }
        SetLattice::new(live_vars)
    }
}

impl LivenessTransfer {
    fn add_rvalue_vars(&self, vars: &mut std::collections::HashSet<LiveVar>, rvalue: &Rvalue) {
        match rvalue {
            Rvalue::Use(operand) => {
                self.add_operand_vars(vars, operand);
            }
            Rvalue::TableAccess { pk_values, .. } => {
                for pk_val in pk_values {
                    self.add_operand_vars(vars, pk_val);
                }
            }
            Rvalue::ArrayAccess { array, index } => {
                self.add_operand_vars(vars, array);
                self.add_operand_vars(vars, index);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.add_operand_vars(vars, operand);
            }
            Rvalue::BinaryOp { left, right, .. } => {
                self.add_operand_vars(vars, left);
                self.add_operand_vars(vars, right);
            }
        }
    }

    fn add_operand_vars(&self, vars: &mut std::collections::HashSet<LiveVar>, operand: &Operand) {
        if let Operand::Var(var_id) = operand {
            vars.insert(LiveVar(*var_id));
        }
    }
}

/// Analyze live variables in a function (backward, function-level)
pub fn analyze_live_variables(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<SetLattice<LiveVar>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Backward,
        AnalysisKind::May,
        LivenessTransfer,
    );
    analysis.analyze(func, cfg_program)
}
