//! Constant propagation analysis using unified SetLattice approach
//! This is a forward analysis that tracks constant values of variables.

use crate::cfg::{CfgProgram, ControlFlowEdge, FunctionCfg, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{
    AnalysisLevel, ConstantData, ConstantValue, DataflowAnalysis, DataflowResults, Direction,
    Lattice, SetLattice, TransferFunction,
};

/// Transfer function for constant analysis using SetLattice<ConstantData>
pub struct ConstantAnalysisSetLatticeTransfer;

impl TransferFunction<SetLattice<ConstantData>> for ConstantAnalysisSetLatticeTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<ConstantData>,
    ) -> SetLattice<ConstantData> {
        if state.is_top {
            return state.clone();
        }

        let mut constant_data: std::collections::HashSet<ConstantData> = state.set.clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                if let LValue::Variable { var } = lvalue {
                    // Remove any existing constant information for this variable
                    constant_data.retain(|data| data.var_id != *var);

                    // Determine the new constant value for this variable
                    let new_value = self.evaluate_rvalue(rvalue, &constant_data);
                    constant_data.insert(ConstantData {
                        var_id: *var,
                        value: new_value,
                    });
                }
            }
        }

        SetLattice::new(constant_data)
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &SetLattice<ConstantData>,
    ) -> SetLattice<ConstantData> {
        // Control flow edges don't affect constant values
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<ConstantData> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<ConstantData> {
        // At function entry, no constants are known
        SetLattice::bottom().unwrap()
    }
}

impl ConstantAnalysisSetLatticeTransfer {
    fn evaluate_rvalue(
        &self,
        rvalue: &Rvalue,
        constant_data: &std::collections::HashSet<ConstantData>,
    ) -> ConstantValue {
        match rvalue {
            Rvalue::Use(operand) => self.evaluate_operand(operand, constant_data),
            Rvalue::BinaryOp { left, right, op } => {
                let left_val = self.evaluate_operand(left, constant_data);
                let right_val = self.evaluate_operand(right, constant_data);
                
                match (left_val, right_val) {
                    (ConstantValue::Constant(l), ConstantValue::Constant(r)) => {
                        // Try to evaluate constant binary operation
                        self.evaluate_binary_op(&l, &r, op).unwrap_or(ConstantValue::Top)
                    }
                    (ConstantValue::Bottom, _) | (_, ConstantValue::Bottom) => ConstantValue::Bottom,
                    _ => ConstantValue::Top,
                }
            }
            Rvalue::UnaryOp { operand, op } => {
                let operand_val = self.evaluate_operand(operand, constant_data);
                match operand_val {
                    ConstantValue::Constant(c) => {
                        self.evaluate_unary_op(&c, op).unwrap_or(ConstantValue::Top)
                    }
                    ConstantValue::Bottom => ConstantValue::Bottom,
                    ConstantValue::Top => ConstantValue::Top,
                }
            }
            _ => ConstantValue::Top, // Non-constant expression
        }
    }

    fn evaluate_operand(
        &self,
        operand: &Operand,
        constant_data: &std::collections::HashSet<ConstantData>,
    ) -> ConstantValue {
        match operand {
            Operand::Const(literal) => ConstantValue::Constant(literal.clone()),
            Operand::Var(var_id) => {
                // Look up the constant value for this variable
                constant_data
                    .iter()
                    .find(|data| data.var_id == *var_id)
                    .map(|data| data.value.clone())
                    .unwrap_or(ConstantValue::Bottom)
            }
        }
    }

    fn evaluate_binary_op(
        &self,
        _left: &crate::cfg::Literal,
        _right: &crate::cfg::Literal,
        _op: &crate::cfg::BinaryOp,
    ) -> Option<ConstantValue> {
        // Simplified - just return Top for now
        // In a full implementation, you would evaluate arithmetic operations
        Some(ConstantValue::Top)
    }

    fn evaluate_unary_op(
        &self,
        _operand: &crate::cfg::Literal,
        _op: &crate::cfg::UnaryOp,
    ) -> Option<ConstantValue> {
        // Simplified - just return Top for now
        Some(ConstantValue::Top)
    }
}

/// Analyze constant propagation for a function using SetLattice approach
pub fn analyze_constants_setlattice(
    func: &FunctionCfg,
    cfg_program: &CfgProgram,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<ConstantData>> {
    let analysis = DataflowAnalysis::new(level, Direction::Forward, ConstantAnalysisSetLatticeTransfer);
    analysis.analyze(func, cfg_program)
}
