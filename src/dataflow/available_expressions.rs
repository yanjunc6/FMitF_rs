use crate::cfg::{
    ControlFlowEdge, EdgeType, FieldId, FunctionCfg, LValue, Operand, Rvalue, Statement, TableId,
    VarId,
};
use crate::dataflow::{
    AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};

/// Transfer function for Available Expressions analysis.
/// Available expressions analysis is a forward analysis that tracks which expressions
/// are available (have been computed and their operands haven't been redefined) at each point.
#[derive(Debug, Clone)]
pub struct AvailableExpressionsTransfer;

impl AvailableExpressionsTransfer {
    /// Checks if an Rvalue represents a computation that should be tracked
    fn is_tracked_expression(rvalue: &Rvalue) -> bool {
        match rvalue {
            Rvalue::Use(_) => false, // Simple variable use is not a "computation"
            Rvalue::TableAccess { .. }
            | Rvalue::ArrayAccess { .. }
            | Rvalue::UnaryOp { .. }
            | Rvalue::BinaryOp { .. } => true,
        }
    }

    /// Checks if the given Rvalue uses the specified variable
    fn expr_uses_var(rvalue: &Rvalue, var_id: VarId) -> bool {
        match rvalue {
            Rvalue::Use(op) => Self::operand_uses_var(op, var_id),
            Rvalue::TableAccess { pk_values, .. } => pk_values
                .iter()
                .any(|pk_value| Self::operand_uses_var(pk_value, var_id)),
            Rvalue::ArrayAccess { array, index } => {
                Self::operand_uses_var(array, var_id) || Self::operand_uses_var(index, var_id)
            }
            Rvalue::UnaryOp { operand, .. } => Self::operand_uses_var(operand, var_id),
            Rvalue::BinaryOp { left, right, .. } => {
                Self::operand_uses_var(left, var_id) || Self::operand_uses_var(right, var_id)
            }
        }
    }

    /// Checks if the given Operand uses the specified variable
    fn operand_uses_var(operand: &Operand, var_id: VarId) -> bool {
        matches!(operand, Operand::Var(v) if *v == var_id)
    }

    /// Checks if an Rvalue is killed by a table assignment
    fn expr_killed_by_table_assign(
        rvalue: &Rvalue,
        assigned_table_id: TableId,
        assigned_field_id: FieldId,
    ) -> bool {
        match rvalue {
            Rvalue::TableAccess { table, field, .. } => {
                *table == assigned_table_id && *field == assigned_field_id
            }
            _ => false,
        }
    }
}

impl TransferFunction<SetLattice<Rvalue>> for AvailableExpressionsTransfer {
    fn transfer_statement(
        &self,
        stmt: &Statement,
        state: &SetLattice<Rvalue>,
    ) -> SetLattice<Rvalue> {
        if state.is_top {
            return state.clone();
        }

        let mut available_exprs = state.set.clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                match lvalue {
                    LValue::Variable { var } => {
                        // KILL: Remove any expression that uses the defined variable
                        available_exprs.retain(|expr| !Self::expr_uses_var(expr, *var));
                    }
                    LValue::ArrayElement { array, .. } => {
                        // KILL: Remove any expression that uses the array variable
                        available_exprs.retain(|expr| !Self::expr_uses_var(expr, *array));
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't kill local variable expressions
                    }
                }

                // GEN: Add the computed expression if it's trackable
                if Self::is_tracked_expression(rvalue) {
                    available_exprs.insert(rvalue.clone());
                }
            }
        }

        SetLattice::new(available_exprs)
    }

    fn transfer_edge(
        &self,
        edge: &ControlFlowEdge,
        state: &SetLattice<Rvalue>,
    ) -> SetLattice<Rvalue> {
        match &edge.edge_type {
            EdgeType::ConditionalTrue { .. } | EdgeType::ConditionalFalse { .. } => {
                // Conditions don't generate new expressions but might use variables
                // For available expressions, we typically don't kill expressions based on conditions
                state.clone()
            }
            _ => {
                // Other edge types don't affect available expressions
                state.clone()
            }
        }
    }

    fn initial_value(&self) -> SetLattice<Rvalue> {
        // Start with empty set for forward analysis
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self) -> SetLattice<Rvalue> {
        // At function entry, no expressions are available
        SetLattice::bottom().unwrap()
    }
}

/// Run available expressions analysis on a function
/// Available expressions analysis tracks which expressions are available
/// (computed and operands not redefined) at each program point.
pub fn analyze_available_expressions(
    func: &FunctionCfg,
    level: AnalysisLevel,
) -> DataflowResults<SetLattice<Rvalue>> {
    let analysis = DataflowAnalysis::new(level, Direction::Forward, AvailableExpressionsTransfer);
    analysis.analyze(func)
}
