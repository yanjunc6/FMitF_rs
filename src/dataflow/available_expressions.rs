use crate::cfg::{FieldId, FunctionCfg, Operand, Rvalue, Statement, TableId, Terminator, VarId};
use crate::dataflow::{DataflowAnalysis, DataflowResults, Direction, SetLattice, TransferFunction};
use std::collections::HashSet;
use std::fmt::Debug; // Required for SetLattice<T> if T needs Debug

// Ensure Rvalue, Operand, Constant, BinaryOp, UnaryOp in cfg/mod.rs
// derive PartialEq, Eq, Hash, Clone, Debug.
// Note: If Rvalue contains f64 (e.g., via Constant::Float), deriving Eq and Hash
// for Rvalue will be problematic. You might need to handle f64 specially
// (e.g., by wrapping it or disallowing floats in tracked expressions).

/// Transfer function for Available Expressions analysis.
#[derive(Debug, Clone)]
pub struct AvailableExpressionsTransfer;

impl AvailableExpressionsTransfer {
    /// Checks if an Rvalue represents a computation that can be "available".
    fn is_tracked_expression(rvalue: &Rvalue) -> bool {
        matches!(
            rvalue,
            Rvalue::TableAccess { .. } | Rvalue::UnaryOp { .. } | Rvalue::BinaryOp { .. }
        )
    }

    /// Checks if the given Rvalue uses the specified variable.
    fn expr_uses_var(rvalue: &Rvalue, var_id: VarId) -> bool {
        match rvalue {
            Rvalue::Use(op) => Self::operand_uses_var(op, var_id),
            Rvalue::TableAccess { pk_values, .. } => pk_values
                .iter()
                .any(|pk_value| Self::operand_uses_var(pk_value, var_id)),
            Rvalue::UnaryOp { operand, .. } => Self::operand_uses_var(operand, var_id),
            Rvalue::BinaryOp { left, right, .. } => {
                Self::operand_uses_var(left, var_id) || Self::operand_uses_var(right, var_id)
            }
        }
    }

    /// Checks if the given Operand is or contains the specified variable.
    fn operand_uses_var(operand: &Operand, var_id: VarId) -> bool {
        match operand {
            Operand::Var(v) => *v == var_id,
            Operand::Const(_) => false,
        }
    }

    /// Checks if an Rvalue (specifically a TableAccess) is killed by an assignment
    /// to the given table and field.
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
    fn boundary_value(&self) -> SetLattice<Rvalue> {
        // At the entry of the function, no expressions are available.
        SetLattice {
            set: HashSet::new(),
        }
    }

    fn initial_value(&self) -> SetLattice<Rvalue> {
        // For forward analysis, initial value is typically the same as boundary value
        SetLattice {
            set: HashSet::new(),
        }
    }

    fn transfer_statement(
        &self,
        stmt: &Statement,
        in_state: &SetLattice<Rvalue>,
    ) -> SetLattice<Rvalue> {
        let mut available_after_statement = in_state.set.clone();

        match stmt {
            Statement::Assign {
                var: defined_var,
                rvalue,
                ..
            } => {
                // KILL: Any expression that uses `defined_var` is no longer available.
                available_after_statement.retain(|expr| !Self::expr_uses_var(expr, *defined_var));

                // GEN: The expression `rvalue` itself becomes available if trackable.
                if Self::is_tracked_expression(rvalue) {
                    available_after_statement.insert(rvalue.clone());
                }
            }
            Statement::TableAssign {
                table,
                field,
                pk_values: _, // Operands for composite PK
                value: _,     // Operand for value
                ..
            } => {
                // KILL: Any expression that reads from the specific table and field.
                available_after_statement
                    .retain(|expr| !Self::expr_killed_by_table_assign(expr, *table, *field));
            }
        }
        SetLattice {
            set: available_after_statement,
        }
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        in_state: &SetLattice<Rvalue>,
    ) -> SetLattice<Rvalue> {
        // Terminators typically don't generate or kill expressions in available expressions analysis.
        // So, the state is passed through.
        SetLattice {
            set: in_state.set.clone(),
        }
    }
}

/// Runs the Available Expressions analysis on a given function CFG.
///
/// Available Expressions analysis is a forward dataflow analysis.
/// An expression is "available" at a point if it has been computed on every
/// path to that point, and its operands have not been redefined since their last computation.
/// The lattice elements are sets of Rvalues. The meet operation is set intersection.
pub fn analyze_available_expressions(func: &FunctionCfg) -> DataflowResults<SetLattice<Rvalue>> {
    let transfer_fn = AvailableExpressionsTransfer {};
    let analysis = DataflowAnalysis::new(Direction::Forward, transfer_fn);
    analysis.analyze(func)
}
