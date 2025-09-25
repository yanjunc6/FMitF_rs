use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlockId, BinaryOp, Function, Instruction, InstructionKind, Operand, Terminator, UnaryOp,
    VariableId,
};

/// Available expression for tracking computations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AvailExpr {
    pub dest: VariableId, // variable being assigned
    pub op: ExprKind,     // the computation
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    BinaryOp {
        op: BinaryOp,
        left: Operand,
        right: Operand,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Operand,
    },
    Use(Operand),
    Call {
        func: crate::cfg::FunctionId,
        args: Vec<Operand>,
    },
}

/// Transfer function for available expressions analysis
pub struct AvailExprTransfer;

impl TransferFunction<SetLattice<AvailExpr>> for AvailExprTransfer {
    /// For each instruction, add generated expressions and kill expressions using redefined variables
    fn transfer_instruction(
        &self,
        inst: &Instruction,
        _stmt_loc: StmtLoc,
        state: &SetLattice<AvailExpr>,
    ) -> SetLattice<AvailExpr> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match &inst.kind {
            InstructionKind::Assign { dest, src } => {
                // Kill expressions that use the assigned variable
                result_set.retain(|expr| !self.expr_uses_var(expr, *dest));

                // Gen: Add new expression
                result_set.insert(AvailExpr {
                    dest: *dest,
                    op: ExprKind::Use(src.clone()),
                });
            }
            InstructionKind::BinaryOp {
                dest,
                op,
                left,
                right,
            } => {
                // Kill expressions that use the assigned variable
                result_set.retain(|expr| !self.expr_uses_var(expr, *dest));

                // Gen: Add new expression
                result_set.insert(AvailExpr {
                    dest: *dest,
                    op: ExprKind::BinaryOp {
                        op: *op,
                        left: left.clone(),
                        right: right.clone(),
                    },
                });
            }
            InstructionKind::UnaryOp { dest, op, operand } => {
                // Kill expressions that use the assigned variable
                result_set.retain(|expr| !self.expr_uses_var(expr, *dest));

                // Gen: Add new expression
                result_set.insert(AvailExpr {
                    dest: *dest,
                    op: ExprKind::UnaryOp {
                        op: *op,
                        operand: operand.clone(),
                    },
                });
            }
            InstructionKind::Call { dest, func, args } => {
                if let Some(dest_var) = dest {
                    // Kill expressions that use the assigned variable
                    result_set.retain(|expr| !self.expr_uses_var(expr, *dest_var));

                    // Gen: Add new expression
                    result_set.insert(AvailExpr {
                        dest: *dest_var,
                        op: ExprKind::Call {
                            func: *func,
                            args: args.clone(),
                        },
                    });
                }
            }
            InstructionKind::TableGet { dest, .. } => {
                // Kill expressions that use the assigned variable
                result_set.retain(|expr| !self.expr_uses_var(expr, *dest));
                // Don't generate expression for table operations (they're not pure)
            }
            InstructionKind::TableSet { .. } | InstructionKind::Assert { .. } => {
                // These instructions don't define variables or generate expressions
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_terminator(
        &self,
        _term: &Terminator,
        _block_id: BasicBlockId,
        state: &SetLattice<AvailExpr>,
    ) -> SetLattice<AvailExpr> {
        // Terminators don't generate expressions
        state.clone()
    }

    fn initial_value(&self) -> SetLattice<AvailExpr> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(
        &self,
        _func: &Function,
        _block: &crate::cfg::BasicBlock,
    ) -> SetLattice<AvailExpr> {
        // At function entry, no expressions are available
        SetLattice::bottom().unwrap()
    }
}

impl AvailExprTransfer {
    fn expr_uses_var(&self, expr: &AvailExpr, var: VariableId) -> bool {
        // Check if the variable is used in the expression
        match &expr.op {
            ExprKind::Use(operand) => self.operand_uses_var(operand, var),
            ExprKind::BinaryOp { left, right, .. } => {
                self.operand_uses_var(left, var) || self.operand_uses_var(right, var)
            }
            ExprKind::UnaryOp { operand, .. } => self.operand_uses_var(operand, var),
            ExprKind::Call { args, .. } => args.iter().any(|arg| self.operand_uses_var(arg, var)),
        }
    }

    fn operand_uses_var(&self, operand: &Operand, var: VariableId) -> bool {
        match operand {
            Operand::Variable(v) => *v == var,
            Operand::Constant(_) | Operand::Global(_) | Operand::Table(_) => false,
        }
    }
}

/// Analyze available expressions in a function (forward, function-level)
/// available expression can be safely re-used (or common-sub-expression–eliminated) without re-computing it.
pub fn analyze_available_expressions(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<SetLattice<AvailExpr>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::Must,
        AvailExprTransfer,
    );
    analysis.analyze(func, program)
}
