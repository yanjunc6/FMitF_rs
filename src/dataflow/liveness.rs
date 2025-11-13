use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, Function, Instruction, InstructionKind, Operand, Terminator,
    VariableId,
};

/// Variable identifier for liveness analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiveVar(pub VariableId);

/// Transfer function for live variables analysis
pub struct LivenessTransfer;

impl TransferFunction<SetLattice<LiveVar>> for LivenessTransfer {
    /// For each instruction, remove defined variables and add used variables
    fn transfer_instruction(
        &self,
        inst: &Instruction,
        _stmt_loc: StmtLoc,
        state: &SetLattice<LiveVar>,
    ) -> SetLattice<LiveVar> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match &inst.kind {
            InstructionKind::Assign { dest, src } => {
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, src);
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
            }
            InstructionKind::BinaryOp {
                dest, left, right, ..
            } => {
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, left);
                self.add_operand_vars(&mut result_set, right);
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
            }
            InstructionKind::UnaryOp { dest, operand, .. } => {
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, operand);
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
            }
            InstructionKind::Call { dest, args, .. } => {
                // Add used variables (gen)
                for arg in args {
                    self.add_operand_vars(&mut result_set, arg);
                }
                // Remove defined variable (kill)
                if let Some(dest_var) = dest {
                    result_set.remove(&LiveVar(*dest_var));
                }
            }
            InstructionKind::TableGet { dest, keys, .. } => {
                // Add used variables (gen)
                for key in keys {
                    self.add_operand_vars(&mut result_set, key);
                }
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
            }
            InstructionKind::TableSet { keys, value, .. } => {
                // Add used variables (gen)
                for key in keys {
                    self.add_operand_vars(&mut result_set, key);
                }
                self.add_operand_vars(&mut result_set, value);
                // No variable is defined by TableSet
            }
            InstructionKind::Assert { condition, .. } => {
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, condition);
                // No variable is defined by Assert
            }
        }

        SetLattice::new(result_set)
    }

    fn transfer_terminator(
        &self,
        term: &Terminator,
        _block_id: BasicBlockId,
        state: &SetLattice<LiveVar>,
    ) -> SetLattice<LiveVar> {
        if state.is_top() {
            return SetLattice::top_element();
        }

        let mut result_set = state.as_set().unwrap().clone();

        match term {
            Terminator::Branch { condition, .. } => {
                self.add_operand_vars(&mut result_set, condition);
            }
            Terminator::Return(Some(return_val)) => {
                self.add_operand_vars(&mut result_set, return_val);
            }
            _ => {
                // Jump, Return(None), HopExit, Abort don't use any variables
            }
        }

        SetLattice::new(result_set)
    }

    fn initial_value(&self) -> SetLattice<LiveVar> {
        SetLattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &Function, block: &BasicBlock) -> SetLattice<LiveVar> {
        let mut live_vars = std::collections::HashSet::new();
        // For backward analysis, boundary is at return/abort blocks
        if let Terminator::Return(Some(return_val)) = &block.terminator {
            self.add_operand_vars(&mut live_vars, return_val);
        }
        SetLattice::new(live_vars)
    }
}

impl LivenessTransfer {
    fn add_operand_vars(&self, vars: &mut std::collections::HashSet<LiveVar>, operand: &Operand) {
        match operand {
            Operand::Variable(var_id) => {
                vars.insert(LiveVar(*var_id));
            }
            Operand::Constant(_) | Operand::Global(_) | Operand::Table(_) => {
                // Constants and globals don't contribute to liveness
            }
        }
    }
}

/// Analyze live variables in a function (backward, function-level)
pub fn analyze_live_variables(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<SetLattice<LiveVar>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Backward,
        AnalysisKind::May,
        LivenessTransfer,
    );
    analysis.analyze(func, program)
}

//TODO: we need to mod-ref analysis to detect whether variable is modified in a hop