use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, Function, Instruction, InstructionKind, Operand, Program, Terminator,
    VariableId,
};
use std::collections::HashSet;

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
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, src);
            }
            InstructionKind::BinaryOp {
                dest, left, right, ..
            } => {
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, left);
                self.add_operand_vars(&mut result_set, right);
            }
            InstructionKind::UnaryOp { dest, operand, .. } => {
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, operand);
            }
            InstructionKind::Call { dest, args, .. } => {
                // Remove defined variable (kill)
                if let Some(dest_var) = dest {
                    result_set.remove(&LiveVar(*dest_var));
                }
                // Add used variables (gen)
                for arg in args {
                    self.add_operand_vars(&mut result_set, arg);
                }
            }
            InstructionKind::TableGet { dest, keys, .. } => {
                // Remove defined variable (kill)
                result_set.remove(&LiveVar(*dest));
                // Add used variables (gen)
                for key in keys {
                    self.add_operand_vars(&mut result_set, key);
                }
            }
            InstructionKind::TableSet { keys, value, .. } => {
                // No variable is defined by TableSet
                // Add used variables (gen)
                for key in keys {
                    self.add_operand_vars(&mut result_set, key);
                }
                self.add_operand_vars(&mut result_set, value);
            }
            InstructionKind::Assert { condition, .. } => {
                // No variable is defined by Assert
                // Add used variables (gen)
                self.add_operand_vars(&mut result_set, condition);
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

fn collect_to_unit_return_vars(block: &BasicBlock, program: &Program) -> HashSet<VariableId> {
    let mut vars = HashSet::new();
    for inst in &block.instructions {
        if let InstructionKind::Call { func, args, .. } = &inst.kind {
            let called = &program.functions[*func];
            if called.name == "to_unit_return" {
                for arg in args {
                    if let Operand::Variable(var_id) = arg {
                        vars.insert(*var_id);
                    }
                }
            }
        }
    }
    vars
}

fn augment_exit_liveness_with_to_unit_return(
    func: &Function,
    program: &Program,
    results: &mut DataflowResults<SetLattice<LiveVar>>,
) {
    for &block_id in &func.all_blocks {
        let block = &program.basic_blocks[block_id];
        if !matches!(
            block.terminator,
            Terminator::HopExit { .. } | Terminator::Return(_)
        ) {
            continue;
        }

        let explicit_return_vars = collect_to_unit_return_vars(block, program);
        if explicit_return_vars.is_empty() {
            continue;
        }

        let mut merged = results
            .block_exit
            .get(&block_id)
            .and_then(|lattice| lattice.as_set().cloned())
            .unwrap_or_default();

        for var_id in explicit_return_vars {
            merged.insert(LiveVar(var_id));
        }

        results.block_exit.insert(block_id, SetLattice::new(merged));
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
    let mut results = analysis.analyze(func, program);
    augment_exit_liveness_with_to_unit_return(func, program, &mut results);
    results
}

/// Analyze live variables in a function (backward, hop-level)
pub fn analyze_live_variables_hop(
    func: &Function,
    program: &crate::cfg::Program,
) -> DataflowResults<SetLattice<LiveVar>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Hop,
        Direction::Backward,
        AnalysisKind::May,
        LivenessTransfer,
    );
    let mut results = analysis.analyze(func, program);
    augment_exit_liveness_with_to_unit_return(func, program, &mut results);
    results
}
