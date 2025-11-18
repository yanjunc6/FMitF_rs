use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice,
    TransferFunction,
};

use crate::cfg::{Function, FunctionKind, Program};
use std::collections::HashMap;

impl<L: Lattice, T: TransferFunction<L>> DataflowAnalysis<L, T> {
    pub fn new(
        level: AnalysisLevel,
        direction: Direction,
        kind: AnalysisKind,
        transfer: T,
    ) -> Self {
        Self {
            direction,
            level,
            kind,
            transfer,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Run dataflow analysis on a function using fixed point algorithm
    pub fn analyze(&self, func: &Function, program: &Program) -> DataflowResults<L> {
        // Skip abstract functions (they have no implementation)
        if matches!(func.kind, FunctionKind::Operator) {
            return DataflowResults {
                block_entry: HashMap::new(),
                block_exit: HashMap::new(),
                stmt_entry: HashMap::new(),
                stmt_exit: HashMap::new(),
            };
        }
        match self.direction {
            Direction::Forward => self.analyze_forward(func, program),
            Direction::Backward => self.analyze_backward(func, program),
        }
    }

    /// Run forward analysis
    pub fn analyze_forward(&self, func: &Function, program: &Program) -> DataflowResults<L> {
        let mut block_entry = HashMap::new();
        let mut block_exit = HashMap::new();
        let mut stmt_entry = HashMap::new();
        let mut stmt_exit = HashMap::new();

        // Initialize all blocks with initial value
        for &block_id in &func.all_blocks {
            block_entry.insert(block_id, self.transfer.initial_value());
            block_exit.insert(block_id, self.transfer.initial_value());
        }

        // Set entry block with boundary value
        if let Some(entry_block) = func.entry_block {
            block_entry.insert(
                entry_block,
                self.transfer
                    .boundary_value(func, &program.basic_blocks[entry_block]),
            );
        }

        // Fixed point algorithm
        let mut changed = true;

        while changed {
            changed = false;
            for &block_id in &func.all_blocks {
                let block = &program.basic_blocks[block_id];

                // Compute block entry by merging predecessors
                let new_entry = if block.predecessors.is_empty() {
                    block_entry[&block_id].clone()
                } else {
                    let mut merged = self.transfer.initial_value();
                    for &pred_id in &block.predecessors {
                        if self.level == AnalysisLevel::Hop
                            && matches!(
                                program.basic_blocks[pred_id].terminator,
                                crate::cfg::Terminator::HopExit { .. }
                            )
                        {
                            continue;
                        }
                        if let Some(pred_exit) = block_exit.get(&pred_id) {
                            merged = match self.kind {
                                AnalysisKind::May => merged.join(pred_exit),
                                AnalysisKind::Must => merged.meet(pred_exit),
                            };
                        }
                    }
                    merged
                };

                // Process instructions
                let mut state = new_entry.clone();
                for (idx, inst) in block.instructions.iter().enumerate() {
                    let loc = super::StmtLoc {
                        block: block_id,
                        index: idx,
                    };
                    stmt_entry.insert(loc, state.clone());
                    state = self
                        .transfer
                        .transfer_instruction(inst, loc.clone(), &state);
                    stmt_exit.insert(loc, state.clone());
                }

                // Process terminator
                state = self
                    .transfer
                    .transfer_terminator(&block.terminator, block_id, &state);

                // Check for changes
                if new_entry != block_entry[&block_id] || state != block_exit[&block_id] {
                    changed = true;
                    block_entry.insert(block_id, new_entry);
                    block_exit.insert(block_id, state);
                }
            }
        }

        DataflowResults {
            block_entry,
            block_exit,
            stmt_entry,
            stmt_exit,
        }
    }

    /// Run backward analysis
    pub fn analyze_backward(&self, func: &Function, program: &Program) -> DataflowResults<L> {
        let mut block_entry = HashMap::new();
        let mut block_exit = HashMap::new();
        let mut stmt_entry = HashMap::new();
        let mut stmt_exit = HashMap::new();

        // Initialize all blocks with initial value
        for &block_id in &func.all_blocks {
            block_entry.insert(block_id, self.transfer.initial_value());
            block_exit.insert(block_id, self.transfer.initial_value());
        }

        // Set exit blocks with boundary value (blocks with Return or Abort terminators)
        for &block_id in &func.all_blocks {
            let block = &program.basic_blocks[block_id];
            if matches!(
                block.terminator,
                crate::cfg::Terminator::Return(_) | crate::cfg::Terminator::Abort
            ) {
                let boundary_val = self.transfer.boundary_value(func, block);
                block_exit.insert(block_id, boundary_val);
            }
        }

        // Fixed point algorithm
        let mut changed = true;

        while changed {
            changed = false;
            for &block_id in func.all_blocks.iter().rev() {
                let block = &program.basic_blocks[block_id];

                // Check if this is an exit block
                let is_exit_block = matches!(
                    block.terminator,
                    crate::cfg::Terminator::Return(_) | crate::cfg::Terminator::Abort
                );

                let new_exit = if is_exit_block {
                    // Exit blocks should use their boundary value
                    block_exit[&block_id].clone()
                } else {
                    // For non-exit blocks, we need to find successors by examining terminators
                    let mut merged = self.transfer.initial_value();

                    // Get successor blocks from terminator
                    let successors = match &block.terminator {
                        crate::cfg::Terminator::Jump(target) => vec![*target],
                        crate::cfg::Terminator::Branch {
                            if_true, if_false, ..
                        } => vec![*if_true, *if_false],
                        crate::cfg::Terminator::HopExit { next_hop } => {
                            if let Some(first_block) = program.hops[*next_hop].entry_block {
                                if self.level == AnalysisLevel::Function {
                                    vec![first_block]
                                } else {
                                    vec![]
                                }
                            } else {
                                vec![]
                            }
                        }
                        crate::cfg::Terminator::Return(_) | crate::cfg::Terminator::Abort => vec![],
                    };

                    for succ_id in successors {
                        if let Some(succ_entry) = block_entry.get(&succ_id) {
                            merged = match self.kind {
                                AnalysisKind::May => merged.join(succ_entry),
                                AnalysisKind::Must => merged.meet(succ_entry),
                            };
                        }
                    }
                    merged
                };

                // Process terminator first
                let mut state = new_exit.clone();
                state = self
                    .transfer
                    .transfer_terminator(&block.terminator, block_id, &state);

                // Process instructions in reverse order
                for (idx, inst) in block.instructions.iter().enumerate().rev() {
                    let loc = super::StmtLoc {
                        block: block_id,
                        index: idx,
                    };
                    stmt_exit.insert(loc, state.clone());
                    state = self
                        .transfer
                        .transfer_instruction(inst, loc.clone(), &state);
                    stmt_entry.insert(loc, state.clone());
                }

                // Check for changes
                if state != block_entry[&block_id] || new_exit != block_exit[&block_id] {
                    changed = true;
                    block_entry.insert(block_id, state);
                    block_exit.insert(block_id, new_exit);
                }
            }
        }

        DataflowResults {
            block_entry,
            block_exit,
            stmt_entry,
            stmt_exit,
        }
    }
}
