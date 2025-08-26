use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice,
    TransferFunction,
};

use crate::cfg::{CfgProgram, EdgeType, FunctionCfg, FunctionImplementation};
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
    pub fn analyze(&self, func: &FunctionCfg, cfg_program: &CfgProgram) -> DataflowResults<L> {
        if func.implementation == FunctionImplementation::Abstract {
            return DataflowResults {
                block_entry: HashMap::new(),
                block_exit: HashMap::new(),
                stmt_entry: HashMap::new(),
                stmt_exit: HashMap::new(),
            };
        }
        match self.direction {
            Direction::Forward => self.analyze_forward(func, cfg_program),
            Direction::Backward => self.analyze_backward(func, cfg_program),
        }
    }

    /// Run forward analysis
    pub fn analyze_forward(
        &self,
        func: &FunctionCfg,
        cfg_program: &CfgProgram,
    ) -> DataflowResults<L> {
        let mut block_entry = HashMap::new();
        let mut block_exit = HashMap::new();
        let mut stmt_entry = HashMap::new();
        let mut stmt_exit = HashMap::new();

        // Initialize all blocks with initial value
        for &block_id in &func.blocks {
            block_entry.insert(block_id, self.transfer.initial_value());
            block_exit.insert(block_id, self.transfer.initial_value());
        }

        // Set entry block with boundary value
        if let Some(entry_hop) = func.entry_hop {
            if let Some(entry_block) = cfg_program.hops[entry_hop].entry_block {
                block_entry.insert(
                    entry_block,
                    self.transfer
                        .boundary_value(func, &cfg_program.blocks[entry_block]),
                );
            }
        }

        // fixed point algorithm
        let mut changed = true;

        while changed {
            changed = false;
            for &block_id in &func.blocks {
                let block = &cfg_program.blocks[block_id];

                // Compute block entry by merging predecessors
                let new_entry = if block.predecessors.is_empty() {
                    block_entry[&block_id].clone()
                } else {
                    let mut merged = self.transfer.initial_value();
                    for pred_edge in &block.predecessors {
                        // Check if this is a hop boundary for hop-level analysis
                        let is_hop_boundary = self.level == AnalysisLevel::Hop
                            && matches!(pred_edge.edge_type, EdgeType::HopExit { .. });

                        if is_hop_boundary {
                            // At hop boundaries, use initial value for hop-level analysis
                            merged = match self.kind {
                                AnalysisKind::May => merged.join(&self.transfer.initial_value()),
                                AnalysisKind::Must => merged.meet(&self.transfer.initial_value()),
                            };
                        } else if let Some(pred_exit) = block_exit.get(&pred_edge.from) {
                            let transferred = self.transfer.transfer_edge(pred_edge, pred_exit);
                            merged = match self.kind {
                                AnalysisKind::May => merged.join(&transferred),
                                AnalysisKind::Must => merged.meet(&transferred),
                            };
                        }
                    }
                    merged
                };

                // Process statements
                let mut state = new_entry.clone();
                for (idx, stmt) in block.statements.iter().enumerate() {
                    let loc = super::StmtLoc {
                        block: block_id,
                        index: idx,
                    };
                    stmt_entry.insert(loc, state.clone());
                    state = self.transfer.transfer_statement(stmt, loc.clone(), &state);
                    stmt_exit.insert(loc, state.clone());
                }

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
    pub fn analyze_backward(
        &self,
        func: &FunctionCfg,
        cfg_program: &CfgProgram,
    ) -> DataflowResults<L> {
        let mut block_entry = HashMap::new();
        let mut block_exit = HashMap::new();
        let mut stmt_entry = HashMap::new();
        let mut stmt_exit = HashMap::new();

        // Initialize all blocks with initial value
        for &block_id in &func.blocks {
            block_entry.insert(block_id, self.transfer.initial_value());
            block_exit.insert(block_id, self.transfer.initial_value());
        }

        // Set exit blocks with boundary value
        for &block_id in &func.blocks {
            let block = &cfg_program.blocks[block_id];
            if block.successors.is_empty()
                || block
                    .successors
                    .iter()
                    .any(|e| matches!(e.edge_type, EdgeType::Return { .. } | EdgeType::Abort))
            {
                let boundary_val = self.transfer.boundary_value(func, block);
                block_exit.insert(block_id, boundary_val);
            }
        }

        // Fixed point algorithm
        let mut changed = true;

        while changed {
            changed = false;
            for &block_id in func.blocks.iter().rev() {
                let block = &cfg_program.blocks[block_id];

                // Compute block exit by merging successors
                // TODO: not sure why we have to check return and abort, might be a problem from cfg.
                let is_exit_block = block.successors.is_empty()
                    || block
                        .successors
                        .iter()
                        .any(|e| matches!(e.edge_type, EdgeType::Return { .. } | EdgeType::Abort));

                let new_exit = if is_exit_block {
                    // Exit blocks should use their boundary value, not compute from successors
                    block_exit[&block_id].clone()
                } else {
                    let mut merged = self.transfer.initial_value();
                    for succ_edge in &block.successors {
                        // Check if this is a hop boundary for hop-level analysis
                        let is_hop_boundary = self.level == AnalysisLevel::Hop
                            && matches!(succ_edge.edge_type, EdgeType::HopExit { .. });

                        if is_hop_boundary {
                            // At hop boundaries, use initial value for hop-level analysis
                            merged = match self.kind {
                                AnalysisKind::May => merged.join(&self.transfer.initial_value()),
                                AnalysisKind::Must => merged.meet(&self.transfer.initial_value()),
                            };
                        } else if let Some(succ_entry) = block_entry.get(&succ_edge.to) {
                            let transferred = self.transfer.transfer_edge(succ_edge, succ_entry);
                            merged = match self.kind {
                                AnalysisKind::May => merged.join(&transferred),
                                AnalysisKind::Must => merged.meet(&transferred),
                            };
                        }
                    }
                    merged
                };

                // Process statements in reverse order
                let mut state = new_exit.clone();
                for (idx, stmt) in block.statements.iter().enumerate().rev() {
                    let loc = super::StmtLoc {
                        block: block_id,
                        index: idx,
                    };
                    // For backward analysis: current state represents what's live after this statement (live-out)
                    stmt_exit.insert(loc, state.clone());
                    // Apply transfer function to get what's live before this statement (live-in)
                    state = self.transfer.transfer_statement(stmt, loc.clone(), &state);
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
