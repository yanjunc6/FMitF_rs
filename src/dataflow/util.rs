use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg;
use crate::cfg::{CfgProgram, EdgeType, FunctionCfg, FunctionImplementation};
use std::collections::HashMap;

use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

impl<T: Eq + Hash + Clone + Debug> PartialEq for SetLattice<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self.is_top, other.is_top) {
            (true, true) => true,                    // ⊤ == ⊤
            (false, false) => self.set == other.set, // compare sets
            _ => false,                              // ⊤ ̸= anything else
        }
    }
}
impl<T: Eq + Hash + Clone + Debug> Eq for SetLattice<T> {}

impl<T: Eq + Hash + Clone + Debug> SetLattice<T> {
    /// Convenience constructor for an ordinary set element (not ⊤).
    pub fn new(elements: impl IntoIterator<Item = T>) -> Self {
        Self {
            set: elements.into_iter().collect(),
            is_top: false,
        }
    }

    /// Construct ⊤ explicitly when you need it.
    pub fn top_element() -> Self {
        Self {
            set: HashSet::new(),
            is_top: true,
        }
    }

    /// Get a reference to the underlying set.
    /// Returns None if this is the top element.
    pub fn as_set(&self) -> Option<&HashSet<T>> {
        if self.is_top {
            None
        } else {
            Some(&self.set)
        }
    }

    /// Check if this is the top element
    pub fn is_top(&self) -> bool {
        self.is_top
    }
}

impl<T: Eq + Hash + Clone + Debug> Lattice for SetLattice<T> {
    /* ---------- lattice bounds ---------- */

    fn bottom() -> Option<Self> {
        Some(Self {
            set: HashSet::new(), // ∅
            is_top: false,
        })
    }

    fn top() -> Option<Self> {
        Some(Self::top_element()) // ⊤
    }

    /* ---------- lattice operations ---------- */

    // meet (intersection)
    fn meet(&self, other: &Self) -> Self {
        match (self.is_top, other.is_top) {
            (true, true) => self.clone(),   // ⊤ ∧ ⊤ = ⊤
            (true, false) => other.clone(), // ⊤ ∧ X  = X
            (false, true) => self.clone(),  // X ∧ ⊤  = X
            (false, false) => {
                let inter: HashSet<_> = self.set.intersection(&other.set).cloned().collect();
                Self {
                    set: inter,
                    is_top: false,
                }
            }
        }
    }

    // join (union)
    fn join(&self, other: &Self) -> Self {
        match (self.is_top, other.is_top) {
            (true, _) | (_, true) => Self::top_element(), // ⊤ ∨ X = ⊤
            (false, false) => {
                let uni: HashSet<_> = self.set.union(&other.set).cloned().collect();
                Self {
                    set: uni,
                    is_top: false,
                }
            }
        }
    }

    /* ---------- (optional) tighter ≤ than the default ---------- */

    // A ⩽ B  ≜  A ⊆ B (with ⊤ on top)
    fn less_equal(&self, other: &Self) -> bool {
        match (self.is_top, other.is_top) {
            (true, true) => true,   // ⊤ ⩽ ⊤
            (true, false) => false, // ⊤ ⩽ X  is false
            (false, true) => true,  // X  ⩽ ⊤  is true
            (false, false) => self.set.is_subset(&other.set),
        }
    }
}

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
                    state = self.transfer.transfer_statement(stmt, &state);
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
                block_exit.insert(block_id, self.transfer.boundary_value(func, block));
            }
        }

        // Fixed point algorithm
        let mut changed = true;

        while changed {
            changed = false;
            for &block_id in func.blocks.iter().rev() {
                let block = &cfg_program.blocks[block_id];

                // Compute block exit by merging successors
                let new_exit = if block.successors.is_empty() {
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
                    stmt_exit.insert(loc, state.clone());
                    state = self.transfer.transfer_statement(stmt, &state);
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
