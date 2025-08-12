use super::{
    AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{
    BasicBlock, BasicBlockId, CfgProgram, EdgeType, FunctionCfg, FunctionImplementation, HopCfg,
};
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
    pub fn new(level: AnalysisLevel, direction: Direction, transfer: T) -> Self {
        Self {
            direction,
            level,
            transfer,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn hop_level(direction: Direction, transfer: T) -> Self {
        Self {
            direction,
            level: AnalysisLevel::Hop,
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
        match self.level {
            AnalysisLevel::Function => self.analyze_function_level(func, cfg_program),
            AnalysisLevel::Hop => self.analyze_hop_level(func, cfg_program),
        }
    }

    /// Run function-level analysis: data flows across all hops in the function
    fn analyze_function_level(
        &self,
        func: &FunctionCfg,
        cfg_program: &CfgProgram,
    ) -> DataflowResults<L> {
        let mut block_entry: HashMap<BasicBlockId, L> = HashMap::new();
        let mut block_exit: HashMap<BasicBlockId, L> = HashMap::new();
        let mut stmt_entry: HashMap<super::StmtLoc, L> = HashMap::new();
        let mut stmt_exit: HashMap<super::StmtLoc, L> = HashMap::new();

        // Initialize all blocks in the function
        self.initialize_blocks_from_list(
            &func.blocks,
            func,
            cfg_program,
            &mut block_entry,
            &mut block_exit,
        );

        // Fixed point iteration over all function blocks
        let mut changed = true;
        while changed {
            changed = false;

            for &block_id in &func.blocks {
                let block = &cfg_program.blocks[block_id];
                let (old_in, old_out) = self.get_in_out_values(&block_entry, &block_exit, block_id);

                let (new_in, new_out, stmt_states) = match self.direction {
                    Direction::Forward => {
                        let new_in =
                            self.compute_in_value(func, block, block_id, &block_entry, &block_exit);
                        let (new_out, stmt_states) =
                            self.transfer_block_with_stmt_states(block, block_id, &new_in);
                        (new_in, new_out, stmt_states)
                    }
                    Direction::Backward => {
                        let new_out = self.compute_out_value(
                            func,
                            block,
                            block_id,
                            &block_entry,
                            &block_exit,
                        );
                        let (new_in, stmt_states) = self
                            .transfer_block_backward_with_stmt_states(block, block_id, &new_out);
                        (new_in, new_out, stmt_states)
                    }
                };

                // Update statement-level states
                for (stmt_loc, (entry_state, exit_state)) in stmt_states {
                    stmt_entry.insert(stmt_loc, entry_state);
                    stmt_exit.insert(stmt_loc, exit_state);
                }

                if self.update_and_check_change(
                    &mut block_entry,
                    &mut block_exit,
                    block_id,
                    new_in,
                    new_out,
                    old_in,
                    old_out,
                ) {
                    changed = true;
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

    /// Run hop-level analysis: each hop analyzed separately, treating hop boundaries as boundaries
    fn analyze_hop_level(
        &self,
        func: &FunctionCfg,
        cfg_program: &CfgProgram,
    ) -> DataflowResults<L> {
        let mut block_entry: HashMap<BasicBlockId, L> = HashMap::new();
        let mut block_exit: HashMap<BasicBlockId, L> = HashMap::new();
        let mut stmt_entry: HashMap<super::StmtLoc, L> = HashMap::new();
        let mut stmt_exit: HashMap<super::StmtLoc, L> = HashMap::new();

        // For each hop, run dataflow analysis independently
        for &hop_id in &func.hops {
            if let Some(hop) = cfg_program.hops.get(hop_id) {
                // Initialize blocks in this hop only
                self.initialize_blocks_from_list(
                    &hop.blocks,
                    func,
                    cfg_program,
                    &mut block_entry,
                    &mut block_exit,
                );

                // Fixed point iteration over blocks in this hop only
                let mut changed = true;
                while changed {
                    changed = false;

                    for &block_id in &hop.blocks {
                        let block = &cfg_program.blocks[block_id];
                        let (old_in, old_out) =
                            self.get_in_out_values(&block_entry, &block_exit, block_id);

                        let (new_in, new_out, stmt_states) = match self.direction {
                            Direction::Forward => {
                                let new_in = self.compute_in_value_hop_level(
                                    hop,
                                    block,
                                    block_id,
                                    &block_entry,
                                    &block_exit,
                                );
                                let (new_out, stmt_states) =
                                    self.transfer_block_with_stmt_states(block, block_id, &new_in);
                                (new_in, new_out, stmt_states)
                            }
                            Direction::Backward => {
                                let new_out = self.compute_out_value_hop_level(
                                    hop,
                                    block,
                                    block_id,
                                    &block_entry,
                                    &block_exit,
                                );
                                let (new_in, stmt_states) = self
                                    .transfer_block_backward_with_stmt_states(
                                        block, block_id, &new_out,
                                    );
                                (new_in, new_out, stmt_states)
                            }
                        };

                        // Update statement-level states
                        for (stmt_loc, (entry_state, exit_state)) in stmt_states {
                            stmt_entry.insert(stmt_loc, entry_state);
                            stmt_exit.insert(stmt_loc, exit_state);
                        }

                        if self.update_and_check_change(
                            &mut block_entry,
                            &mut block_exit,
                            block_id,
                            new_in,
                            new_out,
                            old_in,
                            old_out,
                        ) {
                            changed = true;
                        }
                    }
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

    fn initialize_blocks_from_list(
        &self,
        block_list: &[BasicBlockId],
        func: &FunctionCfg,
        cfg_program: &CfgProgram,
        block_entry: &mut HashMap<BasicBlockId, L>,
        block_exit: &mut HashMap<BasicBlockId, L>,
    ) {
        for &block_id in block_list {
            let block = &cfg_program.blocks[block_id];
            let is_boundary = self.is_boundary_block(func, cfg_program, block, block_id);
            let default_value = L::bottom().unwrap_or_else(|| self.transfer.initial_value());

            match self.direction {
                Direction::Forward => {
                    let init_entry = if is_boundary {
                        self.transfer.boundary_value()
                    } else {
                        default_value.clone()
                    };
                    block_entry.insert(block_id, init_entry);
                    block_exit.insert(block_id, default_value);
                }
                Direction::Backward => {
                    let init_exit = if is_boundary {
                        self.transfer.boundary_value()
                    } else {
                        default_value.clone()
                    };
                    block_entry.insert(block_id, default_value);
                    block_exit.insert(block_id, init_exit);
                }
            }
        }
    }

    fn is_boundary_block(
        &self,
        func: &FunctionCfg,
        cfg_program: &CfgProgram,
        block: &BasicBlock,
        block_id: BasicBlockId,
    ) -> bool {
        match self.direction {
            Direction::Forward => {
                // Entry block for forward analysis
                func.entry_hop
                    .and_then(|hop_id| cfg_program.hops.get(hop_id))
                    .and_then(|hop| hop.entry_block)
                    .map(|entry_block| entry_block == block_id)
                    .unwrap_or(false)
            }
            Direction::Backward => {
                // Exit block for backward analysis (has return or abort edges)
                block
                    .successors
                    .iter()
                    .any(|edge| matches!(edge.edge_type, EdgeType::Return { .. } | EdgeType::Abort))
            }
        }
    }

    fn get_in_out_values(
        &self,
        block_entry: &HashMap<BasicBlockId, L>,
        block_exit: &HashMap<BasicBlockId, L>,
        block_id: BasicBlockId,
    ) -> (L, L) {
        (
            block_entry[&block_id].clone(),
            block_exit[&block_id].clone(),
        )
    }

    fn compute_in_value(
        &self,
        _func: &FunctionCfg,
        block: &BasicBlock,
        block_id: BasicBlockId,
        block_entry: &HashMap<BasicBlockId, L>,
        block_exit: &HashMap<BasicBlockId, L>,
    ) -> L {
        let neighbors = match self.direction {
            Direction::Forward => &block.predecessors,
            Direction::Backward => &block.successors,
        };

        if neighbors.is_empty() {
            // No neighbors - keep current value for boundary blocks
            match self.direction {
                Direction::Forward => block_entry[&block_id].clone(),
                Direction::Backward => block_exit[&block_id].clone(),
            }
        } else {
            // For function-level analysis, join all neighbor values
            let mut result: Option<L> = None;
            for edge in neighbors {
                let neighbor_value = match self.direction {
                    Direction::Forward => &block_exit[&edge.from],
                    Direction::Backward => &block_entry[&edge.to],
                };
                result = Some(match result {
                    None => neighbor_value.clone(),
                    Some(acc) => acc.join(neighbor_value),
                });
            }
            result.unwrap_or_else(|| L::bottom().unwrap_or_else(|| self.transfer.initial_value()))
        }
    }

    fn compute_out_value(
        &self,
        _func: &FunctionCfg,
        block: &BasicBlock,
        block_id: BasicBlockId,
        block_entry: &HashMap<BasicBlockId, L>,
        block_exit: &HashMap<BasicBlockId, L>,
    ) -> L {
        let neighbors = &block.successors;

        match self.direction {
            Direction::Forward => {
                // For forward analysis, filter out return and abort edges as they represent
                // function exits, not control flow to other blocks
                let control_flow_edges: Vec<_> = neighbors
                    .iter()
                    .filter(|edge| {
                        !matches!(edge.edge_type, EdgeType::Return { .. } | EdgeType::Abort)
                    })
                    .collect();

                if control_flow_edges.is_empty() {
                    // No control flow neighbors - this is a boundary block
                    block_exit[&block_id].clone()
                } else {
                    // Join all successor entry values from control flow edges
                    let mut result: Option<L> = None;
                    for edge in control_flow_edges {
                        let neighbor_value = &block_entry[&edge.to];
                        result = Some(match result {
                            None => neighbor_value.clone(),
                            Some(acc) => acc.join(neighbor_value),
                        });
                    }
                    result.unwrap_or_else(|| {
                        L::bottom().unwrap_or_else(|| self.transfer.initial_value())
                    })
                }
            }
            Direction::Backward => {
                // For backward analysis, we need to handle return/abort edges specially
                // because they don't point to other blocks but contain liveness information
                let control_flow_edges: Vec<_> = neighbors
                    .iter()
                    .filter(|edge| {
                        !matches!(edge.edge_type, EdgeType::Return { .. } | EdgeType::Abort)
                    })
                    .collect();

                if control_flow_edges.is_empty() {
                    // No control flow neighbors - this is a boundary block with return/abort edges
                    // Start with boundary value and apply transfer function for return/abort edges
                    let mut result = self.transfer.boundary_value();
                    for edge in neighbors {
                        if matches!(edge.edge_type, EdgeType::Return { .. } | EdgeType::Abort) {
                            result = self.transfer.transfer_edge(edge, &result);
                        }
                    }
                    result
                } else {
                    // Join all successor entry values from control flow edges
                    let mut result: Option<L> = None;
                    for edge in control_flow_edges {
                        let neighbor_value = &block_entry[&edge.to];
                        result = Some(match result {
                            None => neighbor_value.clone(),
                            Some(acc) => acc.join(neighbor_value),
                        });
                    }
                    result.unwrap_or_else(|| {
                        L::bottom().unwrap_or_else(|| self.transfer.initial_value())
                    })
                }
            }
        }
    }

    fn compute_in_value_hop_level(
        &self,
        hop: &HopCfg,
        block: &BasicBlock,
        block_id: BasicBlockId,
        block_entry: &HashMap<BasicBlockId, L>,
        block_exit: &HashMap<BasicBlockId, L>,
    ) -> L {
        let neighbors = match self.direction {
            Direction::Forward => &block.predecessors,
            Direction::Backward => &block.successors,
        };

        if neighbors.is_empty() {
            // No neighbors - keep current value for boundary blocks
            match self.direction {
                Direction::Forward => block_entry[&block_id].clone(),
                Direction::Backward => block_exit[&block_id].clone(),
            }
        } else {
            // Join values from neighbors that are within the same hop
            let mut result: Option<L> = None;
            for edge in neighbors {
                let neighbor_block_id = match self.direction {
                    Direction::Forward => edge.from,
                    Direction::Backward => edge.to,
                };

                // Only consider neighbors that are within the same hop
                if hop.blocks.contains(&neighbor_block_id) {
                    let neighbor_value = match self.direction {
                        Direction::Forward => &block_exit[&neighbor_block_id],
                        Direction::Backward => &block_entry[&neighbor_block_id],
                    };
                    result = Some(match result {
                        None => neighbor_value.clone(),
                        Some(acc) => acc.join(neighbor_value),
                    });
                } else {
                    // Neighbor is outside this hop - use initial value for boundary
                    let initial = self.transfer.initial_value();
                    result = Some(match result {
                        None => initial,
                        Some(acc) => acc.join(&initial),
                    });
                }
            }
            result.unwrap_or_else(|| L::bottom().unwrap_or_else(|| self.transfer.initial_value()))
        }
    }

    fn compute_out_value_hop_level(
        &self,
        hop: &HopCfg,
        block: &BasicBlock,
        block_id: BasicBlockId,
        block_entry: &HashMap<BasicBlockId, L>,
        block_exit: &HashMap<BasicBlockId, L>,
    ) -> L {
        let neighbors = &block.successors;

        if neighbors.is_empty() {
            // No neighbors - keep current value for boundary blocks
            block_exit[&block_id].clone()
        } else {
            // Join values from successors that are within the same hop
            let mut result: Option<L> = None;
            for edge in neighbors {
                let neighbor_block_id = edge.to;

                // Only consider neighbors that are within the same hop
                if hop.blocks.contains(&neighbor_block_id) {
                    let neighbor_value = &block_entry[&neighbor_block_id];
                    result = Some(match result {
                        None => neighbor_value.clone(),
                        Some(acc) => acc.join(neighbor_value),
                    });
                } else {
                    // Neighbor is outside this hop - use initial value for boundary
                    let initial = self.transfer.initial_value();
                    result = Some(match result {
                        None => initial,
                        Some(acc) => acc.join(&initial),
                    });
                }
            }
            result.unwrap_or_else(|| L::bottom().unwrap_or_else(|| self.transfer.initial_value()))
        }
    }

    fn transfer_block_with_stmt_states(
        &self,
        block: &BasicBlock,
        block_id: BasicBlockId,
        in_value: &L,
    ) -> (L, Vec<(super::StmtLoc, (L, L))>) {
        let mut current = in_value.clone();
        let mut stmt_states = Vec::new();

        match self.direction {
            Direction::Forward => {
                // Apply statements in order
                for (stmt_index, stmt) in block.statements.iter().enumerate() {
                    let stmt_loc = super::StmtLoc {
                        block: block_id,
                        index: stmt_index,
                    };
                    let stmt_entry = current.clone();
                    current = self.transfer.transfer_statement(stmt, &current);
                    let stmt_exit = current.clone();
                    stmt_states.push((stmt_loc, (stmt_entry, stmt_exit)));
                }

                // Apply edges
                for edge in &block.successors {
                    current = self.transfer.transfer_edge(edge, &current);
                }
            }
            Direction::Backward => {
                // Apply edges in reverse first
                for edge in block.successors.iter().rev() {
                    current = self.transfer.transfer_edge(edge, &current);
                }

                // Apply statements in reverse
                for (stmt_index, stmt) in block.statements.iter().enumerate().rev() {
                    let stmt_loc = super::StmtLoc {
                        block: block_id,
                        index: stmt_index,
                    };
                    let stmt_exit = current.clone();
                    current = self.transfer.transfer_statement(stmt, &current);
                    let stmt_entry = current.clone();
                    stmt_states.push((stmt_loc, (stmt_entry, stmt_exit)));
                }
            }
        }

        (current, stmt_states)
    }

    fn transfer_block_backward_with_stmt_states(
        &self,
        block: &BasicBlock,
        block_id: BasicBlockId,
        out_value: &L,
    ) -> (L, Vec<(super::StmtLoc, (L, L))>) {
        let mut current = out_value.clone();
        let mut stmt_states = Vec::new();

        // For backward analysis: apply edges in reverse, then statements in reverse
        for edge in block.successors.iter().rev() {
            current = self.transfer.transfer_edge(edge, &current);
        }

        for (stmt_index, stmt) in block.statements.iter().enumerate().rev() {
            let stmt_loc = super::StmtLoc {
                block: block_id,
                index: stmt_index,
            };
            let stmt_exit = current.clone();
            current = self.transfer.transfer_statement(stmt, &current);
            let stmt_entry = current.clone();
            stmt_states.push((stmt_loc, (stmt_entry, stmt_exit)));
        }

        (current, stmt_states)
    }

    fn update_and_check_change(
        &self,
        block_entry: &mut HashMap<BasicBlockId, L>,
        block_exit: &mut HashMap<BasicBlockId, L>,
        block_id: BasicBlockId,
        new_in: L,
        new_out: L,
        old_in: L,
        old_out: L,
    ) -> bool {
        let mut changed = false;

        match self.direction {
            Direction::Forward => {
                if new_in != old_in {
                    block_entry.insert(block_id, new_in);
                    changed = true;
                }
                if new_out != old_out {
                    block_exit.insert(block_id, new_out);
                    changed = true;
                }
            }
            Direction::Backward => {
                if new_out != old_out {
                    block_exit.insert(block_id, new_out);
                    changed = true;
                }
                if new_in != old_in {
                    block_entry.insert(block_id, new_in);
                    changed = true;
                }
            }
        }

        changed
    }
}
