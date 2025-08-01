use super::{
    AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Lattice, SetLattice,
    TransferFunction,
};
use crate::cfg::{BasicBlock, BasicBlockId, EdgeType, FunctionCfg};
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
    pub fn analyze(&self, func: &FunctionCfg) -> DataflowResults<L> {
        let mut entry: HashMap<BasicBlockId, L> = HashMap::new();
        let mut exit: HashMap<BasicBlockId, L> = HashMap::new();

        // Initialize all blocks
        self.initialize_blocks(func, &mut entry, &mut exit);

        // Fixed point iteration
        let mut changed = true;
        while changed {
            changed = false;

            for (block_id, block) in func.blocks.iter() {
                let (old_in, old_out) = self.get_in_out_values(&entry, &exit, block_id);
                let new_in = self.compute_in_value(func, block, block_id, &entry, &exit);
                let new_out = self.transfer_block(block, &new_in);

                if self.update_and_check_change(
                    &mut entry, &mut exit, block_id, new_in, new_out, old_in, old_out,
                ) {
                    changed = true;
                }
            }
        }

        DataflowResults { entry, exit }
    }

    fn initialize_blocks(
        &self,
        func: &FunctionCfg,
        entry: &mut HashMap<BasicBlockId, L>,
        exit: &mut HashMap<BasicBlockId, L>,
    ) {
        for (block_id, block) in func.blocks.iter() {
            let is_boundary = self.is_boundary_block(func, block, block_id);
            let default_value = L::bottom().unwrap_or_else(|| self.transfer.initial_value());

            match self.direction {
                Direction::Forward => {
                    let init_entry = if is_boundary {
                        self.transfer.boundary_value()
                    } else {
                        default_value.clone()
                    };
                    entry.insert(block_id, init_entry);
                    exit.insert(block_id, default_value);
                }
                Direction::Backward => {
                    let init_exit = if is_boundary {
                        self.transfer.boundary_value()
                    } else {
                        default_value.clone()
                    };
                    entry.insert(block_id, default_value);
                    exit.insert(block_id, init_exit);
                }
            }
        }
    }

    fn is_boundary_block(
        &self,
        func: &FunctionCfg,
        block: &BasicBlock,
        block_id: BasicBlockId,
    ) -> bool {
        match self.direction {
            Direction::Forward => {
                // Entry block for forward analysis
                func.entry_hop
                    .and_then(|hop_id| func.hops.get(hop_id))
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
        entry: &HashMap<BasicBlockId, L>,
        exit: &HashMap<BasicBlockId, L>,
        block_id: BasicBlockId,
    ) -> (L, L) {
        (entry[&block_id].clone(), exit[&block_id].clone())
    }

    fn compute_in_value(
        &self,
        _func: &FunctionCfg,
        block: &BasicBlock,
        block_id: BasicBlockId,
        entry: &HashMap<BasicBlockId, L>,
        exit: &HashMap<BasicBlockId, L>,
    ) -> L {
        let neighbors = match self.direction {
            Direction::Forward => &block.predecessors,
            Direction::Backward => &block.successors,
        };

        if neighbors.is_empty() {
            // No neighbors - keep current value for boundary blocks
            match self.direction {
                Direction::Forward => entry[&block_id].clone(),
                Direction::Backward => exit[&block_id].clone(),
            }
        } else {
            // Join all neighbor values, but filter hop exits for hop-level analysis
            let mut result: Option<L> = None;
            for edge in neighbors {
                // For hop-level analysis, ignore hop exit edges (treat as boundaries)
                if matches!(self.level, AnalysisLevel::Hop)
                    && matches!(edge.edge_type, EdgeType::HopExit { .. })
                {
                    // Use initial value for hop exit boundaries
                    let initial = self.transfer.initial_value();
                    result = Some(match result {
                        None => initial,
                        Some(acc) => acc.join(&initial),
                    });
                } else {
                    // Normal data flow
                    let neighbor_value = match self.direction {
                        Direction::Forward => &exit[&edge.from],
                        Direction::Backward => &entry[&edge.to],
                    };
                    result = Some(match result {
                        None => neighbor_value.clone(),
                        Some(acc) => acc.join(neighbor_value),
                    });
                }
            }
            result.unwrap_or_else(|| L::bottom().unwrap_or_else(|| self.transfer.initial_value()))
        }
    }

    fn transfer_block(&self, block: &BasicBlock, in_value: &L) -> L {
        let mut current = in_value.clone();

        match self.direction {
            Direction::Forward => {
                // Apply statements in order, then edges
                for stmt in &block.statements {
                    current = self.transfer.transfer_statement(stmt, &current);
                }
                for edge in &block.successors {
                    current = self.transfer.transfer_edge(edge, &current);
                }
            }
            Direction::Backward => {
                // Apply edges in reverse, then statements in reverse
                for edge in block.successors.iter().rev() {
                    current = self.transfer.transfer_edge(edge, &current);
                }
                for stmt in block.statements.iter().rev() {
                    current = self.transfer.transfer_statement(stmt, &current);
                }
            }
        }

        current
    }

    fn update_and_check_change(
        &self,
        entry: &mut HashMap<BasicBlockId, L>,
        exit: &mut HashMap<BasicBlockId, L>,
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
                    entry.insert(block_id, new_in);
                    changed = true;
                }
                if new_out != old_out {
                    exit.insert(block_id, new_out);
                    changed = true;
                }
            }
            Direction::Backward => {
                if new_out != old_out {
                    exit.insert(block_id, new_out);
                    changed = true;
                }
                if new_in != old_in {
                    entry.insert(block_id, new_in);
                    changed = true;
                }
            }
        }

        changed
    }
}
