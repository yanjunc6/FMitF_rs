// dataflow/mod.rs
use crate::cfg::{BasicBlock, BasicBlockId, FunctionCfg, Statement, Terminator};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

mod liveness;
pub use liveness::analyze_live_variables;

mod reaching_definitions;
pub use reaching_definitions::{analyze_reaching_definitions, Definition as ReachingDefinition};

mod available_expressions;
pub use available_expressions::analyze_available_expressions;

/// Direction of dataflow analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Backward,
}

/// Trait for lattice values in dataflow analysis
pub trait Lattice: Clone + Eq + Debug {
    /// Bottom element of the lattice
    fn bottom() -> Self;

    /// Top element of the lattice (for some analyses)
    fn top() -> Self;

    /// Meet operation (∧ or ∨ depending on the lattice)
    fn meet(&self, other: &Self) -> Self;

    /// Check if this value is less than or equal to another in the lattice order
    fn less_equal(&self, other: &Self) -> bool {
        self.meet(other) == *self
    }
}

/// Trait for transfer functions
pub trait TransferFunction<L: Lattice> {
    /// Apply transfer function for a statement
    fn transfer_statement(&self, stmt: &Statement, state: &L) -> L;

    /// Apply transfer function for a terminator
    fn transfer_terminator(&self, term: &Terminator, state: &L) -> L;

    /// Get initial value for entry/exit of function
    fn initial_value(&self) -> L;

    /// Get boundary value for function parameters (for forward analysis)
    /// or return statements (for backward analysis)
    fn boundary_value(&self) -> L;
}

/// General monotone dataflow analysis framework
pub struct DataflowAnalysis<L: Lattice, T: TransferFunction<L>> {
    pub direction: Direction,
    pub transfer: T,
    _phantom: std::marker::PhantomData<L>,
}

/// Results of dataflow analysis
pub struct DataflowResults<L: Lattice> {
    /// Values at entry of each basic block
    pub entry: HashMap<BasicBlockId, L>,
    /// Values at exit of each basic block
    pub exit: HashMap<BasicBlockId, L>,
}

impl<L: Lattice, T: TransferFunction<L>> DataflowAnalysis<L, T> {
    pub fn new(direction: Direction, transfer: T) -> Self {
        Self {
            direction,
            transfer,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Run dataflow analysis on a function
    pub fn analyze(&self, func: &FunctionCfg) -> DataflowResults<L> {
        let mut entry: HashMap<BasicBlockId, L> = HashMap::new();
        let mut exit: HashMap<BasicBlockId, L> = HashMap::new();

        // Initialize all blocks with bottom
        for (block_id, _) in func.blocks.iter() {
            entry.insert(block_id, L::bottom());
            exit.insert(block_id, L::bottom());
        }

        // Set initial values
        match self.direction {
            Direction::Forward => {
                // Set entry of first blocks to initial value
                if let Some(entry_hop) = func.entry_hop {
                    if let Some(hop) = func.hops.get(entry_hop) {
                        if let Some(entry_block) = hop.entry_block {
                            entry.insert(entry_block, self.transfer.initial_value());
                        }
                    }
                }
            }
            Direction::Backward => {
                // Set exit of blocks with return to initial value
                for (block_id, block) in func.blocks.iter() {
                    if matches!(block.terminator, Terminator::Return(_)) {
                        exit.insert(block_id, self.transfer.initial_value());
                    }
                }
            }
        }

        // Worklist algorithm
        let mut worklist: VecDeque<BasicBlockId> = func.blocks.iter().map(|(id, _)| id).collect();

        while let Some(block_id) = worklist.pop_front() {
            let block = &func.blocks[block_id];

            let old_val = match self.direction {
                Direction::Forward => exit[&block_id].clone(),
                Direction::Backward => entry[&block_id].clone(),
            };

            let new_val = match self.direction {
                Direction::Forward => {
                    // Compute entry as meet of predecessors' exits
                    let preds = self.get_predecessors(func, block_id);
                    let entry_val = if preds.is_empty() {
                        entry[&block_id].clone()
                    } else {
                        preds
                            .iter()
                            .map(|pred| &exit[pred])
                            .fold(L::top(), |acc, val| acc.meet(val))
                    };
                    entry.insert(block_id, entry_val.clone());

                    // Compute exit by applying transfer function
                    self.transfer_block_forward(block, &entry_val)
                }
                Direction::Backward => {
                    // Compute exit as meet of successors' entries
                    let succs = self.get_successors(block);
                    let exit_val = if succs.is_empty() {
                        exit[&block_id].clone()
                    } else {
                        succs
                            .iter()
                            .map(|succ| &entry[succ])
                            .fold(L::top(), |acc, val| acc.meet(val))
                    };
                    exit.insert(block_id, exit_val.clone());

                    // Compute entry by applying transfer function
                    self.transfer_block_backward(block, &exit_val)
                }
            };

            // Update and add to worklist if changed
            let changed = match self.direction {
                Direction::Forward => {
                    if new_val != old_val {
                        exit.insert(block_id, new_val);
                        true
                    } else {
                        false
                    }
                }
                Direction::Backward => {
                    if new_val != old_val {
                        entry.insert(block_id, new_val);
                        true
                    } else {
                        false
                    }
                }
            };

            if changed {
                // Add affected blocks to worklist
                match self.direction {
                    Direction::Forward => {
                        worklist.extend(self.get_successors(block));
                    }
                    Direction::Backward => {
                        worklist.extend(self.get_predecessors(func, block_id));
                    }
                }
            }
        }

        DataflowResults { entry, exit }
    }

    fn transfer_block_forward(&self, block: &BasicBlock, state: &L) -> L {
        let mut current = state.clone();

        // Apply transfer functions for statements
        for stmt in &block.statements {
            current = self.transfer.transfer_statement(stmt, &current);
        }

        // Apply transfer function for terminator
        self.transfer
            .transfer_terminator(&block.terminator, &current)
    }

    fn transfer_block_backward(&self, block: &BasicBlock, state: &L) -> L {
        // First apply terminator
        let mut current = self.transfer.transfer_terminator(&block.terminator, state);

        // Then apply statements in reverse order
        for stmt in block.statements.iter().rev() {
            current = self.transfer.transfer_statement(stmt, &current);
        }

        current
    }

    fn get_successors(&self, block: &BasicBlock) -> Vec<BasicBlockId> {
        match &block.terminator {
            Terminator::Goto(target) => vec![*target],
            Terminator::Branch {
                then_block,
                else_block,
                ..
            } => {
                vec![*then_block, *else_block]
            }
            Terminator::Return(_) | Terminator::Abort | Terminator::HopExit { .. } => vec![],
        }
    }

    fn get_predecessors(&self, func: &FunctionCfg, block_id: BasicBlockId) -> Vec<BasicBlockId> {
        let mut preds = Vec::new();

        for (pred_id, pred_block) in func.blocks.iter() {
            match &pred_block.terminator {
                Terminator::Goto(target) if *target == block_id => {
                    preds.push(pred_id);
                }
                Terminator::Branch {
                    then_block,
                    else_block,
                    ..
                } => {
                    if *then_block == block_id || *else_block == block_id {
                        preds.push(pred_id);
                    }
                }
                _ => {}
            }
        }

        preds
    }
}

/// Common lattice implementation for set-based analyses
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SetLattice<T: Clone + Eq + Hash + Debug> {
    pub set: HashSet<T>,
}

impl<T: Clone + Eq + Hash + Debug> Lattice for SetLattice<T> {
    fn bottom() -> Self {
        Self {
            set: HashSet::new(),
        }
    }

    fn top() -> Self {
        // For finite sets, we'd need to know the universe
        // For now, we'll handle this in specific analyses
        Self {
            set: HashSet::new(),
        }
    }

    fn meet(&self, other: &Self) -> Self {
        // Default to union for may analyses
        // Override for must analyses (intersection)
        Self {
            set: self.set.union(&other.set).cloned().collect(),
        }
    }
}
