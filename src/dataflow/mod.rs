// dataflow/mod.rs
use crate::cfg::{BasicBlockId, ControlFlowEdge, Statement};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

mod util;

mod liveness;
pub use liveness::analyze_live_variables;

mod reaching_definitions;
pub use reaching_definitions::analyze_reaching_definitions;

mod available_expressions;
pub use available_expressions::analyze_available_expressions;

mod table_mod_ref;
pub use table_mod_ref::{analyze_table_mod_ref, AccessType, TableAccess};

/// Direction of dataflow analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Backward,
}

/// Level of dataflow analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnalysisLevel {
    /// Function-level analysis: data flows between hops across the entire function
    Function,
    /// Hop-level analysis: each hop analyzed individually (hop exits treated as boundaries)
    Hop,
}

/// Trait for lattice values in dataflow analysis
pub trait Lattice: Clone + Eq + Debug {
    /// Bottom element of the lattice
    fn bottom() -> Option<Self>;

    /// Top element of the lattice (for some analyses)
    fn top() -> Option<Self>;

    /// Meet operation (∧)
    fn meet(&self, other: &Self) -> Self;

    /// Join operation (∨)
    fn join(&self, other: &Self) -> Self;

    /// Check if this value is less than or equal to another in the lattice order
    fn less_equal(&self, other: &Self) -> bool {
        self.meet(other) == *self
    }
}

/// Trait for transfer functions
pub trait TransferFunction<L: Lattice> {
    /// Apply transfer function for a statement
    fn transfer_statement(&self, stmt: &Statement, state: &L) -> L;

    /// Apply transfer function for a control flow edge
    fn transfer_edge(&self, edge: &ControlFlowEdge, state: &L) -> L;

    /// Get initial value for entry/exit of function
    fn initial_value(&self) -> L;

    /// Get boundary value for function parameters (for forward analysis)
    /// or return statements (for backward analysis)
    fn boundary_value(&self) -> L;
}

/// General monotone dataflow analysis framework
pub struct DataflowAnalysis<L: Lattice, T: TransferFunction<L>> {
    pub direction: Direction,
    pub level: AnalysisLevel,
    pub transfer: T,
    _phantom: std::marker::PhantomData<L>,
}

/// Location of a statement inside the CFG.
/// (We avoid adding an explicit `StatementId` to `cfg` by keeping it local.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtLoc {
    /// Basic block that owns the statement
    pub block: BasicBlockId,
    /// Zero-based index inside `BasicBlock::statements`
    pub index: usize,
}

/// Results of dataflow analysis
pub struct DataflowResults<L: Lattice> {
    /// Value immediately before the first statement of the block
    pub block_entry: HashMap<BasicBlockId, L>,
    /// Value immediately after the last statement of the block and after its
    /// terminator edge(s)
    pub block_exit: HashMap<BasicBlockId, L>,
    /// Value just *before* executing a statement
    pub stmt_entry: HashMap<StmtLoc, L>,
    /// Value just *after* executing a statement
    pub stmt_exit: HashMap<StmtLoc, L>,
}

/// Powerset lattice based on `HashSet<T>`.
#[derive(Clone, Debug)]
pub struct SetLattice<T: Eq + Hash + Clone + Debug> {
    set: HashSet<T>, // meaningless when `is_top == true`
    is_top: bool,
}
