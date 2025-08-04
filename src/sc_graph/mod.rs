use crate::cfg::{FunctionId, HopId};
use std::collections::HashSet;

mod sc_graph_builder;
pub use sc_graph_builder::SCGraphBuilder;

/// Represents a unique node identifier in the SC-Graph.
/// Since HopId is only unique within a function, we need both FunctionId and HopId.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SCGraphNodeId {
    pub function_id: FunctionId,
    pub hop_id: HopId,
}

/// Represents an edge type in the SC-Graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EdgeType {
    /// Sequential edge (directed), representing program order within a transaction.
    /// S-edges connect hops that are sequentially executed within the same transaction.
    S,
    /// Conflict edge (undirected), representing a potential conflict between hops
    /// from different transactions that access the same table with at least one write.
    C,
}

/// Represents an edge in the SC-Graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SCGraphEdge {
    /// The source node.
    pub source: SCGraphNodeId,
    /// The target node.
    pub target: SCGraphNodeId,
    /// The type of the edge (S or C).
    pub edge_type: EdgeType,
}

/// The Serializability Conflict Graph.
///
/// The SC-Graph contains:
/// - Nodes: Each node is represented by a unique (FunctionId, HopId) pair from the CFG
/// - S-edges (directed): Connect hops that are sequentially executed within the same transaction
/// - C-edges (undirected): Connect hops from different transactions that have conflicting table accesses
///
/// Conflicting table accesses occur when:
/// - One hop reads and another writes the same table, OR
/// - Both hops write to the same table
#[derive(Debug)]
pub struct SCGraph {
    /// Set of all node IDs that are nodes in the SC-Graph.
    pub nodes: HashSet<SCGraphNodeId>,
    /// List of all edges in the SC-Graph.
    pub edges: Vec<SCGraphEdge>,
}
