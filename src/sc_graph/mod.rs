use crate::cfg::{FunctionId, HopId};
use id_arena::{Arena, Id};
use std::collections::HashMap;

mod sc_graph_builder;
pub use sc_graph_builder::SCGraphBuilder;

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

/// Represents a node in the SC-Graph, which corresponds to a Hop in the CFG.
#[derive(Debug, Clone)]
pub struct SCGraphNode {
    /// The ID of the corresponding Hop in the CFG.
    pub hop_id: HopId,
    /// The ID of the CFG Function (transaction) this hop belongs to.
    pub function_id: FunctionId,
}

pub type SCGraphNodeId = Id<SCGraphNode>;

/// Represents an edge in the SC-Graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SCGraphEdge {
    /// The source SCGraphNodeId.
    pub source: SCGraphNodeId,
    /// The target SCGraphNodeId.
    pub target: SCGraphNodeId,
    /// The type of the edge (S or C).
    pub edge_type: EdgeType,
}

/// The Serializability Conflict Graph.
/// 
/// The SC-Graph contains:
/// - Nodes: Each node represents a hop from the CFG
/// - S-edges (directed): Connect hops that are sequentially executed within the same transaction
/// - C-edges (undirected): Connect hops from different transactions that have conflicting table accesses
///
/// Conflicting table accesses occur when:
/// - One hop reads and another writes the same table, OR
/// - Both hops write to the same table
#[derive(Debug)]
pub struct SCGraph {
    /// Arena storing all nodes (hops) in the SC-Graph.
    pub nodes: Arena<SCGraphNode>,
    /// List of all edges in the SC-Graph.
    pub edges: Vec<SCGraphEdge>,
    /// Mapping from CFG HopId to SCGraphNodeId for efficient lookups.
    hop_to_node: HashMap<HopId, SCGraphNodeId>,
}
