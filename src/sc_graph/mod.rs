use crate::cfg::{FunctionId, HopId};
use std::collections::{BTreeSet, HashMap, HashSet};

mod sc_graph_builder;
pub use sc_graph_builder::SCGraphBuilder;

mod sc_graph_simplify;
pub use sc_graph_simplify::update_scgraph_for_commutative_errors;

mod sc_graph_deadlock_elimination;
pub use sc_graph_deadlock_elimination::{
    combine_for_deadlock_elimination, CombinedEdge, CombinedPiece, CombinedSCGraph, CombinedVertex,
    CombinedVertexId,
};

/// Represents a unique node identifier in the SC-Graph.
/// Since HopId is only unique within a function, we need both FunctionId and HopId.
/// We also include an instance number to handle multiple concurrent instances
/// of the same transaction running with different parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SCGraphNodeId {
    pub function_id: FunctionId,
    pub hop_id: HopId,
    pub instance: u32,
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
#[derive(Debug, Clone)]
pub struct SCGraph {
    /// Set of all node IDs that are nodes in the SC-Graph.
    pub nodes: HashSet<SCGraphNodeId>,
    /// List of all edges in the SC-Graph.
    pub edges: Vec<SCGraphEdge>,
}

impl Default for SCGraph {
    fn default() -> Self {
        Self {
            nodes: HashSet::new(),
            edges: Vec::new(),
        }
    }
}

impl SCGraph {
    /// Determines if a specific hop has any C-edges (conflict edges) in this SC-Graph.
    ///
    /// This is used by codegen to set the `isRecordDep` field correctly:
    /// - If a hop has C-edges AND is not the last hop in a transaction, isRecordDep = true
    /// - Otherwise, isRecordDep = false
    pub fn hop_has_c_edges(&self, function_id: FunctionId, hop_id: HopId) -> bool {
        // Check if any edge involving this hop (across any instance) is a C-edge
        self.edges.iter().any(|edge| {
            matches!(edge.edge_type, EdgeType::C)
                && ((edge.source.function_id == function_id && edge.source.hop_id == hop_id)
                    || (edge.target.function_id == function_id && edge.target.hop_id == hop_id))
        })
    }
}

/// Represents the type of a hop based on whether it's merged with other hops.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HopType {
    /// A normal hop that is not merged with other hops
    NormalHop,
    /// The first hop in a merged sequence
    MergedHopBegin,
    /// A middle hop in a merged sequence (not first, not last)
    MergedHop,
    /// The last hop in a merged sequence
    MergedHopEnd,
}

/// Determines the hop type for each hop based on the combined (merged) SC-graph.
/// 
/// Returns a map from (FunctionId, HopId) to HopType.
/// 
/// For merged hops (those in the same CombinedVertex):
/// - The first hop in program order is MergedHopBegin
/// - The last hop in program order is MergedHopEnd
/// - All middle hops are MergedHop
/// 
/// For non-merged hops: NormalHop
pub fn determine_hop_types(
    combined_graph: &CombinedSCGraph,
) -> HashMap<(FunctionId, HopId), HopType> {
    let mut hop_types = HashMap::new();

    for vertex in &combined_graph.vertices {
        // Each vertex after splitting should have exactly one piece (one transaction instance)
        for piece in &vertex.pieces {
            let function_id = piece.function_id;
            let hop_ids: BTreeSet<HopId> = piece.hop_ids.clone();

            if hop_ids.len() == 1 {
                // Single hop in this vertex - it's a normal hop
                for hop_id in hop_ids {
                    hop_types.insert((function_id, hop_id), HopType::NormalHop);
                }
            } else if hop_ids.len() > 1 {
                // Multiple hops merged together
                // Sort by hop_id index to determine first, middle, and last
                let mut sorted_hops: Vec<HopId> = hop_ids.into_iter().collect();
                sorted_hops.sort_by_key(|h| h.index());

                for (i, hop_id) in sorted_hops.iter().enumerate() {
                    let hop_type = if i == 0 {
                        HopType::MergedHopBegin
                    } else if i == sorted_hops.len() - 1 {
                        HopType::MergedHopEnd
                    } else {
                        HopType::MergedHop
                    };
                    hop_types.insert((function_id, *hop_id), hop_type);
                }
            }
        }
    }

    hop_types
}

/// Calculates the conflicts map for each hop based on the normal SC-graph.
/// 
/// Returns a map from (FunctionId, HopId) to a map of (chain_index -> hop_index).
/// 
/// For each hop, we find all C-edges connecting to hops in other transactions,
/// and record only the FIRST hop in each conflicting transaction chain.
/// 
/// Parameters:
/// - scgraph: The normal (non-merged) SC-graph containing C-edges
/// - program: The CFG program to determine transaction order and hop indices
/// 
/// The chain_index corresponds to the position of the transaction in the Chains() array
/// returned by global.go (i.e., the order in program.all_transactions).
/// 
/// The hop_index is the position of the hop within that transaction's hop list
/// (not the HopId, but the sequential index 0, 1, 2, ...).
pub fn calculate_conflicts(
    scgraph: &SCGraph,
    program: &crate::cfg::Program,
) -> HashMap<(FunctionId, HopId), HashMap<i32, i32>> {
    let mut conflicts: HashMap<(FunctionId, HopId), HashMap<i32, i32>> = HashMap::new();

    // Build a map from function_id to chain_index
    let mut func_to_chain_index: HashMap<FunctionId, i32> = HashMap::new();
    for (idx, &func_id) in program.all_transactions.iter().enumerate() {
        func_to_chain_index.insert(func_id, idx as i32);
    }

    // Build a map from (function_id, hop_id) to hop_index within that transaction
    let mut hop_to_index: HashMap<(FunctionId, HopId), i32> = HashMap::new();
    for &func_id in &program.all_transactions {
        let function = &program.functions[func_id];
        for (hop_idx, &hop_id) in function.hops.iter().enumerate() {
            hop_to_index.insert((func_id, hop_id), hop_idx as i32);
        }
    }

    // For each C-edge, record the conflict in both directions
    for edge in &scgraph.edges {
        if matches!(edge.edge_type, EdgeType::C) {
            let source_node = edge.source;
            let target_node = edge.target;

            // Only process if they're from different transactions
            if source_node.function_id != target_node.function_id {
                // Record conflict from source to target
                if let (Some(&target_chain_idx), Some(&target_hop_idx)) = (
                    func_to_chain_index.get(&target_node.function_id),
                    hop_to_index.get(&(target_node.function_id, target_node.hop_id)),
                ) {
                    let source_key = (source_node.function_id, source_node.hop_id);
                    let conflicts_map = conflicts.entry(source_key).or_insert_with(HashMap::new);
                    
                    // Only record if this chain doesn't already have a conflict recorded
                    // (we want the FIRST conflicting hop in that chain)
                    conflicts_map.entry(target_chain_idx).or_insert(target_hop_idx);
                }

                // Record conflict from target to source
                if let (Some(&source_chain_idx), Some(&source_hop_idx)) = (
                    func_to_chain_index.get(&source_node.function_id),
                    hop_to_index.get(&(source_node.function_id, source_node.hop_id)),
                ) {
                    let target_key = (target_node.function_id, target_node.hop_id);
                    let conflicts_map = conflicts.entry(target_key).or_insert_with(HashMap::new);
                    
                    // Only record if this chain doesn't already have a conflict recorded
                    conflicts_map.entry(source_chain_idx).or_insert(source_hop_idx);
                }
            }
        }
    }

    conflicts
}

