use crate::cfg::{CfgProgram, FunctionId as CfgFunctionId, HopId as CfgHopId, NodeId as CfgNodeId};
use id_arena::{Arena, Id};
use std::collections::{HashMap, HashSet};

/// Represents an edge type in the SC-Graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EdgeType {
    /// Sequential edge, representing program order within a function.
    S,
    /// Conflict edge, representing a potential conflict between hops on the same node
    /// from different functions.
    C,
}

/// Represents a node in the SC-Graph, which corresponds to a Hop in the CFG.
#[derive(Debug, Clone)]
pub struct SCGraphNode {
    /// The ID of the corresponding Hop in the CFG.
    pub cfg_hop_id: CfgHopId,
    /// The ID of the CFG Function this hop belongs to.
    pub cfg_function_id: CfgFunctionId,
    /// The ID of the CFG Node (e.g., server, client) this hop executes on.
    pub cfg_node_id: CfgNodeId,
}

pub type SCGraphNodeId = Id<SCGraphNode>;

/// Represents an edge in the SC-Graph.
/// Edges are between SCGraphNodes.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Edge {
    /// The source SCGraphNodeId.
    pub source: SCGraphNodeId,
    /// The target SCGraphNodeId.
    pub target: SCGraphNodeId,
    /// The type of the edge (S or C).
    pub edge_type: EdgeType,
}

impl Edge {
    pub fn new(source: SCGraphNodeId, target: SCGraphNodeId, edge_type: EdgeType) -> Self {
        Self {
            source,
            target,
            edge_type,
        }
    }
}

/// The Serializability Conflict Graph.
#[derive(Debug)]
pub struct SCGraph {
    /// Arena storing all nodes (hops) in the SC-Graph.
    pub nodes: Arena<SCGraphNode>,
    /// List of all edges in the SC-Graph.
    pub edges: Vec<Edge>,
    /// Mapping from CFG HopId to SCGraphNodeId, used during construction and for lookups.
    cfg_hop_to_sc_node: HashMap<CfgHopId, SCGraphNodeId>,
}

impl SCGraph {
    /// Creates a new SC-Graph from a given CFG program.
    pub fn new(cfg_program: &CfgProgram) -> Self {
        let mut nodes_arena = Arena::new(); // Renamed to avoid conflict
        let mut edges = Vec::new();
        let mut cfg_hop_to_sc_node_map = HashMap::new(); // Renamed to avoid conflict

        // 1. Create SCGraphNodes from CFG Hops
        for (cfg_func_id, cfg_function) in cfg_program.functions.iter() {
            for (cfg_hop_id, cfg_hop) in cfg_function.hops.iter() {
                let sc_node_data = SCGraphNode {
                    cfg_hop_id,
                    cfg_function_id: cfg_func_id,
                    cfg_node_id: cfg_hop.node_id,
                };
                let sc_node_id = nodes_arena.alloc(sc_node_data);
                cfg_hop_to_sc_node_map.insert(cfg_hop_id, sc_node_id);
            }
        }

        // 2. Add S-edges (Sequential edges within each function's hops)
        for (_cfg_func_id, cfg_function) in cfg_program.functions.iter() {
            let mut prev_cfg_hop_id: Option<CfgHopId> = None;
            for &current_cfg_hop_id in &cfg_function.hop_order {
                if let Some(prev_id) = prev_cfg_hop_id {
                    let source_sc_node_id = cfg_hop_to_sc_node_map[&prev_id];
                    let target_sc_node_id = cfg_hop_to_sc_node_map[&current_cfg_hop_id];
                    edges.push(Edge::new(source_sc_node_id, target_sc_node_id, EdgeType::S));
                }
                prev_cfg_hop_id = Some(current_cfg_hop_id);
            }
        }

        // 3. Add C-edges (Conflict edges between hops on the same CFG node but from different functions)
        let mut hop_ids_on_cfg_node: HashMap<CfgNodeId, Vec<CfgHopId>> = HashMap::new();
        for (_cfg_func_id, cfg_function) in cfg_program.functions.iter() {
            for (cfg_hop_id, cfg_hop) in cfg_function.hops.iter() {
                hop_ids_on_cfg_node
                    .entry(cfg_hop.node_id)
                    .or_default()
                    .push(cfg_hop_id);
            }
        }

        for (_cfg_node_id, hop_ids) in hop_ids_on_cfg_node {
            for i in 0..hop_ids.len() {
                for j in (i + 1)..hop_ids.len() {
                    let cfg_hop1_id = hop_ids[i];
                    let cfg_hop2_id = hop_ids[j];

                    let sc_node1_id = cfg_hop_to_sc_node_map[&cfg_hop1_id];
                    let sc_node2_id = cfg_hop_to_sc_node_map[&cfg_hop2_id];

                    // Check if hops are from different functions by looking at SCGraphNode properties
                    if nodes_arena[sc_node1_id].cfg_function_id
                        != nodes_arena[sc_node2_id].cfg_function_id
                    {
                        edges.push(Edge::new(sc_node1_id, sc_node2_id, EdgeType::C));
                        // Optionally, add the reverse edge if C-edges are strictly undirected
                        // edges.push(Edge::new(sc_node2_id, sc_node1_id, EdgeType::C));
                    }
                }
            }
        }

        SCGraph {
            nodes: nodes_arena,
            edges,
            cfg_hop_to_sc_node: cfg_hop_to_sc_node_map,
        }
    }

    /// Finds unique simple cycles in the SC-Graph that contain at least one S-edge and one C-edge.
    /// A cycle is represented by a list of CFG HopIds.
    pub fn find_mixed_cycles(&self) -> Vec<Vec<CfgHopId>> {
        let mut mixed_cycles = Vec::new();
        let mut visited_paths_for_cycle_detection: HashSet<Vec<CfgHopId>> = HashSet::new();

        for (start_sc_node_id, _start_node_data) in self.nodes.iter() {
            let mut path: Vec<SCGraphNodeId> = vec![start_sc_node_id];
            let mut visited_in_dfs: HashSet<SCGraphNodeId> = HashSet::new();
            visited_in_dfs.insert(start_sc_node_id);

            self.dfs_find_cycles(
                start_sc_node_id, // current_sc_node_id
                start_sc_node_id, // start_sc_node_id_for_cycle
                &mut path,
                &mut visited_in_dfs,
                &mut mixed_cycles,
                &mut visited_paths_for_cycle_detection,
                0, // s_edges_count
                0, // c_edges_count
            );
        }
        mixed_cycles
    }

    fn dfs_find_cycles(
        &self,
        current_sc_node_id: SCGraphNodeId,
        start_sc_node_id_for_cycle: SCGraphNodeId,
        path: &mut Vec<SCGraphNodeId>, // Path of SCGraphNodeIds
        visited_in_dfs: &mut HashSet<SCGraphNodeId>,
        mixed_cycles: &mut Vec<Vec<CfgHopId>>,
        visited_paths_for_cycle_detection: &mut HashSet<Vec<CfgHopId>>,
        s_edges_count: usize,
        c_edges_count: usize,
    ) {
        for edge in &self.edges {
            if edge.source == current_sc_node_id {
                let neighbor_sc_node_id = edge.target;
                let mut next_s_edges_count = s_edges_count;
                let mut next_c_edges_count = c_edges_count;

                match edge.edge_type {
                    EdgeType::S => next_s_edges_count += 1,
                    EdgeType::C => next_c_edges_count += 1,
                }

                if neighbor_sc_node_id == start_sc_node_id_for_cycle {
                    // Found a cycle
                    if path.len() >= 2 && next_s_edges_count > 0 && next_c_edges_count > 0 {
                        // Convert path of SCGraphNodeId to Vec<CfgHopId>
                        let cycle_cfg_hops: Vec<CfgHopId> = path
                            .iter()
                            .map(|&sc_id| self.nodes[sc_id].cfg_hop_id)
                            .collect();

                        let canonical_cycle = self.canonicalize_cycle(&cycle_cfg_hops);
                        if visited_paths_for_cycle_detection.insert(canonical_cycle.clone()) {
                            mixed_cycles.push(canonical_cycle);
                        }
                    }
                } else if !visited_in_dfs.contains(&neighbor_sc_node_id) {
                    path.push(neighbor_sc_node_id);
                    visited_in_dfs.insert(neighbor_sc_node_id);

                    self.dfs_find_cycles(
                        neighbor_sc_node_id,
                        start_sc_node_id_for_cycle,
                        path,
                        visited_in_dfs,
                        mixed_cycles,
                        visited_paths_for_cycle_detection,
                        next_s_edges_count,
                        next_c_edges_count,
                    );

                    visited_in_dfs.remove(&neighbor_sc_node_id);
                    path.pop();
                }
            }
        }
    }

    /// Helper to normalize a cycle representation (of CfgHopIds) to its canonical form.
    fn canonicalize_cycle(&self, cycle_cfg_hops: &[CfgHopId]) -> Vec<CfgHopId> {
        if cycle_cfg_hops.is_empty() {
            return Vec::new();
        }
        // Find the CfgHopId with the smallest index value to start the canonical form
        let min_hop_id_val = cycle_cfg_hops.iter().min_by_key(|h| h.index()).unwrap();
        let min_pos = cycle_cfg_hops
            .iter()
            .position(|&h| h == *min_hop_id_val)
            .unwrap();

        let mut canonical = Vec::with_capacity(cycle_cfg_hops.len());
        for i in 0..cycle_cfg_hops.len() {
            canonical.push(cycle_cfg_hops[(min_pos + i) % cycle_cfg_hops.len()]);
        }
        canonical
    }

    /// Returns the number of nodes (hops), S-edges, and C-edges in the graph.
    pub fn stats(&self) -> (usize, usize, usize) {
        let s_edges = self
            .edges
            .iter()
            .filter(|e| e.edge_type == EdgeType::S)
            .count();
        let c_edges = self
            .edges
            .iter()
            .filter(|e| e.edge_type == EdgeType::C)
            .count();
        (self.nodes.len(), s_edges, c_edges)
    }

    /// Get the SCGraphNodeId for a given CfgHopId.
    pub fn get_sc_node_id(&self, cfg_hop_id: CfgHopId) -> Option<SCGraphNodeId> {
        self.cfg_hop_to_sc_node.get(&cfg_hop_id).cloned()
    }
}
