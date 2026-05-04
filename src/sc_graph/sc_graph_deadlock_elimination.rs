// src/sc_graph/sc_graph_deadlock_elimination.rs

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};

use super::{EdgeType, SCGraph, SCGraphNodeId};
use crate::cfg::{FunctionId, HopId};

/// The final, combined SC-Graph after eliminating deadlock-prone SC-cycles.
/// - Vertices are acyclical under S-edges after merging SCCs over S-edges (post C-edge contraction).
/// - Each final vertex corresponds to a single transaction instance (function_id + instance),
///   containing all hop_ids from that transaction that were in the same merged SCC.
/// - Directed S-edges connect these vertices; Undirected C-edges connect all vertices that
///   were part of the same merged SCC (pairwise).
#[derive(Debug, Clone)]
pub struct CombinedSCGraph {
    /// The final set of vertices (each typically corresponds to exactly one CombinedPiece).
    pub vertices: Vec<CombinedVertex>,
    /// Edges between combined vertices. S-edges are directed; C-edges are undirected
    /// (we store them once with source <= target).
    pub edges: Vec<CombinedEdge>,
    /// Mapping from original nodes to the final combined vertex ID they belong to.
    pub original_to_combined: HashMap<SCGraphNodeId, CombinedVertexId>,
}

/// Statistics for deadlock elimination on an SC-graph.
///
/// Cycle counting is defined as follows:
/// - Project the SC-graph to an undirected graph.
/// - Use a deterministic DFS spanning forest.
/// - Count each fundamental cycle induced by a non-tree edge when that cycle contains at
///   least one S-edge and at least one C-edge.
///
/// This avoids duplicated counting because every non-tree edge generates at most one
/// fundamental cycle in the chosen spanning forest.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct DeadlockEliminationStats {
    /// Number of combined vertices whose hop set contains more than one hop.
    pub merged_node_count: usize,
    /// Total number of hops contained in merged vertices.
    pub merged_hop_count: usize,
    /// Average number of hops per merged vertex.
    pub average_merged_node_size: f64,
    /// Number of SC-cycles before deadlock elimination.
    pub sc_cycle_count: usize,
    /// Number of SC-cycles after deadlock elimination.
    pub merged_sc_cycle_count: usize,
}

/// Identifier for a combined vertex in the final graph.
pub type CombinedVertexId = usize;

/// A vertex in the final combined graph.
/// Note: After splitting by transaction, each vertex will contain exactly one CombinedPiece,
/// but we keep it as a Vec for extensibility.
#[derive(Debug, Clone)]
pub struct CombinedVertex {
    pub id: CombinedVertexId,
    /// Pieces in this vertex grouped by (function_id, instance).
    /// After splitting, this will contain exactly one entry.
    pub pieces: Vec<CombinedPiece>,
}

/// A "piece" in a combined vertex, corresponding to a transaction instance
/// and the set of its hop_ids that ended up in this vertex.
#[derive(Debug, Clone)]
pub struct CombinedPiece {
    pub function_id: FunctionId,
    pub instance: u32,
    /// All hop_ids (from that function_id+instance) that merged into this vertex.
    pub hop_ids: BTreeSet<HopId>,
}

/// An edge in the final combined graph.
/// - For S edges (directed), source -> target.
/// - For C edges (undirected), we store canonical source <= target.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CombinedEdge {
    pub source: CombinedVertexId,
    pub target: CombinedVertexId,
    pub edge_type: EdgeType,
}

impl CombinedSCGraph {
    /// Sanity check that this combined graph is acyclic under S-edges.
    pub fn is_acyclic(&self) -> bool {
        is_acyclic_dag(self.vertices.len(), &self.edges)
    }
}

/// Collect deadlock-elimination statistics for an SC-graph and its combined graph.
pub fn collect_deadlock_elimination_stats(
    scg: &SCGraph,
    combined: &CombinedSCGraph,
) -> DeadlockEliminationStats {
    let mut merged_node_count = 0usize;
    let mut merged_hop_count = 0usize;

    for vertex in &combined.vertices {
        let vertex_hop_count: usize = vertex.pieces.iter().map(|piece| piece.hop_ids.len()).sum();
        if vertex_hop_count > 1 {
            merged_node_count += 1;
            merged_hop_count += vertex_hop_count;
        }
    }

    let average_merged_node_size = if merged_node_count == 0 {
        0.0
    } else {
        merged_hop_count as f64 / merged_node_count as f64
    };

    DeadlockEliminationStats {
        merged_node_count,
        merged_hop_count,
        average_merged_node_size,
        sc_cycle_count: count_sc_cycles_before_merging(scg),
        merged_sc_cycle_count: count_sc_cycles_in_combined_graph(combined),
    }
}

/// Main entry: eliminate deadlock-prone SC-cycles using the described steps:
/// 1) Ensure S-edges are directed (already given by the SCGraphEdge).
/// 2) Contract all nodes connected by C-edges (undirected) into single vertices.
/// 3) On the graph of those vertices with S-edges only, contract strongly connected components (SCCs) into single vertices until acyclic.
/// 4) Split each merged SCC vertex into multiple per-transaction vertices (function_id + instance).
///    IMPORTANT: Only preserve the original C-edges from the input graph. Do NOT create new C-edges
///    between vertices just because they ended up in the same SCC.
///    (C-edges between nodes merged into the same combined vertex become internal and are omitted.)
pub fn combine_for_deadlock_elimination(scg: &SCGraph) -> CombinedSCGraph {
    // Step 2: Contract all nodes connected by C-edges into single vertices (C-components).
    let (ccomp_id_of, _ccomp_nodes, ccomp_count) = contract_c_edges(scg);

    // Step 1 & 2 result: the graph is a directed graph whose edges are the original S-edges,
    // with vertices being C-components. Now build that directed graph:
    let sgraph = build_component_sgraph(scg, &ccomp_id_of, ccomp_count);

    // Step 3: Merge all vertices involved in the same directed cycle into one until acyclic.
    // This is exactly computing the SCCs of the component S-graph, and contracting them.
    let (scc_id_of_ccomp, scc_sizes) = scc_kosaraju(&sgraph);
    let scc_count = scc_sizes.len();

    // Build per-SCC grouping of original nodes by transaction (function_id + instance).
    // scc_groups[scc_id][(function_id, instance)] -> Vec<SCGraphNodeId>
    let mut scc_groups: Vec<BTreeMap<(FunctionId, u32), Vec<SCGraphNodeId>>> =
        vec![BTreeMap::new(); scc_count];

    for node in &scg.nodes {
        let ccomp_id = ccomp_id_of[node];
        let scc_id = scc_id_of_ccomp[&ccomp_id];
        scc_groups[scc_id]
            .entry((node.function_id, node.instance))
            .or_default()
            .push(*node);
    }

    // Step 4: Split each SCC into per-transaction vertices and record mapping.
    let mut vertices: Vec<CombinedVertex> = Vec::new();
    let mut original_to_combined: HashMap<SCGraphNodeId, CombinedVertexId> = HashMap::new();
    // Track which final vertices belong to each SCC, so we can add undirected C-edges among them.
    let mut scc_vertex_ids: Vec<Vec<CombinedVertexId>> = vec![Vec::new(); scc_count];

    for scc_id in 0..scc_count {
        for (&(function_id, instance), nodes) in &scc_groups[scc_id] {
            // Aggregate hop_ids for this transaction instance within this SCC.
            let hop_ids: BTreeSet<HopId> = nodes.iter().map(|n| n.hop_id).collect();

            let id = vertices.len();
            // Map all original nodes in this group to this final per-transaction vertex.
            for n in nodes {
                original_to_combined.insert(*n, id);
            }

            vertices.push(CombinedVertex {
                id,
                pieces: vec![CombinedPiece {
                    function_id,
                    instance,
                    hop_ids,
                }],
            });

            scc_vertex_ids[scc_id].push(id);
        }
    }

    // Build edges:
    // - S-edges: induced by original S-edges between new per-transaction vertices.
    // - C-edges: add undirected edges among all per-transaction vertices that came from the same SCC.
    let mut edge_set: HashSet<CombinedEdge> = HashSet::new();

    // Add S-edges (directed).
    for e in &scg.edges {
        if let EdgeType::S = e.edge_type {
            if let (Some(&from_id), Some(&to_id)) = (
                original_to_combined.get(&e.source),
                original_to_combined.get(&e.target),
            ) {
                if from_id != to_id {
                    edge_set.insert(CombinedEdge {
                        source: from_id,
                        target: to_id,
                        edge_type: EdgeType::S,
                    });
                }
            }
        }
    }

    // Add C-edges: preserve only the original C-edges from the input graph.
    // We should NOT create new C-edges between vertices just because they're in the same SCC.
    // The SCC merging is for eliminating deadlocks in S-edges, not for creating new C-edges.
    for e in &scg.edges {
        if let EdgeType::C = e.edge_type {
            if let (Some(&from_id), Some(&to_id)) = (
                original_to_combined.get(&e.source),
                original_to_combined.get(&e.target),
            ) {
                if from_id != to_id {
                    // Ensure canonical ordering for undirected C-edges
                    let (a, b) = if from_id < to_id {
                        (from_id, to_id)
                    } else {
                        (to_id, from_id)
                    };
                    edge_set.insert(CombinedEdge {
                        source: a,
                        target: b,
                        edge_type: EdgeType::C,
                    });
                }
            }
        }
    }

    // Materialize edges with a stable order: (edge_type, source, target).
    let mut edges: Vec<CombinedEdge> = edge_set.into_iter().collect();
    edges.sort_by_key(|e| {
        let et = match e.edge_type {
            EdgeType::C => 0usize,
            EdgeType::S => 1usize,
        };
        (et, e.source, e.target)
    });

    CombinedSCGraph {
        vertices,
        edges,
        original_to_combined,
    }
}

/// Build undirected connected components using only C-edges.
/// Returns:
/// - mapping: SCGraphNodeId -> ccomp_id
/// - ccomp_nodes: Vec of components, each component is Vec<SCGraphNodeId>
/// - ccomp_count
fn contract_c_edges(
    scg: &SCGraph,
) -> (
    HashMap<SCGraphNodeId, usize>,
    Vec<Vec<SCGraphNodeId>>,
    usize,
) {
    // Build undirected adjacency using only C-edges.
    let mut adj: HashMap<SCGraphNodeId, Vec<SCGraphNodeId>> = HashMap::new();
    for n in &scg.nodes {
        adj.entry(*n).or_default();
    }
    for e in &scg.edges {
        if let EdgeType::C = e.edge_type {
            adj.entry(e.source).or_default().push(e.target);
            adj.entry(e.target).or_default().push(e.source);
        }
    }

    // BFS/DFS to find connected components.
    let mut ccomp_id_of: HashMap<SCGraphNodeId, usize> = HashMap::new();
    let mut ccomp_nodes: Vec<Vec<SCGraphNodeId>> = Vec::new();

    let mut visited: HashSet<SCGraphNodeId> = HashSet::new();
    for &n in adj.keys() {
        if visited.contains(&n) {
            continue;
        }
        let mut queue = VecDeque::new();
        queue.push_back(n);
        visited.insert(n);
        let mut comp: Vec<SCGraphNodeId> = Vec::new();

        while let Some(u) = queue.pop_front() {
            comp.push(u);
            if let Some(neis) = adj.get(&u) {
                for &v in neis {
                    if !visited.contains(&v) {
                        visited.insert(v);
                        queue.push_back(v);
                    }
                }
            }
        }

        let id = ccomp_nodes.len();
        for node in &comp {
            ccomp_id_of.insert(*node, id);
        }
        ccomp_nodes.push(comp);
    }

    let ccomp_count = ccomp_nodes.len();

    (ccomp_id_of, ccomp_nodes, ccomp_count)
}

/// Build the directed graph among C-components using only S-edges.
/// Returns adjacency list representation: Vec<Vec<usize>> where indices are ccomp IDs.
fn build_component_sgraph(
    scg: &SCGraph,
    ccomp_id_of: &HashMap<SCGraphNodeId, usize>,
    ccomp_count: usize,
) -> Vec<Vec<usize>> {
    let mut adj: Vec<HashSet<usize>> = vec![HashSet::new(); ccomp_count];
    for e in &scg.edges {
        if let EdgeType::S = e.edge_type {
            if let (Some(&u), Some(&v)) = (ccomp_id_of.get(&e.source), ccomp_id_of.get(&e.target)) {
                if u != v {
                    adj[u].insert(v);
                }
            }
        }
    }
    adj.into_iter().map(|s| s.into_iter().collect()).collect()
}

/// Compute SCCs using Kosaraju's algorithm.
/// Input: adjacency list for 0..n-1
/// Returns (mapping from node -> scc_id, scc_count).
fn scc_kosaraju(adj: &Vec<Vec<usize>>) -> (HashMap<usize, usize>, Vec<usize>) {
    let n = adj.len();

    // 1) Order by finish time in DFS on original graph.
    let mut visited = vec![false; n];
    let mut order: Vec<usize> = Vec::with_capacity(n);

    fn dfs1(u: usize, adj: &Vec<Vec<usize>>, visited: &mut [bool], order: &mut Vec<usize>) {
        visited[u] = true;
        for &v in &adj[u] {
            if !visited[v] {
                dfs1(v, adj, visited, order);
            }
        }
        order.push(u);
    }

    for u in 0..n {
        if !visited[u] {
            dfs1(u, adj, &mut visited, &mut order);
        }
    }

    // 2) Build reverse graph.
    let mut radj: Vec<Vec<usize>> = vec![Vec::new(); n];
    for u in 0..n {
        for &v in &adj[u] {
            radj[v].push(u);
        }
    }

    // 3) DFS on reversed graph in reverse finish order to assign SCC IDs.
    let mut comp_id: HashMap<usize, usize> = HashMap::with_capacity(n);
    let mut current_id = 0usize;
    visited.fill(false);

    fn dfs2(
        u: usize,
        radj: &Vec<Vec<usize>>,
        visited: &mut [bool],
        comp_id: &mut HashMap<usize, usize>,
        id: usize,
    ) {
        visited[u] = true;
        comp_id.insert(u, id);
        for &v in &radj[u] {
            if !visited[v] {
                dfs2(v, radj, visited, comp_id, id);
            }
        }
    }

    while let Some(u) = order.pop() {
        if !visited[u] {
            dfs2(u, &radj, &mut visited, &mut comp_id, current_id);
            current_id += 1;
        }
    }

    let mut comp_sizes = vec![0usize; current_id];
    for &id in comp_id.values() {
        comp_sizes[id] += 1;
    }

    (comp_id, comp_sizes)
}

/// Count SC-cycles in the original graph using an undirected DFS spanning forest.
pub fn count_sc_cycles_before_merging(scg: &SCGraph) -> usize {
    let mut nodes: Vec<SCGraphNodeId> = scg.nodes.iter().copied().collect();
    nodes.sort();

    let node_to_index: HashMap<SCGraphNodeId, usize> = nodes
        .iter()
        .enumerate()
        .map(|(index, node)| (*node, index))
        .collect();

    let adjacency = build_labeled_undirected_adjacency(
        nodes.len(),
        scg.edges.iter().filter_map(|edge| {
            let source = node_to_index.get(&edge.source).copied()?;
            let target = node_to_index.get(&edge.target).copied()?;
            Some((source, target, edge.edge_type.clone()))
        }),
    );

    count_mixed_cycles_in_undirected_graph(&adjacency)
}

/// Count SC-cycles in the merged graph using an undirected DFS spanning forest.
pub fn count_sc_cycles_in_combined_graph(combined: &CombinedSCGraph) -> usize {
    let adjacency = build_labeled_undirected_adjacency(
        combined.vertices.len(),
        combined
            .edges
            .iter()
            .map(|edge| (edge.source, edge.target, edge.edge_type.clone())),
    );

    count_mixed_cycles_in_undirected_graph(&adjacency)
}

const EDGE_KIND_S: u8 = 1;
const EDGE_KIND_C: u8 = 2;

fn build_labeled_undirected_adjacency<I>(vertex_count: usize, edges: I) -> Vec<Vec<(usize, u8)>>
where
    I: IntoIterator<Item = (usize, usize, EdgeType)>,
{
    let mut edge_map: HashMap<(usize, usize), u8> = HashMap::new();

    for (source, target, edge_type) in edges {
        if source == target {
            continue;
        }

        let (a, b) = if source < target {
            (source, target)
        } else {
            (target, source)
        };
        let kind = match edge_type {
            EdgeType::S => EDGE_KIND_S,
            EdgeType::C => EDGE_KIND_C,
        };
        edge_map
            .entry((a, b))
            .and_modify(|mask| *mask |= kind)
            .or_insert(kind);
    }

    let mut adjacency = vec![Vec::new(); vertex_count];
    for ((a, b), mask) in edge_map {
        adjacency[a].push((b, mask));
        adjacency[b].push((a, mask));
    }

    for neighbors in &mut adjacency {
        neighbors.sort_by_key(|(neighbor, _)| *neighbor);
    }

    adjacency
}

fn count_mixed_cycles_in_undirected_graph(graph: &[Vec<(usize, u8)>]) -> usize {
    let vertex_count = graph.len();
    if vertex_count == 0 {
        return 0;
    }

    let mut pair_has_both = HashSet::new();
    for (u, neighbors) in graph.iter().enumerate() {
        for &(v, mask) in neighbors {
            if u < v && mask == (EDGE_KIND_S | EDGE_KIND_C) {
                pair_has_both.insert((u, v));
            }
        }
    }

    let mut total = pair_has_both.len();
    let mut visited = vec![false; vertex_count];
    let mut path: Vec<usize> = Vec::new();
    let mut path_key_set: HashSet<Vec<usize>> = HashSet::new();

    for start in 0..vertex_count {
        visited.fill(false);
        path.clear();
        visited[start] = true;
        path.push(start);
        enumerate_cycles_from_start(
            start,
            start,
            graph,
            &mut visited,
            &mut path,
            0,
            0,
            &mut path_key_set,
            &mut total,
        );
    }

    total
}

fn enumerate_cycles_from_start(
    start: usize,
    current: usize,
    graph: &[Vec<(usize, u8)>],
    visited: &mut [bool],
    path: &mut Vec<usize>,
    s_count: usize,
    c_count: usize,
    path_key_set: &mut HashSet<Vec<usize>>,
    total: &mut usize,
) {
    for &(neighbor, kind_mask) in &graph[current] {
        if neighbor == start {
            if path.len() >= 3 {
                let next_s_count = s_count + usize::from(kind_mask & EDGE_KIND_S != 0);
                let next_c_count = c_count + usize::from(kind_mask & EDGE_KIND_C != 0);
                if next_s_count > 0 && next_c_count > 0 {
                    let canonical = canonicalize_cycle(path);
                    if path_key_set.insert(canonical) {
                        *total += 1;
                    }
                }
            }
            continue;
        }

        if neighbor <= start || visited[neighbor] {
            continue;
        }

        visited[neighbor] = true;
        path.push(neighbor);
        enumerate_cycles_from_start(
            start,
            neighbor,
            graph,
            visited,
            path,
            s_count + usize::from(kind_mask & EDGE_KIND_S != 0),
            c_count + usize::from(kind_mask & EDGE_KIND_C != 0),
            path_key_set,
            total,
        );
        path.pop();
        visited[neighbor] = false;
    }
}

fn canonicalize_cycle(path: &[usize]) -> Vec<usize> {
    let forward = path.to_vec();
    let mut reverse = path.to_vec();
    reverse[1..].reverse();
    if reverse < forward {
        reverse
    } else {
        forward
    }
}

/// Check acyclicity under S-edges of a directed graph given number of vertices and edges.
/// C-edges are undirected and are ignored for acyclicity.
fn is_acyclic_dag(vertex_count: usize, edges: &Vec<CombinedEdge>) -> bool {
    // Kahn's algorithm using only S-edges
    let mut indeg = vec![0usize; vertex_count];
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); vertex_count];
    for e in edges {
        if let EdgeType::S = e.edge_type {
            adj[e.source].push(e.target);
            indeg[e.target] += 1;
        }
    }

    let mut q: VecDeque<usize> = (0..vertex_count).filter(|&i| indeg[i] == 0).collect();
    let mut seen = 0usize;

    while let Some(u) = q.pop_front() {
        seen += 1;
        for &v in &adj[u] {
            indeg[v] -= 1;
            if indeg[v] == 0 {
                q.push_back(v);
            }
        }
    }

    seen == vertex_count
}
