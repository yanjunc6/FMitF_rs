// src/sc_graph/sc_graph_deadlock_elimination.rs

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};

use super::{EdgeType, SCGraph, SCGraphNodeId};
use crate::cfg::{FunctionId, HopId};

/// The final, combined SC-Graph after eliminating deadlock-prone SC-cycles.
/// - Vertices are acyclical after merging SCCs over S-edges (post C-edge contraction).
/// - Each vertex contains "pieces" that are grouped by (function_id, instance).
/// - Edges are directed and are induced only by S-edges between the final vertices.
#[derive(Debug, Clone)]
pub struct CombinedSCGraph {
    /// The final, acyclic set of vertices.
    pub vertices: Vec<CombinedVertex>,
    /// Directed edges between combined vertices (acyclic).
    pub edges: Vec<CombinedEdge>,
    /// Mapping from original nodes to the final combined vertex ID they belong to.
    pub original_to_combined: HashMap<SCGraphNodeId, CombinedVertexId>,
}

/// Identifier for a combined vertex in the final graph.
pub type CombinedVertexId = usize;

/// A vertex in the final combined graph.
#[derive(Debug, Clone)]
pub struct CombinedVertex {
    pub id: CombinedVertexId,
    /// Pieces in this vertex grouped by (function_id, instance).
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

/// A directed edge in the final combined graph.
#[derive(Debug, Clone)]
pub struct CombinedEdge {
    pub source: CombinedVertexId,
    pub target: CombinedVertexId,
}

impl CombinedSCGraph {
    /// Sanity check that this combined graph is acyclic.
    pub fn is_acyclic(&self) -> bool {
        is_acyclic_dag(self.vertices.len(), &self.edges)
    }
}

/// Main entry: eliminate deadlock-prone SC-cycles using the described steps:
/// 1) Ensure S-edges are directed (already given by the SCGraphEdge).
/// 2) Contract all nodes connected by C-edges (undirected) into single vertices.
/// 3) On the graph of those vertices with S-edges only, contract strongly connected components (SCCs) into single vertices until acyclic.
/// 4) Within each merged vertex, combine pieces that belong to the same transaction (same function_id + instance).
pub fn combine_for_deadlock_elimination(scg: &SCGraph) -> CombinedSCGraph {
    // Step 2: Contract all nodes connected by C-edges into single vertices (C-components).
    let (ccomp_id_of, _ccomp_nodes, ccomp_count) = contract_c_edges(scg);

    // Step 1 & 2 result: the graph is a directed graph whose edges are the original S-edges,
    // with vertices being C-components. Now build that directed graph:
    let sgraph = build_component_sgraph(scg, &ccomp_id_of, ccomp_count);

    // Step 3: Merge all vertices involved in the same directed cycle into one until acyclic.
    // This is exactly computing the SCCs of the component S-graph, and contracting them.
    let (scc_id_of_ccomp, scc_count) = scc_kosaraju(&sgraph);

    // Build final vertices: gather original nodes per final SCC.
    let mut final_vertex_to_nodes: Vec<Vec<SCGraphNodeId>> = vec![Vec::new(); scc_count];
    for (node, &ccomp_id) in ccomp_id_of.iter() {
        let final_id = scc_id_of_ccomp[&ccomp_id];
        final_vertex_to_nodes[final_id].push(*node);
    }

    // Step 4: Within each merged vertex, combine pieces for the same transaction (function_id + instance).
    let mut vertices: Vec<CombinedVertex> = Vec::with_capacity(scc_count);
    let mut original_to_combined: HashMap<SCGraphNodeId, CombinedVertexId> = HashMap::new();

    for (final_id, nodes) in final_vertex_to_nodes.into_iter().enumerate() {
        let mut by_tx: BTreeMap<(FunctionId, u32), BTreeSet<HopId>> = BTreeMap::new();
        for n in &nodes {
            by_tx
                .entry((n.function_id, n.instance))
                .or_default()
                .insert(n.hop_id);
            original_to_combined.insert(*n, final_id);
        }
        let pieces = by_tx
            .into_iter()
            .map(|((function_id, instance), hop_ids)| CombinedPiece {
                function_id,
                instance,
                hop_ids,
            })
            .collect();

        vertices.push(CombinedVertex {
            id: final_id,
            pieces,
        });
    }

    // Build edges among final vertices: induced by original S-edges between SCCs.
    let mut edge_set: HashSet<(CombinedVertexId, CombinedVertexId)> = HashSet::new();
    for e in &scg.edges {
        if let EdgeType::S = e.edge_type {
            if let (Some(&from_cc), Some(&to_cc)) =
                (ccomp_id_of.get(&e.source), ccomp_id_of.get(&e.target))
            {
                let from_final = scc_id_of_ccomp[&from_cc];
                let to_final = scc_id_of_ccomp[&to_cc];
                if from_final != to_final {
                    edge_set.insert((from_final, to_final));
                }
            }
        }
    }

    let mut edges: Vec<CombinedEdge> = edge_set
        .into_iter()
        .map(|(s, t)| CombinedEdge {
            source: s,
            target: t,
        })
        .collect();

    // Optional: sort edges for stable output
    edges.sort_by_key(|e| (e.source, e.target));

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
fn scc_kosaraju(adj: &Vec<Vec<usize>>) -> (HashMap<usize, usize>, usize) {
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

    (comp_id, current_id)
}

/// Check acyclicity of a directed graph given number of vertices and edges.
fn is_acyclic_dag(
    vertex_count: usize,
    edges: &Vec<crate::sc_graph::sc_graph_deadlock_elimination::CombinedEdge>,
) -> bool {
    // Kahn's algorithm
    let mut indeg = vec![0usize; vertex_count];
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); vertex_count];
    for e in edges {
        adj[e.source].push(e.target);
        indeg[e.target] += 1;
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
