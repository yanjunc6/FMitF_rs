//! Count SC-cycles using petgraph.
//!
//! An **SC-cycle** is a cycle that contains at least one S-edge *and* at least
//! one C-edge.
//!
//! ## Strategy
//!
//! Build a **full undirected graph** (all edges) and a **C-only undirected
//! graph** (only C-edges).  Since S-edges alone are guaranteed to never form
//! a cycle, every cycle in the full graph is either C-only or SC.  Therefore:
//!
//!     SC-cycles = cycles(full) − cycles(C-only)
//!
//! ## Counting modes
//!
//! Two counting modes are provided:
//!
//! - **Fundamental** (fast): Returns the dimension of the SC-subspace of the
//!   cycle space, i.e. the number of *independent* SC-cycles.
//!   Formula: `ν(full) − ν(C-only)` where `ν = |E| − |V| + #CC`.
//!   Complexity: **O(n + e)**
//!
//! - **Simple** (exact, expensive): Enumerates *all* distinct simple cycles by
//!   exhaustive XOR combination of a fundamental cycle basis, then validates
//!   each candidate.
//!   Complexity: **O(2^ν × e)** — exponential in the cyclomatic number.
//!   Only practical for ν ≲ 25–30.

use petgraph::algo;
use petgraph::prelude::*;
use petgraph::visit::EdgeRef;
use std::collections::{HashMap, HashSet, VecDeque};

use super::{EdgeType, SCGraph, SCGraphNodeId};

// ---------------------------------------------------------------------------
// Public configuration
// ---------------------------------------------------------------------------

/// Selects how cycles are counted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CycleCountMode {
    /// Count **independent** cycles (dimension of the cycle space).
    ///
    /// This is the cyclomatic number (circuit rank):
    ///     ν = |E| − |V| + #connected_components
    ///
    /// Fast: O(n + e).
    Fundamental,

    /// Count **all distinct simple cycles** via exhaustive enumeration of the
    /// cycle space.
    ///
    /// Exponential: O(2^ν × e). Panics if ν > 63.
    Simple,
}

// ---------------------------------------------------------------------------
// Graph construction helpers
// ---------------------------------------------------------------------------

/// Build an undirected `petgraph` from the edges of an [`SCGraph`] that
/// satisfy `predicate`.
///
/// Only nodes that are endpoints of at least one accepted edge are included.
/// (Isolated nodes contribute equally to |V| and #CC, so they do not affect
/// the cyclomatic number.)
fn build_filtered_graph(
    scg: &SCGraph,
    predicate: impl Fn(&EdgeType) -> bool,
) -> Graph<(), (), Undirected> {
    let mut g = Graph::<(), (), Undirected>::default();
    let mut id_to_idx: HashMap<SCGraphNodeId, NodeIndex> = HashMap::new();

    for e in &scg.edges {
        if predicate(&e.edge_type) {
            let u = *id_to_idx.entry(e.source).or_insert_with(|| g.add_node(()));
            let v = *id_to_idx.entry(e.target).or_insert_with(|| g.add_node(()));
            g.add_edge(u, v, ());
        }
    }

    g
}

/// Build an undirected `petgraph` from the edges of a
/// [`CombinedSCGraph`](super::CombinedSCGraph) that satisfy `predicate`.
fn build_filtered_combined_graph(
    combined: &super::CombinedSCGraph,
    predicate: impl Fn(&EdgeType) -> bool,
) -> Graph<(), (), Undirected> {
    let mut g = Graph::<(), (), Undirected>::default();
    let mut id_to_idx: HashMap<super::CombinedVertexId, NodeIndex> = HashMap::new();

    for e in &combined.edges {
        if predicate(&e.edge_type) {
            let u = *id_to_idx.entry(e.source).or_insert_with(|| g.add_node(()));
            let v = *id_to_idx.entry(e.target).or_insert_with(|| g.add_node(()));
            g.add_edge(u, v, ());
        }
    }

    g
}

// ---------------------------------------------------------------------------
// Mode 1: Fundamental (cyclomatic number)
// ---------------------------------------------------------------------------

/// Cyclomatic number (circuit rank) of an undirected graph.
///
///     ν = |E| − |V| + #connected_components
///
/// Always ≥ 0 for any valid simple or multigraph.
fn cyclomatic_number(graph: &Graph<(), (), Undirected>) -> usize {
    let e = graph.edge_count() as i64;
    let v = graph.node_count() as i64;
    let cc = algo::connected_components(graph) as i64;

    (e - v + cc).max(0) as usize
}

// ---------------------------------------------------------------------------
// Mode 2: Simple cycle enumeration
// ---------------------------------------------------------------------------

/// Find a spanning forest via DFS. Returns the set of edge indices that
/// form tree edges.
fn spanning_forest(graph: &Graph<(), (), Undirected>) -> HashSet<EdgeIndex> {
    let mut tree_edges = HashSet::new();
    let mut visited = vec![false; graph.node_count()];

    for start in graph.node_indices() {
        if visited[start.index()] {
            continue;
        }
        let mut stack = vec![start];
        visited[start.index()] = true;

        while let Some(node) = stack.pop() {
            for edge in graph.edges(node) {
                let neighbor = edge.target();
                if !visited[neighbor.index()] {
                    visited[neighbor.index()] = true;
                    tree_edges.insert(edge.id());
                    stack.push(neighbor);
                }
            }
        }
    }

    tree_edges
}

/// Find the unique path between `from` and `to` in the spanning tree,
/// returning the edge indices along the path.
fn tree_path(
    graph: &Graph<(), (), Undirected>,
    tree_edges: &HashSet<EdgeIndex>,
    from: NodeIndex,
    to: NodeIndex,
) -> Vec<EdgeIndex> {
    if from == to {
        return vec![];
    }

    // BFS restricted to tree edges
    let mut parent: HashMap<NodeIndex, (NodeIndex, EdgeIndex)> = HashMap::new();
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    queue.push_back(from);
    visited.insert(from);

    while let Some(node) = queue.pop_front() {
        if node == to {
            break;
        }
        for edge in graph.edges(node) {
            if !tree_edges.contains(&edge.id()) {
                continue;
            }
            let neighbor = edge.target();
            if !visited.contains(&neighbor) {
                visited.insert(neighbor);
                parent.insert(neighbor, (node, edge.id()));
                queue.push_back(neighbor);
            }
        }
    }

    // Trace path from `to` back to `from`
    let mut path = Vec::new();
    let mut current = to;
    while current != from {
        let (prev, eidx) = parent[&current];
        path.push(eidx);
        current = prev;
    }
    path
}

/// Compute fundamental cycles as bit-vectors over the edge set.
///
/// Each returned `Vec<bool>` has length `graph.edge_count()` with `true`
/// at positions corresponding to edges in that fundamental cycle.
fn fundamental_cycle_basis(graph: &Graph<(), (), Undirected>) -> Vec<Vec<bool>> {
    let num_edges = graph.edge_count();
    if num_edges == 0 {
        return vec![];
    }

    let tree_edges = spanning_forest(graph);

    // Non-tree edges (chords) — each defines one fundamental cycle
    let chords: Vec<EdgeIndex> = graph
        .edge_indices()
        .filter(|e| !tree_edges.contains(e))
        .collect();

    let mut cycles = Vec::with_capacity(chords.len());

    for chord in chords {
        let (u, v) = graph.edge_endpoints(chord).unwrap();
        let path_edges = tree_path(graph, &tree_edges, u, v);

        let mut cycle_vec = vec![false; num_edges];
        cycle_vec[chord.index()] = true;
        for eidx in path_edges {
            cycle_vec[eidx.index()] = true;
        }
        cycles.push(cycle_vec);
    }

    cycles
}

/// Check whether a set of edges (given as a bit-vector) forms a single
/// simple cycle: every involved vertex has degree exactly 2 and the
/// induced subgraph is connected.
fn is_simple_cycle(graph: &Graph<(), (), Undirected>, edge_bits: &[bool]) -> bool {
    let mut degree: HashMap<NodeIndex, usize> = HashMap::new();
    let mut edge_count = 0usize;

    for (i, &present) in edge_bits.iter().enumerate() {
        if present {
            edge_count += 1;
            let (u, v) = graph.edge_endpoints(EdgeIndex::new(i)).unwrap();
            *degree.entry(u).or_insert(0) += 1;
            *degree.entry(v).or_insert(0) += 1;
        }
    }

    // A simple cycle needs at least 3 edges
    if edge_count < 3 {
        return false;
    }

    // Every involved vertex must have degree exactly 2
    if degree.values().any(|&d| d != 2) {
        return false;
    }

    // Connectivity: BFS from any involved vertex, traversing only
    // edges in the candidate set
    let start = *degree.keys().next().unwrap();
    let mut visited_nodes = HashSet::new();
    let mut queue = VecDeque::new();
    visited_nodes.insert(start);
    queue.push_back(start);

    while let Some(node) = queue.pop_front() {
        for edge in graph.edges(node) {
            if !edge_bits[edge.id().index()] {
                continue;
            }
            let neighbor = edge.target();
            if !visited_nodes.contains(&neighbor) {
                visited_nodes.insert(neighbor);
                queue.push_back(neighbor);
            }
        }
    }

    visited_nodes.len() == degree.len()
}

/// Count all distinct simple cycles in an undirected graph by enumerating
/// all non-zero elements of the cycle space and validating each.
///
/// # Complexity
///
/// O(2^ν × e) where ν is the cyclomatic number.
///
/// # Panics
///
/// Panics if ν > 63 (would overflow the `u64` subset mask).
fn count_simple_cycles(graph: &Graph<(), (), Undirected>) -> usize { let basis =
    fundamental_cycle_basis(graph);
    let nu = basis.len();

    if nu == 0 {
        return 0;
    }

    assert!(
        nu <= 63,
        "Cyclomatic number {} is too large for exhaustive enumeration (max 63)",
        nu
    );

    let num_edges = graph.edge_count();
    let mut count = 0usize;

    // Enumerate all 2^ν − 1 non-empty subsets of the basis
    let total_subsets = 1u64 << nu;
    for mask in 1..total_subsets {
        // XOR the selected fundamental cycles
        let mut xor_edges = vec![false; num_edges];
        for (i, cycle) in basis.iter().enumerate() {
            if mask & (1u64 << i) != 0 {
                for j in 0..num_edges {
                    xor_edges[j] ^= cycle[j];
                }
            }
        }

        if is_simple_cycle(graph, &xor_edges) {
            count += 1;
        }
    }

    count
}

// ---------------------------------------------------------------------------
// Unified internal dispatch
// ---------------------------------------------------------------------------

/// Count cycles in an undirected graph according to the selected mode.
fn count_cycles(graph: &Graph<(), (), Undirected>, mode: CycleCountMode) -> usize {
    match mode {
        CycleCountMode::Fundamental => cyclomatic_number(graph),
        CycleCountMode::Simple => count_simple_cycles(graph),
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Count SC-cycles in the original (pre-merge) SC graph.
///
/// An SC-cycle is a cycle containing ≥ 1 S-edge **and** ≥ 1 C-edge.
///
/// Because S-edges alone never form cycles:
///
///     SC-cycles = cycles(full) − cycles(C-only)
///
/// # Parameters
///
/// - `mode`: Selects between fast fundamental (independent) cycle count or
///   exact simple cycle enumeration.
///
/// # Panics
///
/// In [`CycleCountMode::Simple`], panics if the cyclomatic number exceeds 63.
pub fn count_sc_cycles_before_merging(scg: &SCGraph, mode: CycleCountMode) -> usize {
    let full = build_filtered_graph(scg, |_| true);
    let c_only = build_filtered_graph(scg, |t| matches!(t, EdgeType::C));

    let full_cycles = count_cycles(&full, mode);
    let c_cycles = count_cycles(&c_only, mode);

    full_cycles.saturating_sub(c_cycles)
}

/// Count SC-cycles in the combined (post-merge) graph.
///
/// Semantics are identical to [`count_sc_cycles_before_merging`] but
/// operates on a [`CombinedSCGraph`](super::CombinedSCGraph).
///
/// # Parameters
///
/// - `mode`: Selects between fast fundamental (independent) cycle count or
///   exact simple cycle enumeration.
///
/// # Panics
///
/// In [`CycleCountMode::Simple`], panics if the cyclomatic number exceeds 63.
pub fn count_sc_cycles_in_combined_graph(
    combined: &super::CombinedSCGraph,
    mode: CycleCountMode,
) -> usize {
    let full = build_filtered_combined_graph(combined, |_| true);
    let c_only = build_filtered_combined_graph(combined, |t| matches!(t, EdgeType::C));

    let full_cycles = count_cycles(&full, mode);
    let c_cycles = count_cycles(&c_only, mode);

    full_cycles.saturating_sub(c_cycles)
}
