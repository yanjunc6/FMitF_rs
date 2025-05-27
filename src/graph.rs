use crate::ast::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Edge types in SC-Graph
#[derive(Debug, Clone, PartialEq)]
pub enum EdgeType {
    S, // Sequential (program order)
    C, // Conflict
}

/// Undirected edge representation
#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    pub v1: usize,
    pub v2: usize,
    pub edge_type: EdgeType,
}

impl Edge {
    fn new(v1: usize, v2: usize, edge_type: EdgeType) -> Self {
        // Ensure consistent ordering for undirected edges
        let (v1, v2) = if v1 <= v2 { (v1, v2) } else { (v2, v1) };
        Self { v1, v2, edge_type }
    }

    fn connects(&self, u: usize, v: usize) -> bool {
        (self.v1 == u && self.v2 == v) || (self.v1 == v && self.v2 == u)
    }

    fn other_vertex(&self, v: usize) -> Option<usize> {
        if self.v1 == v {
            Some(self.v2)
        } else if self.v2 == v {
            Some(self.v1)
        } else {
            None
        }
    }
}

/// Undirected SC-Graph
#[derive(Debug)]
pub struct SCGraph {
    pub hops: Vec<Rc<HopBlock>>,
    pub edges: Vec<Edge>,
    pub hop_to_function: HashMap<usize, String>,
}

impl SCGraph {
    pub fn new(program: &Program) -> Self {
        let mut graph = Self {
            hops: Vec::new(),
            edges: Vec::new(),
            hop_to_function: HashMap::new(),
        };
        graph.build(program);
        graph
    }

    fn build(&mut self, program: &Program) {
        // Collect all hops and track their functions
        let mut hop_index = 0;
        for func in &program.functions {
            for hop in &func.hops {
                self.hops.push(hop.clone());
                self.hop_to_function.insert(hop_index, func.name.clone());
                hop_index += 1;
            }
        }

        // Add S edges (sequential within function)
        let mut current_hop_index = 0;
        for func in &program.functions {
            for i in 0..func.hops.len().saturating_sub(1) {
                let curr_idx = current_hop_index + i;
                let next_idx = current_hop_index + i + 1;
                self.edges.push(Edge::new(curr_idx, next_idx, EdgeType::S));
            }
            current_hop_index += func.hops.len();
        }

        // Add C edges (conservative conflict detection)
        for i in 0..self.hops.len() {
            for j in (i + 1)..self.hops.len() {
                if self.has_conflict(i, j) {
                    self.edges.push(Edge::new(i, j, EdgeType::C));
                }
            }
        }
    }

    fn has_conflict(&self, hop1_idx: usize, hop2_idx: usize) -> bool {
        if self.are_in_same_function(hop1_idx, hop2_idx) {
            return false;
        }
        self.hops[hop1_idx].node.name == self.hops[hop2_idx].node.name
    }

    fn are_in_same_function(&self, hop1_idx: usize, hop2_idx: usize) -> bool {
        self.hop_to_function.get(&hop1_idx) == self.hop_to_function.get(&hop2_idx)
    }

    pub fn get_function_name(&self, hop_idx: usize) -> Option<&String> {
        self.hop_to_function.get(&hop_idx)
    }

    /// Find unique simple cycles that contain both S and C edges
    pub fn find_mixed_cycles(&self) -> Vec<Vec<Rc<HopBlock>>> {
        let mut cycles = Vec::new();
        let mut seen_cycles = HashSet::new();

        for start in 0..self.hops.len() {
            let mut path = vec![start];
            let mut visited = vec![false; self.hops.len()];
            self.dfs_find_cycles(
                start,
                start,
                &mut path,
                &mut visited,
                &mut cycles,
                &mut seen_cycles,
            );
        }

        cycles
    }

    fn dfs_find_cycles(
        &self,
        current: usize,
        start: usize,
        path: &mut Vec<usize>,
        visited: &mut Vec<bool>,
        cycles: &mut Vec<Vec<Rc<HopBlock>>>,
        seen_cycles: &mut HashSet<Vec<usize>>,
    ) {
        visited[current] = true;

        // Find all neighbors
        for edge in &self.edges {
            if let Some(neighbor) = edge.other_vertex(current) {
                if neighbor == start && path.len() >= 3 {
                    // Found a cycle back to start
                    if self.path_has_both_edge_types(path) {
                        // Create canonical representation
                        let canonical = self.canonical_cycle(path);

                        // Only add if we haven't seen this cycle before
                        if seen_cycles.insert(canonical) {
                            let cycle_hops: Vec<Rc<HopBlock>> =
                                path.iter().map(|&idx| self.hops[idx].clone()).collect();
                            cycles.push(cycle_hops);
                        }
                    }
                } else if !visited[neighbor] {
                    path.push(neighbor);
                    self.dfs_find_cycles(neighbor, start, path, visited, cycles, seen_cycles);
                    path.pop();
                }
            }
        }

        visited[current] = false;
    }

    /// Create canonical representation of a cycle
    fn canonical_cycle(&self, cycle: &[usize]) -> Vec<usize> {
        if cycle.is_empty() {
            return Vec::new();
        }

        // Find the minimum element
        let min_val = *cycle.iter().min().unwrap();
        let min_pos = cycle.iter().position(|&x| x == min_val).unwrap();

        // Create the rotation starting from minimum element
        let mut canonical = Vec::new();
        canonical.extend_from_slice(&cycle[min_pos..]);
        canonical.extend_from_slice(&cycle[..min_pos]);

        // Also check the reverse direction
        let mut reverse = canonical.clone();
        reverse.reverse();
        // For reverse, we need to rotate so it also starts with the minimum
        let min_pos_rev = reverse.iter().position(|&x| x == min_val).unwrap();
        let mut canonical_rev = Vec::new();
        canonical_rev.extend_from_slice(&reverse[min_pos_rev..]);
        canonical_rev.extend_from_slice(&reverse[..min_pos_rev]);

        // Return the lexicographically smaller one
        if canonical <= canonical_rev {
            canonical
        } else {
            canonical_rev
        }
    }

    fn path_has_both_edge_types(&self, path: &[usize]) -> bool {
        if path.len() < 3 {
            return false;
        }

        let mut has_s = false;
        let mut has_c = false;

        // Check edges between consecutive vertices in the path (including back to start)
        for i in 0..path.len() {
            let curr = path[i];
            let next = path[(i + 1) % path.len()];

            // Find the edge between curr and next
            for edge in &self.edges {
                if edge.connects(curr, next) {
                    match edge.edge_type {
                        EdgeType::S => has_s = true,
                        EdgeType::C => has_c = true,
                    }
                    break;
                }
            }
        }

        has_s && has_c
    }

    pub fn stats(&self) -> (usize, usize, usize) {
        let vertices = self.hops.len();
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
        (vertices, s_edges, c_edges)
    }
}
