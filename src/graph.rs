use crate::ast::*;
use std::rc::Rc;

/// Edge types in SC-Graph
#[derive(Debug, Clone, PartialEq)]
pub enum EdgeType {
    S, // Sequential (program order)
    C, // Conflict
}

/// Simple SC-Graph using vertex indices
#[derive(Debug)]
pub struct SCGraph {
    // Store all hops in a Vec - index is the vertex ID
    pub hops: Vec<Rc<HopBlock>>,
    // Store edges as simple pairs of indices
    pub edges: Vec<(usize, usize, EdgeType)>,
}

impl SCGraph {
    pub fn new(program: &Program) -> Self {
        let mut graph = Self {
            hops: Vec::new(),
            edges: Vec::new(),
        };
        graph.build(program);
        graph
    }

    fn build(&mut self, program: &Program) {
        // Collect all hops
        for func in &program.functions {
            for hop in &func.hops {
                self.hops.push(hop.clone());
            }
        }

        // Add S edges (sequential within function)
        let mut hop_index = 0;
        for func in &program.functions {
            for i in 0..func.hops.len().saturating_sub(1) {
                let curr_idx = hop_index + i;
                let next_idx = hop_index + i + 1;
                self.edges.push((curr_idx, next_idx, EdgeType::S));
            }
            hop_index += func.hops.len();
        }

        // Add C edges (conservative conflict detection)
        for i in 0..self.hops.len() {
            for j in (i + 1)..self.hops.len() {
                if self.has_conflict(i, j) {
                    self.edges.push((i, j, EdgeType::C));
                }
            }
        }
    }

    /// Conservative conflict detection: same node = potential conflict
    fn has_conflict(&self, hop1_idx: usize, hop2_idx: usize) -> bool {
        self.hops[hop1_idx].node.name == self.hops[hop2_idx].node.name
    }

    /// Find cycles that contain both S and C edges
    pub fn find_mixed_cycles(&self) -> Vec<Vec<Rc<HopBlock>>> {
        let mut cycles = Vec::new();
        let mut visited = vec![false; self.hops.len()];
        let mut path = Vec::new();

        for i in 0..self.hops.len() {
            if !visited[i] {
                self.dfs_cycles(i, &mut visited, &mut path, &mut cycles);
            }
        }

        // Filter cycles that have both S and C edges
        cycles.into_iter()
            .filter(|cycle| self.has_both_edge_types(cycle))
            .collect()
    }

    fn dfs_cycles(
        &self,
        current: usize,
        visited: &mut Vec<bool>,
        path: &mut Vec<usize>,
        cycles: &mut Vec<Vec<Rc<HopBlock>>>,
    ) {
        // Check if current vertex is already in path (cycle detected)
        if let Some(start_idx) = path.iter().position(|&idx| idx == current) {
            // Found a cycle - convert indices to hops
            let cycle_hops: Vec<Rc<HopBlock>> = path[start_idx..]
                .iter()
                .map(|&idx| self.hops[idx].clone())
                .collect();
            cycles.push(cycle_hops);
            return;
        }

        if visited[current] {
            return;
        }

        visited[current] = true;
        path.push(current);

        // Follow all outgoing edges
        for &(from, to, _) in &self.edges {
            if from == current {
                self.dfs_cycles(to, visited, path, cycles);
            }
        }

        path.pop();
    }

    fn has_both_edge_types(&self, cycle: &[Rc<HopBlock>]) -> bool {
        let mut has_s = false;
        let mut has_c = false;

        // Find indices of cycle hops
        let cycle_indices: Vec<usize> = cycle.iter()
            .map(|hop| {
                self.hops.iter()
                    .position(|h| Rc::ptr_eq(h, hop))
                    .unwrap()
            })
            .collect();

        // Check edges between consecutive cycle vertices
        for i in 0..cycle_indices.len() {
            let curr_idx = cycle_indices[i];
            let next_idx = cycle_indices[(i + 1) % cycle_indices.len()];
            
            // Find edge between these vertices
            for &(from, to, ref edge_type) in &self.edges {
                if from == curr_idx && to == next_idx {
                    match edge_type {
                        EdgeType::S => has_s = true,
                        EdgeType::C => has_c = true,
                    }
                }
            }
        }

        has_s && has_c
    }

    pub fn stats(&self) -> (usize, usize, usize) {
        let vertices = self.hops.len();
        let s_edges = self.edges.iter().filter(|(_, _, e)| *e == EdgeType::S).count();
        let c_edges = self.edges.iter().filter(|(_, _, e)| *e == EdgeType::C).count();
        (vertices, s_edges, c_edges)
    }
}