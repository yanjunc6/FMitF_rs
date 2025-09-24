use crate::cfg::HopId;
use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge};
use std::collections::HashSet;

// Import types from parent module
use super::{CommutativeUnit, Interleaving};
use crate::verification::scope::SliceId;

#[derive(Clone, Copy, PartialEq)]
enum Direction {
    Unknown,
    ABeforeB,
    BBeforeA,
}

/// Special interleavings that need to be tracked
#[derive(Clone)]
pub struct SpecialInterleavings {
    /// slice 0 followed by slice 1
    pub a_then_b: Interleaving,
    /// slice 1 followed by slice 0  
    pub b_then_a: Interleaving,
}

pub struct InterleavingGenerator;

impl InterleavingGenerator {
    pub fn new() -> Self {
        InterleavingGenerator
    }

    /// Generate all legal interleavings of two hop slices based on conflict constraints
    #[allow(dead_code)]
    pub fn generate_legal_interleavings(
        &self,
        sc_graph: &SCGraph,
        unit: &CommutativeUnit,
    ) -> Vec<Interleaving> {
        // Find all conflict edges between the two slices (excluding the unit's c_edge)
        let slice_a = unit.hops_per_slice.get(&0).cloned().unwrap_or_default();
        let slice_b = unit.hops_per_slice.get(&1).cloned().unwrap_or_default();
        let conflicts = self.find_cross_slice_conflicts(sc_graph, &slice_a, &slice_b, &unit.c_edge);

        // Generate all legal interleavings using the algorithm from verification.md
        self.generate_all_merges(&slice_a, &slice_b, &conflicts)
    }

    /// Find conflict edges between two hop slices, excluding the given edge
    #[allow(dead_code)]
    fn find_cross_slice_conflicts(
        &self,
        sc_graph: &SCGraph,
        slice_a: &[HopId],
        slice_b: &[HopId],
        exclude_edge: &SCGraphEdge,
    ) -> HashSet<(HopId, HopId)> {
        let mut conflicts = HashSet::new();
        let slice_a_set: HashSet<HopId> = slice_a.iter().cloned().collect();
        let slice_b_set: HashSet<HopId> = slice_b.iter().cloned().collect();

        for edge in &sc_graph.edges {
            if edge.edge_type == EdgeType::C && edge != exclude_edge {
                let source_hop = edge.source.hop_id;
                let target_hop = edge.target.hop_id;

                // Check if this is a cross-slice conflict
                if (slice_a_set.contains(&source_hop) && slice_b_set.contains(&target_hop))
                    || (slice_b_set.contains(&source_hop) && slice_a_set.contains(&target_hop))
                {
                    conflicts.insert((source_hop, target_hop));
                    conflicts.insert((target_hop, source_hop)); // Add both directions for lookup
                }
            }
        }

        conflicts
    }

    /// Generate all legal merges based on conflict constraints (implements Algorithm 1 from verification.md)
    #[allow(dead_code)]
    fn generate_all_merges(
        &self,
        slice_a: &[HopId],
        slice_b: &[HopId],
        conflicts: &HashSet<(HopId, HopId)>,
    ) -> Vec<Interleaving> {
        let mut result = Vec::new();

        fn dfs(
            slice_a: &[HopId],
            slice_b: &[HopId],
            conflicts: &HashSet<(HopId, HopId)>,
            i: usize,
            j: usize,
            placed: &mut Vec<(SliceId, HopId)>,
            direction: Direction,
            result: &mut Vec<Interleaving>,
        ) {
            // Base case: both slices exhausted
            if i == slice_a.len() && j == slice_b.len() {
                result.push(placed.clone());
                return;
            }

            // Try placing next hop from slice A
            if i < slice_a.len() {
                let hop_a = slice_a[i];
                if let Some(new_dir) =
                    check_direction_constraint(hop_a, 0, placed, conflicts, direction)
                {
                    placed.push((0, hop_a));
                    dfs(
                        slice_a,
                        slice_b,
                        conflicts,
                        i + 1,
                        j,
                        placed,
                        new_dir,
                        result,
                    );
                    placed.pop();
                }
            }

            // Try placing next hop from slice B
            if j < slice_b.len() {
                let hop_b = slice_b[j];
                if let Some(new_dir) =
                    check_direction_constraint(hop_b, 1, placed, conflicts, direction)
                {
                    placed.push((1, hop_b));
                    dfs(
                        slice_a,
                        slice_b,
                        conflicts,
                        i,
                        j + 1,
                        placed,
                        new_dir,
                        result,
                    );
                    placed.pop();
                }
            }
        }

        fn check_direction_constraint(
            current_hop: HopId,
            current_slice: SliceId,
            placed: &[(SliceId, HopId)],
            conflicts: &HashSet<(HopId, HopId)>,
            current_direction: Direction,
        ) -> Option<Direction> {
            // Scan placed hops from right to left to find the first conflicting hop from opposite slice
            for &(placed_slice, placed_hop) in placed.iter().rev() {
                if placed_slice != current_slice && conflicts.contains(&(current_hop, placed_hop)) {
                    // Found conflicting pair
                    let required_direction = if current_slice == 0 {
                        Direction::ABeforeB
                    } else {
                        Direction::BBeforeA
                    };

                    match current_direction {
                        Direction::Unknown => return Some(required_direction),
                        dir if dir == required_direction => return Some(dir),
                        _ => return None, // Constraint violation
                    }
                }
            }

            Some(current_direction) // No conflicts found, continue with current direction
        }

        let mut placed: Vec<(SliceId, HopId)> = Vec::new();
        dfs(
            slice_a,
            slice_b,
            conflicts,
            0,
            0,
            &mut placed,
            Direction::Unknown,
            &mut result,
        );

        result
    }

    /// Extract the two special interleavings (slice0+slice1 and slice1+slice0)
    pub fn extract_special_interleavings(&self, unit: &CommutativeUnit) -> SpecialInterleavings {
        let slice_a = unit.hops_per_slice.get(&0).cloned().unwrap_or_default();
        let slice_b = unit.hops_per_slice.get(&1).cloned().unwrap_or_default();
        let mut a_then_b = Vec::new();
        let mut b_then_a = Vec::new();

        // A followed by B
        for &hop in &slice_a {
            a_then_b.push((0, hop));
        }
        for &hop in &slice_b {
            a_then_b.push((1, hop));
        }

        // B followed by A
        for &hop in &slice_b {
            b_then_a.push((1, hop));
        }
        for &hop in &slice_a {
            b_then_a.push((0, hop));
        }

        SpecialInterleavings { a_then_b, b_then_a }
    }
}
