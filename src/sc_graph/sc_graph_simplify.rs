use super::{EdgeType, SCGraph};

/// Update SC graph by removing verified commutative C-edges while keeping error C-edges
pub fn update_scgraph_for_commutative_errors(
    mut scgraph: SCGraph,
    verified_not_commutative_pairs: Vec<(usize, usize)>,
) -> SCGraph {
    // Keep all S-edges and C-edges that have errors (not commutative_pairs)
    // Remove C-edges that were verified as commutative (no errors means they are commutative)

    // Convert hop_id pairs to a set for fast lookup
    let error_pairs: std::collections::HashSet<(usize, usize)> =
        verified_not_commutative_pairs.into_iter().collect();

    // Filter edges: keep S-edges and C-edges that have verification errors
    scgraph.edges.retain(|edge| {
        match edge.edge_type {
            EdgeType::S => true, // Always keep S-edges
            EdgeType::C => {
                // For C-edges, only keep if they have verification errors (not commutative)
                let hop1_idx = edge.source.hop_id.index();
                let hop2_idx = edge.target.hop_id.index();

                // Check both orderings since C-edges are undirected
                error_pairs.contains(&(hop1_idx, hop2_idx))
                    || error_pairs.contains(&(hop2_idx, hop1_idx))
            }
        }
    });

    scgraph
}
