use super::{EdgeType, SCGraph};

/// Update SC graph by removing verified commutative C-edges while keeping error C-edges
pub fn update_scgraph_for_commutative_errors(
    mut scgraph: SCGraph,
    verified_not_commutative_pairs: Vec<(
        crate::verification::Boogie::VerificationNodeId,
        crate::verification::Boogie::VerificationNodeId,
    )>,
) -> SCGraph {
    // Keep all S-edges and C-edges that have errors (not commutative_pairs)
    // Remove C-edges that were verified as commutative (no errors means they are commutative)

    // Convert node pairs to a set for fast lookup
    let mut error_nodes = std::collections::HashSet::new();
    for (node1, node2) in verified_not_commutative_pairs {
        let tuple1 = (node1.function_id, node1.instance, node1.hop_id);
        let tuple2 = (node2.function_id, node2.instance, node2.hop_id);
        error_nodes.insert((tuple1, tuple2));
    }

    // Filter edges: keep S-edges and C-edges that have verification errors
    scgraph.edges.retain(|edge| {
        match edge.edge_type {
            EdgeType::S => true, // Always keep S-edges
            EdgeType::C => {
                // For C-edges, only keep if they have verification errors (not commutative)
                let node1 = (
                    edge.source.function_id.index(),
                    edge.source.instance,
                    edge.source.hop_id.index(),
                );
                let node2 = (
                    edge.target.function_id.index(),
                    edge.target.instance,
                    edge.target.hop_id.index(),
                );

                let has_error =
                    error_nodes.contains(&(node1, node2)) || error_nodes.contains(&(node2, node1));

                has_error
            }
        }
    });

    scgraph
}
