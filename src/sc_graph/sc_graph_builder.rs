use super::{EdgeType, SCGraph, SCGraphNodeId};
use crate::cfg::{CfgProgram, FunctionType, HopId};
use crate::dataflow::{
    analyze_table_mod_ref, AccessType, AnalysisLevel, DataflowResults, SetLattice, TableAccess,
};
use std::collections::{HashMap, HashSet};

/// Builder for constructing SC-Graphs from CFG programs.
pub struct SCGraphBuilder {
    /// Number of instances to create for each transaction function
    pub num_instances: u32,
}

impl SCGraphBuilder {
    /// Creates a new builder with the specified number of instances per transaction.
    pub fn new(num_instances: u32) -> Self {
        Self {
            num_instances: num_instances.max(1), // Ensure at least 1 instance
        }
    }

    /// Creates a builder with default of 2 instances per transaction.
    pub fn default() -> Self {
        Self::new(2)
    }

    /// Builds an SC-Graph from a CFG program.
    ///
    /// The process involves:
    /// 1. Creating nodes for each hop in transaction functions (with multiple instances)
    /// 2. Adding S-edges between sequential hops within the same transaction instance
    /// 3. Analyzing table accesses using hop-level dataflow analysis
    /// 4. Adding C-edges between conflicting hops from different transaction instances
    pub fn build(&self, cfg_program: &CfgProgram) -> SCGraph {
        let mut sc_graph = SCGraph {
            nodes: HashSet::new(),
            edges: Vec::new(),
        };

        // Step 1: Create nodes for all hops in transaction functions (with multiple instances)
        for (function_id, function) in cfg_program.functions.iter() {
            // Only process transaction functions (skip partition functions)
            if function.function_type == FunctionType::Transaction {
                for (hop_id, _hop) in function.hops.iter() {
                    // Create multiple instances of this hop
                    for instance in 0..self.num_instances {
                        let node_id = SCGraphNodeId {
                            function_id,
                            hop_id,
                            instance,
                        };
                        sc_graph.nodes.insert(node_id);
                    }
                }
            }
        }

        // Step 2: Add S-edges (sequential edges within transactions)
        self.add_sequential_edges(&mut sc_graph, cfg_program);

        // Step 3: Analyze table accesses for all hops and add C-edges
        self.add_conflict_edges(&mut sc_graph, cfg_program);

        sc_graph
    }

    /// Adds S-edges between sequential hops within each transaction function instance.
    fn add_sequential_edges(&self, sc_graph: &mut SCGraph, cfg_program: &CfgProgram) {
        for (function_id, function) in cfg_program.functions.iter() {
            // Only process transaction functions
            if function.function_type == FunctionType::Transaction {
                let hop_order = &function.hop_order;

                // Add S-edges for each instance
                for instance in 0..self.num_instances {
                    // Add S-edges between consecutive hops within the same instance
                    for window in hop_order.windows(2) {
                        if let [prev_hop_id, curr_hop_id] = window {
                            let prev_node_id = SCGraphNodeId {
                                function_id,
                                hop_id: *prev_hop_id,
                                instance,
                            };
                            let curr_node_id = SCGraphNodeId {
                                function_id,
                                hop_id: *curr_hop_id,
                                instance,
                            };

                            // Only add edge if both hops are in the SC-Graph
                            if sc_graph.nodes.contains(&prev_node_id)
                                && sc_graph.nodes.contains(&curr_node_id)
                            {
                                let edge = super::SCGraphEdge {
                                    source: prev_node_id,
                                    target: curr_node_id,
                                    edge_type: EdgeType::S,
                                };
                                sc_graph.edges.push(edge);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Analyzes table accesses and adds C-edges between conflicting hops from different transactions.
    fn add_conflict_edges(&self, sc_graph: &mut SCGraph, cfg_program: &CfgProgram) {
        // Analyze table accesses for each hop using hop-level dataflow analysis
        let mut hop_table_accesses: HashMap<HopId, HashSet<TableAccess>> = HashMap::new();

        for (_function_id, function) in cfg_program.functions.iter() {
            // Only analyze transaction functions
            if function.function_type == FunctionType::Transaction {
                // Run hop-level table mod-ref analysis
                let analysis_results = analyze_table_mod_ref(function, AnalysisLevel::Hop);

                // Extract table accesses for each hop
                for (hop_id, _hop) in function.hops.iter() {
                    let hop_accesses =
                        Self::extract_hop_table_accesses(hop_id, function, &analysis_results);
                    hop_table_accesses.insert(hop_id, hop_accesses);
                }
            }
        }

        // Add C-edges between conflicting hops from different transactions
        self.add_conflict_edges_from_accesses(sc_graph, cfg_program, &hop_table_accesses);
    }

    /// Extracts table accesses for a specific hop from the dataflow analysis results.
    fn extract_hop_table_accesses(
        hop_id: HopId,
        function: &crate::cfg::FunctionCfg,
        analysis_results: &DataflowResults<SetLattice<TableAccess>>,
    ) -> HashSet<TableAccess> {
        let mut hop_accesses = HashSet::new();

        // Get the hop's blocks
        if let Some(hop) = function.hops.get(hop_id) {
            for &block_id in &hop.blocks {
                // For hop-level analysis, we want the exit values of each block within the hop
                if let Some(block_exit_lattice) = analysis_results.exit.get(&block_id) {
                    if let Some(access_set) = block_exit_lattice.as_set() {
                        for access in access_set {
                            hop_accesses.insert(access.clone());
                        }
                    }
                }
            }
        }

        hop_accesses
    }

    /// Adds C-edges between hops that have conflicting table accesses and belong to different transaction instances.
    fn add_conflict_edges_from_accesses(
        &self,
        sc_graph: &mut SCGraph,
        cfg_program: &CfgProgram,
        hop_table_accesses: &HashMap<HopId, HashSet<TableAccess>>,
    ) {
        // Get all hops with their corresponding transaction functions
        let mut hops_with_functions: Vec<(HopId, crate::cfg::FunctionId)> = Vec::new();

        for (function_id, function) in cfg_program.functions.iter() {
            if function.function_type == FunctionType::Transaction {
                for (hop_id, _hop) in function.hops.iter() {
                    hops_with_functions.push((hop_id, function_id));
                }
            }
        }

        // Check all pairs of hops - now we need to consider all instances
        for i in 0..hops_with_functions.len() {
            for j in 0..hops_with_functions.len() {
                let (hop1_id, func1_id) = hops_with_functions[i];
                let (hop2_id, func2_id) = hops_with_functions[j];

                // Check for conflicts between different transaction instances
                if let (Some(accesses1), Some(accesses2)) = (
                    hop_table_accesses.get(&hop1_id),
                    hop_table_accesses.get(&hop2_id),
                ) {
                    if Self::have_conflicting_accesses(accesses1, accesses2) {
                        // Create C-edges between all instances of conflicting hops
                        for instance1 in 0..self.num_instances {
                            for instance2 in 0..self.num_instances {
                                // Skip if same function and same instance (no conflict within same transaction instance)
                                if func1_id == func2_id && instance1 == instance2 {
                                    continue;
                                }

                                // Skip if same hop in same instance (would create self-loop)
                                if hop1_id == hop2_id && instance1 == instance2 {
                                    continue;
                                }

                                let node1_id = SCGraphNodeId {
                                    function_id: func1_id,
                                    hop_id: hop1_id,
                                    instance: instance1,
                                };
                                let node2_id = SCGraphNodeId {
                                    function_id: func2_id,
                                    hop_id: hop2_id,
                                    instance: instance2,
                                };

                                // For undirected C-edges, only create one edge per pair by ensuring consistent ordering
                                // This eliminates duplicate edges since C-edges are undirected
                                if node1_id < node2_id {
                                    // Only add edge if both hops are in the SC-Graph
                                    if sc_graph.nodes.contains(&node1_id)
                                        && sc_graph.nodes.contains(&node2_id)
                                    {
                                        let edge = super::SCGraphEdge {
                                            source: node1_id,
                                            target: node2_id,
                                            edge_type: EdgeType::C,
                                        };
                                        sc_graph.edges.push(edge);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Checks if two sets of table accesses have conflicts.
    ///
    /// A conflict exists if:
    /// - One hop reads and another writes the same table, OR
    /// - Both hops write to the same table
    fn have_conflicting_accesses(
        accesses1: &HashSet<TableAccess>,
        accesses2: &HashSet<TableAccess>,
    ) -> bool {
        for access1 in accesses1 {
            for access2 in accesses2 {
                // Same table accessed
                if access1.table_id == access2.table_id {
                    match (access1.access_type, access2.access_type) {
                        // Read-Write conflict
                        (AccessType::Read, AccessType::Write) |
                        (AccessType::Write, AccessType::Read) |
                        // Write-Write conflict
                        (AccessType::Write, AccessType::Write) => {
                            return true;
                        }
                        // Read-Read is not a conflict
                        (AccessType::Read, AccessType::Read) => {
                            // No conflict
                        }
                    }
                }
            }
        }
        false
    }
}
