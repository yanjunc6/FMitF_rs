use super::{EdgeType, SCGraph, SCGraphNodeId};
use crate::cfg::{FunctionKind, HopId, Program};
use crate::dataflow::{
    analyze_table_mod_ref, AccessType, DataflowResults, SetLattice, TableAccess,
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
    pub fn build(&self, program: &Program) -> SCGraph {
        let mut sc_graph = SCGraph {
            nodes: HashSet::new(),
            edges: Vec::new(),
        };

        // Step 1: Create nodes for all hops in transaction functions
        for &function_id in &program.all_transactions {
            let function = &program.functions[function_id];
            if function.kind == FunctionKind::Transaction {
                for &hop_id in &function.hops {
                    // Create multiple instances of this hop
                    for instance in 0..self.num_instances {
                        sc_graph.nodes.insert(SCGraphNodeId {
                            function_id,
                            hop_id,
                            instance,
                        });
                    }
                }
            }
        }

        // Step 2: Sequential S-edges
        self.add_sequential_edges(&mut sc_graph, program);
        // Step 3: Conflict C-edges
        self.add_conflict_edges(&mut sc_graph, program);
        sc_graph
    }

    /// Adds S-edges between sequential hops within each transaction function instance.
    fn add_sequential_edges(&self, sc_graph: &mut SCGraph, program: &Program) {
        for &function_id in &program.all_transactions {
            let function = &program.functions[function_id];
            if function.kind == FunctionKind::Transaction {
                let hop_order = &function.hops; // current order as stored
                                                // Add S-edges for each instance
                for instance in 0..self.num_instances {
                    // Add S-edges between consecutive hops within the same instance
                    for window in hop_order.windows(2) {
                        if let [prev_hop_id, curr_hop_id] = window {
                            let prev_node = SCGraphNodeId {
                                function_id,
                                hop_id: *prev_hop_id,
                                instance,
                            };
                            let curr_node = SCGraphNodeId {
                                function_id,
                                hop_id: *curr_hop_id,
                                instance,
                            };
                            if sc_graph.nodes.contains(&prev_node)
                                && sc_graph.nodes.contains(&curr_node)
                            {
                                sc_graph.edges.push(super::SCGraphEdge {
                                    source: prev_node,
                                    target: curr_node,
                                    edge_type: EdgeType::S,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    /// Analyzes table accesses and adds C-edges between conflicting hops from different transactions.
    fn add_conflict_edges(&self, sc_graph: &mut SCGraph, program: &Program) {
        // Analyze table accesses for each hop using hop-level dataflow analysis
        let mut hop_table_accesses: HashMap<HopId, HashSet<TableAccess>> = HashMap::new();

        for &function_id in &program.all_transactions {
            let function = &program.functions[function_id];
            if function.kind == FunctionKind::Transaction {
                // Run hop-level table mod-ref analysis
                let analysis_results = analyze_table_mod_ref(function, program);

                // Extract table accesses for each hop
                for &hop_id in &function.hops {
                    let accesses =
                        Self::extract_hop_table_accesses(program, hop_id, &analysis_results);
                    hop_table_accesses.insert(hop_id, accesses);
                }
            }
        }

        // Add C-edges between conflicting hops from different transactions
        self.add_conflict_edges_from_accesses(sc_graph, program, &hop_table_accesses);
    }

    /// Extracts table accesses for a specific hop from the dataflow analysis results.
    fn extract_hop_table_accesses(
        prog: &Program,
        hop_id: HopId,
        analysis_results: &DataflowResults<SetLattice<TableAccess>>,
    ) -> HashSet<TableAccess> {
        let mut set = HashSet::new();

        // Get the hop's blocks
        let hop = &prog.hops[hop_id];
        for &block_id in &hop.blocks {
            // For hop-level analysis, we want the exit values of each block within the hop
            if let Some(exit) = analysis_results.block_exit.get(&block_id) {
                if let Some(accesses) = exit.as_set() {
                    for a in accesses {
                        set.insert(a.clone());
                    }
                }
            }
        }

        set
    }

    /// Adds C-edges between hops that have conflicting table accesses and belong to different transaction instances.
    fn add_conflict_edges_from_accesses(
        &self,
        sc_graph: &mut SCGraph,
        program: &Program,
        hop_table_accesses: &HashMap<HopId, HashSet<TableAccess>>,
    ) {
        // Get all hops with their corresponding transaction functions
        let mut hops_with_functions = Vec::new();
        for &function_id in &program.all_transactions {
            let function = &program.functions[function_id];
            if function.kind == FunctionKind::Transaction {
                for &hop_id in &function.hops {
                    hops_with_functions.push((hop_id, function_id));
                }
            }
        }
        // Check all pairs of hops - now we need to consider all instances
        for i in 0..hops_with_functions.len() {
            for j in 0..hops_with_functions.len() {
                let (hop1, func1) = hops_with_functions[i];
                let (hop2, func2) = hops_with_functions[j];
                if let (Some(acc1), Some(acc2)) =
                    (hop_table_accesses.get(&hop1), hop_table_accesses.get(&hop2))
                {
                    if Self::have_conflicting_accesses(acc1, acc2) {
                        for instance1 in 0..self.num_instances {
                            for instance2 in 0..self.num_instances {
                                if func1 == func2 && instance1 == instance2 {
                                    continue;
                                }
                                if hop1 == hop2 && instance1 == instance2 {
                                    continue;
                                }
                                let node1 = SCGraphNodeId {
                                    function_id: func1,
                                    hop_id: hop1,
                                    instance: instance1,
                                };
                                let node2 = SCGraphNodeId {
                                    function_id: func2,
                                    hop_id: hop2,
                                    instance: instance2,
                                };
                                if node1 < node2
                                    && sc_graph.nodes.contains(&node1)
                                    && sc_graph.nodes.contains(&node2)
                                {
                                    sc_graph.edges.push(super::SCGraphEdge {
                                        source: node1,
                                        target: node2,
                                        edge_type: EdgeType::C,
                                    });
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
    fn have_conflicting_accesses(a1: &HashSet<TableAccess>, a2: &HashSet<TableAccess>) -> bool {
        for x in a1 {
            for y in a2 {
                if x.table == y.table && x.field == y.field {
                    match (x.access_type, y.access_type) {
                        (AccessType::Read, AccessType::Write)
                        | (AccessType::Write, AccessType::Read)
                        | (AccessType::Write, AccessType::Write) => return true,
                        _ => {}
                    }
                }
            }
        }
        false
    }
}
