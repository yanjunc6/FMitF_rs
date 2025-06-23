// src/verify/commutativity_check.rs

use crate::cfg::{CfgProgram, FunctionId, HopId, TableId, VarId};
use crate::dataflow::{analyze_live_variables, analyze_table_mod_ref, AccessType};
use crate::sc_graph::{Edge, SCGraph};
use crate::verification::interleaving::enumerate_interleavings;
use std::collections::HashSet;

pub struct VerificationUnit {
    pub prefix_a: Vec<HopId>,          // e.g. [A₁..Aₘ₋₁]
    pub prefix_b: Vec<HopId>,          // e.g. [B₁..Bₖ₋₁]
    pub final_a: HopId,                // Aₘ
    pub final_b: HopId,                // Bₖ
    pub function_a: FunctionId,        // Function containing prefix_a and final_a
    pub function_b: FunctionId,        // Function containing prefix_b and final_b
    pub merges: Vec<Vec<HopId>>,       // All merges of prefix_a and prefix_b
    pub relevant_tables: Vec<TableId>, // table to be compared (final_a and final_b read/write)
    pub relevant_vars: Vec<VarId>, // variables to be compared (live var at the end of final_a and final_b)
}

// Creates the VerificationUnit for a single c-edge (Aₘ,Bₖ).
pub fn create_verification_unit(
    c_edge: Edge,
    cfg: &CfgProgram,
    sc_graph: &SCGraph,
) -> VerificationUnit {
    // Extract SC graph nodes from the edge
    let sc_node_a = &sc_graph.nodes[c_edge.source];
    let sc_node_b = &sc_graph.nodes[c_edge.target];

    // 1) Identify prefix_a + final_a + function_a
    let function_a = sc_node_a.cfg_function_id;
    let final_a = sc_node_a.cfg_hop_id;
    let func_cfg_a = &cfg.functions[function_a];

    // Get prefix for function A (all hops up to but not including final_a)
    let prefix_a: Vec<HopId> = func_cfg_a
        .hop_order
        .iter()
        .take_while(|&&hop_id| hop_id != final_a)
        .cloned()
        .collect();

    // 2) Identify prefix_b + final_b + function_b
    let function_b = sc_node_b.cfg_function_id;
    let final_b = sc_node_b.cfg_hop_id;
    let func_cfg_b = &cfg.functions[function_b];

    // Get prefix for function B (all hops up to but not including final_b)
    let prefix_b: Vec<HopId> = func_cfg_b
        .hop_order
        .iter()
        .take_while(|&&hop_id| hop_id != final_b)
        .cloned()
        .collect();

    // 3) Use dataflow to find relevant tables and variables
    let liveness_results_a = analyze_live_variables(func_cfg_a);
    let liveness_results_b = analyze_live_variables(func_cfg_b);

    let table_mod_ref_a = analyze_table_mod_ref(func_cfg_a);
    let table_mod_ref_b = analyze_table_mod_ref(func_cfg_b);

    // Get live variables at the exit of final hops
    let mut live_vars_a = HashSet::new();
    let mut live_vars_b = HashSet::new();

    // For function A - get live vars at final_a exit
    if let Some(hop_cfg_a) = func_cfg_a.hops.get(final_a) {
        for &block_id in &hop_cfg_a.blocks {
            if let Some(exit_state) = liveness_results_a.exit.get(&block_id) {
                live_vars_a.extend(exit_state.set.iter().cloned());
            }
        }
    }

    // For function B - get live vars at final_b exit
    if let Some(hop_cfg_b) = func_cfg_b.hops.get(final_b) {
        for &block_id in &hop_cfg_b.blocks {
            if let Some(exit_state) = liveness_results_b.exit.get(&block_id) {
                live_vars_b.extend(exit_state.set.iter().cloned());
            }
        }
    }

    // Union of live variables from both functions
    let relevant_vars: Vec<VarId> = live_vars_a.union(&live_vars_b).cloned().collect();

    // Extract relevant tables from both functions
    let mut relevant_tables_set = HashSet::new();

    // For function A - get tables accessed in final_a
    if let Some(hop_cfg_a) = func_cfg_a.hops.get(final_a) {
        for &block_id in &hop_cfg_a.blocks {
            if let Some(exit_state) = table_mod_ref_a.exit.get(&block_id) {
                for table_access in &exit_state.set {
                    if table_access.access_type == AccessType::Write {
                        // Only consider tables that are written
                        relevant_tables_set.insert(table_access.table_id);
                    }
                }
            }
        }
    }

    // For function B - get tables accessed in final_b
    if let Some(hop_cfg_b) = func_cfg_b.hops.get(final_b) {
        for &block_id in &hop_cfg_b.blocks {
            if let Some(exit_state) = table_mod_ref_b.exit.get(&block_id) {
                for table_access in &exit_state.set {
                    if table_access.access_type == AccessType::Write {
                        // Only consider tables that are written
                        relevant_tables_set.insert(table_access.table_id);
                    }
                }
            }
        }
    }

    let relevant_tables: Vec<TableId> = relevant_tables_set.into_iter().collect();

    // 4) Enumerate merges
    let merges = enumerate_interleavings(&prefix_a, &prefix_b);

    // Return the constructed VerificationUnit
    VerificationUnit {
        prefix_a,
        prefix_b,
        final_a,
        final_b,
        function_a,
        function_b,
        merges,
        relevant_tables,
        relevant_vars,
    }
}
