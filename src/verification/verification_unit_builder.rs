use crate::cfg::{CfgProgram, FunctionCfg, HopId, Operand, TableId, Terminator, VarId};
use crate::dataflow::analyze_live_variables; // For performing liveness analysis
use crate::dataflow::{DataflowResults, SetLattice};
use crate::sc_graph::{Edge, EdgeType, SCGraph};
use crate::verification::VerificationUnit;
use std::collections::HashSet; // For type hints from analyze_live_variables

pub struct VerificationUnitBuilder<'a> {
    cfg_program: &'a CfgProgram,
    sc_graph: &'a SCGraph,
}

impl<'a> VerificationUnitBuilder<'a> {
    /// Creates a new `VerificationUnitBuilder`.
    ///
    /// # Arguments
    /// * `cfg_program`: A reference to the complete Control Flow Graph program.
    /// * `sc_graph`: A reference to the Serializability Conflict Graph.
    pub fn new(cfg_program: &'a CfgProgram, sc_graph: &'a SCGraph) -> Self {
        Self {
            cfg_program,
            sc_graph,
        }
    }

    /// Builds a `VerificationUnit` for a given C-edge from the SC-Graph.
    /// Liveness analysis is performed internally for the involved functions.
    ///
    /// # Arguments
    /// * `c_edge`: The specific C-edge to build the verification unit for.
    ///
    /// # Returns
    /// An `Option<VerificationUnit>`. Returns `Some(VerificationUnit)` if successful,
    /// or `None` if the provided edge is not a C-edge or if essential information is missing.
    pub fn build(&self, c_edge: &Edge) -> Option<VerificationUnit> {
        if c_edge.edge_type != EdgeType::C {
            return None;
        }

        let sc_node1 = self.sc_graph.nodes.get(c_edge.source)?;
        let sc_node2 = self.sc_graph.nodes.get(c_edge.target)?;

        if sc_node1.cfg_node_id != sc_node2.cfg_node_id {
            return None;
        }
        let conflict_node_id = sc_node1.cfg_node_id;

        if sc_node1.cfg_function_id == sc_node2.cfg_function_id {
            return None;
        }

        let func1_id = sc_node1.cfg_function_id;
        let hop1_id = sc_node1.cfg_hop_id;
        let func1_cfg = self.cfg_program.functions.get(func1_id)?;

        let func2_id = sc_node2.cfg_function_id;
        let hop2_id = sc_node2.cfg_hop_id;
        let func2_cfg = self.cfg_program.functions.get(func2_id)?;

        // Perform liveness analysis internally
        let liveness_results_func1 = analyze_live_variables(func1_cfg);
        let liveness_results_func2 = analyze_live_variables(func2_cfg);

        let live_vars_at_hop1_exit =
            self.get_live_vars_at_hop_exit(hop1_id, func1_cfg, &liveness_results_func1);
        let live_vars_at_hop2_exit =
            self.get_live_vars_at_hop_exit(hop2_id, func2_cfg, &liveness_results_func2);

        let relevant_tables: Vec<TableId> = self
            .cfg_program
            .nodes
            .get(conflict_node_id)
            .map_or_else(Vec::new, |node_info| node_info.tables.clone());

        Some(VerificationUnit {
            conflict_node: conflict_node_id,
            func1_id,
            hop1_id,
            func2_id,
            hop2_id,
            relevant_tables,
            live_vars_at_hop1_exit,
            live_vars_at_hop2_exit,
        })
    }

    /// Helper method to determine the set of live variables at the exit of a specific hop.
    fn get_live_vars_at_hop_exit(
        &self,
        hop_id: HopId,
        func_cfg: &FunctionCfg,
        liveness_results: &DataflowResults<SetLattice<VarId>>,
    ) -> HashSet<VarId> {
        let mut combined_live_vars = HashSet::new();

        if let Some(hop_cfg) = func_cfg.hops.get(hop_id) {
            for &block_id in &hop_cfg.blocks {
                if let Some(block_cfg) = func_cfg.blocks.get(block_id) {
                    match &block_cfg.terminator {
                        Terminator::HopExit { .. } => {
                            if let Some(block_exit_state) = liveness_results.exit.get(&block_id) {
                                combined_live_vars.extend(block_exit_state.set.iter().cloned());
                            }
                        }
                        Terminator::Return(opt_operand) => {
                            if let Some(block_exit_state) = liveness_results.exit.get(&block_id) {
                                combined_live_vars.extend(block_exit_state.set.iter().cloned());
                            }
                            if let Some(Operand::Var(var_id)) = opt_operand {
                                combined_live_vars.insert(*var_id);
                            }
                        }
                        Terminator::Abort => {
                            if let Some(block_exit_state) = liveness_results.exit.get(&block_id) {
                                combined_live_vars.extend(block_exit_state.set.iter().cloned());
                            }
                        }
                        _ => {
                            // Other terminators (Goto, Branch) don't signify the end of a hop's liveness concern
                        }
                    }
                }
            }
        }
        combined_live_vars
    }
}
