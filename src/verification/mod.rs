// src/verify/mod.rs

pub mod commutativity_check;
pub mod code_generation;
pub mod interleaving;
pub mod execution;
pub mod results;

use crate::cfg::CfgProgram;
use crate::sc_graph::{SCGraph, EdgeType};
use execution::VerificationExecution;

// Re-export key types for CLI
pub use self::results::VerificationResults;

pub struct VerificationManager<'a> {
    pub cfg: &'a CfgProgram,
    pub sc_graph: &'a SCGraph,
}

impl<'a> VerificationManager<'a> {
    pub fn new(
        cfg: &'a CfgProgram,
        sc_graph: &'a SCGraph,
    ) -> Self {
        Self { cfg, sc_graph }
    }

    /// 1) For each C-edge, build a VerificationUnit
    ///     1.1) Generate Boogie (generate_boogie_for_unit)
    ///     1.2) Run the solver (execute_boogie)
    /// 2) return a VerificationExecution object
    pub fn run_commutativity_pipeline(&self) -> VerificationExecution {
        let mut execution = VerificationExecution::new();
        
        // Get all C-edges (commutativity edges) from the SC graph
        let c_edges: Vec<_> = self.sc_graph.edges.iter()
            .filter(|edge| edge.edge_type == EdgeType::C)
            .collect();
        
        // Process each C-edge
        for edge in c_edges {
            // 1) Create a VerificationUnit for this C-edge
            let verification_unit = commutativity_check::create_verification_unit(
                edge.clone(),
                self.cfg,
                self.sc_graph,
            );
            
            // 2) Generate Boogie code for this unit
            let boogie_code = code_generation::generate_boogie_for_unit(&verification_unit);
            
            // 3) Execute the Boogie verification and store the result
            execution.execute_boogie(edge.clone(), boogie_code);
        }
        
        execution
    }
}