// src/verify/mod.rs

pub mod commutativity_check;
pub mod code_generation;
pub mod interleaving;
pub mod execution;

use crate::cfg::CfgProgram;
use crate::sc_graph::{SCGraph, EdgeType};
use execution::{VerificationResult};

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
        // TODO
    }
}