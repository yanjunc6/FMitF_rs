use crate::cfg::{FunctionId, HopId, Program as CfgProgram};
use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge};
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::Boogie::{self, BoogieProcedure};
use rayon::prelude::*;
use std::collections::HashMap;

mod boogie_helpers;
pub mod splitter;
mod interleaving_generator;
mod slice_analyzer;
mod strategy;

// Re-exported modules used by strategy
use strategy::CommutativeStrategy;

#[derive(Clone)]
pub struct CommutativeUnit {
    pub c_edge: SCGraphEdge,
    /// Continuous hops per slice; slice 0 corresponds to c_edge.source, slice 1 to c_edge.target
    pub hops_per_slice: HashMap<SliceId, Vec<HopId>>,
    pub func_per_slice: HashMap<SliceId, FunctionId>,
    /// Optional block subset per slice for recursive splitting.
    /// If absent, the whole hop blocks are used.
    pub blocks_per_slice: HashMap<SliceId, Vec<crate::cfg::BasicBlockId>>,
}

/// Represents an interleaving as a sequence of (slice_id, hop_id)
pub type Interleaving = Vec<(SliceId, HopId)>;

pub struct CommutativeVerificationManager {
    pub commutative_units: Vec<CommutativeUnit>,
}

impl CommutativeVerificationManager {
    pub fn new() -> Self {
        CommutativeVerificationManager {
            commutative_units: Vec::new(),
        }
    }

    pub fn create_unit_for_edge(edge: &SCGraphEdge) -> CommutativeUnit {
        let mut unit = CommutativeUnit {
            c_edge: edge.clone(),
            hops_per_slice: HashMap::new(),
            func_per_slice: HashMap::new(),
            blocks_per_slice: HashMap::new(),
        };
        unit.hops_per_slice.insert(0, vec![edge.source.hop_id]);
        unit.hops_per_slice.insert(1, vec![edge.target.hop_id]);
        unit.func_per_slice.insert(0, edge.source.function_id);
        unit.func_per_slice.insert(1, edge.target.function_id);
        unit
    }

    /// Create simple commutative units from the SC-graph with only the hops on the ends
    pub fn create_simple_commutative_units(&mut self, sc_graph: &SCGraph) {
        // Analyze the SC-graph to identify commutative units
        for edge in &sc_graph.edges {
            if edge.edge_type == EdgeType::C {
                self.commutative_units.push(Self::create_unit_for_edge(edge));
            }
        }
    }

    pub fn build_boogie_program_for_unit(
        cfg_program: &CfgProgram,
        unit: &CommutativeUnit,
    ) -> Results<Option<Boogie::BoogieProgram>> {
        let slice0 = unit.hops_per_slice.get(&0).cloned().unwrap_or_default();
        let slice1 = unit.hops_per_slice.get(&1).cloned().unwrap_or_default();

        if slice0.is_empty() || slice1.is_empty() {
            return Ok(None);
        }

        let mut unit_base = BaseVerificationGenerator::new(cfg_program);
        unit_base.generator.program.name = format!(
            "commutative_f{}_i{}_h{}_vs_f{}_i{}_h{}",
            unit.c_edge.source.function_id.index(),
            unit.c_edge.source.instance,
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.function_id.index(),
            unit.c_edge.target.instance,
            unit.c_edge.target.hop_id.index()
        );

        let procedure = Self::create_commutative_verification_procedure(&mut unit_base, cfg_program, unit)?;
        unit_base.generator.program.procedures.push(procedure);
        Ok(Some(unit_base.generator.program))
    }

    /// Generate Commutative (Slice Commutativity) verification Boogie programs
    /// Each pair of conflicting transaction slices gets its own verification
    pub fn generate_commutative_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        // Create simple commutative units from the SC-graph
        self.create_simple_commutative_units(sc_graph);

        // Process units in parallel using rayon
        let results: Result<
            Vec<Option<Boogie::BoogieProgram>>,
            Vec<crate::util::CompilerError>,
        > = self
            .commutative_units
            .par_iter()
            .map(|unit| -> Result<
                Option<Boogie::BoogieProgram>,
                Vec<crate::util::CompilerError>,
            > {
                Self::build_boogie_program_for_unit(cfg_program, unit)
            })
            .collect();

        // Filter out None values and collect results
        let programs = results?.into_iter().filter_map(|opt| opt).collect();

        Ok(programs)
    }

    /// Create a procedure to verify slice commutativity
    fn create_commutative_verification_procedure(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        unit: &CommutativeUnit,
    ) -> Results<BoogieProcedure> {
        let mut strat = CommutativeStrategy::new(base, cfg_program, unit);
        strat.run()
    }
}
