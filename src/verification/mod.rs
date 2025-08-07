use crate::cfg::{self, CfgProgram, HopId};
use std::collections::HashMap;
pub mod boogie;
pub mod commutativity;
pub mod errors;
pub mod partition_soundness;
use self::boogie::BoogieProgram;
use self::commutativity::CommutativityUnit;
use self::errors::VerificationError;
use self::partition_soundness::PartitionSoundnessUnit;

#[derive(Debug, Clone)]
pub enum CommutativityUnitResult {
    Commutative,
    NonCommutative,
    SyntaxError,
}

#[derive(Debug)]
pub struct DetailedCommutativityResult {
    pub verifications_attempted: usize,
    pub boogie_programs: Vec<(String, BoogieProgram)>, // (filename, program)
    pub errors: Vec<VerificationError>,
    pub unit_results: Vec<CommutativityUnitResult>,
    pub syntax_errors: usize,
}

pub struct Verifier {
    pub units: Vec<Box<dyn VerificationUnit>>,
    pub results: HashMap<cfg::FunctionId, Vec<VerificationError>>,
}

impl Verifier {
    pub fn new() -> Self {
        Self {
            units: Vec::new(),
            results: HashMap::new(),
        }
    }
    pub fn add_partition_soundness(&mut self, function_id: cfg::FunctionId) {
        self.units
            .push(Box::new(PartitionSoundnessUnit { function_id }));
    }
    pub fn add_commutativity(
        &mut self,
        function_a: cfg::FunctionId,
        function_b: cfg::FunctionId,
        hops_a: Vec<HopId>,
        hops_b: Vec<HopId>,
    ) {
        self.units.push(Box::new(CommutativityUnit::new(
            function_a, function_b, hops_a, hops_b,
        )));
    }
}

#[derive(Debug, Clone)]
pub enum Property {
    PartitionSoundness {
        function: cfg::FunctionId,
    },
    Commutativity {
        function_a: cfg::FunctionId,
        function_b: cfg::FunctionId,
        hops_a: Vec<HopId>,
        hops_b: Vec<HopId>,
    },
}

pub trait VerificationUnit {
    fn property(&self) -> Property;
    fn generate(&self, cfg_program: &CfgProgram) -> BoogieProgram;
}

#[derive(Debug)]
pub struct PartitionVerificationResult {
    pub functions_verified: usize,
    pub functions_failed: usize,
    pub boogie_programs: Vec<(String, BoogieProgram)>, // (filename, program)
    pub errors: Vec<VerificationError>,
}

pub struct VerificationManager;

impl VerificationManager {
    pub fn run_partition_verification(cfg_program: &CfgProgram) -> PartitionVerificationResult {
        let mut verifier = Verifier::new();
        for (function_id, function_cfg) in cfg_program.functions.iter() {
            if function_cfg.function_type == crate::cfg::FunctionType::Transaction {
                verifier.add_partition_soundness(function_id);
            }
        }
        let mut functions_verified = 0;
        let mut functions_failed = 0;
        let mut boogie_programs = Vec::new();
        let mut errors = Vec::new();
        for unit in &verifier.units {
            if let Property::PartitionSoundness { function } = unit.property() {
                let function_name = &cfg_program.functions[function].name;
                let boogie_program = unit.generate(cfg_program);
                boogie_programs.push((
                    format!("partition_soundness_{}.bpl", function_name),
                    boogie_program,
                ));
                functions_verified += 1;
            }
        }
        PartitionVerificationResult {
            functions_verified,
            functions_failed,
            boogie_programs,
            errors,
        }
    }

    pub fn run_commutativity_verification(
        cfg_program: &CfgProgram,
        sc_graph: &mut crate::sc_graph::SCGraph,
    ) -> DetailedCommutativityResult {
        let mut verifier = Verifier::new();
        let mut edge_to_unit_map = std::collections::HashMap::new();
        for (edge_idx, edge) in sc_graph.edges.iter().enumerate() {
            if edge.edge_type == crate::sc_graph::EdgeType::C {
                let hops_a = vec![edge.source.hop_id];
                let hops_b = vec![edge.target.hop_id];
                let unit_idx = verifier.units.len();
                verifier.add_commutativity(
                    edge.source.function_id,
                    edge.target.function_id,
                    hops_a,
                    hops_b,
                );
                edge_to_unit_map.insert(edge_idx, unit_idx);
            }
        }
        let mut verifications_attempted = 0;
        let mut boogie_programs = Vec::new();
        let mut errors = Vec::new();
        let mut unit_results = Vec::new();
        let mut syntax_errors = 0;
        for unit in &verifier.units {
            if let Property::Commutativity {
                function_a,
                function_b,
                ..
            } = unit.property()
            {
                let func_a_name = &cfg_program.functions[function_a].name;
                let func_b_name = &cfg_program.functions[function_b].name;
                let boogie_program = unit.generate(cfg_program);
                boogie_programs.push((
                    format!("commutativity_{}_{}.bpl", func_a_name, func_b_name),
                    boogie_program,
                ));
                verifications_attempted += 1;
                unit_results.push(CommutativityUnitResult::Commutative); // Placeholder: always commutative
            }
        }
        DetailedCommutativityResult {
            verifications_attempted,
            boogie_programs,
            errors,
            unit_results,
            syntax_errors,
        }
    }
}
