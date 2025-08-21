use super::errors::Results;
pub use super::errors::{SpannedError, VerificationError};
use super::Boogie::{self, BoogieExpr, BoogieExprKind};
use crate::cfg::{CfgProgram, FunctionId, HopId};
use crate::sc_graph::{self, EdgeType, SCGraph, SCGraphEdge, SCGraphNodeId};

pub struct CommutativeUnit {
    pub c_edge: SCGraphEdge,
    pub hops_A: Vec<HopId>, // continuous hops where the last hop is source of c_edge
    pub hops_B: Vec<HopId>, // continuous hops where the last hop is target of c_edge
    pub func_id_A: FunctionId,
    pub func_id_B: FunctionId,
}

pub struct CommutativeVerificationManager {
    pub commutative_units: Vec<CommutativeUnit>,
}

impl CommutativeVerificationManager {
    pub fn new() -> Self {
        CommutativeVerificationManager {
            commutative_units: Vec::new(),
        }
    }

    /// Create simple commutative units from the SC-graph with only the hops on the ends
    pub fn create_simple_commutative_units(&mut self, sc_graph: &SCGraph) {
        // Analyze the SC-graph to identify commutative units
        for edge in &sc_graph.edges {
            if edge.edge_type == EdgeType::C {
                let mut unit = CommutativeUnit {
                    c_edge: edge.clone(),
                    hops_A: Vec::new(),
                    hops_B: Vec::new(),
                    func_id_A: edge.source.function_id,
                    func_id_B: edge.target.function_id,
                };
                unit.hops_A.push(edge.source.hop_id);
                unit.hops_B.push(edge.target.hop_id);
                self.commutative_units.push(unit);
            }
        }
    }

    /// Generate Commutative (Slice Commutativity) verification Boogie programs
    /// Each pair of conflicting transaction slices gets its own verification
    pub fn generate_commutative_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let mut programs = Vec::new();
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        // Create simple commutative units from the SC-graph
        self.create_simple_commutative_units(sc_graph);

        for unit in self.commutative_units.iter() {
            let func_id_A = unit.func_id_A;
            let func_id_B = unit.func_id_B;

            let slice_a: &Vec<HopId> = &unit.hops_A;
            let slice_b: &Vec<HopId> = &unit.hops_B;

            if !slice_a.is_empty() && !slice_b.is_empty() {
                let mut program = base_program.clone();
                program.name = format!(
                    "commutative_Hop{}_vs_Hop{}",
                    unit.c_edge.source.hop_id.index(),
                    unit.c_edge.target.hop_id.index()
                );

                let mut generator =
                    Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);

                // Generate commutative verification procedure
                let procedure = self.create_commutative_verification_procedure(
                    &mut generator,
                    cfg_program,
                    &sc_graph,
                    &unit,
                )?;

                generator.program.procedures.push(procedure);
                programs.push(generator.program);
            }
        }
        Ok(programs)
    }

    /// Create a procedure to verify slice commutativity
    fn create_commutative_verification_procedure(
        &self,
        _generator: &mut Boogie::gen_Boogie::BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
        unit: &CommutativeUnit,
    ) -> Results<Boogie::BoogieProcedure> {
        let procedure_name = format!(
            "Check_SliceCommut_Hop{}_vs_Hop{}",
            unit.c_edges.source.hop_id.index(), // note it is compare with the LAST hop in each hops sequence
            unit.c_edges.target.hop_id.index() // note it is compare with the LAST hop in each hops sequence
        );
        // First call generate_legal_interleavings to find all legal interleavings, there are two special interleaving, one is slice_a + slice_b,
        //     the other is slice_b + slice_a.
        // These two interleaving should be treated specially

        // Second, use liveness analysis find the live in and live out variable for each slice.

        // Third, havoc all the table, including the live in variables.
        // Copy the havoc table, live in variables

        // For each interleaving:
        // 1. at beginning, reverse the table and live in variable back to copied state (so that each interleaving uses the same starting point)

        // 2. generate two special interleaving: slice_a + slice_b and slice_b + slice_a, store there table state and live out var with special name

        // 3. generate the hops according to the interleaving, note, since all the stuff is in the same procedure, you need to make sure
        //    the label is different (variable can be the same though)
        // my suggestion is to change the gen_Boogie.rs function related to label generation, let it accept an option with enumerate number,
        //    then, gen label with this number (similar to variable gen prefix)

        // At the end of each interleaving, assert, all the table written by last hop (through dataflow analysis table_mod_ref.rs) and live out variable
        //    are either the same as slice_a + slice_b one or slice_b + slice_a one
        todo!()
    }

    /// Generate all legal interleavings of two hop slices
    fn generate_legal_interleavings(
        &self,
        sc_graph: &SCGraph,
        unit: &CommutativeUnit
    ) -> Vec<Vec<(HopId, bool)>> {
        // Returns list of interleavings, where each element is (hop_id, is_from_slice_a)

        // first find all the conflict edge between two slices of hops in sc_graph, we should ignore the c_edge in unit

        #[derive(Clone, Copy, PartialEq)]
        enum Direction {
            ABeforeB,
            BBeforeA,
        }


        // generate all the interleaving that satisfy either:
        // for all conflict edge (hop1, hop2) between slice_a and slice_b,  hop1 is placed before hop2,
        // or for all conflict edge (hop1, hop2) between slice_a and slice_b, hop2 is placed before hop1.
        // again please ignore the c_edges in unit (you should use the set find before)

        // please change the return type so that, there are two special interleaving, one is slice_a + slice_b,
        // the other is slice_b + slice_a, these two will be treated individually.
        todo!()
    }
}
