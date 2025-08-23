use crate::verification::errors::Results;
pub use crate::verification::errors::{SpannedError, VerificationError};
use crate::verification::Boogie::{self, gen_Boogie::BoogieProgramGenerator, BoogieLine, BoogieProcedure};
use crate::cfg::{CfgProgram, FunctionId, HopId};
use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge};
use std::collections::HashSet;

mod interleaving_generator;
mod slice_analyzer;
mod boogie_helpers;

use interleaving_generator::{InterleavingGenerator, SpecialInterleavings};
use slice_analyzer::SliceAnalyzer;
use boogie_helpers::{BoogieStateManager, VariableSnapshots};

pub struct CommutativeUnit {
    pub c_edge: SCGraphEdge,
    pub hops_A: Vec<HopId>, // continuous hops where the last hop is source of c_edge
    pub hops_B: Vec<HopId>, // continuous hops where the last hop is target of c_edge
    pub func_id_A: FunctionId,
    pub func_id_B: FunctionId,
}

/// Represents an interleaving as a sequence of (hop_id, is_from_slice_a)
pub type Interleaving = Vec<(HopId, bool)>;

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
            let slice_a: &Vec<HopId> = &unit.hops_A;
            let slice_b: &Vec<HopId> = &unit.hops_B;

            if !slice_a.is_empty() && !slice_b.is_empty() {
                let mut program = base_program.clone();
                program.name = format!(
                    "commutative_simple_hop{}_vs_hop{}",
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
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
        unit: &CommutativeUnit,
    ) -> Results<BoogieProcedure> {
        let procedure_name = format!(
            "Check_SliceCommut_Hop{}_vs_Hop{}",
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.hop_id.index()
        );

        // Create helper components
        let interleaving_gen = InterleavingGenerator::new();
        let analyzer = SliceAnalyzer::new();
        let state_manager = BoogieStateManager::new();

        // Generate all legal interleavings
        let interleavings = interleaving_gen.generate_legal_interleavings(sc_graph, unit);
        let special = interleaving_gen.extract_special_interleavings(&unit.hops_A, &unit.hops_B);

        // Analyze liveness for both slices
        let analysis_info = analyzer.analyze_slice_info(cfg_program, unit)?;

        let mut lines = Vec::new();

        // Add procedure header comment
        BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "Slice commutativity verification: hop {} vs hop {}",
                unit.c_edge.source.hop_id.index(),
                unit.c_edge.target.hop_id.index()
            ),
        );

        // Step 1: Havoc all tables and live-in variables to create initial state
        state_manager.havoc_initial_state(generator, cfg_program, &analysis_info, &mut lines)?;

        // Step 2: Save initial state for restoration
        state_manager.save_initial_state(generator, cfg_program, &analysis_info, &mut lines)?;

        // Step 3: Execute the two special interleavings and save their final states
        let (a_then_b_vars, b_then_a_vars) = self.execute_special_interleavings(
            generator,
            cfg_program,
            &special,
            &analysis_info,
            &mut lines,
            &state_manager,
        )?;

        // Step 4: For each legal interleaving, verify it produces one of the special results
        for (i, interleaving) in interleavings.iter().enumerate() {
            self.verify_interleaving_equivalence(
                generator,
                cfg_program,
                interleaving,
                i,
                &analysis_info,
                &a_then_b_vars,
                &b_then_a_vars,
                &mut lines,
                &state_manager,
            )?;
        }

        // Generate procedure parameters (empty for this verification)
        let params = Vec::new();

        // Collect all modified globals using analysis info
        let mut modifies = HashSet::new();

        // Add table variables that are written by either slice
        modifies.extend(analysis_info.tables_written_a.iter().cloned());
        modifies.extend(analysis_info.tables_written_b.iter().cloned());

        let procedure = BoogieProcedure {
            name: procedure_name,
            params,
            modifies: modifies.into_iter().collect(),
            lines,
        };

        Ok(procedure)
    }

    /// Execute the two special interleavings and return their final state variable names
    fn execute_special_interleavings(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        special: &SpecialInterleavings,
        analysis_info: &slice_analyzer::SliceAnalysisInfo,
        lines: &mut Vec<BoogieLine>,
        state_manager: &BoogieStateManager,
    ) -> Results<(VariableSnapshots, VariableSnapshots)> {
        BoogieProgramGenerator::add_comment(
            lines,
            "--- Step 3: Execute special interleavings ---".to_string(),
        );

        // Execute A then B
        BoogieProgramGenerator::add_comment(lines, "Executing A then B:".to_string());
        let a_then_b_vars = self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            &special.a_then_b,
            "a_then_b",
            analysis_info,
            lines,
            true,
            state_manager,
        )?;

        // Reset state
        state_manager.restore_initial_state(generator, cfg_program, analysis_info, lines)?;

        // Execute B then A
        BoogieProgramGenerator::add_comment(lines, "Executing B then A:".to_string());
        let b_then_a_vars = self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            &special.b_then_a,
            "b_then_a",
            analysis_info,
            lines,
            true,
            state_manager,
        )?;

        Ok((a_then_b_vars, b_then_a_vars))
    }

    /// Execute a specific interleaving and snapshot the final state
    fn execute_interleaving_and_snapshot(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        interleaving: &Interleaving,
        suffix: &str,
        analysis_info: &slice_analyzer::SliceAnalysisInfo,
        lines: &mut Vec<BoogieLine>,
        is_special: bool,
        state_manager: &BoogieStateManager,
    ) -> Results<VariableSnapshots> {
        // Execute each hop in the interleaving
        for &(hop_id, is_from_a) in interleaving.iter() {
            let label_suffix = if is_from_a {
                format!("A_{}", suffix)
            } else {
                format!("B_{}", suffix)
            };
            let function_name = if is_from_a { "functionA" } else { "functionB" };
            // check if it is last hop that has same value of is_from_a
            let is_last_hop = {
                let mut found = false;
                for &(next_hop_id, next_is_from_a) in interleaving.iter().rev() {
                    if next_is_from_a == is_from_a {
                        if next_hop_id == hop_id {
                            found = true;
                        }
                        break;
                    }
                }
                found
            };
            state_manager.execute_hop_with_unique_labels(
                generator,
                cfg_program,
                hop_id,
                &label_suffix,
                function_name,
                lines,
                is_last_hop,
            )?;
        }

        // Snapshot final state if this is a special interleaving
        if is_special {
            state_manager.snapshot_final_state(generator, cfg_program, analysis_info, suffix, lines)
        } else {
            // For regular interleavings, just return empty snapshots
            Ok(VariableSnapshots::empty())
        }
    }

    /// Verify that each interleaving produces results equivalent to one of the special interleavings
    fn verify_interleaving_equivalence(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        interleaving: &Interleaving,
        interleaving_index: usize,
        analysis_info: &slice_analyzer::SliceAnalysisInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
        lines: &mut Vec<BoogieLine>,
        state_manager: &BoogieStateManager,
    ) -> Results<()> {
        BoogieProgramGenerator::add_comment(
            lines,
            format!("--- Verifying interleaving {} ---", interleaving_index),
        );

        // Reset to initial state
        state_manager.restore_initial_state(generator, cfg_program, analysis_info, lines)?;

        // Execute this interleaving
        let suffix = format!("interleaving_{}", interleaving_index);
        self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            interleaving,
            &suffix,
            analysis_info,
            lines,
            false,
            state_manager,
        )?;

        // Generate assertions comparing final state to special interleavings
        state_manager.assert_equivalence_to_special_interleavings(
            generator,
            cfg_program,
            analysis_info,
            a_then_b_vars,
            b_then_a_vars,
            lines,
        )?;

        Ok(())
    }
}

