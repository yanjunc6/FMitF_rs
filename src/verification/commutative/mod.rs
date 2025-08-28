use crate::cfg::{CfgProgram, FunctionId, HopId};
use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge};
use crate::verification::errors::Results;
pub use crate::verification::errors::{SpannedError, VerificationError};
use crate::verification::Boogie::{self, gen_Boogie::BoogieProgramGenerator, BoogieProcedure};
use rayon::prelude::*;
use std::collections::HashSet;

mod boogie_helpers;
mod interleaving_generator;
mod slice_analyzer;

use boogie_helpers::{BoogieStateManager, VariableSnapshots};
use interleaving_generator::{InterleavingGenerator, SpecialInterleavings};
use slice_analyzer::SliceAnalyzer;

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
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        // Create simple commutative units from the SC-graph
        self.create_simple_commutative_units(sc_graph);

        // Process units in parallel using rayon
        let results: Result<
            Vec<Option<Boogie::BoogieProgram>>,
            Vec<crate::verification::errors::SpannedError>,
        > = self
            .commutative_units
            .par_iter()
            .map(
                |unit| -> Result<
                    Option<Boogie::BoogieProgram>,
                    Vec<crate::verification::errors::SpannedError>,
                > {
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

                        // Create a temporary manager for this parallel execution
                        let temp_manager = CommutativeVerificationManager::new();

                        // Generate commutative verification procedure
                        match temp_manager.create_commutative_verification_procedure(
                            &mut generator,
                            cfg_program,
                            &sc_graph,
                            &unit,
                        ) {
                            Ok(procedure) => {
                                generator.program.procedures.push(procedure);
                                Ok(Some(generator.program))
                            }
                            Err(e) => Err(e),
                        }
                    } else {
                        Ok(None)
                    }
                },
            )
            .collect();

        // Filter out None values and collect results
        let programs = results?.into_iter().filter_map(|opt| opt).collect();

        Ok(programs)
    }

    /// Create a procedure to verify slice commutativity
    fn create_commutative_verification_procedure(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        _sc_graph: &SCGraph, // underscore prefix to indicate unused parameter
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
        // let interleavings = interleaving_gen.generate_legal_interleavings(sc_graph, unit);
        let special = interleaving_gen.extract_special_interleavings(&unit.hops_A, &unit.hops_B);

        // Analyze liveness for both slices
        let analysis_info = analyzer.analyze_slice_info(cfg_program, unit)?;

        // Generate procedure parameters (empty for this verification)
        let params = Vec::new();

        // Collect all modified globals using analysis info
        let mut modifies = HashSet::new();
        // Add all table variables that are read or written by either slice
        // (since all of them get havoc'd, which counts as assignment in Boogie)
        modifies.extend(analysis_info.tables_read_a.iter().cloned());
        modifies.extend(analysis_info.tables_written_a.iter().cloned());
        modifies.extend(analysis_info.tables_read_b.iter().cloned());
        modifies.extend(analysis_info.tables_written_b.iter().cloned());

        // Create the procedure and add it to the generator
        let procedure = BoogieProcedure {
            name: procedure_name,
            params,
            local_vars: Vec::new(), // Will be populated automatically
            modifies: modifies.into_iter().collect(),
            lines: Vec::new(), // Will be populated by generator methods
        };

        generator.program.procedures.push(procedure);
        let proc_index = generator.program.procedures.len() - 1;
        generator.set_current_procedure(proc_index);

        // Add procedure header comment
        generator.add_comment_to_current_procedure(format!(
            "Slice commutativity verification: hop {} vs hop {}",
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.hop_id.index()
        ));

        // Step 1: Havoc all tables and live-in variables to create initial state
        state_manager.havoc_initial_state(generator, cfg_program, &analysis_info, unit)?;

        // Step 2: Save initial state for restoration
        state_manager.save_initial_state(generator, cfg_program, &analysis_info, unit)?;

        // Step 3: Execute the two special interleavings and save their final states
        let (a_then_b_vars, b_then_a_vars) = self.execute_special_interleavings(
            generator,
            cfg_program,
            &special,
            &analysis_info,
            &state_manager,
            unit,
        )?;

        // Step 4: Verify that the two special interleavings produce equivalent results
        self.verify_special_interleavings_equivalence(
            generator,
            cfg_program,
            &analysis_info,
            &a_then_b_vars,
            &b_then_a_vars,
            &state_manager,
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.hop_id.index(),
        )?;

        // Step 5: For each legal interleaving, verify it produces one of the special results
        // for (i, interleaving) in interleavings.iter().enumerate() {
        //     self.verify_interleaving_equivalence(
        //         generator,
        //         cfg_program,
        //         interleaving,
        //         i,
        //         &analysis_info,
        //         &a_then_b_vars,
        //         &b_then_a_vars,
        //         &state_manager,
        //         unit.c_edge.source.hop_id.index(),
        //         unit.c_edge.target.hop_id.index(),
        //     )?;
        // }

        // Clear current procedure reference
        generator.clear_current_procedure();

        // Return the completed procedure
        Ok(generator.program.procedures.pop().unwrap())
    }

    /// Execute the two special interleavings and return their final state variable names
    fn execute_special_interleavings(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        special: &SpecialInterleavings,
        analysis_info: &slice_analyzer::SliceAnalysisInfo,
        state_manager: &BoogieStateManager,
        unit: &CommutativeUnit,
    ) -> Results<(VariableSnapshots, VariableSnapshots)> {
        generator.add_comment_to_current_procedure(
            "--- Step 3: Execute special interleavings ---".to_string(),
        );

        // Execute A then B
        generator.add_comment_to_current_procedure("Executing A then B:".to_string());
        let a_then_b_vars = self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            &special.a_then_b,
            "a_then_b",
            analysis_info,
            true,
            state_manager,
        )?;

        // Reset state
        state_manager.restore_initial_state(generator, cfg_program, analysis_info, unit)?;

        // Execute B then A
        generator.add_comment_to_current_procedure("Executing B then A:".to_string());
        let b_then_a_vars = self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            &special.b_then_a,
            "b_then_a",
            analysis_info,
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
        should_snapshot: bool,
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
                is_last_hop,
            )?;
        }

        // Snapshot final state if requested
        if should_snapshot {
            state_manager.snapshot_final_state(generator, cfg_program, analysis_info, suffix)
        } else {
            Ok(VariableSnapshots::empty())
        }
    }

    /// Verify that the two special interleavings (A→B and B→A) produce equivalent results
    fn verify_special_interleavings_equivalence(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &slice_analyzer::SliceAnalysisInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
        state_manager: &BoogieStateManager,
        hop_id_a: usize,
        hop_id_b: usize,
    ) -> Results<()> {
        generator.add_comment_to_current_procedure(
            "--- Step 4: Verify A→B ≡ B→A (Special interleavings equivalence) ---".to_string(),
        );

        state_manager.assert_special_interleavings_equivalence(
            generator,
            cfg_program,
            analysis_info,
            a_then_b_vars,
            b_then_a_vars,
            hop_id_a,
            hop_id_b,
        )?;

        Ok(())
    }

    // Verify that a specific interleaving produces equivalent results to the special interleavings
    // fn verify_interleaving_equivalence(
    //     &self,
    //     generator: &mut BoogieProgramGenerator,
    //     cfg_program: &CfgProgram,
    //     interleaving: &Interleaving,
    //     interleaving_index: usize,
    //     analysis_info: &slice_analyzer::SliceAnalysisInfo,
    //     a_then_b_vars: &VariableSnapshots,
    //     b_then_a_vars: &VariableSnapshots,
    //     state_manager: &BoogieStateManager,
    //     hop_id_a: usize,
    //     hop_id_b: usize,
    // ) -> Results<()> {
    //     generator.add_comment_to_current_procedure(format!(
    //         "--- Verifying interleaving {} ---",
    //         interleaving_index + 1
    //     ));

    //     // Reset to initial state before executing this interleaving
    //     state_manager.restore_initial_state(generator, cfg_program, analysis_info)?;

    //     // Execute the interleaving (without snapshotting intermediate states)
    //     self.execute_interleaving_and_snapshot(
    //         generator,
    //         cfg_program,
    //         interleaving,
    //         &format!("interleaving_{}", interleaving_index),
    //         analysis_info,
    //         false, // Don't snapshot final state
    //         state_manager,
    //     )?;

    //     // Assert equivalence to one of the special interleavings
    //     state_manager.assert_equivalence_to_special_interleavings(
    //         generator,
    //         cfg_program,
    //         analysis_info,
    //         a_then_b_vars,
    //         b_then_a_vars,
    //         hop_id_a,
    //         hop_id_b,
    //     )?;

    //     Ok(())
    // }
}
