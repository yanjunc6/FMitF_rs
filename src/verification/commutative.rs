use super::errors::Results;
use super::Boogie;
use crate::cfg::{CfgProgram, HopId};

pub use super::errors::{SpannedError, VerificationError};

pub struct CommutativeVerificationManager {}

impl CommutativeVerificationManager {
    /// Generate Commutative (Slice Commutativity) verification Boogie programs
    /// Each pair of conflicting transaction slices gets its own verification
    pub fn generate_commutative_verification(
        &self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let mut programs = Vec::new();
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        // For commutative verification, we need to analyze conflicts between transaction slices
        // This requires SC-graph information which is not available here yet

        // Placeholder: Generate commutative verification for pairs of transaction functions
        let transaction_functions: Vec<_> = cfg_program
            .root_functions
            .iter()
            .filter(|&&func_id| {
                cfg_program.functions[func_id].function_type
                    == crate::cfg::FunctionType::Transaction
            })
            .collect();

        for (i, &&func_a_id) in transaction_functions.iter().enumerate() {
            for &&func_b_id in transaction_functions.iter().skip(i + 1) {
                let func_a = &cfg_program.functions[func_a_id];
                let func_b = &cfg_program.functions[func_b_id];

                // Create a slice from each function's hops
                let slice_a: Vec<HopId> = func_a.hops.clone();
                let slice_b: Vec<HopId> = func_b.hops.clone();

                // Placeholder conflict edges (would need SC-graph analysis)
                let c_edges: Vec<(HopId, HopId)> = Vec::new();

                if !slice_a.is_empty() && !slice_b.is_empty() {
                    let mut program = base_program.clone();
                    program.name = format!("{}_vs_{}_commutative", func_a.name, func_b.name);

                    let mut generator =
                        Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);

                    // Generate commutative verification procedure
                    let procedure = self.create_commutative_verification_procedure(
                        &mut generator,
                        cfg_program,
                        &slice_a,
                        &slice_b,
                        &c_edges,
                        &func_a.name,
                        &func_b.name,
                    )?;

                    generator.program.procedures.push(procedure);
                    programs.push(generator.program);
                }
            }
        }

        Ok(programs)
    }

    /// Create a procedure to verify slice commutativity
    fn create_commutative_verification_procedure(
        &self,
        _generator: &mut Boogie::gen_Boogie::BoogieProgramGenerator,
        _cfg_program: &CfgProgram,
        slice_a: &[HopId],
        slice_b: &[HopId],
        c_edges: &[(HopId, HopId)],
        func_a_name: &str,
        func_b_name: &str,
    ) -> Results<Boogie::BoogieProcedure> {
        let procedure_name = format!("Check_SliceCommut_{}_vs_{}", func_a_name, func_b_name);

        let mut lines = Vec::new();

        // Add procedure header comment
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "Slice Commutativity Verification: {} vs {}",
                func_a_name, func_b_name
            ),
        );

        // Analyze conflict edges
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "Verifying commutativity between {} conflicting slices",
                if c_edges.is_empty() { "non-" } else { "" }
            ),
        );

        // Step 1: Snapshot initial state
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "1. Snapshot initial world state".to_string(),
        );
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "// var DB_initial := DB; var LiveIns_initial := LiveIns;".to_string(),
        );

        // Step 2: Execute first ordering (slice A then slice B)
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "2. Execute first ordering: {} then {}",
                func_a_name, func_b_name
            ),
        );

        for hop_id in slice_a {
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                &mut lines,
                format!(
                    "call execute_hop_{}(); // from {}",
                    hop_id.index(),
                    func_a_name
                ),
            );
        }

        for hop_id in slice_b {
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                &mut lines,
                format!(
                    "call execute_hop_{}(); // from {}",
                    hop_id.index(),
                    func_b_name
                ),
            );
        }

        // Step 3: Save first result
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "// var DB_first := DB; var LiveOuts_first := LiveOuts;".to_string(),
        );

        // Step 4: Reset and execute reverse ordering (slice B then slice A)
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "3. Reset and execute reverse ordering: {} then {}",
                func_b_name, func_a_name
            ),
        );
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "// DB := DB_initial; LiveIns := LiveIns_initial;".to_string(),
        );

        for hop_id in slice_b {
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                &mut lines,
                format!(
                    "call execute_hop_{}(); // from {}",
                    hop_id.index(),
                    func_b_name
                ),
            );
        }

        for hop_id in slice_a {
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                &mut lines,
                format!(
                    "call execute_hop_{}(); // from {}",
                    hop_id.index(),
                    func_a_name
                ),
            );
        }

        // Step 5: Generate assertions for commutativity
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "4. Assert identical results (commutativity)".to_string(),
        );

        let db_assertion = Boogie::gen_Boogie::BoogieProgramGenerator::gen_true_expr(); // Placeholder

        let db_error = Boogie::ErrorMessage {
            msg: format!(
                "Database states must be identical for commutative slices {} and {}",
                func_a_name, func_b_name
            ),
        };

        Boogie::gen_Boogie::BoogieProgramGenerator::add_assertion(
            &mut lines,
            db_assertion,
            db_error,
        );

        let output_assertion = Boogie::gen_Boogie::BoogieProgramGenerator::gen_true_expr(); // Placeholder

        let output_error = Boogie::ErrorMessage {
            msg: format!(
                "Live-out variables must be identical for commutative slices {} and {}",
                func_a_name, func_b_name
            ),
        };

        Boogie::gen_Boogie::BoogieProgramGenerator::add_assertion(
            &mut lines,
            output_assertion,
            output_error,
        );

        // Step 6: Legal interleaving analysis (if there are conflicts)
        if !c_edges.is_empty() {
            self.add_legal_interleaving_analysis(
                &mut lines,
                slice_a,
                slice_b,
                c_edges,
                func_a_name,
                func_b_name,
            )?;
        }

        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "End of slice commutativity verification".to_string(),
        );

        Ok(Boogie::BoogieProcedure {
            name: procedure_name,
            params: vec![], // Commutative procedures use havoc for initial conditions
            modifies: vec![], // Placeholder
            lines,
        })
    }

    /// Add analysis for legal interleavings when there are conflicts
    fn add_legal_interleaving_analysis(
        &self,
        lines: &mut Vec<Boogie::BoogieLine>,
        slice_a: &[HopId],
        slice_b: &[HopId],
        c_edges: &[(HopId, HopId)],
        func_a_name: &str,
        func_b_name: &str,
    ) -> Results<()> {
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            lines,
            format!(
                "5. Legal interleaving analysis for {} conflicting edges",
                c_edges.len()
            ),
        );

        // Generate all legal interleavings
        let interleavings = self.generate_legal_interleavings(slice_a, slice_b, c_edges);

        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            lines,
            format!(
                "Found {} legal interleavings to verify",
                interleavings.len()
            ),
        );

        // For each legal interleaving, verify it produces same results
        for (interleaving_index, interleaving) in interleavings.iter().enumerate() {
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                lines,
                format!(
                    "Verifying interleaving {} with {} hops",
                    interleaving_index + 1,
                    interleaving.len()
                ),
            );

            // Execute this specific interleaving
            for (hop_id, is_from_a) in interleaving {
                let source_func = if *is_from_a { func_a_name } else { func_b_name };
                Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                    lines,
                    format!(
                        "call execute_hop_{}(); // from {} in interleaving {}",
                        hop_id.index(),
                        source_func,
                        interleaving_index + 1
                    ),
                );
            }

            // Assert this interleaving produces same result
            let interleaving_assertion =
                Boogie::gen_Boogie::BoogieProgramGenerator::gen_true_expr(); // Placeholder

            let interleaving_error = Boogie::ErrorMessage {
                msg: format!(
                    "Interleaving {} of {} vs {} must produce identical results to sequential execution",
                    interleaving_index + 1, func_a_name, func_b_name
                ),
            };

            Boogie::gen_Boogie::BoogieProgramGenerator::add_assertion(
                lines,
                interleaving_assertion,
                interleaving_error,
            );
        }

        Ok(())
    }

    /// Generate all legal interleavings of two hop slices
    fn generate_legal_interleavings(
        &self,
        slice_a: &[HopId],
        slice_b: &[HopId],
        c_edges: &[(HopId, HopId)], // Conflict edges between hops
    ) -> Vec<Vec<(HopId, bool)>> {
        // Returns list of interleavings, where each element is (hop_id, is_from_slice_a)

        // Build conflict lookup for fast checking
        let mut conflicts = std::collections::HashSet::new();
        for &(hop1, hop2) in c_edges {
            conflicts.insert((hop1, hop2));
            conflicts.insert((hop2, hop1)); // Bidirectional
        }

        #[derive(Clone, Copy, PartialEq)]
        enum Direction {
            Unknown,
            ABeforeB,
            BBeforeA,
        }

        let mut result = Vec::new();

        fn dfs(
            slice_a: &[HopId],
            slice_b: &[HopId],
            conflicts: &std::collections::HashSet<(HopId, HopId)>,
            i: usize, // next index in slice_a
            j: usize, // next index in slice_b
            placed: &mut Vec<(HopId, bool)>,
            dir: Direction,
            result: &mut Vec<Vec<(HopId, bool)>>,
        ) {
            // Base case: both slices exhausted
            if i >= slice_a.len() && j >= slice_b.len() {
                result.push(placed.clone());
                return;
            }

            // Try placing next hop from slice_a
            if i < slice_a.len() {
                let hop_a = slice_a[i];
                let mut new_dir = dir;
                let mut valid = true;

                // Check conflicts with previously placed hops from slice_b
                for k in (0..placed.len()).rev() {
                    let (prev_hop, prev_is_a) = placed[k];
                    if !prev_is_a && conflicts.contains(&(hop_a, prev_hop)) {
                        // Found conflict between hop_a and prev_hop (from slice_b)
                        match dir {
                            Direction::Unknown => {
                                new_dir = Direction::ABeforeB;
                                break;
                            }
                            Direction::BBeforeA => {
                                valid = false; // Would violate established direction
                                break;
                            }
                            Direction::ABeforeB => break, // Consistent with direction
                        }
                    }
                }

                if valid {
                    placed.push((hop_a, true));
                    dfs(
                        slice_a,
                        slice_b,
                        conflicts,
                        i + 1,
                        j,
                        placed,
                        new_dir,
                        result,
                    );
                    placed.pop();
                }
            }

            // Try placing next hop from slice_b
            if j < slice_b.len() {
                let hop_b = slice_b[j];
                let mut new_dir = dir;
                let mut valid = true;

                // Check conflicts with previously placed hops from slice_a
                for k in (0..placed.len()).rev() {
                    let (prev_hop, prev_is_a) = placed[k];
                    if prev_is_a && conflicts.contains(&(hop_b, prev_hop)) {
                        // Found conflict between hop_b and prev_hop (from slice_a)
                        match dir {
                            Direction::Unknown => {
                                new_dir = Direction::BBeforeA;
                                break;
                            }
                            Direction::ABeforeB => {
                                valid = false; // Would violate established direction
                                break;
                            }
                            Direction::BBeforeA => break, // Consistent with direction
                        }
                    }
                }

                if valid {
                    placed.push((hop_b, false));
                    dfs(
                        slice_a,
                        slice_b,
                        conflicts,
                        i,
                        j + 1,
                        placed,
                        new_dir,
                        result,
                    );
                    placed.pop();
                }
            }
        }

        let mut placed = Vec::new();
        dfs(
            slice_a,
            slice_b,
            &conflicts,
            0,
            0,
            &mut placed,
            Direction::Unknown,
            &mut result,
        );
        result
    }
}
