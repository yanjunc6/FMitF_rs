use super::errors::Results;
use super::Boogie;
use crate::cfg::{CfgProgram, HopId};

pub use super::errors::{SpannedError, VerificationError};

pub struct CommutativeVerificationManager {}

impl CommutativeVerificationManager {
    /// Generate P-2 (Slice Commutativity) verification Boogie programs
    /// Each pair of conflicting transaction slices gets its own verification
    pub fn generate_p2_verification(
        &self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let mut programs = Vec::new();
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        // For P-2 verification, we need to analyze conflicts between transaction slices
        // This requires SC-graph information which is not available here yet

        // Placeholder: Generate P-2 verification for pairs of transaction functions
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
                    program.name = format!("{}_vs_{}_p2", func_a.name, func_b.name);

                    let mut generator =
                        Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);

                    // Generate P-2 verification procedure for these slices
                    let procedure = self.gen_p2_slice_procedure(
                        &mut generator,
                        cfg_program,
                        &slice_a,
                        &slice_b,
                        &c_edges,
                    )?;
                    generator.program.procedures.push(procedure);
                    programs.push(generator.program);
                }
            }
        }

        Ok(programs)
    }

    /// Generate all legal interleavings of two hop slices for P-2 verification
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

    /// Generate P-2 verification procedure for two conflicting slices  
    fn gen_p2_slice_procedure(
        &self,
        generator: &mut Boogie::gen_Boogie::BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        slice_a: &[HopId],
        slice_b: &[HopId],
        c_edges: &[(HopId, HopId)],
    ) -> Results<Boogie::BoogieProcedure> {
        let procedure_name = format!(
            "Check_SliceCommut_{}_{}",
            slice_a
                .iter()
                .map(|h| h.index().to_string())
                .collect::<Vec<_>>()
                .join("_"),
            slice_b
                .iter()
                .map(|h| h.index().to_string())
                .collect::<Vec<_>>()
                .join("_")
        );

        let mut lines = Vec::new();
        lines.push(Boogie::BoogieLine::Comment(format!(
            "P-2 Slice Commutativity Verification"
        )));

        // Generate legal interleavings
        let interleavings = self.generate_legal_interleavings(slice_a, slice_b, c_edges);

        // For now, just generate the first interleaving as a placeholder
        if let Some(first_interleaving) = interleavings.first() {
            lines.push(Boogie::BoogieLine::Comment(format!(
                "Testing interleaving with {} hops",
                first_interleaving.len()
            )));

            // 1. Snapshot initial state
            lines.push(Boogie::BoogieLine::Comment(
                "1. Snapshot initial world state".to_string(),
            ));
            lines.push(Boogie::BoogieLine::Comment(
                "// havoc DB0; havoc LiveIns0; (placeholder)".to_string(),
            ));

            // 2. Execute first ordering
            lines.push(Boogie::BoogieLine::Comment(
                "2. Execute first ordering".to_string(),
            ));
            for (hop_id, is_from_a) in first_interleaving {
                let slice_name = if *is_from_a { "A" } else { "B" };
                lines.push(Boogie::BoogieLine::Comment(format!(
                    "call Hop_{}_{}_procedure(); // placeholder",
                    slice_name,
                    hop_id.index()
                )));
            }

            // 3. Save first result
            lines.push(Boogie::BoogieLine::Comment(
                "// var DB1 := DB; var OUT1 := LiveOuts; (placeholder)".to_string(),
            ));

            // 4. Reset and execute reverse ordering
            lines.push(Boogie::BoogieLine::Comment(
                "3. Reset and execute reverse ordering".to_string(),
            ));
            lines.push(Boogie::BoogieLine::Comment(
                "// DB := DB0; LiveIns := LiveIns0; (placeholder)".to_string(),
            ));

            // Execute in reverse slice order (A<->B but preserve internal order)
            for (hop_id, is_from_a) in first_interleaving {
                let slice_name = if !*is_from_a { "A" } else { "B" }; // Flip slice
                lines.push(Boogie::BoogieLine::Comment(format!(
                    "call Hop_{}_{}_procedure(); // placeholder",
                    slice_name,
                    hop_id.index()
                )));
            }

            // 5. Generate assertions
            lines.push(Boogie::BoogieLine::Comment(
                "4. Assert identical results".to_string(),
            ));

            // Placeholder assertions (need proper implementation)
            let db_assertion = Boogie::BoogieExpr {
                kind: Boogie::BoogieExprKind::BoolConst(true), // Placeholder
            };

            lines.push(Boogie::BoogieLine::Assert(
                db_assertion,
                Boogie::ErrorMessage {
                    msg: "Database states must be identical after both orderings".to_string(),
                },
            ));

            let output_assertion = Boogie::BoogieExpr {
                kind: Boogie::BoogieExprKind::BoolConst(true), // Placeholder
            };

            lines.push(Boogie::BoogieLine::Assert(
                output_assertion,
                Boogie::ErrorMessage {
                    msg: "Live-out variables must be identical after both orderings".to_string(),
                },
            ));
        }

        lines.push(Boogie::BoogieLine::Comment(
            "End of P-2 verification".to_string(),
        ));

        Ok(Boogie::BoogieProcedure {
            name: procedure_name,
            params: vec![],   // P-2 procedures use havoc for initial conditions
            modifies: vec![], // Placeholder
            lines,
        })
    }
}
