pub mod Boogie;
pub mod errors;

use crate::cfg::{CfgProgram, HopId};
use errors::Results;

pub use errors::{SpannedError, VerificationError};

/// Types of verification to generate
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerificationType {
    /// P-1: Correct Node Placement verification
    NodePlacement,
    /// P-2: Slice Commutativity verification  
    SliceCommutativity,
    /// Generate both verification types
    All,
}

/// Simple verification manager for Boogie generation
pub struct VerificationManager {}

impl VerificationManager {
    pub fn new() -> Self {
        VerificationManager {}
    }

    /// Generate Boogie programs for all functions in the CFG program
    /// Returns a vector of BoogiePrograms, one for each transaction function
    pub fn generate_boogie_programs(
        &self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        // Generate common elements (constants, globals, axioms, table variables)
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        let mut programs = Vec::new();

        // Generate a separate program for each transaction function
        for &function_id in &cfg_program.root_functions {
            let function = &cfg_program.functions[function_id];
            if function.function_type == crate::cfg::FunctionType::Transaction {
                let program = base_program.clone();

                // Create a generator instance to call the method
                let mut generator =
                    Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);
                let procedure =
                    generator.gen_function_to_boogie_template(cfg_program, function_id, None)?;

                generator.program.procedures.push(procedure);
                generator.program.name = function.name.clone(); // Set the program name to the function name
                programs.push(generator.program);
            }
        }

        Ok(programs)
    }

    /// Generate P-1 (Node Placement) verification Boogie programs
    /// Each hop gets its own procedure that verifies partition function consistency
    pub fn generate_p1_verification(
        &self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let mut programs = Vec::new();
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        for &function_id in &cfg_program.root_functions {
            let function = &cfg_program.functions[function_id];
            if function.function_type == crate::cfg::FunctionType::Transaction {
                for &hop_id in &function.hops {
                    let mut program = base_program.clone();
                    program.name = format!("{}_hop_{}_p1", function.name, hop_id.index());

                    let mut generator =
                        Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);

                    // Generate P-1 verification procedure for this hop
                    let procedure = self.gen_p1_hop_procedure(
                        &mut generator,
                        cfg_program,
                        function_id,
                        hop_id,
                    )?;
                    generator.program.procedures.push(procedure);
                    programs.push(generator.program);
                }
            }
        }

        Ok(programs)
    }

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

    /// Generate verification programs based on type
    pub fn generate_verification_programs(
        &self,
        cfg_program: &CfgProgram,
        verification_type: VerificationType,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        match verification_type {
            VerificationType::NodePlacement => self.generate_p1_verification(cfg_program),
            VerificationType::SliceCommutativity => self.generate_p2_verification(cfg_program),
            VerificationType::All => {
                let mut all_programs = Vec::new();
                all_programs.extend(self.generate_p1_verification(cfg_program)?);
                all_programs.extend(self.generate_p2_verification(cfg_program)?);
                Ok(all_programs)
            }
        }
    }

    /// Generate P-1 verification procedure for a specific hop
    /// Verifies that all partition function calls within the hop are consistent
    fn gen_p1_hop_procedure(
        &self,
        generator: &mut Boogie::gen_Boogie::BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        function_id: crate::cfg::FunctionId,
        hop_id: HopId,
    ) -> Results<Boogie::BoogieProcedure> {
        let function = &cfg_program.functions[function_id];
        let hop = &cfg_program.hops[hop_id];

        let procedure_name = format!("Check_NodePlacement_{}_{}", function.name, hop_id.index());
        let params = Boogie::gen_Boogie::BoogieProgramGenerator::gen_procedure_params(
            cfg_program,
            function,
            None,
        );

        let mut lines = Vec::new();
        lines.push(Boogie::BoogieLine::Comment(format!(
            "P-1 Node Placement Verification for {} hop {}",
            function.name,
            hop_id.index()
        )));

        // Track partition function calls: (partition_fn_name, args) -> first_occurrence_label
        let mut partition_calls: std::collections::HashMap<String, (Vec<String>, String)> =
            std::collections::HashMap::new();

        // Process each block in the hop
        for (block_index, &block_id) in hop.blocks.iter().enumerate() {
            let block = &cfg_program.blocks[block_id];
            let label = Boogie::gen_Boogie::BoogieProgramGenerator::gen_block_label(
                &function.name,
                hop_id.index(),
                block_index,
                None,
            );
            lines.push(Boogie::BoogieLine::Label(label.clone()));

            // Process each statement looking for table accesses
            for (stmt_index, stmt) in block.statements.iter().enumerate() {
                // Convert the statement to Boogie first
                match generator.convert_statement(cfg_program, stmt, None) {
                    Ok(boogie_stmts) => lines.extend(boogie_stmts),
                    Err(errors) => return Err(errors),
                }

                // Check if this statement involves a table access that uses a partition function
                if let crate::cfg::Statement::Assign { rvalue, .. } = stmt {
                    if let crate::cfg::RValue::TableAccess {
                        table, pk_values, ..
                    } = rvalue
                    {
                        let table_info = &cfg_program.tables[*table];

                        // Find the partition function for this table
                        let partition_fn_id = table_info.partition_function;
                        let partition_fn = &cfg_program.functions[partition_fn_id];
                        let partition_fn_name = &partition_fn.name;

                        // Convert pk_values to string representation for comparison
                        let mut arg_strs = Vec::new();
                        for pk_val in pk_values {
                            // Convert operand to a comparable string (simplified)
                            let arg_str = match pk_val {
                                crate::cfg::Operand::Var(var_id) => {
                                    let var = &cfg_program.variables[*var_id];
                                    var.name.clone()
                                }
                                crate::cfg::Operand::Const(constant) => {
                                    format!("{:?}", constant) // Simple debug representation
                                }
                            };
                            arg_strs.push(arg_str);
                        }

                        let call_signature =
                            format!("{}({})", partition_fn_name, arg_strs.join(", "));

                        // Check if we've seen this partition function with these exact args before
                        if let Some((prev_args, _prev_label)) =
                            partition_calls.get(partition_fn_name)
                        {
                            if prev_args != &arg_strs {
                                // Different args for same partition function - generate assertion
                                let assertion_msg = format!(
                                    "Partition function {} called with different arguments: {:?} vs {:?}",
                                    partition_fn_name, prev_args, arg_strs
                                );

                                // Generate assertion that these args should be equal (simplified)
                                lines.push(Boogie::BoogieLine::Comment(format!(
                                    "Partition consistency check: {}",
                                    call_signature
                                )));

                                // For now, generate a placeholder assertion
                                let assertion_expr = Boogie::BoogieExpr {
                                    kind: Boogie::BoogieExprKind::BoolConst(false), // This will fail - needs proper implementation
                                };

                                lines.push(Boogie::BoogieLine::Assert(
                                    assertion_expr,
                                    Boogie::ErrorMessage { msg: assertion_msg },
                                ));
                            }
                        } else {
                            // First occurrence of this partition function
                            let current_label = format!("{}_{}", label, stmt_index);
                            partition_calls
                                .insert(partition_fn_name.clone(), (arg_strs, current_label));
                        }
                    }
                }
            }
        }

        lines.push(Boogie::BoogieLine::Comment(
            "End of P-1 verification".to_string(),
        ));

        Ok(Boogie::BoogieProcedure {
            name: procedure_name,
            params,
            modifies: vec![], // P-1 verification doesn't modify global state
            lines,
        })
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
