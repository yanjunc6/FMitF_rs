use super::errors::Results;
use super::Boogie;
use crate::cfg::{CfgProgram, HopId};

pub use super::errors::{SpannedError, VerificationError};

pub struct PartitionVerificationManager {}

impl PartitionVerificationManager {
    /// Generate P-1 (Partition Node Placement) verification Boogie programs
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
                    program.name = format!("partition_{}_hop_{}_p1", function.name, hop_id.index());

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

        let procedure_name = format!("check_partition_{}_{}", function.name, hop_id.index());
        let params = Boogie::gen_Boogie::BoogieProgramGenerator::gen_procedure_params(
            cfg_program,
            function,
            None,
        );

        let mut lines = Vec::new();
        lines.push(Boogie::BoogieLine::Comment(format!(
            "P-1 Partition Node Placement Verification for {} hop {}",
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
}
