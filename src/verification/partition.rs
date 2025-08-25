use super::errors::Results;
use super::Boogie::{
    gen_Boogie::BoogieProgramGenerator, BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine,
    BoogieProcedure, BoogieProgram, ErrorMessage,
};
use crate::cfg::{CfgProgram, FunctionId, FunctionType, LValue, Operand, RValue, Statement};
use std::collections::HashMap;

pub use super::errors::{SpannedError, VerificationError};

pub struct PartitionVerificationManager {}

impl PartitionVerificationManager {
    pub fn new() -> Self {
        PartitionVerificationManager {}
    }

    /// Generate Partition (Node Placement Consistency) verification Boogie programs
    /// Each transaction function gets its own partition consistency verification procedure
    pub fn generate_partition_verification(
        &self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<BoogieProgram>> {
        let mut programs = Vec::new();

        // Generate one program per transaction function
        for &function_id in &cfg_program.root_functions {
            let function = &cfg_program.functions[function_id];

            // Skip partition functions - we only verify transactions
            if function.function_type == FunctionType::Partition {
                continue;
            }

            // Generate program for this transaction function
            let program =
                self.generate_function_partition_verification(cfg_program, function_id)?;
            programs.push(program);
        }

        Ok(programs)
    }

    /// Generate partition verification program for a specific transaction function
    fn generate_function_partition_verification(
        &self,
        cfg_program: &CfgProgram,
        function_id: FunctionId,
    ) -> Results<BoogieProgram> {
        let function = &cfg_program.functions[function_id];

        // Create base program with common elements
        let mut generator = BoogieProgramGenerator::with_program(
            BoogieProgramGenerator::gen_base_program(cfg_program)?,
        );

        generator.program.name = format!("partition_verification_{}", function.name);

        // Generate verification procedure that simulates CFG and inserts assertions inline
        let procedure =
            self.generate_verification_procedure(&mut generator, cfg_program, function_id)?;

        generator.program.procedures.push(procedure);
        Ok(generator.program)
    }

    /// Generate the main verification procedure that simulates CFG with inline assertions
    fn generate_verification_procedure(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        function_id: FunctionId,
    ) -> Results<BoogieProcedure> {
        let function = &cfg_program.functions[function_id];

        // Generate procedure parameters
        let params = BoogieProgramGenerator::gen_procedure_params(cfg_program, function, None);

        // Collect modified global variables using dataflow analysis
        let modifies = BoogieProgramGenerator::collect_modified_globals_from_dataflow(
            cfg_program,
            function_id,
        );

        // Create the procedure and add it to the generator
        let procedure = BoogieProcedure {
            name: format!("verify_partition_{}", function.name),
            params,
            local_vars: Vec::new(), // Will be populated automatically
            modifies,
            lines: Vec::new(), // Will be populated by generator methods
        };

        generator.program.procedures.push(procedure);
        let proc_index = generator.program.procedures.len() - 1;
        generator.set_current_procedure(proc_index);

        // Add procedure header comment
        generator.add_comment_to_current_procedure(format!(
            "Partition verification for function: {}",
            function.name
        ));

        // Generate function start label
        let start_label =
            BoogieProgramGenerator::gen_function_start_label(&function.name, None, None);
        generator.add_line_to_current_procedure(BoogieLine::Label(start_label));

        // Track partition calls per hop for consistency checking
        // Key: (hop_id, partition_function_id) - same partition function across different tables
        let mut partition_call_tracker: HashMap<
            (crate::cfg::HopId, crate::cfg::FunctionId),
            Vec<Operand>,
        > = HashMap::new();

        // Traverse each hop in order
        for (hop_index, &hop_id) in function.hops.iter().enumerate() {
            let hop = &cfg_program.hops[hop_id];

            generator.add_comment_to_current_procedure(format!("--- Hop {} ---", hop_index));

            // Process each basic block in the hop
            for &block_id in hop.blocks.iter() {
                let block = &cfg_program.blocks[block_id];

                // Generate block label using gen_basic_block_label with block_id
                let block_label =
                    BoogieProgramGenerator::gen_basic_block_label(block_id, None, None);
                generator.add_line_to_current_procedure(BoogieLine::Label(block_label));

                // Process each statement in the block
                for statement in &block.statements {
                    // First, translate the statement to Boogie
                    let boogie_lines = generator.convert_statement(cfg_program, statement, None)?;
                    generator.add_lines_to_current_procedure(boogie_lines);

                    // Then check if this statement contains table accesses and insert assertions
                    self.check_and_insert_partition_assertion(
                        generator,
                        cfg_program,
                        statement,
                        hop_id,
                        &mut partition_call_tracker,
                    )?;
                }

                // Generate control flow using the existing gen_basic_block_edges function
                let mut block_edges = Vec::new();
                generator.gen_basic_block_edges(
                    &mut block_edges,
                    cfg_program,
                    block_id,
                    false,
                    &function.name,
                    None,
                    None,
                );
                generator.add_lines_to_current_procedure(block_edges);
            }
        }

        // Generate function end label
        let end_label = BoogieProgramGenerator::gen_function_end_label(&function.name, None, None);
        generator.add_line_to_current_procedure(BoogieLine::Label(end_label));

        // Generate function return label
        let return_label =
            BoogieProgramGenerator::gen_function_return_label(&function.name, None, None);
        generator.add_line_to_current_procedure(BoogieLine::Label(return_label));

        // Generate function abort label
        let abort_label =
            BoogieProgramGenerator::gen_function_abort_label(&function.name, None, None);
        generator.add_line_to_current_procedure(BoogieLine::Label(abort_label));

        // Clear current procedure reference
        generator.clear_current_procedure();

        // Return the completed procedure
        Ok(generator.program.procedures.pop().unwrap())
    }

    /// Check statement for table accesses and insert partition consistency assertions if needed
    fn check_and_insert_partition_assertion(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        statement: &Statement,
        hop_id: crate::cfg::HopId,
        partition_call_tracker: &mut HashMap<
            (crate::cfg::HopId, crate::cfg::FunctionId),
            Vec<Operand>,
        >,
    ) -> Results<()> {
        let Statement::Assign { lvalue, rvalue, .. } = statement;

        // Check LValue for table field assignments
        if let LValue::TableField {
            table, pk_values, ..
        } = lvalue
        {
            self.handle_table_access(
                generator,
                cfg_program,
                hop_id,
                *table,
                pk_values,
                partition_call_tracker,
            )?;
        }

        // Check RValue for table accesses
        if let RValue::TableAccess {
            table, pk_values, ..
        } = rvalue
        {
            self.handle_table_access(
                generator,
                cfg_program,
                hop_id,
                *table,
                pk_values,
                partition_call_tracker,
            )?;
        }

        Ok(())
    }

    /// Handle a table access by checking for partition consistency and inserting assertions
    fn handle_table_access(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        hop_id: crate::cfg::HopId,
        table_id: crate::cfg::TableId,
        pk_values: &[Operand],
        partition_call_tracker: &mut HashMap<
            (crate::cfg::HopId, crate::cfg::FunctionId),
            Vec<Operand>,
        >,
    ) -> Results<()> {
        // Get the partition function for this table
        let table = &cfg_program.tables[table_id];
        let partition_function_id = table.partition_function;
        let key = (hop_id, partition_function_id);

        // Map table primary key values to partition function arguments
        // The table.partition_fields tells us which table fields correspond to which partition parameters
        let mut partition_args = Vec::new();
        for &partition_field_id in &table.partition_fields {
            // Find the index of this field in the table's primary keys
            if let Some(pk_index) = table
                .primary_keys
                .iter()
                .position(|&pk_id| pk_id == partition_field_id)
            {
                if pk_index < pk_values.len() {
                    partition_args.push(pk_values[pk_index].clone());
                }
            }
        }

        // Check if we've seen this partition function called in this hop before
        if let Some(previous_args) = partition_call_tracker.get(&key) {
            // We have a previous call - insert consistency assertion
            self.insert_partition_consistency_assertion(
                generator,
                cfg_program,
                partition_function_id,
                previous_args,
                &partition_args,
            )?;
        } else {
            // First call to this partition function in this hop - just record it
            partition_call_tracker.insert(key, partition_args);
        }

        Ok(())
    }

    /// Insert assertion that checks partition function consistency between two calls
    fn insert_partition_consistency_assertion(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        partition_function_id: crate::cfg::FunctionId,
        previous_args: &[Operand],
        current_args: &[Operand],
    ) -> Results<()> {
        let partition_function = &cfg_program.functions[partition_function_id];

        generator.add_comment_to_current_procedure(format!(
            "Partition consistency check for function '{}': all calls in same hop must have same args",
            partition_function.name
        ));

        // Generate equality conditions for each argument pair
        let mut equality_conditions = Vec::new();

        for (prev_arg, curr_arg) in previous_args.iter().zip(current_args.iter()) {
            let prev_expr = generator.convert_operand(cfg_program, prev_arg, None)?;
            let curr_expr = generator.convert_operand(cfg_program, curr_arg, None)?;

            let equality = BoogieExpr {
                kind: BoogieExprKind::BinOp(
                    Box::new(prev_expr),
                    BoogieBinOp::Eq,
                    Box::new(curr_expr),
                ),
            };

            equality_conditions.push(equality);
        }

        // Use the general conjunction function from gen_Boogie.rs
        let args_equal = BoogieProgramGenerator::gen_conjunction(equality_conditions);

        // Assert (args are equal) - all accesses to same partition function in same hop must have same args
        let error_msg = ErrorMessage {
            msg: format!(
                "Partition function '{}' called with different arguments in the same hop, violating single-node constraint",
                partition_function.name
            ),
        };

        generator.add_assertion_to_current_procedure(args_equal, error_msg);

        Ok(())
    }
}
