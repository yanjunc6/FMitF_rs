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
        let mut lines = Vec::new();

        // Add procedure header comment
        BoogieProgramGenerator::add_comment(
            &mut lines,
            format!("Partition verification for function: {}", function.name),
        );

        // Generate function start label
        let start_label = BoogieProgramGenerator::gen_function_start_label(&function.name);
        lines.push(BoogieLine::Label(start_label));

        // Track partition calls per hop for consistency checking
        let mut partition_call_tracker: HashMap<
            (crate::cfg::HopId, crate::cfg::TableId),
            Vec<Operand>,
        > = HashMap::new();

        // Traverse each hop in order
        for (hop_index, &hop_id) in function.hops.iter().enumerate() {
            let hop = &cfg_program.hops[hop_id];

            BoogieProgramGenerator::add_comment(&mut lines, format!("--- Hop {} ---", hop_index));

            // Process each basic block in the hop
            for (block_index, &block_id) in hop.blocks.iter().enumerate() {
                let block = &cfg_program.blocks[block_id];

                // Generate block label
                let block_label = BoogieProgramGenerator::gen_block_label(
                    &function.name,
                    hop_index,
                    block_index,
                    None,
                );
                lines.push(BoogieLine::Label(block_label));

                // Process each statement in the block
                for statement in &block.statements {
                    // First, translate the statement to Boogie
                    let boogie_lines = generator.convert_statement(cfg_program, statement, None)?;
                    lines.extend(boogie_lines);

                    // Then check if this statement contains table accesses and insert assertions
                    self.check_and_insert_partition_assertion(
                        generator,
                        cfg_program,
                        statement,
                        hop_id,
                        &mut partition_call_tracker,
                        &mut lines,
                    )?;
                }

                // Generate control flow using the existing gen_Boogie.rs function
                generator.gen_basic_block_edges(
                    &mut lines,
                    cfg_program,
                    block_id,
                    &function.name,
                    hop_index,
                );
            }
        }

        // Generate function end label
        let end_label = BoogieProgramGenerator::gen_function_end_label(&function.name);
        lines.push(BoogieLine::Label(end_label));

        // Generate procedure parameters
        let params = BoogieProgramGenerator::gen_procedure_params(cfg_program, function, None);

        // Collect modified global variables using dataflow analysis
        let modifies = BoogieProgramGenerator::collect_modified_globals_from_dataflow(
            cfg_program,
            function_id,
        );

        let procedure = BoogieProcedure {
            name: format!("verify_partition_{}", function.name),
            params,
            modifies,
            lines,
        };

        Ok(procedure)
    }

    /// Check statement for table accesses and insert partition consistency assertions if needed
    fn check_and_insert_partition_assertion(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        statement: &Statement,
        hop_id: crate::cfg::HopId,
        partition_call_tracker: &mut HashMap<
            (crate::cfg::HopId, crate::cfg::TableId),
            Vec<Operand>,
        >,
        lines: &mut Vec<BoogieLine>,
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
                lines,
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
                lines,
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
            (crate::cfg::HopId, crate::cfg::TableId),
            Vec<Operand>,
        >,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        let key = (hop_id, table_id);

        // Check if we've seen this partition function called in this hop before
        if let Some(previous_args) = partition_call_tracker.get(&key) {
            // We have a previous call - insert consistency assertion
            self.insert_partition_consistency_assertion(
                generator,
                cfg_program,
                table_id,
                previous_args,
                pk_values,
                lines,
            )?;
        } else {
            // First call to this partition function in this hop - just record it
            partition_call_tracker.insert(key, pk_values.to_vec());
        }

        Ok(())
    }

    /// Insert assertion that checks partition function consistency between two calls
    fn insert_partition_consistency_assertion(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        table_id: crate::cfg::TableId,
        previous_args: &[Operand],
        current_args: &[Operand],
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        let table = &cfg_program.tables[table_id];
        let partition_function = &cfg_program.functions[table.partition_function];

        BoogieProgramGenerator::add_comment(
            lines,
            format!(
                "Partition consistency check for function '{}' on table '{}'",
                partition_function.name, table.name
            ),
        );

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

        // Assert !(args are equal) - if same args, this should fail
        let not_args_equal = BoogieExpr {
            kind: BoogieExprKind::UnOp(super::Boogie::BoogieUnOp::Not, Box::new(args_equal)),
        };

        let error_msg = ErrorMessage {
            msg: format!(
                "Partition function '{}' called with same arguments in the same hop, violating single-node constraint",
                partition_function.name
            ),
        };

        BoogieProgramGenerator::add_assertion(lines, not_args_equal, error_msg);

        Ok(())
    }
}
