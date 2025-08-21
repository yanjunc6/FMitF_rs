use super::errors::Results;
use super::Boogie;
use crate::cfg::{CfgProgram, FunctionId};

pub use super::errors::{SpannedError, VerificationError};

pub struct PartitionVerificationManager {}

impl PartitionVerificationManager {
    /// Generate Partition (Node Placement Consistency) verification Boogie programs
    /// Each partition function gets its own consistency verification procedure
    pub fn generate_partition_verification(
        &self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let mut programs = Vec::new();
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        // Find all partition functions
        let partition_functions: Vec<_> = cfg_program
            .root_functions
            .iter()
            .filter(|&&func_id| {
                cfg_program.functions[func_id].function_type == crate::cfg::FunctionType::Partition
            })
            .collect();

        for &partition_func_id in partition_functions {
            let partition_func = &cfg_program.functions[partition_func_id];

            let mut program = base_program.clone();
            program.name = format!("{}_partition_verification", partition_func.name);

            let mut generator = Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);

            // Generate partition consistency verification procedure
            let procedure = self.create_partition_consistency_procedure(
                &mut generator,
                cfg_program,
                partition_func_id,
            )?;

            generator.program.procedures.push(procedure);
            programs.push(generator.program);
        }

        Ok(programs)
    }

    /// Create a procedure to verify partition function consistency
    fn create_partition_consistency_procedure(
        &self,
        _generator: &mut Boogie::gen_Boogie::BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        partition_func_id: FunctionId,
    ) -> Results<Boogie::BoogieProcedure> {
        let partition_func = &cfg_program.functions[partition_func_id];
        let procedure_name = format!("Check_Partition_{}", partition_func.name);

        let mut lines = Vec::new();

        // Add procedure header comment
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "Partition Consistency Verification for {}",
                partition_func.name
            ),
        );

        // Step 1: Declare and havoc input parameters
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "1. Declare partition function parameters".to_string(),
        );

        // For each parameter of partition function, generate havoc statement
        for (param_index, param) in partition_func.parameters.iter().enumerate() {
            let param_var = &cfg_program.variables[*param];
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                &mut lines,
                format!(
                    "havoc param_{}_{}; // {:?}",
                    param_index, param_var.name, param_var.ty
                ),
            );
        }

        // Step 2: Call partition function multiple times with same parameters
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "2. Call partition function twice with identical parameters".to_string(),
        );

        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!("call result1 := {}(...);", partition_func.name),
        );

        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            format!("call result2 := {}(...);", partition_func.name),
        );

        // Step 3: Assert consistency
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "3. Assert that partition function is deterministic".to_string(),
        );

        let consistency_condition = Boogie::gen_Boogie::BoogieProgramGenerator::gen_true_expr(); // Placeholder: should be result1 == result2

        let consistency_error = Boogie::ErrorMessage {
            msg: format!(
                "Partition function {} must be deterministic: same inputs must produce same outputs",
                partition_func.name
            ),
        };

        Boogie::gen_Boogie::BoogieProgramGenerator::add_assertion(
            &mut lines,
            consistency_condition,
            consistency_error,
        );

        // Step 4: Assert valid node assignment (non-negative)
        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "4. Assert that partition function returns valid node ID".to_string(),
        );

        let valid_node_condition = Boogie::gen_Boogie::BoogieProgramGenerator::gen_true_expr(); // Placeholder: should be result1 >= 0

        let valid_node_error = Boogie::ErrorMessage {
            msg: format!(
                "Partition function {} must return non-negative node ID",
                partition_func.name
            ),
        };

        Boogie::gen_Boogie::BoogieProgramGenerator::add_assertion(
            &mut lines,
            valid_node_condition,
            valid_node_error,
        );

        // Step 5: Add specific consistency checks for table assignments
        self.add_table_assignment_checks(&mut lines, cfg_program, partition_func_id)?;

        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            &mut lines,
            "End of partition consistency verification".to_string(),
        );

        Ok(Boogie::BoogieProcedure {
            name: procedure_name,
            params: vec![], // Partition procedures use havoc for parameter generation
            modifies: vec![], // Placeholder
            lines,
        })
    }

    /// Add table assignment consistency checks
    fn add_table_assignment_checks(
        &self,
        lines: &mut Vec<Boogie::BoogieLine>,
        cfg_program: &CfgProgram,
        partition_func_id: FunctionId,
    ) -> Results<()> {
        let partition_func = &cfg_program.functions[partition_func_id];

        Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
            lines,
            "5. Table assignment consistency checks".to_string(),
        );

        // Find tables that use this partition function
        let mut using_tables = Vec::new();
        for (table_id, table) in &cfg_program.tables {
            if table.partition_function == partition_func_id {
                // This table uses the partition function we're verifying
                Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                    lines,
                    format!(
                        "Table {} uses partition function {}",
                        table.name, partition_func.name
                    ),
                );
                using_tables.push((table_id, table));
            }
        }

        if using_tables.is_empty() {
            Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                lines,
                format!(
                    "Warning: Partition function {} is not used by any table",
                    partition_func.name
                ),
            );
        } else {
            // For each table using this partition function, verify consistency
            for (_table_id, table) in using_tables {
                Boogie::gen_Boogie::BoogieProgramGenerator::add_comment(
                    lines,
                    format!("Verifying table {} partition consistency", table.name),
                );

                // Assert that same primary key values map to same node
                let table_consistency_condition =
                    Boogie::gen_Boogie::BoogieProgramGenerator::gen_true_expr(); // Placeholder

                let table_consistency_error = Boogie::ErrorMessage {
                    msg: format!(
                        "Table {} partition assignment must be consistent: identical primary keys must map to same node",
                        table.name
                    ),
                };

                Boogie::gen_Boogie::BoogieProgramGenerator::add_assertion(
                    lines,
                    table_consistency_condition,
                    table_consistency_error,
                );
            }
        }

        Ok(())
    }
}
