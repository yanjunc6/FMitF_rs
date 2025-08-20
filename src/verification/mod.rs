pub mod Boogie;
pub mod errors;

use crate::cfg::CfgProgram;
use errors::Results;

pub use errors::{SpannedError, VerificationError};

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
                let mut program = base_program.clone();
                let procedure =
                    Boogie::gen_Boogie::BoogieProgramGenerator::gen_function_to_boogie_template(
                        cfg_program,
                        function_id,
                        None,
                    )?;
                program.procedures.push(procedure);
                program.name = function.name.clone(); // Set the program name to the function name
                programs.push(program);
            }
        }

        Ok(programs)
    }
}
