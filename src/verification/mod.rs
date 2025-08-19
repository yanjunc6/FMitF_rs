pub mod Boogie;

use crate::cfg::CfgProgram;
use std::path::Path;

/// Simple verification manager for Boogie generation
pub struct VerificationManager {
}

impl VerificationManager {
    pub fn new() -> Self {
        VerificationManager {}
    }

    /// Generate Boogie files for all functions in the CFG program
    pub fn generate_boogie_files(&self, cfg_program: &CfgProgram, output_dir: &Path) -> std::io::Result<()> {
        use std::fs;
        use std::io::Write;

        // Create Boogie output directory
        let boogie_dir = output_dir.join("Boogie");
        fs::create_dir_all(&boogie_dir)?;

        let mut generator = Boogie::gen_Boogie::BoogieProgramGenerator::new();
        generator.gen_complete_program(cfg_program);

        // Write the complete Boogie program to a file
        let boogie_file_path = boogie_dir.join("program.bpl");
        let mut file = fs::File::create(&boogie_file_path)?;

        // Write global variables
        for (name, var_decl) in &generator.program.global_vars {
            if var_decl.is_const {
                writeln!(file, "const {} : {};", name, 
                    Boogie::gen_Boogie::BoogieProgramGenerator::boogie_type_to_string(&var_decl.var_type))?;
            } else {
                writeln!(file, "var {} : {};", name, 
                    Boogie::gen_Boogie::BoogieProgramGenerator::boogie_type_to_string(&var_decl.var_type))?;
            }
        }

        // Write other declarations
        for decl in &generator.program.other_declarations {
            writeln!(file, "{}", decl)?;
        }

        // Write procedures
        for procedure in &generator.program.procedures {
            writeln!(file, "\nprocedure {}(", procedure.name)?;
            for (i, param) in procedure.params.iter().enumerate() {
                if i > 0 { write!(file, ", ")?; }
                write!(file, "{}: {}", param.var_name, 
                    Boogie::gen_Boogie::BoogieProgramGenerator::boogie_type_to_string(&param.var_type))?;
            }
            writeln!(file, ")")?;

            if !procedure.modifies.is_empty() {
                writeln!(file, "  modifies {};", procedure.modifies.join(", "))?;
            }

            writeln!(file, "{{")?;
            for line in &procedure.lines {
                writeln!(file, "{}", line)?;
            }
            writeln!(file, "}}")?;
        }

        println!("Generated Boogie program: {}", boogie_file_path.display());
        Ok(())
    }
}
