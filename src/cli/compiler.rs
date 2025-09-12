//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use super::stage::execute_stage; // Import the new helper
use crate::ast::Program;
use crate::frontend::parse_and_analyze_program;
use crate::pretty::PrettyPrint;
use crate::util::{CompilerError, DiagnosticReporter};
use std::fs;
use std::path::PathBuf;

// ============================================================================
// --- Compiler Structure
// ============================================================================

/// Main compiler orchestrator
pub struct Compiler {
    pub reporter: DiagnosticReporter,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            reporter: DiagnosticReporter::new(),
        }
    }

    /// Add source file for error reporting
    pub fn add_source(&mut self, filename: &str, content: &str) {
        self.reporter.add_source(filename, content);
    }

    /// Report compiler errors and return a formatted error message
    fn handle_compiler_errors(&mut self, errors: Vec<CompilerError>, context: &str) -> String {
        // Always try to report errors, but don't fail if reporting fails
        if let Err(report_err) = self.reporter.report_all(&errors) {
            eprintln!(
                "Warning: Failed to report {} errors: {}",
                context, report_err
            );
        }
        format!("{} failed with {} errors", context, errors.len())
    }

    /// Compile a single source file
    pub fn read_file(
        &mut self,
        input_path: &PathBuf,
    ) -> Result<(String, String), Box<dyn std::error::Error>> {
        // Read input file
        let input_content = std::fs::read_to_string(input_path)?;
        let filename = input_path.to_string_lossy().to_string();

        // Add source for error reporting
        self.add_source(&filename, &input_content);

        Ok((filename, input_content))
    }

    /// Run the full compilation pipeline
    pub fn run_pipeline(
        &mut self,
        input_path: &PathBuf,
        output_dir: &PathBuf,
        _instances: usize,
    ) -> Result<(), Box<dyn std::error::Error>> {
        println!("Starting compilation pipeline...");

        const TOTAL_STAGES: usize = 5;
        let mut current_stage = 0;

        // Stage 1: Frontend (Parsing)
        current_stage += 1;
        let _program = execute_stage(
            "Frontend",
            current_stage,
            TOTAL_STAGES,
            || -> Result<_, Box<dyn std::error::Error>> {
                // 1. Parse prelude file
                let prelude = self.read_file(&PathBuf::from("prelude.transact"))?;
                let prelude_program =
                    parse_and_analyze_program(None, &prelude.1, "prelude.transact")
                        .map_err(|errors| self.handle_compiler_errors(errors, "Prelude parsing"))?;

                // 2. Parse the main source file
                let source = self.read_file(input_path)?;
                let source_filename = input_path.to_string_lossy();
                // Convert to static string for the lifetime requirement
                let source_filename_static =
                    Box::leak(source_filename.to_string().into_boxed_str()) as &'static str;
                let source_program = parse_and_analyze_program(
                    Some(prelude_program),
                    &source.1,
                    source_filename_static,
                )
                .map_err(|errors| self.handle_compiler_errors(errors, "Source parsing"))?;

                self.write_ast_pretty(&source_program, output_dir)?;

                // 3. Merge them together
                Ok(source_program)
            },
        )?;

        // TODO: Stage 2: CFG Generation
        // let cfg_program = cfg::build_cfg(program)?;

        // TODO: Stage 3: Optimization
        // let optimized_cfg = optimization::optimize(cfg_program)?;

        // TODO: Stage 4: SC-Graph Generation
        // let sc_graph = sc_graph::build_sc_graph(optimized_cfg, instances)?;

        // TODO: Stage 5: Output Generation
        // output_manager::write_outputs(sc_graph, output_dir)?;

        println!("✅ Compilation pipeline completed successfully!");
        Ok(())
    }

    /// Write AST pretty print to file
    fn write_ast_pretty(
        &self,
        program: &Program,
        output_dir: &PathBuf,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Create output directory if it doesn't exist
        fs::create_dir_all(output_dir)?;

        // Write AST pretty print using the PrettyPrint trait
        let ast_file = output_dir.join("ast_pretty.txt");
        let mut file = fs::File::create(&ast_file)?;
        program.pretty_print(&mut file)?;

        println!("📄 AST written to: {}", ast_file.display());
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
