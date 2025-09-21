//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use super::stage::execute_stage; // Import the new helper
use crate::ast::Program;
use crate::cfg;
use crate::frontend::parse_and_analyze_program;
use crate::util::{CompilerError, DiagnosticReporter};
use colored::*;
use std::fs;
use std::path::PathBuf;

// ============================================================================
// --- Compiler Structure
// ============================================================================

/// Custom error type for stage failures
#[derive(Debug)]
struct StageError {
    message: String,
}

impl std::fmt::Display for StageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for StageError {}

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

    /// Report compiler errors and return a custom stage error
    fn handle_compiler_errors(&mut self, errors: Vec<CompilerError>) -> StageError {
        // Report all errors using the diagnostic reporter (this will print them)
        if let Err(report_err) = self.reporter.report_all(&errors) {
            eprintln!("Warning: Failed to report errors: {}", report_err);
        }
        // Return a simple message for the stage error line
        StageError {
            message: format!("Failed with {} errors", errors.len()),
        }
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
        println!("{} compilation pipeline", "Starting".bold().green());

        const TOTAL_STAGES: usize = 5;
        let mut current_stage = 0;

        // Stage 1: Frontend (Parsing)
        current_stage += 1;
        let program = execute_stage(
            "Frontend",
            current_stage,
            TOTAL_STAGES,
            || -> Result<Program, Box<dyn std::error::Error>> {
                // 1. Parse prelude file
                let prelude_path = PathBuf::from("src/cli/prelude.transact");
                let prelude = self.read_file(&prelude_path)?;
                let prelude_filename = prelude_path.to_string_lossy();
                let prelude_filename_static =
                    Box::leak(prelude_filename.to_string().into_boxed_str()) as &'static str;

                let prelude_program =
                    parse_and_analyze_program(None, &prelude.1, prelude_filename_static).map_err(
                        |errors| -> Box<dyn std::error::Error> {
                            Box::new(self.handle_compiler_errors(errors))
                        },
                    )?;

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
                .map_err(|errors| -> Box<dyn std::error::Error> {
                    Box::new(self.handle_compiler_errors(errors))
                })?;

                self.write_ast_pretty(&source_program, output_dir)?;

                // 3. Return the program
                Ok(source_program)
            },
        )?;

        // Stage 2: CFG Generation
        current_stage += 1;
        let _cfg_program = execute_stage(
            "CFG Generation",
            current_stage,
            TOTAL_STAGES,
            || -> Result<cfg::Program, Box<dyn std::error::Error>> {
                // Generate CFG from AST using cfg_builder
                let cfg_program: cfg::Program = cfg::cfg_builder::build(program);

                // Write CFG pretty print
                self.write_cfg_pretty(&cfg_program, output_dir)?;

                Ok(cfg_program)
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

        println!("{}", "Completed".green().bold());
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

        // Write AST with debug information (includes IDs, name resolution, and type resolution if constants are enabled)
        let ast_file = output_dir.join("ast_pretty.txt");
        let mut file = fs::File::create(&ast_file)?;
        program.pretty_print_with_debug(&mut file)?;

        println!("📄 AST file written to: {}", ast_file.display());
        Ok(())
    }

    /// Write CFG pretty print to file
    fn write_cfg_pretty(
        &self,
        program: &cfg::Program,
        output_dir: &PathBuf,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Create output directory if it doesn't exist
        fs::create_dir_all(output_dir)?;

        // Write CFG with debug information (includes IDs and type information)
        let cfg_file = output_dir.join("cfg_pretty.txt");
        let mut file = fs::File::create(&cfg_file)?;
        program.pretty_print_with_debug(&mut file)?;

        println!("📄 CFG file written to: {}", cfg_file.display());
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
