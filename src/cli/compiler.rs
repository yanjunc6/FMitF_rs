//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use crate::ast::Program;
use crate::pretty::{PrettyPrinter, AstPrinter};  // Use VisitorAstPrinter instead
use crate::util::DiagnosticReporter;
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

    /// Compile a single source file
    pub fn parse_file(
        &mut self,
        input_path: &PathBuf,
    ) -> Result<Program, Box<dyn std::error::Error>> {
        // Read input file
        let input_content = std::fs::read_to_string(input_path)?;
        let filename = input_path.to_string_lossy().to_string();

        // Add source for error reporting
        self.add_source(&filename, &input_content);

        // Parse the program
        match ast_old::parse_and_analyze_program(&input_content) {
            Ok(program) => {
                println!("✅ Parse stage successful!");
                println!("Program has {} functions", program.functions.len());

                // TODO: Add more compilation stages here:
                // - CFG generation
                // - Optimization
                // - SC-Graph generation

                Ok(program)
            }
            Err(parse_errors) => {
                println!("❌ Parse stage failed");
                for error in &parse_errors {
                    self.reporter.report(error, &filename);
                }
                Err("Parse errors occurred".into())
            }
        }
    }

    /// Run the full compilation pipeline
    pub fn run_pipeline(
        &mut self,
        input_path: &PathBuf,
        output_dir: &PathBuf,
        _instances: usize,
    ) -> Result<(), Box<dyn std::error::Error>> {
        println!("🚀 Starting compilation pipeline...");
        println!("Input: {}", input_path.display());

        // Stage 1: Parse
        let program = self.parse_file(input_path)?;

        // Write AST pretty print to file
        self.write_ast_pretty(&program, output_dir)?;

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

        // Write AST pretty print using visitor pattern
        let ast_file = output_dir.join("ast_pretty.txt");
        use crate::pretty::print_program_visitor;
        let ast_content = print_program_visitor(program);
        fs::write(&ast_file, ast_content)?;

        println!("📄 AST written to: {}", ast_file.display());
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
