//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use std::path::PathBuf;
use crate::ast::{self, Program};
use crate::util::DiagnosticReporter;

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
    pub fn compile_file(&mut self, input_path: &PathBuf) -> Result<Program, Box<dyn std::error::Error>> {
        // Read input file
        let input_content = std::fs::read_to_string(input_path)?;
        let filename = input_path.to_string_lossy().to_string();
        
        // Add source for error reporting
        self.add_source(&filename, &input_content);
        
        // Parse the program
        match ast::parse_program(&input_content) {
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
    pub fn run_pipeline(&mut self, input_path: &PathBuf, _output_dir: &PathBuf, _instances: usize) -> Result<(), Box<dyn std::error::Error>> {
        println!("🚀 Starting compilation pipeline...");
        println!("Input: {}", input_path.display());
        
        // Stage 1: Parse
        let _program = self.compile_file(input_path)?;
        
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
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
