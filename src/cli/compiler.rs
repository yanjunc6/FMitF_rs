//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use super::stage::execute_stage; // Import the new helper
use crate::ast::Program;
use crate::cfg;
use crate::frontend::parse_and_analyze_program;
use crate::optimization::CfgOptimizer;
use crate::util::{CompilerError, DiagnosticReporter};
use crate::verification::Boogie::{BoogieError, BoogieProgram};
use crate::verification::{
    verify_result_process::VerifyResultProcessor, VerificationManager, VerificationType,
};
use colored::*;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

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
    log_file: Option<fs::File>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            reporter: DiagnosticReporter::new(),
            log_file: None,
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
        instances: usize,
        enable_optimization: bool,
    ) -> Result<(), Box<dyn std::error::Error>> {
        println!("{} compilation pipeline", "Starting".bold().green());

        // Initialize logger and write opening line
        self.init_logger(output_dir)?;
        self.log_line("===== Compiler run started =====")?;

        const TOTAL_STAGES: usize = 5; // Added Verification stage
        let mut current_stage = 0;

        // Stage 1: Frontend (Parsing)
        current_stage += 1;
        self.log_line(format!(
            "[Stage {}/{}] Frontend - start",
            current_stage, TOTAL_STAGES
        ))?;
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
        let cfg_program = execute_stage(
            "CFG Generation",
            current_stage,
            TOTAL_STAGES,
            || -> Result<cfg::Program, Box<dyn std::error::Error>> {
                // Generate CFG from AST using cfg_builder
                let cfg_program: cfg::Program = cfg::cfg_builder::build(program);

                // Write CFG pretty print
                self.write_cfg_pretty(&cfg_program, output_dir, "cfg_pretty.txt")?;

                Ok(cfg_program)
            },
        )?;

        // Stage 3: Optimization (optional)
        current_stage += 1;
        let optimized_or_cfg_program = if enable_optimization {
            execute_stage(
                "Optimization",
                current_stage,
                TOTAL_STAGES,
                || -> Result<cfg::Program, Box<dyn std::error::Error>> {
                    let optimizer = CfgOptimizer::default_passes();
                    let mut optimized_program = cfg_program;
                    let _results = optimizer.optimize_program(&mut optimized_program);
                    self.write_cfg_pretty(&optimized_program, output_dir, "cfg_opt_pretty.txt")?;
                    Ok(optimized_program)
                },
            )?
        } else {
            cfg_program
        };

        // Stage 4: SC-Graph (Build + Combine + DOT Outputs)
        current_stage += 1;
        let (sc, _combined) = execute_stage(
            "SC-Graph",
            current_stage,
            TOTAL_STAGES,
            || -> Result<(crate::sc_graph::SCGraph, crate::sc_graph::CombinedSCGraph), Box<dyn std::error::Error>> {
                let builder = crate::sc_graph::SCGraphBuilder::new(instances as u32);
                let sc = builder.build(&optimized_or_cfg_program);
                let combined = crate::sc_graph::combine_for_deadlock_elimination(&sc);
                // Unified DOT writer with empty prefix -> sc_graph.dot and combined_sc_graph.dot
                self.write_sc_graph_dots(&sc, &optimized_or_cfg_program, output_dir, "")?;
                Ok((sc, combined))
            },
        )?;

        // Stage 5: Verification (Commutative) -> generate Boogie, run, process results, and write simplified graphs
        current_stage += 1;
        let _ = execute_stage(
            "Verification",
            current_stage,
            TOTAL_STAGES,
            || -> Result<(), Box<dyn std::error::Error>> {
                // 1) Generate Boogie programs for commutative verification
                let verifier = VerificationManager::new();
                let programs: Vec<BoogieProgram> = verifier
                    .generate_verification_programs(
                        &optimized_or_cfg_program,
                        &sc,
                        VerificationType::Commutative,
                    )
                    .map_err(|errs| {
                        // Report now and convert to a simple boxed error
                        let _ = self.reporter.report_all(&errs);
                        let msg =
                            format!("Verification generation failed with {} errors", errs.len());
                        Box::<dyn std::error::Error>::from(msg)
                    })?;

                // Ensure output directory exists
                fs::create_dir_all(output_dir)?;
                let boogie_dir = output_dir.join("Boogie");
                fs::create_dir_all(&boogie_dir)?;

                // 2) Write each Boogie program to a .bpl file and run Boogie in parallel with progress
                use indicatif::{ParallelProgressIterator, ProgressBar, ProgressStyle};
                use rayon::prelude::*;

                let mut all_boogie_errors: Vec<BoogieError> = Vec::new();

                let total = programs.len() as u64;
                let pb = ProgressBar::new(total);
                pb.set_style(
                    ProgressStyle::with_template(
                        "[{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}",
                    )
                    .unwrap()
                    .progress_chars("=>-"),
                );
                pb.set_message("running Boogie");

                // Run write+boogie per program in parallel, collecting outputs for sequential logging
                let run_results: Vec<(String, Result<std::process::Output, std::io::Error>)> =
                    programs
                        .par_iter()
                        .progress_with(pb.clone())
                        .map(|program| {
                            let file_name = format!("{}.bpl", program.name);
                            let file_path = boogie_dir.join(&file_name);
                            // Write .bpl file
                            let write_res = (|| -> Result<(), std::io::Error> {
                                use std::io::Write;
                                let mut f = fs::File::create(&file_path)?;
                                write!(f, "{}", program)?;
                                Ok(())
                            })();

                            let output = match write_res {
                                Ok(()) => Command::new("boogie")
                                    .arg("/quiet")
                                    .arg("/errorTrace:0")
                                    .arg(file_path.to_string_lossy().to_string())
                                    .output(),
                                Err(e) => Err(e),
                            };

                            (file_name, output.map_err(|e| e))
                        })
                        .collect();

                // Finish the progress bar before logging
                pb.finish_with_message("Boogie runs complete");

                // 3) Append outputs to compiler.log and parse errors sequentially
                for (file_name, output) in run_results {
                    match output {
                        Ok(out) => {
                            self.log_line(format!("===== Boogie run for {} =====", file_name))?;
                            self.log_block("stdout:", &String::from_utf8_lossy(&out.stdout))?;
                            self.log_block("stderr:", &String::from_utf8_lossy(&out.stderr))?;

                            // Parse Boogie output lines for embedded {:msg "..."}
                            let stdout_str = String::from_utf8_lossy(&out.stdout);
                            let stderr_str = String::from_utf8_lossy(&out.stderr);
                            let mut parse_streams = vec![stdout_str.as_ref(), stderr_str.as_ref()];
                            for s in parse_streams.drain(..) {
                                for cap in s.match_indices("{:msg \"") {
                                    let start = cap.0 + "{:msg \"".len();
                                    if let Some(end_rel) = s[start..].find("\"}") {
                                        let end = start + end_rel;
                                        let payload = &s[start..end];
                                        if let Some(err) = crate::verification::Boogie::BoogieError::from_boogie_string(payload) {
                                            all_boogie_errors.push(err);
                                        }
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            self.log_line(format!(
                                "===== Boogie run for {} FAILED: {} =====",
                                file_name, e
                            ))?;
                        }
                    }
                }

                // 4) Process results and write simplified graphs
                let (verification_errors, simplified) =
                    VerifyResultProcessor::process_boogie_errors(
                        &optimized_or_cfg_program,
                        sc.clone(),
                        all_boogie_errors,
                    );

                // If there are user-facing verification errors, report them now
                if !verification_errors.is_empty() {
                    // Best effort reporting; ignore result
                    let _ = self.reporter.report_all(&verification_errors);
                }

                // Write simplified graphs (unified) with prefix "simplified"
                self.write_sc_graph_dots(
                    &simplified,
                    &optimized_or_cfg_program,
                    output_dir,
                    "simplified",
                )?;

                Ok(())
            },
        )?;

        println!("{}", "Completed".green().bold());
        Ok(())
    }

    /// Write AST pretty print to file
    fn write_ast_pretty(
        &mut self,
        program: &Program,
        output_dir: &PathBuf,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Create output directory if it doesn't exist
        fs::create_dir_all(output_dir)?;

        // Write AST with debug information (includes IDs, name resolution, and type resolution if constants are enabled)
        let ast_file = output_dir.join("ast_pretty.txt");
        let mut file = fs::File::create(&ast_file)?;
        program.pretty_print_with_debug(&mut file)?;

        self.log_line(format!("📄 AST file written to: {}", ast_file.display()))?;
        Ok(())
    }

    /// Write CFG pretty print to file
    fn write_cfg_pretty(
        &mut self,
        program: &cfg::Program,
        output_dir: &PathBuf,
        filename: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Create output directory if it doesn't exist
        fs::create_dir_all(output_dir)?;

        // Write CFG with debug information (includes IDs and type information)
        let cfg_file = output_dir.join(filename);
        let mut file = fs::File::create(&cfg_file)?;
        program.pretty_print_with_debug(&mut file)?;

        self.log_line(format!("📄 CFG file written to: {}", cfg_file.display()))?;
        Ok(())
    }
    // TOTAL_STAGES already defined above

    /// Unified method to write SCGraph and its combined variant as DOT files.
    /// If `prefix` is empty, writes to sc_graph.dot and combined_sc_graph.dot.
    /// Otherwise writes to {prefix}_sc_graph.dot and {prefix}_combined_sc_graph.dot.
    fn write_sc_graph_dots(
        &mut self,
        sc: &crate::sc_graph::SCGraph,
        program: &cfg::Program,
        output_dir: &PathBuf,
        prefix: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use crate::pretty::{CombinedSCGraphDotPrinter, PrettyPrint, SCGraphDotPrinter};
        std::fs::create_dir_all(output_dir)?;

        let sc_filename = if prefix.is_empty() {
            "sc_graph.dot".to_string()
        } else {
            format!("{}_sc_graph.dot", prefix)
        };
        let combined_filename = if prefix.is_empty() {
            "combined_sc_graph.dot".to_string()
        } else {
            format!("{}_combined_sc_graph.dot", prefix)
        };

        // Write raw graph
        let sc_path = output_dir.join(&sc_filename);
        let mut sc_file = std::fs::File::create(&sc_path)?;
        SCGraphDotPrinter::new(sc, program).pretty_print(&mut sc_file)?;

        // Compute and write combined graph
        let combined = crate::sc_graph::combine_for_deadlock_elimination(sc);
        let combined_path = output_dir.join(&combined_filename);
        let mut c_file = std::fs::File::create(&combined_path)?;
        CombinedSCGraphDotPrinter::new(&combined, program).pretty_print(&mut c_file)?;

        // Log instead of printing to stdout
        self.log_line(format!(
            "📄 SC Graph DOT files written: {}, {}",
            sc_path.display(),
            combined_path.display()
        ))?;
        Ok(())
    }

    // -----------------------
    // Simple logger utilities
    // -----------------------
    fn init_logger(&mut self, output_dir: &PathBuf) -> std::io::Result<()> {
        fs::create_dir_all(output_dir)?;
        // Create a fresh log file each run (truncate if exists)
        let file = fs::File::create(output_dir.join("compiler.log"))?;
        self.log_file = Some(file);
        Ok(())
    }

    fn log_line<S: AsRef<str>>(&mut self, s: S) -> std::io::Result<()> {
        use std::io::Write;
        if let Some(f) = self.log_file.as_mut() {
            writeln!(f, "{}", s.as_ref())?;
        }
        Ok(())
    }

    fn log_block<S: AsRef<str>>(&mut self, header: &str, body: S) -> std::io::Result<()> {
        use std::io::Write;
        if let Some(f) = self.log_file.as_mut() {
            writeln!(f, "{}", header)?;
            writeln!(f, "{}", body.as_ref())?;
        }
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
