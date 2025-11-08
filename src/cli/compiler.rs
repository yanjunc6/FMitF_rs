//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use super::stage::execute_stage; // Import the new helper
use crate::ast::Program;
use crate::cfg;
use crate::cli::log::Logger;
use crate::cli::summary::{emit_summary, RunSummary};
use crate::codegen;
use crate::frontend::parse_and_analyze_program;
use crate::optimization::CfgOptimizer;
use crate::util::{CompilerError, DiagnosticReporter};
use crate::verification::Boogie::{BoogieError, BoogieProgram, VerificationNodeId};
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
    logger: Logger,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            reporter: DiagnosticReporter::new(),
            logger: Logger::new(),
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
        enable_verification: bool,
        loop_unroll: u32,
        timeout_secs: u32,
    ) -> Result<(), Box<dyn std::error::Error>> {
        println!("{} compilation pipeline", "Starting".bold().green());

        // Initialize logger and write opening line
        self.logger.init(output_dir)?;
        self.logger.line("===== Compiler run started =====")?;
        self.logger.line(format!(
            "Boogie configuration: loopUnroll = {}, timeLimit = {}s",
            loop_unroll, timeout_secs
        ))?;

        let total_stages =
            4 + if enable_verification { 1 } else { 0 } + if enable_optimization { 1 } else { 0 };
        let mut current_stage = 0;
        let mut summary = RunSummary::new(instances);
        summary.boogie_loop_unroll = loop_unroll as usize;
        summary.boogie_timeout_secs = timeout_secs as usize;

        // Stage 1: Frontend (Parsing)
        current_stage += 1;
        self.logger.line(format!(
            "[Stage {}/{}] Frontend - start",
            current_stage, total_stages
        ))?;
        let program = execute_stage(
            "Frontend",
            current_stage,
            total_stages,
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
            total_stages,
            || -> Result<cfg::Program, Box<dyn std::error::Error>> {
                // Generate CFG from AST using cfg_builder
                let cfg_program: cfg::Program = cfg::cfg_builder::build(program);

                // Write CFG pretty print
                self.write_cfg_pretty(&cfg_program, output_dir, "cfg_pretty.txt")?;

                Ok(cfg_program)
            },
        )?;

        // Stage 3: Optimization (optional)

        let optimized_or_cfg_program = if enable_optimization {
            current_stage += 1;
            execute_stage(
                "Optimization",
                current_stage,
                total_stages,
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

        summary.function_count = optimized_or_cfg_program.functions.len();
        summary.hop_count = optimized_or_cfg_program.hops.len();

        // Stage 4: SC-Graph (Build + Combine + DOT Outputs)
        current_stage += 1;
        let (sc, _combined) = execute_stage(
            "SC-Graph",
            current_stage,
            total_stages,
            || -> Result<(crate::sc_graph::SCGraph, crate::sc_graph::CombinedSCGraph), Box<dyn std::error::Error>> {
                let builder = crate::sc_graph::SCGraphBuilder::new(instances as u32);
                let sc = builder.build(&optimized_or_cfg_program);
                let combined = crate::sc_graph::combine_for_deadlock_elimination(&sc);
                // Unified DOT writer with empty prefix -> sc_graph.dot and combined_sc_graph.dot
                self.write_sc_graph_dots(&sc, &optimized_or_cfg_program, output_dir, "")?;
                Ok((sc, combined))
            },
        )?;

        summary.sc_c_edges = sc
            .edges
            .iter()
            .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
            .count();

        // Stage 5: Verification (Commutative) -> generate Boogie, run, process results, and write simplified graphs
        if !enable_verification {
            summary.simplified_sc_c_edges = summary.sc_c_edges;
        } else {
            current_stage += 1;
            let _ = execute_stage(
                "Verification",
                current_stage,
                total_stages,
                || -> Result<(), Box<dyn std::error::Error>> {
                    // 1) Generate Boogie programs for all configured verification types
                    let verifier = VerificationManager::new();
                    let mut programs: Vec<BoogieProgram> = Vec::new();

                    let partition_programs = verifier
                        .generate_verification_programs(
                            &optimized_or_cfg_program,
                            &sc,
                            VerificationType::HopPartition,
                        )
                        .map_err(|errs| {
                            let _ = self.reporter.report_all(&errs);
                            let msg = format!(
                                "Verification generation failed with {} errors",
                                errs.len()
                            );
                            Box::<dyn std::error::Error>::from(msg)
                        })?;
                    programs.extend(partition_programs);

                    let commutative_programs = verifier
                        .generate_verification_programs(
                            &optimized_or_cfg_program,
                            &sc,
                            VerificationType::Commutative,
                        )
                        .map_err(|errs| {
                            // Report now and convert to a simple boxed error
                            let _ = self.reporter.report_all(&errs);
                            let msg = format!(
                                "Verification generation failed with {} errors",
                                errs.len()
                            );
                            Box::<dyn std::error::Error>::from(msg)
                        })?;
                    programs.extend(commutative_programs);

                    summary.verification_total = programs.len();
                    summary.verification_pass = 0;
                    summary.verification_errors = 0;
                    summary.verification_timeouts = 0;
                    summary.boogie_compile_failures = 0;

                    // Ensure output directory exists
                    fs::create_dir_all(output_dir)?;
                    let boogie_dir = output_dir.join("Boogie");
                    if boogie_dir.exists() {
                        fs::remove_dir_all(&boogie_dir)?;
                    }
                    fs::create_dir_all(&boogie_dir)?;

                    // 2) Write each Boogie program to a .bpl file and run Boogie in parallel with progress
                    use indicatif::{ProgressBar, ProgressStyle};
                    use rayon::prelude::*;
                    use std::collections::HashSet;
                    use std::sync::atomic::{AtomicUsize, Ordering};
                    use std::sync::{Arc, Mutex};

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

                    // Thread-safe tracking: running files set and completed counter
                    let running_files: Arc<Mutex<HashSet<String>>> =
                        Arc::new(Mutex::new(HashSet::new()));
                    let completed_count = Arc::new(AtomicUsize::new(0));

                    // Run write+boogie per program in parallel, collecting outputs for sequential logging
                    let run_results: Vec<(String, Result<std::process::Output, std::io::Error>)> =
                        programs
                            .par_iter()
                            .map(|program| {
                                let file_name = format!("{}.bpl", program.name);
                                let file_path = boogie_dir.join(&file_name);

                                // Write .bpl file (fast, don't track this)
                                let write_res = (|| -> Result<(), std::io::Error> {
                                    use std::io::Write;
                                    let mut f = fs::File::create(&file_path)?;
                                    write!(f, "{}", program)?;
                                    Ok(())
                                })();

                                let output = match write_res {
                                    Ok(()) => {
                                        // Add to running set RIGHT before calling Boogie
                                        {
                                            let mut files = running_files.lock().unwrap();
                                            files.insert(file_name.clone());
                                            let files_list: Vec<String> =
                                                files.iter().cloned().collect();
                                            let completed = completed_count.load(Ordering::Relaxed);
                                            let msg = if files_list.len() <= 3 {
                                                format!("Running: {}", files_list.join(", "))
                                            } else {
                                                format!(
                                                    "Running: {}, {}, {} (+{} more)",
                                                    files_list[0],
                                                    files_list[1],
                                                    files_list[2],
                                                    files_list.len() - 3
                                                )
                                            };
                                            pb.set_position(completed as u64);
                                            pb.set_message(msg);
                                        }

                                        // Actually run Boogie
                                        let result = Command::new("boogie")
                                            .arg("/quiet")
                                            .arg("/errorTrace:0")
                                            .arg(format!("/loopUnroll:{}", loop_unroll))
                                            .arg(format!("/timeLimit:{}", timeout_secs))
                                            .arg(file_path.to_string_lossy().to_string())
                                            .output();

                                        // Remove from running set and increment completed counter
                                        {
                                            let mut files = running_files.lock().unwrap();
                                            files.remove(&file_name);
                                            let new_completed =
                                                completed_count.fetch_add(1, Ordering::Relaxed) + 1;
                                            let files_list: Vec<String> =
                                                files.iter().cloned().collect();
                                            if !files_list.is_empty() {
                                                let msg = if files_list.len() <= 3 {
                                                    format!("Running: {}", files_list.join(", "))
                                                } else {
                                                    format!(
                                                        "Running: {}, {}, {} (+{} more)",
                                                        files_list[0],
                                                        files_list[1],
                                                        files_list[2],
                                                        files_list.len() - 3
                                                    )
                                                };
                                                pb.set_position(new_completed as u64);
                                                pb.set_message(msg);
                                            } else {
                                                pb.set_position(new_completed as u64);
                                            }
                                        }

                                        result
                                    }
                                    Err(e) => Err(e),
                                };

                                (file_name, output.map_err(|e| e))
                            })
                            .collect();

                    // Finish the progress bar before logging
                    pb.finish_with_message("Boogie runs complete");

                    // 3) Append outputs to compiler.log and parse errors sequentially
                    let mut exec_errors = Vec::new();

                    for (file_name, output) in run_results {
                        match output {
                            Ok(out) => {
                                self.logger
                                    .line(format!("===== Boogie run for {} =====", file_name))?;
                                self.logger
                                    .block("stdout:", &String::from_utf8_lossy(&out.stdout))?;
                                self.logger
                                    .block("stderr:", &String::from_utf8_lossy(&out.stderr))?;

                                let stdout_str = String::from_utf8_lossy(&out.stdout);
                                let stderr_str = String::from_utf8_lossy(&out.stderr);
                                let combined_output = format!("{}\n{}", stdout_str, stderr_str);

                                // Check for compilation errors (non-zero exit code or error patterns)
                                if !out.status.success()
                                    || combined_output.contains("parse errors detected")
                                    || combined_output.contains("name resolution errors detected")
                                    || combined_output.contains("type errors detected")
                                    || combined_output.contains("type checking errors detected")
                                {
                                    summary.boogie_compile_failures += 1;
                                    exec_errors.push(format!(
                                    "Boogie compilation failed for {}: Check compiler.log for details.", 
                                    file_name
                                ));
                                    continue; // Continue logging other results instead of returning immediately
                                }

                                if combined_output.contains("timed out") {
                                    if let Some(timeout_error) =
                                        Self::build_timeout_error_from_commutative(&file_name)
                                    {
                                        summary.verification_timeouts += 1;
                                        all_boogie_errors.push(timeout_error);
                                    } else {
                                        summary.boogie_compile_failures += 1;
                                        exec_errors.push(format!(
                                            "Boogie verification timed out for unknown file {}.",
                                            file_name
                                        ));
                                    }
                                    continue; // Continue logging other results instead of returning immediately
                                }

                                let mut is_pass = true;

                                // Parse Boogie output lines for S-expressions
                                // Boogie outputs the {:msg "..."} content directly, not wrapped
                                let mut parse_streams =
                                    vec![stdout_str.as_ref(), stderr_str.as_ref()];
                                for s in parse_streams.drain(..) {
                                    // Split into lines and try to parse each line as an S-expression
                                    for line in s.lines() {
                                        let line = line.trim();
                                        if line.starts_with('(') && line.ends_with(')') {
                                            // This might be an S-expression - try to parse it
                                            if let Some(err) = crate::verification::Boogie::BoogieError::from_boogie_string(line) {
                                            // This is a valid BoogieError (verification result) - add it
                                            summary.verification_errors += 1;
                                            is_pass = false;
                                            all_boogie_errors.push(err);
                                        }
                                        }
                                    }
                                }

                                if is_pass {
                                    summary.verification_pass += 1;
                                }
                            }
                            Err(e) => {
                                self.logger.line(format!(
                                    "===== Boogie run for {} FAILED: {} =====",
                                    file_name, e
                                ))?;
                                summary.boogie_compile_failures += 1;
                                exec_errors
                                    .push(format!("Failed to run Boogie for {}: {}", file_name, e));
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

                    summary.simplified_sc_c_edges = simplified
                        .edges
                        .iter()
                        .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
                        .count();

                    // Write simplified graphs (unified) with prefix "simplified"
                    self.write_sc_graph_dots(
                        &simplified,
                        &optimized_or_cfg_program,
                        output_dir,
                        "simplified",
                    )?;

                    // After logging all results, check if there were any errors
                    if !exec_errors.is_empty() {
                        return Err(Box::from(format!(
                        "Boogie verification failed: {} errors. Check compiler.log for details.",
                        exec_errors.len()
                    )));
                    }

                    // If there are user-facing verification errors, report them now
                    if !verification_errors.is_empty() {
                        // Best effort reporting; ignore result
                        let _ = self.reporter.report_all(&verification_errors);
                    }

                    Ok(())
                },
            )?;
        }

        // Stage 6: Go code generation
        current_stage += 1;
        let _ = execute_stage(
            "Codegen",
            current_stage,
            total_stages,
            || -> Result<(), Box<dyn std::error::Error>> {
                let generated_go_programs =
                    codegen::generate_go_code(&optimized_or_cfg_program, &sc)?;

                let go_dir = output_dir.join("go");
                if go_dir.exists() {
                    fs::remove_dir_all(&go_dir)?;
                }
                fs::create_dir_all(&go_dir)?;

                for program in generated_go_programs {
                    let path = go_dir.join(&program.filename);
                    fs::write(&path, program.content)?;
                }

                self.logger
                    .line(format!("📄 Go code written to: {}", go_dir.display()))?;
                Ok(())
            },
        )?;

        println!("{}", "Completed".green().bold());
        emit_summary(&summary, &mut self.logger);
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

        self.logger
            .line(format!("📄 AST file written to: {}", ast_file.display()))?;
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

        self.logger
            .line(format!("📄 CFG file written to: {}", cfg_file.display()))?;
        Ok(())
    }

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
            "sc_graph_combined.dot".to_string()
        } else {
            format!("{}_sc_graph_combined.dot", prefix)
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
        self.logger.line(format!(
            "📄 SC Graph DOT files written: {}, {}",
            sc_path.display(),
            combined_path.display()
        ))?;
        Ok(())
    }

    fn build_timeout_error_from_commutative(file_name: &str) -> Option<BoogieError> {
        let trimmed = file_name.strip_suffix(".bpl").unwrap_or(file_name);
        if !trimmed.starts_with("commutative_") {
            return None;
        }

        let parts: Vec<&str> = trimmed.split('_').collect();
        if parts.len() != 8 || parts.get(4).copied() != Some("vs") {
            return None;
        }

        let node_1 = Self::parse_commutative_node(parts[1], parts[2], parts[3])?;
        let node_2 = Self::parse_commutative_node(parts[5], parts[6], parts[7])?;

        Some(BoogieError::SpecialInterleavingTimeout { node_1, node_2 })
    }

    fn parse_commutative_node(
        f_part: &str,
        i_part: &str,
        h_part: &str,
    ) -> Option<VerificationNodeId> {
        let function_id = f_part.strip_prefix('f')?.parse().ok()?;
        let instance = i_part.strip_prefix('i')?.parse().ok()?;
        let hop_id = h_part.strip_prefix('h')?.parse().ok()?;

        Some(VerificationNodeId {
            function_id,
            instance,
            hop_id,
        })
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
