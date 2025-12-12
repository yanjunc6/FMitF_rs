//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use super::stage::execute_stage;
use crate::ast::Program;
use crate::cfg;
use crate::cli::data::{self, CEdgeVerificationData, DataCollector, MemoryStats, VerificationResult};
use crate::cli::log::Logger;
use crate::codegen;
use crate::frontend::parse_and_analyze_program;
use crate::optimization::CfgOptimizer;
use crate::util::{CompilerError, DiagnosticReporter};
use crate::verification::Boogie::{BoogieError, BoogieProgram, VerificationNodeId};
use crate::verification::{
    verify_result_process::VerifyResultProcessor, VerificationManager, VerificationType,
};
use colored::*;
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

// ============================================================================
// --- Helper Structures
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

/// Result of running a Boogie verification with /usr/bin/time
struct BoogieRunResult {
    file_name: String,
    duration_ms: f64,
    stdout: String,
    stderr: String,
    memory_stats: MemoryStats,
}

// ============================================================================
// --- Compiler Structure
// ============================================================================

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
        if let Err(report_err) = self.reporter.report_all(&errors) {
            eprintln!("Warning: Failed to report errors: {}", report_err);
        }
        StageError {
            message: format!("Failed with {} errors", errors.len()),
        }
    }

    /// Read a source file
    pub fn read_file(
        &mut self,
        input_path: &PathBuf,
    ) -> Result<(String, String), Box<dyn std::error::Error>> {
        let input_content = std::fs::read_to_string(input_path)?;
        let filename = input_path.to_string_lossy().to_string();
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

        // Initialize logger
        self.logger.init(output_dir)?;
        self.logger.line("===== Compiler run started =====")?;
        self.logger.line(format!(
            "Boogie configuration: loopUnroll = {}, timeLimit = {}s",
            loop_unroll, timeout_secs
        ))?;

        // Initialize data collector
        let input_file_name = input_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();
        let mut data_collector = DataCollector::new(input_file_name, instances);
        data_collector.set_config(loop_unroll as usize, timeout_secs as usize);

        let total_stages =
            4 + if enable_verification { 1 } else { 0 } + if enable_optimization { 1 } else { 0 };
        let mut current_stage = 0;

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
                // Parse prelude
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

                // Parse main source
                let source = self.read_file(input_path)?;
                let source_filename = input_path.to_string_lossy();
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
                let cfg_program: cfg::Program = cfg::cfg_builder::build(program);
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

        data_collector.set_program_stats(
            optimized_or_cfg_program.functions.len(),
            optimized_or_cfg_program.hops.len(),
        );

        // Stage 4: SC-Graph
        current_stage += 1;
        let (mut sc, mut combined) = execute_stage(
            "SC-Graph",
            current_stage,
            total_stages,
            || -> Result<
                (crate::sc_graph::SCGraph, crate::sc_graph::CombinedSCGraph),
                Box<dyn std::error::Error>,
            > {
                let builder = crate::sc_graph::SCGraphBuilder::new(instances as u32);
                let sc = builder.build(&optimized_or_cfg_program);
                let combined = crate::sc_graph::combine_for_deadlock_elimination(&sc);
                self.write_sc_graph_dots(&sc, &optimized_or_cfg_program, output_dir, "")?;
                Ok((sc, combined))
            },
        )?;

        let sc_c_edges = sc
            .edges
            .iter()
            .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
            .count();

        let original_sc = sc.clone();

        // Stage 5: Verification (optional)
        if !enable_verification {
            data_collector.set_sc_stats(sc_c_edges, sc_c_edges);
        } else {
            current_stage += 1;
            let (simplified_sc, simplified_combined) = execute_stage(
                "Verification",
                current_stage,
                total_stages,
                || -> Result<
                    (crate::sc_graph::SCGraph, crate::sc_graph::CombinedSCGraph),
                    Box<dyn std::error::Error>,
                > {
                    self.run_verification_stage(
                        &optimized_or_cfg_program,
                        &sc,
                        &combined,
                        output_dir,
                        loop_unroll,
                        timeout_secs,
                        &mut data_collector,
                    )
                },
            )?;

            sc = simplified_sc;
            combined = simplified_combined;
        }

        // Stage 6: Code Generation
        current_stage += 1;
        let _ = execute_stage(
            "Codegen",
            current_stage,
            total_stages,
            || -> Result<(), Box<dyn std::error::Error>> {
                let generated_go_programs =
                    codegen::generate_go_code(&optimized_or_cfg_program, &sc, &combined)?;

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
        data::emit_summary(&data_collector, &mut self.logger);

        // Generate outputs
        if enable_verification && !data_collector.data().c_edge_verifications.is_empty() {
            // Write data.json
            let data_path = output_dir.join("data.json");
            data_collector.write_to_file(&data_path)?;
            self.logger
                .line(format!("📄 Benchmark data written: {}", data_path.display()))?;

            // Plot histogram
            let histogram_path = output_dir.join("verification_time_histogram.png");
            data_collector.plot_verification_histogram(&histogram_path)?;
            self.logger.line(format!(
                "📄 Verification time histogram written: {}",
                histogram_path.display()
            ))?;

            // Write plotted SC-graph
            self.write_sc_graph_plotted(
                &original_sc,
                &optimized_or_cfg_program,
                &data_collector.data().c_edge_verifications,
                output_dir,
            )?;
        }

        Ok(())
    }

    /// Run the verification stage - separated for clarity
    fn run_verification_stage(
        &mut self,
        program: &cfg::Program,
        sc: &crate::sc_graph::SCGraph,
        _combined: &crate::sc_graph::CombinedSCGraph,
        output_dir: &PathBuf,
        loop_unroll: u32,
        timeout_secs: u32,
        data_collector: &mut DataCollector,
    ) -> Result<
        (crate::sc_graph::SCGraph, crate::sc_graph::CombinedSCGraph),
        Box<dyn std::error::Error>,
    > {
        // Generate Boogie programs
        let verifier = VerificationManager::new();
        let mut programs: Vec<BoogieProgram> = Vec::new();

        let partition_programs = verifier
            .generate_verification_programs(
                program,
                sc,
                VerificationType::HopPartition,
            )
            .map_err(|_errors| {
                Box::<dyn std::error::Error>::from("Partition verification generation failed")
            })?;
        programs.extend(partition_programs);

        let commutative_programs = verifier
            .generate_verification_programs(program, sc, VerificationType::Commutative)
            .map_err(|_errors| {
                Box::<dyn std::error::Error>::from("Commutative verification generation failed")
            })?;
        programs.extend(commutative_programs);

        // Setup Boogie directory
        fs::create_dir_all(output_dir)?;
        let boogie_dir = output_dir.join("Boogie");
        if boogie_dir.exists() {
            fs::remove_dir_all(&boogie_dir)?;
        }
        fs::create_dir_all(&boogie_dir)?;

        // Run verifications in parallel with progress tracking
        let run_results = self.run_boogie_verifications_parallel(
            &programs,
            &boogie_dir,
            loop_unroll,
            timeout_secs,
        )?;

        // Process results and collect data
        let mut all_boogie_errors: Vec<BoogieError> = Vec::new();
        let mut exec_errors = Vec::new();

        for result in run_results {
            self.logger
                .line(format!("===== Boogie run for {} =====", result.file_name))?;
            self.logger.block("stdout:", &result.stdout)?;
            self.logger.block("stderr:", &result.stderr)?;

            // Process verification result
            let verification_result =
                self.process_boogie_result(&result, &mut all_boogie_errors, &mut exec_errors);

            // Extract C-edge info and add to data collector
            if let Some((src_f, src_i, src_h, tgt_f, tgt_i, tgt_h)) =
                Self::parse_commutative_edge_ids(&result.file_name)
            {
                let edge_data = CEdgeVerificationData {
                    source_function_id: src_f,
                    source_instance: src_i,
                    source_hop_id: src_h,
                    target_function_id: tgt_f,
                    target_instance: tgt_i,
                    target_hop_id: tgt_h,
                    duration_ms: result.duration_ms,
                    result: verification_result,
                    eliminated: false, // Will be updated later
                    memory_stats: result.memory_stats,
                    boogie_stdout: result.stdout,
                    boogie_stderr: result.stderr,
                    boogie_file: result.file_name,
                };
                data_collector.add_c_edge_verification(edge_data);
            }
        }

        // Process verification results
        let (verification_errors, simplified) =
            VerifyResultProcessor::process_boogie_errors(program, sc.clone(), all_boogie_errors);

        let simplified_sc_c_edges = simplified
            .edges
            .iter()
            .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
            .count();

        data_collector.set_sc_stats(
            sc.edges
                .iter()
                .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
                .count(),
            simplified_sc_c_edges,
        );

        // Mark eliminated edges (edges that exist in original but not in simplified)
        let simplified_edges: HashSet<_> = simplified
            .edges
            .iter()
            .filter(|e| matches!(e.edge_type, crate::sc_graph::EdgeType::C))
            .map(|e| {
                (
                    e.source.function_id.index(),
                    e.source.instance,
                    e.source.hop_id.index(),
                    e.target.function_id.index(),
                    e.target.instance,
                    e.target.hop_id.index(),
                )
            })
            .collect();

        let eliminated_edges: HashSet<_> = sc
            .edges
            .iter()
            .filter(|e| matches!(e.edge_type, crate::sc_graph::EdgeType::C))
            .map(|e| {
                (
                    e.source.function_id.index(),
                    e.source.instance,
                    e.source.hop_id.index(),
                    e.target.function_id.index(),
                    e.target.instance,
                    e.target.hop_id.index(),
                )
            })
            .filter(|edge| !simplified_edges.contains(edge))
            .collect();

        data_collector.mark_eliminated_edges(&eliminated_edges);

        // Write simplified graphs
        self.write_sc_graph_dots(&simplified, program, output_dir, "simplified")?;

        let simplified_combined = crate::sc_graph::combine_for_deadlock_elimination(&simplified);

        if !verification_errors.is_empty() {
            self.logger
                .line("⚠️  Verification completed with errors")?;
            self.logger
                .line(format!("  Total errors: {}", verification_errors.len()))?;
        }
        if !exec_errors.is_empty() {
            for error in &exec_errors {
                self.logger.line(format!("⚠️  {}", error))?;
            }
        }

        Ok((simplified, simplified_combined))
    }

    /// Run Boogie verifications in parallel using /usr/bin/time -v
    fn run_boogie_verifications_parallel(
        &self,
        programs: &[BoogieProgram],
        boogie_dir: &PathBuf,
        loop_unroll: u32,
        timeout_secs: u32,
    ) -> Result<Vec<BoogieRunResult>, Box<dyn std::error::Error>> {
        use indicatif::{ProgressBar, ProgressStyle};
        use rayon::prelude::*;
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::{Arc, Mutex};

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

        let running_files: Arc<Mutex<HashSet<String>>> = Arc::new(Mutex::new(HashSet::new()));
        let completed_count = Arc::new(AtomicUsize::new(0));

        let run_results: Vec<BoogieRunResult> = programs
            .par_iter()
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

                match write_res {
                    Ok(()) => {
                        // Update progress
                        {
                            let mut files = running_files.lock().unwrap();
                            files.insert(file_name.clone());
                            let files_list: Vec<String> = files.iter().cloned().collect();
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

                        // Run with /usr/bin/time -v
                        let start_time = std::time::Instant::now();
                        let result = Command::new("/usr/bin/time")
                            .arg("-v")
                            .arg("boogie")
                            .arg("/quiet")
                            .arg("/errorTrace:0")
                            .arg(format!("/loopUnroll:{}", loop_unroll))
                            .arg(format!("/timeLimit:{}", timeout_secs))
                            .arg(file_path.to_string_lossy().to_string())
                            .output();
                        let duration = start_time.elapsed();

                        // Update progress
                        {
                            let mut files = running_files.lock().unwrap();
                            files.remove(&file_name);
                            let new_completed =
                                completed_count.fetch_add(1, Ordering::Relaxed) + 1;
                            let files_list: Vec<String> = files.iter().cloned().collect();
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

                        match result {
                            Ok(output) => {
                                let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                                let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                                
                                // Parse memory stats from stderr (/usr/bin/time output)
                                let memory_stats = data::parse_time_output(&stderr);

                                BoogieRunResult {
                                    file_name,
                                    duration_ms: duration.as_secs_f64() * 1000.0,
                                    stdout,
                                    stderr,
                                    memory_stats,
                                }
                            }
                            Err(e) => BoogieRunResult {
                                file_name,
                                duration_ms: 0.0,
                                stdout: String::new(),
                                stderr: format!("Failed to execute: {}", e),
                                memory_stats: MemoryStats::default(),
                            },
                        }
                    }
                    Err(e) => BoogieRunResult {
                        file_name,
                        duration_ms: 0.0,
                        stdout: String::new(),
                        stderr: format!("Failed to write .bpl file: {}", e),
                        memory_stats: MemoryStats::default(),
                    },
                }
            })
            .collect();

        pb.finish_with_message("Boogie runs complete");
        Ok(run_results)
    }

    /// Process a Boogie result and determine verification status
    fn process_boogie_result(
        &self,
        result: &BoogieRunResult,
        all_boogie_errors: &mut Vec<BoogieError>,
        exec_errors: &mut Vec<String>,
    ) -> VerificationResult {
        // Check for compilation errors in stdout (Boogie writes errors to stdout)
        if result.stdout.contains("parse errors detected")
            || result.stdout.contains("name resolution errors detected")
            || result.stdout.contains("type errors detected")
            || result.stdout.contains("type checking errors detected")
        {
            exec_errors.push(format!(
                "Boogie compilation failed for {}: Check compiler.log for details.",
                result.file_name
            ));
            return VerificationResult::CompilationError;
        }

        // Check for timeout in stdout
        if result.stdout.contains("timed out") {
            if let Some(timeout_error) =
                Self::build_timeout_error_from_commutative(&result.file_name)
            {
                all_boogie_errors.push(timeout_error);
                return VerificationResult::Timeout;
            } else {
                exec_errors.push(format!(
                    "Boogie verification timed out for unknown file {}.",
                    result.file_name
                ));
                return VerificationResult::CompilationError;
            }
        }

        // Parse Boogie output for S-expressions (verification errors)
        let mut has_errors = false;
        for line in result.stdout.lines() {
            let line = line.trim();
            if line.starts_with('(') && line.ends_with(')') {
                if let Some(err) = BoogieError::from_boogie_string(line) {
                    all_boogie_errors.push(err);
                    has_errors = true;
                }
            }
        }

        if has_errors {
            VerificationResult::Error
        } else {
            VerificationResult::Pass
        }
    }

    /// Parse commutative edge IDs from filename
    fn parse_commutative_edge_ids(
        file_name: &str,
    ) -> Option<(usize, u32, usize, usize, u32, usize)> {
        let trimmed = file_name.strip_suffix(".bpl").unwrap_or(file_name);
        if !trimmed.starts_with("commutative_") {
            return None;
        }

        let parts: Vec<&str> = trimmed.split('_').collect();
        if parts.len() != 8 || parts.get(4).copied() != Some("vs") {
            return None;
        }

        let source = Self::parse_node_from_parts(parts[1], parts[2], parts[3])?;
        let target = Self::parse_node_from_parts(parts[5], parts[6], parts[7])?;

        Some((
            source.function_id,
            source.instance,
            source.hop_id,
            target.function_id,
            target.instance,
            target.hop_id,
        ))
    }

    /// Build timeout error from commutative filename
    fn build_timeout_error_from_commutative(file_name: &str) -> Option<BoogieError> {
        let trimmed = file_name.strip_suffix(".bpl").unwrap_or(file_name);
        if !trimmed.starts_with("commutative_") {
            return None;
        }

        let parts: Vec<&str> = trimmed.split('_').collect();
        if parts.len() != 8 || parts.get(4).copied() != Some("vs") {
            return None;
        }

        let node_1 = Self::parse_node_from_parts(parts[1], parts[2], parts[3])?;
        let node_2 = Self::parse_node_from_parts(parts[5], parts[6], parts[7])?;

        Some(BoogieError::SpecialInterleavingTimeout { node_1, node_2 })
    }

    /// Parse node from filename parts (f#, i#, h#)
    fn parse_node_from_parts(
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

    // ========================================================================
    // --- Output Writers
    // ========================================================================

    /// Write AST pretty print
    fn write_ast_pretty(
        &mut self,
        program: &Program,
        output_dir: &PathBuf,
    ) -> Result<(), Box<dyn std::error::Error>> {
        fs::create_dir_all(output_dir)?;
        let ast_file = output_dir.join("ast_pretty.txt");
        let mut file = fs::File::create(&ast_file)?;
        program.pretty_print_with_debug(&mut file)?;
        self.logger
            .line(format!("📄 AST file written to: {}", ast_file.display()))?;
        Ok(())
    }

    /// Write CFG pretty print
    fn write_cfg_pretty(
        &mut self,
        program: &cfg::Program,
        output_dir: &PathBuf,
        filename: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        fs::create_dir_all(output_dir)?;
        let cfg_file = output_dir.join(filename);
        let mut file = fs::File::create(&cfg_file)?;
        program.pretty_print_with_debug(&mut file)?;
        self.logger
            .line(format!("📄 CFG file written to: {}", cfg_file.display()))?;
        Ok(())
    }

    /// Write SC-graph DOT files
    fn write_sc_graph_dots(
        &mut self,
        sc: &crate::sc_graph::SCGraph,
        program: &cfg::Program,
        output_dir: &PathBuf,
        prefix: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use crate::pretty::{CombinedSCGraphDotPrinter, PrettyPrint, SCGraphDotPrinter};
        
        fs::create_dir_all(output_dir)?;

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
        let mut sc_file = fs::File::create(&sc_path)?;
        SCGraphDotPrinter::new(sc, program).pretty_print(&mut sc_file)?;

        // Write combined graph
        let combined = crate::sc_graph::combine_for_deadlock_elimination(sc);
        let combined_path = output_dir.join(&combined_filename);
        let mut c_file = fs::File::create(&combined_path)?;
        CombinedSCGraphDotPrinter::new(&combined, program).pretty_print(&mut c_file)?;

        self.logger.line(format!(
            "📄 SC Graph DOT files written: {}, {}",
            sc_path.display(),
            combined_path.display()
        ))?;
        Ok(())
    }

    /// Write plotted SC-graph with timing information
    fn write_sc_graph_plotted(
        &mut self,
        sc: &crate::sc_graph::SCGraph,
        program: &cfg::Program,
        c_edge_verifications: &[CEdgeVerificationData],
        output_dir: &PathBuf,
    ) -> Result<(), Box<dyn std::error::Error>> {
        fs::create_dir_all(output_dir)?;

        let plotted_path = output_dir.join("sc_graph_plotted.dot");
        let mut file = fs::File::create(&plotted_path)?;
        sc.to_dot(program, Some(c_edge_verifications), &mut file)?;

        self.logger.line(format!(
            "📄 Plotted SC-graph DOT file written: {}",
            plotted_path.display()
        ))?;
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
