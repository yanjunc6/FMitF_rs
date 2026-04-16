//! Compiler Implementation
//!
//! This module contains the main compilation orchestration logic.

use super::stage::execute_stage;
use crate::ast::Program;
use crate::cfg;
use crate::cli::cache_manager::CacheRuntimeOptions;
use crate::cli::data::{self, CEdgeVerificationData, DataCollector};
use crate::cli::log::Logger;
use crate::cli::options::CompilerOptions;
use crate::codegen;
use crate::frontend::parse_and_analyze_program;
use crate::optimization::CfgOptimizer;
use crate::util::{CompilerError, DiagnosticReporter};
use crate::verification::commutative::splitter::SplitRuntimeOptions;
use colored::*;
use std::fs;
use std::path::PathBuf;

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
        options: CompilerOptions,
    ) -> Result<(), Box<dyn std::error::Error>> {
        println!("{} compilation pipeline", "Starting".bold().green());

        let cache_options = CacheRuntimeOptions {
            enabled: options.cache.enabled,
            dir: options.cache.dir.clone(),
            max_size_mb: options.cache.max_size_mb,
            clear: options.cache.clear,
        };
        let split_options = SplitRuntimeOptions {
            enabled: options.split.enabled,
            max_depth: options.split.max_depth,
            strategy: options.split.strategy.clone(),
            debug_cuts: options.split.debug,
            debug_enforce_split: options.split.debug_enforce_split,
        };

        // Initialize logger
        self.logger.init(output_dir)?;
        self.logger.line("===== Compiler run started =====")?;
        self.logger.line(format!(
            "Boogie configuration: loopUnroll = {}, timeLimit = {}s",
            options.loop_unroll, options.timeout_secs
        ))?;
        self.logger.line(format!(
            "Verification features: cache={}, split={}, split_max_depth={}, split_strategy={}, split_debug={}",
            cache_options.enabled,
            split_options.enabled,
            split_options.max_depth,
            split_options.strategy,
            options.split.debug
        ))?;

        // Initialize data collector
        let input_file_name = input_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();
        let mut data_collector = DataCollector::new(input_file_name, options.instances);
        data_collector.set_config(options.loop_unroll as usize, options.timeout_secs as usize);

        let total_stages = 4
            + if options.enable_verification { 1 } else { 0 }
            + if options.enable_optimization { 1 } else { 0 };
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
                let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/cli/prelude.transact");
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
        let optimized_or_cfg_program = if options.enable_optimization {
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
                let builder = crate::sc_graph::SCGraphBuilder::new(options.instances as u32);
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
        if !options.enable_verification {
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
                    crate::cli::verification::run_verification_stage(
                        &mut self.logger,
                        &optimized_or_cfg_program,
                        &sc,
                        &combined,
                        output_dir,
                        options.loop_unroll,
                        options.timeout_secs,
                        &cache_options,
                        &split_options,
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
        if options.enable_verification && !data_collector.data().c_edge_verifications.is_empty() {
            // Write data.json
            let data_path = output_dir.join("data.json");
            data_collector.write_to_file(&data_path)?;
            self.logger.line(format!(
                "📄 Benchmark data written: {}",
                data_path.display()
            ))?;

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
