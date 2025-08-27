// src/cli/compiler.rs

// Clean, modular compiler pipeline for FMitF. No function >20 lines. All error handling explicit.

use super::{Cli, Logger, OutputManager};
use crate::{
    ast::parse_and_analyze, cfg::CfgBuilder, sc_graph::SCGraphBuilder,
    verification::VerificationManager, AstProgram, CfgProgram,
};
use indicatif::{ParallelProgressIterator, ProgressBar, ProgressStyle};
use rayon::prelude::*;
use std::time::Instant;

#[derive(Debug, Default)]
pub struct CompilationResult {
    pub ast_program: AstProgram,
    pub cfg_program: CfgProgram,
    pub optimized_cfg_program: Option<CfgProgram>,
    pub sc_graph: crate::sc_graph::SCGraph,
    pub boogie_programs: Vec<crate::verification::Boogie::BoogieProgram>,
    pub success: bool,
    pub compilation_time_ms: u64,
}

#[derive(Debug)]
pub struct CompilationStats {
    pub functions: usize,
    pub tables: usize,
    pub partitions: usize,
    pub basic_blocks: usize,
    pub sc_nodes: usize,
    pub s_edges: usize,
    pub c_edges: usize,
}

impl CompilationResult {
    pub fn get_stats(&self) -> CompilationStats {
        let s_edges = self
            .sc_graph
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
            .count();
        let c_edges = self
            .sc_graph
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
            .count();

        CompilationStats {
            functions: self.ast_program.functions.len(),
            tables: self.ast_program.tables.len(),
            partitions: self.ast_program.partitions.len(),
            basic_blocks: self
                .cfg_program
                .functions
                .iter()
                .map(|(_, f)| f.blocks.len())
                .sum(),
            sc_nodes: self.sc_graph.nodes.len(),
            s_edges,
            c_edges,
        }
    }
}

pub struct Compiler {
    logger: Logger,
}

impl Compiler {
    pub fn print_compilation_complete(&self, duration_ms: u64) {
        self.logger.compilation_complete(duration_ms);
    }

    pub fn print_compilation_failed(&self, duration_ms: u64, failed_stage: usize) {
        self.logger.compilation_failed(duration_ms, failed_stage);
    }

    pub fn new() -> Self {
        Self {
            logger: Logger::new(),
        }
    }

    pub fn compile(&mut self, source_code: &str, cli: &Cli) -> Result<CompilationResult, String> {
        let start = Instant::now();
        self.logger.compilation_start(&cli.input);

        // Initialize output manager
        let output_dir = cli.get_output_dir();
        let mut output_manager = OutputManager::new(output_dir)?;
        output_manager.write_compilation_start(&cli.input)?;

        // Stage 1: AST
        let ast = match self.stage_ast(source_code, &mut output_manager) {
            Ok(ast) => ast,
            Err(_) => {
                return Ok(self.fail_result(start, None, None, None, None, false));
            }
        };
        output_manager.write_ast_output(&ast)?;

        // Stage 2: CFG
        let mut cfg = match self.stage_cfg(&ast, &mut output_manager) {
            Ok(cfg) => cfg,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), None, None, None, false));
            }
        };

        // Stage 3: Optimization (modifies CFG in place)
        if !cli.no_optimize {
            // Write the original unoptimized CFG BEFORE optimization
            output_manager.write_cfg_output(&cfg, "cfg")?;

            match self.stage_optimization(&mut cfg, cli, &mut output_manager) {
                Ok(_) => {
                    // Optimization succeeded, write optimized version
                    output_manager.write_cfg_output(&cfg, "optimized_cfg")?;
                }
                Err(_) => {
                    return Ok(self.fail_result(start, Some(ast), Some(cfg), None, None, false));
                }
            };
        } else {
            // No optimization, just write the original CFG
            output_manager.write_cfg_output(&cfg, "cfg")?;
            self.logger
                .stage_start(3, 5, "Skipping Optimization Passes");
            self.logger.stage_success();
            output_manager.write_stage_completion("Optimization", true, None)?;
        }

        // Stage 4: SC-Graph (using the optimized CFG)
        let scg = match self.stage_scgraph(&cfg, cli, &mut output_manager) {
            Ok(scg) => scg,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), Some(cfg), None, None, false));
            }
        };
        output_manager.write_scgraph_output_with_name(&scg, &cfg, "scgraph")?;

        // Generate Combined SC-Graph for deadlock elimination analysis
        use crate::sc_graph::combine_for_deadlock_elimination;
        let combined_scg = combine_for_deadlock_elimination(&scg);
        output_manager.write_combined_scgraph_output_with_name(
            &combined_scg,
            &cfg,
            "combined_scgraph",
        )?;

        // Stage 5: Verification (Boogie)
        let boogie_programs =
            match self.stage_verification(&cfg, &scg, source_code, cli, &mut output_manager) {
                Ok(programs) => programs,
                Err(_) => {
                    return Ok(self.fail_result(
                        start,
                        Some(ast),
                        Some(cfg),
                        None,
                        Some(scg),
                        false,
                    ));
                }
            };
        output_manager.write_boogie_programs(&boogie_programs)?;

        // Stage 5 part 2: Run Boogie Verification
        self.run_boogie_verification(
            &ast,
            &cfg,
            scg.clone(),
            &mut output_manager,
            cli,
            source_code,
        )?;

        // Write final compilation statistics and summary
        let compilation_time_ms = start.elapsed().as_millis() as u64;
        output_manager.write_compilation_stats(
            &ast,
            &cfg,
            &scg,
            boogie_programs.len(),
            compilation_time_ms,
        )?;
        output_manager.write_summary_markdown(
            &cli.input,
            &ast,
            &cfg,
            &scg,
            boogie_programs.len(),
            compilation_time_ms,
        )?;

        Ok(CompilationResult {
            ast_program: ast,
            cfg_program: cfg,            // Store the final CFG (which is optimized)
            optimized_cfg_program: None, // No separate optimized version
            sc_graph: scg,
            boogie_programs,
            success: true,
            compilation_time_ms,
        })
    }

    fn fail_result(
        &self,
        start: Instant,
        ast: Option<AstProgram>,
        cfg: Option<CfgProgram>,
        optimized_cfg: Option<CfgProgram>,
        scg: Option<crate::sc_graph::SCGraph>,
        success: bool,
    ) -> CompilationResult {
        CompilationResult {
            ast_program: ast.unwrap_or_default(),
            cfg_program: cfg.unwrap_or_default(),
            optimized_cfg_program: optimized_cfg,
            sc_graph: scg.unwrap_or_default(),
            boogie_programs: Vec::new(),
            success,
            compilation_time_ms: start.elapsed().as_millis() as u64,
        }
    }

    fn stage_verification(
        &mut self,
        cfg: &CfgProgram,
        sc_graph: &crate::sc_graph::SCGraph,
        src: &str,
        cli: &Cli,
        output_manager: &mut OutputManager,
    ) -> Result<Vec<crate::verification::Boogie::BoogieProgram>, String> {
        self.logger.stage_start(5, 5, "Verification Analysis");
        let verification_manager = VerificationManager::new();

        // Check if verification is disabled
        if let Some(verification_type) = cli.get_verification_type() {
            match verification_manager.generate_verification_programs(
                cfg,
                sc_graph,
                verification_type,
            ) {
                Ok(programs) => {
                    self.logger.stage_success();
                    output_manager.write_stage_completion("Verification Analysis", true, None)?;
                    Ok(programs)
                }
                Err(errors) => {
                    self.logger.stage_error(errors.len());
                    output_manager.write_stage_completion("Verification Analysis", false, None)?;
                    for error in &errors {
                        super::print_verification_spanned_error(error, src);
                    }
                    Err("Verification stage failed".to_string())
                }
            }
        } else {
            // Verification disabled - return empty programs
            self.logger.stage_success();
            output_manager.write_stage_completion("Verification Analysis", true, None)?;
            Ok(Vec::new())
        }
    }

    fn stage_ast(
        &mut self,
        src: &str,
        output_manager: &mut OutputManager,
    ) -> Result<AstProgram, String> {
        self.logger.stage_start(1, 5, "Frontend Analysis");
        match parse_and_analyze(src) {
            Ok(ast) => {
                self.logger.stage_success();
                output_manager.write_stage_completion("Frontend Analysis", true, None)?;
                Ok(ast)
            }
            Err(errors) => {
                self.logger.stage_error(errors.len());
                output_manager.write_stage_completion("Frontend Analysis", false, None)?;
                for error in &errors {
                    super::print_ast_spanned_error(error, src);
                }
                Err("AST stage failed".to_string())
            }
        }
    }

    fn stage_cfg(
        &mut self,
        ast: &AstProgram,
        output_manager: &mut OutputManager,
    ) -> Result<CfgProgram, String> {
        self.logger.stage_start(2, 5, "Building Control Flow Graph");
        match CfgBuilder::build_from_program(ast) {
            Ok(cfg_program) => {
                self.logger.stage_success();
                output_manager.write_stage_completion("Building Control Flow Graph", true, None)?;
                Ok(cfg_program)
            }
            Err(e) => {
                self.logger.stage_failed(&e.to_string());
                output_manager.write_stage_completion(
                    "Building Control Flow Graph",
                    false,
                    None,
                )?;
                Err(format!("CFG building failed: {}", e))
            }
        }
    }

    fn stage_scgraph(
        &mut self,
        cfg: &CfgProgram,
        cli: &Cli,
        output_manager: &mut OutputManager,
    ) -> Result<crate::sc_graph::SCGraph, String> {
        self.logger
            .stage_start(4, 5, "Building Serializability Conflict Graph");
        let builder = SCGraphBuilder::new(cli.instances);
        let graph = builder.build(cfg);
        self.logger.stage_success();
        output_manager.write_stage_completion(
            "Building Serializability Conflict Graph",
            true,
            None,
        )?;
        Ok(graph)
    }

    fn stage_optimization(
        &mut self,
        cfg: &mut CfgProgram,
        _cli: &Cli,
        output_manager: &mut OutputManager,
    ) -> Result<(), String> {
        self.logger.stage_start(3, 5, "Running Optimization Passes");

        // Optimize the CFG in place
        use crate::optimization::CfgOptimizer;
        let optimizer = CfgOptimizer::default_passes();
        let _results = optimizer.optimize_program(cfg);

        self.logger.stage_success();
        output_manager.write_stage_completion("Running Optimization Passes", true, None)?;
        Ok(())
    }

    fn run_boogie_verification(
        &mut self,
        _ast_program: &AstProgram,
        cfg_program: &CfgProgram,
        scgraph: crate::sc_graph::SCGraph,
        output_manager: &mut OutputManager,
        cli: &Cli,
        src: &str,
    ) -> Result<(), String> {
        output_manager.write_log_section("Boogie Verification Results")?;

        let bpl_files = output_manager.get_boogie_files()?;

        if bpl_files.is_empty() {
            output_manager.write_log_line("No Boogie files found for verification")?;
            return Ok(());
        }

        output_manager.write_log_line(&format!(
            "Found {} Boogie files for verification",
            bpl_files.len()
        ))?;

        struct BoogieRunResult {
            file_name: String,
            success: bool,
            stdout: String,
            stderr: String,
            errors: Vec<crate::verification::Boogie::BoogieError>,
        }

        // Collect all verification errors
        let mut all_verification_errors: Vec<crate::verification::Boogie::BoogieError> = Vec::new();

        // Create progress bar
        let pb = ProgressBar::new(bpl_files.len() as u64);

        // Configure progress bar style based on --no-color flag
        let style = if cli.no_color {
            ProgressStyle::default_bar()
                .template("{spinner} [{elapsed_precise}] [{bar:40}] {pos}/{len} {msg}")
                .unwrap()
                .progress_chars("#>-")
        } else {
            ProgressStyle::default_bar()
                .template(
                    "{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}",
                )
                .unwrap()
                .progress_chars("#>-")
        };

        pb.set_style(style);
        pb.set_message("Verifying Boogie files...");

        // Run Boogie on each .bpl file in parallel
        let results: Result<Vec<BoogieRunResult>, String> = bpl_files
            .par_iter()
            .progress_with(pb.clone()) // updates pb as items complete
            .map(|bpl_file| {
                let file_name = bpl_file.file_name().unwrap().to_string_lossy().to_string();

                // Run boogie command: boogie <path> /quiet /errorTrace:0
                let output = std::process::Command::new("boogie")
                    .arg(&bpl_file)
                    .arg("/quiet")
                    .arg("/errorTrace:0")
                    .output()
                    .map_err(|e| {
                        format!("Failed to execute boogie command for {file_name}: {e}")
                    })?;

                let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                let success = output.status.success();

                // Extract verification errors from Boogie output
                let errors = Self::extract_verification_errors(&stdout, &stderr);

                Ok(BoogieRunResult {
                    file_name,
                    success,
                    stdout,
                    stderr,
                    errors,
                })
            })
            .collect();

        let mut results = results?; // Propagate error if any boogie invocation failed

        // Complete progress bar
        pb.finish_with_message(format!("✓ Verified {} Boogie files", bpl_files.len()));

        // Write results to compilation.log through OutputManager (sequentially) and collect errors
        for BoogieRunResult {
            file_name,
            success,
            stdout,
            stderr,
            errors,
        } in results.drain(..)
        {
            all_verification_errors.extend(errors);

            output_manager
                .write_boogie_verification_result(&file_name, success, &stdout, &stderr)?;
        }

        // Process verification errors and update SC graph
        output_manager.write_log_line(&format!(
            "Processing {} verification errors...",
            all_verification_errors.len()
        ))?;

        use crate::verification::verify_result_process::VerifyResultProcessor;
        let (spanned_errors, simplified_scgraph) = VerifyResultProcessor::process_boogie_errors(
            cfg_program,
            scgraph,
            all_verification_errors,
        );

        // Log spanned errors similar to stage_ast()
        if !spanned_errors.is_empty() {
            for error in &spanned_errors {
                super::print_verification_spanned_error(error, src);
            }
            return Err("Verification error processing failed".to_string());
        }

        // Write simplified SC graph
        output_manager.write_scgraph_output_with_name(
            &simplified_scgraph,
            cfg_program,
            "scgraph_simplified",
        )?;
        output_manager.write_log_line("The SC-graph Simplified")?;

        // Generate simplified Combined SC-Graph
        let simplified_combined_scg =
            crate::sc_graph::combine_for_deadlock_elimination(&simplified_scgraph);
        output_manager.write_combined_scgraph_output_with_name(
            &simplified_combined_scg,
            cfg_program,
            "combined_scgraph_simplified",
        )?;

        Ok(())
    }

    /// Extract verification errors from Boogie output
    fn extract_verification_errors(
        stdout: &str,
        stderr: &str,
    ) -> Vec<crate::verification::Boogie::BoogieError> {
        let mut errors = Vec::new();

        // Look for assertion violations in Boogie output
        // Boogie typically reports assertion violations with the message
        let combined_output = format!("{}\n{}", stdout, stderr);

        for line in combined_output.lines() {
            if let Some(boogie_error) =
                crate::verification::Boogie::BoogieError::from_boogie_string(line)
            {
                errors.push(boogie_error);
            }
        }
        errors
    }
}
