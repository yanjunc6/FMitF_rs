// src/cli/compiler.rs
//! Core compiler implementation that orchestrates the entire compilation pipeline

use super::{Cli, Logger, OutputManager};
use crate::{
    ast::parse_and_analyze, cfg::CfgBuilder, sc_graph::SCGraphBuilder, 
    verification::PartitionVerificationResult, AstProgram, CfgProgram,
};

/// Compilation result containing all intermediate products
#[derive(Debug)]
pub struct CompilationResult {
    pub ast_program: AstProgram,
    pub cfg_program: CfgProgram,
    pub sc_graph: crate::sc_graph::SCGraph,
    pub verification_result: Option<PartitionVerificationResult>,
    pub success: bool,
    pub compilation_time_ms: u64,
}

/// Statistics about the compilation process
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

/// Result of running Boogie verification on a single file
#[derive(Debug)]
pub struct BoogieVerificationResult {
    pub file: String,
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
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

/// Main compiler that coordinates the compilation pipeline
pub struct Compiler {
    logger: Logger,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            logger: Logger::new(),
        }
    }

    /// Compile a .transact file through the entire pipeline
    /// Always generates debug output for completed stages, even on failure
    pub fn compile(&mut self, source_code: String, cli: &Cli) -> Result<CompilationResult, String> {
        let start_time = std::time::Instant::now();

        self.logger.compilation_start(&cli.input);

        // Initialize partial results
        let mut ast_program: Option<AstProgram> = None;
        let mut cfg_program: Option<CfgProgram> = None;
        let mut sc_graph: Option<crate::sc_graph::SCGraph> = None;
        let mut verification_result: Option<PartitionVerificationResult> = None;
        let mut compilation_success = true;
        let mut error_stage = None;

        // Stage 1: AST
        self.logger.stage_start(1, 5, "Frontend Analysis");
        match self.compile_ast(source_code.clone()) {
            Ok(ast) => {
                self.logger.stage_success();
                ast_program = Some(ast);
            }
            Err(err) => {
                compilation_success = false;
                error_stage = Some(1);
                self.logger.stage_failed(&err);
            }
        }

        // Stage 2: CFG (only if AST succeeded)
        if let Some(ref ast) = ast_program {
            self.logger.stage_start(2, 5, "Building Control Flow Graph");
            match self.compile_cfg(ast) {
                Ok(cfg) => {
                    self.logger.stage_success();
                    cfg_program = Some(cfg);
                }
                Err(err) => {
                    compilation_success = false;
                    if error_stage.is_none() {
                        error_stage = Some(2);
                    }
                    self.logger.stage_failed(&err);
                }
            }
        } else {
            self.logger
                .stage_skipped_due_to_error(2, 5, "Building Control Flow Graph");
        }

        // Stage 3: Optimization (skipped for now)
        if cfg_program.is_some() && !cli.no_optimize {
            self.logger
                .stage_start(3, 5, "Optimizing Control Flow Graph");
            self.logger
                .stage_skipped("optimization not yet implemented");
        } else if cfg_program.is_none() {
            self.logger
                .stage_skipped_due_to_error(3, 5, "Optimizing Control Flow Graph");
        }

        // Stage 4: SC-Graph (only if CFG succeeded)
        if let Some(ref cfg) = cfg_program {
            self.logger
                .stage_start(4, 5, "Building Serializability Conflict Graph");
            match self.compile_scgraph(cfg, cli) {
                Ok(graph) => {
                    self.logger.stage_success();
                    sc_graph = Some(graph);
                }
                Err(err) => {
                    compilation_success = false;
                    if error_stage.is_none() {
                        error_stage = Some(4);
                    }
                    self.logger.stage_failed(&err);
                }
            }
        } else {
            self.logger
                .stage_skipped_due_to_error(4, 5, "Building Serializability Conflict Graph");
        }

        // Stage 5: Formal Verification (only if CFG succeeded)
        if let Some(ref cfg) = cfg_program {
            self.logger
                .stage_start(5, 5, "Partition Verification");
            match self.run_partition_verification(cfg, cli) {
                Ok(result) => {
                    self.logger.stage_success();
                    verification_result = Some(result);
                }
                Err(err) => {
                    compilation_success = false;
                    if error_stage.is_none() {
                        error_stage = Some(5);
                    }
                    self.logger.stage_failed(&err);
                }
            }
        } else {
            self.logger
                .stage_skipped_due_to_error(5, 5, "Partition Verification");
        }

        let compilation_time = start_time.elapsed().as_millis() as u64;

        // Create result with whatever stages completed
        let result = CompilationResult {
            ast_program: ast_program.unwrap_or_default(),
            cfg_program: cfg_program.unwrap_or_default(),
            sc_graph: sc_graph.unwrap_or_default(),
            verification_result,
            success: compilation_success,
            compilation_time_ms: compilation_time,
        };

        if compilation_success {
            self.logger.compilation_complete(compilation_time);
        } else {
            self.logger
                .compilation_failed(compilation_time, error_stage.unwrap_or(1));
        }

        // Always return the result so we can generate debug output
        Ok(result)
    }

    fn compile_ast(&mut self, source_code: String) -> Result<AstProgram, String> {
        parse_and_analyze(&source_code).map_err(|errors| {
            self.logger.stage_error(errors.len());
            for error in &errors {
                super::print_spanned_error(error, &source_code);
            }
            "AST stage failed".to_string()
        })
    }

    fn compile_cfg(&mut self, ast_program: &AstProgram) -> Result<CfgProgram, String> {
        CfgBuilder::build_from_program(ast_program)
            .map(|ctx| ctx.program)
            .map_err(|e| format!("CFG building failed: {}", e))
    }

    fn compile_scgraph(
        &mut self,
        cfg_program: &CfgProgram,
        cli: &Cli,
    ) -> Result<crate::sc_graph::SCGraph, String> {
        // Use the number of instances specified in CLI
        let builder = SCGraphBuilder::new(cli.instances);
        Ok(builder.build(cfg_program))
    }

    fn run_partition_verification(
        &mut self,
        cfg_program: &CfgProgram,
        cli: &Cli,
    ) -> Result<PartitionVerificationResult, String> {
        let mut verification_manager = crate::verification::VerificationManager::new();
        let output_dir = cli.get_output_dir();
        
        // Set up Boogie output directory
        let boogie_dir = format!("{}/boogie", output_dir.to_str().unwrap_or("tmp"));
        verification_manager.partition_verifier.set_boogie_output_dir(boogie_dir.clone());
        
        // Run verification
        let result = verification_manager.run_partition_verification(
            cfg_program,
            Some(output_dir.to_str().unwrap_or("tmp")),
        );

        // Log verification results
        self.logger.partition_verification_results(&result);

        // Try to run Boogie verification if files were generated
        if result.boogie_files_generated > 0 {
            match self.run_boogie_verification(&boogie_dir) {
                Ok(boogie_results) => {
                    self.logger.boogie_verification_results(&boogie_results);
                }
                Err(e) => {
                    self.logger.boogie_verification_failed(&e);
                }
            }
        }

        Ok(result)
    }

    fn run_boogie_verification(&self, boogie_dir: &str) -> Result<Vec<BoogieVerificationResult>, String> {
        use std::process::Command;
        use std::fs;

        // Find all .bpl files in the boogie directory
        let boogie_files: Vec<_> = fs::read_dir(boogie_dir)
            .map_err(|e| format!("Failed to read Boogie directory: {}", e))?
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.extension()?.to_str()? == "bpl" {
                    Some(path)
                } else {
                    None
                }
            })
            .collect();

        let mut results = Vec::new();

        for boogie_file in boogie_files {
            let output = Command::new("boogie")
                .arg(boogie_file.to_str().unwrap())
                .output();

            let result = match output {
                Ok(output) => {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    
                    BoogieVerificationResult {
                        file: boogie_file.to_string_lossy().to_string(),
                        success: output.status.success(),
                        stdout: stdout.to_string(),
                        stderr: stderr.to_string(),
                    }
                }
                Err(e) => BoogieVerificationResult {
                    file: boogie_file.to_string_lossy().to_string(),
                    success: false,
                    stdout: String::new(),
                    stderr: format!("Failed to run Boogie: {}", e),
                },
            };

            results.push(result);
        }

        Ok(results)
    }

    /// Handle output based on the CLI configuration
    pub fn handle_output(&self, result: &CompilationResult, cli: &Cli) -> Result<(), String> {
        let output_manager = OutputManager::new();
        let output_dir = cli.get_output_dir();

        // Always write to directory with all artifacts
        output_manager.write_directory_output(result, &output_dir)?;

        Ok(())
    }
}
