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
// src/cli/compiler.rs

// Clean, modular compiler pipeline for FMitF. No function >20 lines. All error handling explicit.

use super::{Cli, Logger, OutputManager};
use crate::{
    ast::parse_and_analyze,
    cfg::CfgBuilder,
    sc_graph::SCGraphBuilder,
    verification::{CommutativityVerificationResult, PartitionVerificationResult},
    AstProgram, CfgProgram,
};
use std::time::Instant;

#[derive(Debug)]
pub struct CompilationResult {
    pub ast_program: AstProgram,
    pub cfg_program: CfgProgram,
    pub sc_graph: crate::sc_graph::SCGraph,
    pub partition_verification_result: Option<PartitionVerificationResult>,
    pub commutativity_verification_result: Option<CommutativityVerificationResult>,
    pub success: bool,
    pub compilation_time_ms: u64,
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

    pub fn compile(&mut self, source_code: String, cli: &Cli) -> Result<CompilationResult, String> {
        let start = Instant::now();
        self.logger.compilation_start(&cli.input);
        let ast = match self.stage_ast(&source_code) {
            Ok(ast) => ast,
            Err(_) => return Ok(self.fail_result(start, None, None, None, None, false)),
        };
        let cfg = match self.stage_cfg(&ast) {
            Ok(cfg) => cfg,
            Err(_) => return Ok(self.fail_result(start, Some(ast), None, None, None, false)),
        };
        let scg = match self.stage_scgraph(&cfg, cli) {
            Ok(scg) => scg,
            Err(_) => return Ok(self.fail_result(start, Some(ast), Some(cfg), None, None, false)),
        };
        let part = match self.stage_partition_verification(&cfg, cli) {
            Ok(part) => part,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), Some(cfg), Some(scg), None, false))
            }
        };
        let comm = match self.stage_commutativity_verification(&cfg, &scg, cli) {
            Ok(comm) => comm,
            Err(_) => {
                return Ok(self.fail_result(
                    start,
                    Some(ast),
                    Some(cfg),
                    Some(scg),
                    Some(part),
                    false,
                ))
            }
        };
        Ok(CompilationResult {
            ast_program: ast,
            cfg_program: cfg,
            sc_graph: scg,
            partition_verification_result: Some(part),
            commutativity_verification_result: Some(comm),
            success: true,
            compilation_time_ms: start.elapsed().as_millis() as u64,
        })
    }

    fn fail_result(
        &self,
        start: Instant,
        ast: Option<AstProgram>,
        cfg: Option<CfgProgram>,
        scg: Option<crate::sc_graph::SCGraph>,
        part: Option<PartitionVerificationResult>,
        success: bool,
    ) -> CompilationResult {
        CompilationResult {
            ast_program: ast.unwrap_or_default(),
            cfg_program: cfg.unwrap_or_default(),
            sc_graph: scg.unwrap_or_default(),
            partition_verification_result: part,
            commutativity_verification_result: None,
            success,
            compilation_time_ms: start.elapsed().as_millis() as u64,
        }
    }

    fn stage_ast(&mut self, src: &str) -> Result<AstProgram, String> {
        self.logger.stage_start(1, 6, "Frontend Analysis");
        let res = parse_and_analyze(src);
        match res {
            Ok(ast) => {
                self.logger.stage_success();
                Ok(ast)
            }
            Err(errors) => {
                self.logger.stage_error(errors.len());
                for e in &errors {
                    super::print_spanned_error(e, src);
                }
                self.logger.stage_failed("AST stage failed");
                Err("AST failed".to_string())
            }
        }
    }

    fn stage_cfg(&mut self, ast: &AstProgram) -> Result<CfgProgram, String> {
        self.logger.stage_start(2, 6, "Building Control Flow Graph");
        let res = CfgBuilder::build_from_program(ast).map(|ctx| ctx.program);
        match res {
            Ok(cfg) => {
                self.logger.stage_success();
                Ok(cfg)
            }
            Err(e) => {
                self.logger.stage_failed(&format!("CFG failed: {}", e));
                Err("CFG failed".to_string())
            }
        }
    }

    fn stage_scgraph(
        &mut self,
        cfg: &CfgProgram,
        cli: &Cli,
    ) -> Result<crate::sc_graph::SCGraph, String> {
        self.logger
            .stage_start(3, 6, "Building Serializability Conflict Graph");
        let builder = SCGraphBuilder::new(cli.instances);
        let scg = builder.build(cfg);
        self.logger.stage_success();
        Ok(scg)
    }

    fn stage_partition_verification(
        &mut self,
        cfg: &CfgProgram,
        cli: &Cli,
    ) -> Result<PartitionVerificationResult, String> {
        self.logger.stage_start(4, 6, "Partition Verification");
        let output_dir = cli.get_output_dir();
        let mut verification_cli = super::VerificationCli::new();
        let res = verification_cli.run_partition_verification(cfg, &output_dir);
        let log_path = output_dir.join("compilation.log");
        let _ = verification_cli.record_boogie_results_to_log(&[], &log_path);
        match res {
            Ok(r) => {
                self.logger.stage_success();
                Ok(r)
            }
            Err(e) => {
                self.logger.stage_failed(&e);
                Err("Partition verification failed".to_string())
            }
        }
    }

    fn stage_commutativity_verification(
        &mut self,
        cfg: &CfgProgram,
        scg: &crate::sc_graph::SCGraph,
        cli: &Cli,
    ) -> Result<CommutativityVerificationResult, String> {
        self.logger.stage_start(5, 6, "Commutativity Verification");
        let output_dir = cli.get_output_dir();
        let mut verification_cli = super::VerificationCli::new();
        let mut scg_clone = scg.clone();
        let res = verification_cli.run_commutativity_verification(
            cfg,
            &mut scg_clone,
            Some(output_dir.to_str().unwrap_or("tmp")),
        );
        let mut boogie_file_results = Vec::new();
        let mut generation_errors = Vec::new();
        if let Ok(entries) = std::fs::read_dir(output_dir.join("boogie")) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().map(|e| e == "bpl").unwrap_or(false) {
                    let filename = path.file_name().unwrap().to_string_lossy().to_string();
                    let output = std::process::Command::new("boogie").arg(&path).output();
                    match output {
                        Ok(output) => {
                            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                            let mut errors = Vec::new();
                            let mut is_generation_error = false;
                            for line in stdout.lines().chain(stderr.lines()) {
                                if line.contains("type checking errors")
                                    || line.contains("syntax error")
                                    || line.contains("wrong type")
                                {
                                    is_generation_error = true;
                                    errors.push(line.to_string());
                                }
                            }
                            if is_generation_error {
                                generation_errors.push(crate::verification::errors::VerificationError::BoogieGenerationError {
                                    function_name: filename.clone(),
                                    message: errors.join("\n"),
                                });
                            }
                            boogie_file_results.push(super::BoogieFileResult {
                                filename,
                                success: !is_generation_error,
                                verified_procedures: 0,
                                errors,
                                stdout,
                                stderr,
                            });
                        }
                        Err(e) => {
                            boogie_file_results.push(super::BoogieFileResult {
                                filename,
                                success: false,
                                verified_procedures: 0,
                                errors: vec![format!("Failed to run Boogie: {}", e)],
                                stdout: String::new(),
                                stderr: String::new(),
                            });
                        }
                    }
                }
            }
        }
        let log_path = output_dir.join("compilation.log");
        let _ = verification_cli.record_boogie_results_to_log(&boogie_file_results, &log_path);
        if !generation_errors.is_empty() {
            self.logger.stage_failed("Boogie/type errors detected");
            return Err("Boogie/type errors detected".to_string());
        }
        match res {
            Ok(r) => {
                self.logger.stage_success();
                Ok(r)
            }
            Err(e) => {
                self.logger.stage_failed(&e);
                Err("Commutativity verification failed".to_string())
            }
        }
    }

    pub fn handle_output(&self, result: &mut CompilationResult, cli: &Cli) -> Result<(), String> {
        let output_manager = OutputManager::new();
        let output_dir = cli.get_output_dir();
        output_manager.write_directory_output(result, &output_dir)?;
        Ok(())
    }
}
