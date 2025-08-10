// src/cli/compiler.rs

// Clean, modular compiler pipeline for FMitF. No function >20 lines. All error handling explicit.

use super::{Cli, Logger, OutputManager};
use crate::{
    ast::parse_and_analyze,
    cfg::CfgBuilder,
    sc_graph::SCGraphBuilder,
    verification::{partition_verification::PartitionVerificationResult, VerificationManager},
    AstProgram, CfgProgram,
};
use std::time::Instant;

#[derive(Debug, Default)]
pub struct CompilationResult {
    pub ast_program: AstProgram,
    pub cfg_program: CfgProgram,
    pub sc_graph: crate::sc_graph::SCGraph,
    pub verification_result: Option<PartitionVerificationResult>,
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
        let ast = match self.stage_ast(source_code) {
            Ok(ast) => ast,
            Err(_) => {
                return Ok(self.fail_result(start, None, None, None, None, false));
            }
        };
        let cfg = match self.stage_cfg(&ast) {
            Ok(cfg) => cfg,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), None, None, None, false));
            }
        };
        let scg = match self.stage_scgraph(&cfg, cli) {
            Ok(scg) => scg,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), Some(cfg), None, None, false));
            }
        };
        let verification_result = match self.run_verification(&cfg, &scg, cli) {
            Ok(result) => Some(result),
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), Some(cfg), Some(scg), None, false));
            }
        };
        Ok(CompilationResult {
            ast_program: ast,
            cfg_program: cfg,
            sc_graph: scg,
            verification_result,
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
        verification_result: Option<PartitionVerificationResult>,
        success: bool,
    ) -> CompilationResult {
        CompilationResult {
            ast_program: ast.unwrap_or_default(),
            cfg_program: cfg.unwrap_or_default(),
            sc_graph: scg.unwrap_or_default(),
            verification_result,
            success,
            compilation_time_ms: start.elapsed().as_millis() as u64,
        }
    }

    fn run_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &crate::sc_graph::SCGraph,
        cli: &Cli,
    ) -> Result<PartitionVerificationResult, String> {
        let mut verification_manager = VerificationManager::new();
        let output_dir = cli.get_output_dir();

        Ok(verification_manager.run_partition_verification(
            cfg_program,
            sc_graph,
            Some(output_dir.to_str().unwrap_or("tmp")),
        ))
    }

    fn stage_ast(&mut self, src: &str) -> Result<AstProgram, String> {
        self.logger.stage_start(1, 5, "Frontend Analysis");
        match parse_and_analyze(src) {
            Ok(ast) => {
                self.logger.stage_success();
                Ok(ast)
            }
            Err(errors) => {
                self.logger.stage_error(errors.len());
                for error in &errors {
                    super::print_spanned_error(error, src);
                }
                Err("AST stage failed".to_string())
            }
        }
    }

    fn stage_cfg(&mut self, ast: &AstProgram) -> Result<CfgProgram, String> {
        self.logger.stage_start(2, 5, "Building Control Flow Graph");
        match CfgBuilder::build_from_program(ast) {
            Ok(cfg_program) => {
                self.logger.stage_success();
                Ok(cfg_program)
            }
            Err(e) => {
                self.logger.stage_failed(&e.to_string());
                Err(format!("CFG building failed: {}", e))
            }
        }
    }

    fn stage_scgraph(
        &mut self,
        cfg: &CfgProgram,
        cli: &Cli,
    ) -> Result<crate::sc_graph::SCGraph, String> {
        self.logger
            .stage_start(4, 5, "Building Serializability Conflict Graph");
        let builder = SCGraphBuilder::new(cli.instances);
        let graph = builder.build(cfg);
        self.logger.stage_success();
        Ok(graph)
    }

    pub fn handle_output(&self, result: &mut CompilationResult, cli: &Cli) -> Result<(), String> {
        let output_manager = OutputManager::new();
        let output_dir = cli.get_output_dir();
        output_manager.write_directory_output(result, &output_dir)?;
        Ok(())
    }
}
