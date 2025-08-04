// src/cli/compiler.rs
//! Core compiler implementation that orchestrates the entire compilation pipeline

use super::{Cli, Logger, OutputManager};
use crate::{
    ast::parse_and_analyze, cfg::CfgBuilder, sc_graph::SCGraphBuilder, AstProgram, CfgProgram,
};

/// Compilation result containing all intermediate products
#[derive(Debug)]
pub struct CompilationResult {
    pub ast_program: AstProgram,
    pub cfg_program: CfgProgram,
    pub sc_graph: crate::sc_graph::SCGraph,
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

impl CompilationResult {
    pub fn get_stats(&self) -> CompilationStats {
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
            sc_nodes: 0, // TODO: Add node count to SCGraph
            s_edges: 0,  // TODO: Add edge count to SCGraph
            c_edges: 0,  // TODO: Add edge count to SCGraph
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
    pub fn compile(&mut self, source_code: String, cli: &Cli) -> Result<CompilationResult, String> {
        let start_time = std::time::Instant::now();

        self.logger.compilation_start(&cli.input);

        // Stage 1: AST
        self.logger.stage_start(1, 4, "Frontend Analysis");
        let ast_program = self.compile_ast(source_code.clone())?;
        self.logger.stage_success();

        // Stage 2: CFG
        self.logger.stage_start(2, 4, "Building Control Flow Graph");
        let cfg_program = self.compile_cfg(&ast_program)?;
        self.logger.stage_success();

        // Stage 3: Optimization (skipped for now)
        if !cli.no_optimize {
            self.logger
                .stage_start(3, 4, "Optimizing Control Flow Graph");
            self.logger
                .stage_skipped("optimization not yet implemented");
        }

        // Stage 4: SC-Graph
        self.logger
            .stage_start(4, 4, "Building Serializability Conflict Graph");
        let sc_graph = self.compile_scgraph(&cfg_program)?;
        self.logger.stage_success();

        let compilation_time = start_time.elapsed().as_millis() as u64;

        let result = CompilationResult {
            ast_program,
            cfg_program,
            sc_graph,
            success: true,
            compilation_time_ms: compilation_time,
        };

        self.logger.compilation_complete(compilation_time);

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
    ) -> Result<crate::sc_graph::SCGraph, String> {
        Ok(SCGraphBuilder::build(cfg_program))
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
