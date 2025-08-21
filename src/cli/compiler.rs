// src/cli/compiler.rs

// Clean, modular compiler pipeline for FMitF. No function >20 lines. All error handling explicit.

use super::{Cli, Logger};
use crate::{
    ast::parse_and_analyze, cfg::CfgBuilder, sc_graph::SCGraphBuilder,
    verification::VerificationManager, AstProgram, CfgProgram,
};
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

        // Stage 1: AST
        let ast = match self.stage_ast(source_code) {
            Ok(ast) => ast,
            Err(_) => {
                return Ok(self.fail_result(start, None, None, None, None, false));
            }
        };
        self.write_ast_output(&ast, cli)?;

        // Stage 2: CFG
        let mut cfg = match self.stage_cfg(&ast) {
            Ok(cfg) => cfg,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), None, None, None, false));
            }
        };

        // Stage 3: Optimization (modifies CFG in place)
        if !cli.no_optimize {
            // Write the original unoptimized CFG BEFORE optimization
            self.write_cfg_output(&cfg, cli, "cfg")?;

            match self.stage_optimization(&mut cfg, cli) {
                Ok(_) => {
                    // Optimization succeeded, write optimized version
                    self.write_cfg_output(&cfg, cli, "optimized_cfg")?;
                }
                Err(_) => {
                    return Ok(self.fail_result(start, Some(ast), Some(cfg), None, None, false));
                }
            };
        } else {
            // No optimization, just write the original CFG
            self.write_cfg_output(&cfg, cli, "cfg")?;
            self.logger
                .stage_start(3, 5, "Skipping Optimization Passes");
            self.logger.stage_success();
        }

        // Stage 4: SC-Graph (using the optimized CFG)
        let scg = match self.stage_scgraph(&cfg, cli) {
            Ok(scg) => scg,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), Some(cfg), None, None, false));
            }
        };
        self.write_scgraph_output(&scg, &cfg, cli)?;

        // Stage 5: Verification (Boogie)
        let boogie_programs = match self.stage_verification(&cfg, &scg, source_code, cli) {
            Ok(programs) => programs,
            Err(_) => {
                return Ok(self.fail_result(start, Some(ast), Some(cfg), None, Some(scg), false));
            }
        };
        self.write_verification_output(&boogie_programs, cli)?;

        // Write compilation summary files
        self.write_compilation_summary(&ast, &cfg, &scg, &boogie_programs, start, cli)?;

        Ok(CompilationResult {
            ast_program: ast,
            cfg_program: cfg,            // Store the final CFG (which is optimized)
            optimized_cfg_program: None, // No separate optimized version
            sc_graph: scg,
            boogie_programs,
            success: true,
            compilation_time_ms: start.elapsed().as_millis() as u64,
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
                    Ok(programs)
                }
                Err(errors) => {
                    self.logger.stage_error(errors.len());
                    for error in &errors {
                        super::print_verification_spanned_error(error, src);
                    }
                    Err("Verification stage failed".to_string())
                }
            }
        } else {
            // Verification disabled - return empty programs
            self.logger.stage_success();
            Ok(Vec::new())
        }
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
                    super::print_ast_spanned_error(error, src);
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

    fn stage_optimization(&mut self, cfg: &mut CfgProgram, _cli: &Cli) -> Result<(), String> {
        self.logger.stage_start(3, 5, "Running Optimization Passes");

        // Optimize the CFG in place
        use crate::optimization::CfgOptimizer;
        let optimizer = CfgOptimizer::default_passes();
        let _results = optimizer.optimize_program(cfg);

        self.logger.stage_success();
        Ok(())
    }

    fn write_ast_output(&self, ast: &AstProgram, cli: &Cli) -> Result<(), String> {
        let output_dir = cli.get_output_dir();
        std::fs::create_dir_all(&output_dir)
            .map_err(|e| format!("Failed to create output directory: {}", e))?;

        // Write AST files directly
        let ast_dump_path = output_dir.join("ast_dump.txt");
        let ast_pretty_path = output_dir.join("ast_pretty.txt");

        // Write AST dump
        let ast_dump = format!("{:#?}", ast);
        std::fs::write(&ast_dump_path, ast_dump)
            .map_err(|e| format!("Failed to write ast_dump.txt: {}", e))?;

        // Write AST pretty print
        use crate::pretty::print_program_to_string;
        let ast_pretty = print_program_to_string(ast)
            .map_err(|e| format!("Failed to pretty print AST: {}", e))?;
        std::fs::write(&ast_pretty_path, ast_pretty)
            .map_err(|e| format!("Failed to write ast_pretty.txt: {}", e))?;

        Ok(())
    }

    fn write_cfg_output(&self, cfg: &CfgProgram, cli: &Cli, prefix: &str) -> Result<(), String> {
        let output_dir = cli.get_output_dir();
        std::fs::create_dir_all(&output_dir)
            .map_err(|e| format!("Failed to create output directory: {}", e))?;

        // Write CFG files with prefix for optimization comparison
        let cfg_dump_path = output_dir.join(format!("{}_dump.txt", prefix));
        let cfg_pretty_path = output_dir.join(format!("{}_pretty.txt", prefix));

        // Write CFG dump
        let cfg_dump = format!("{:#?}", cfg);
        std::fs::write(&cfg_dump_path, cfg_dump)
            .map_err(|e| format!("Failed to write {}_dump.txt: {}", prefix, e))?;

        // Write CFG pretty print
        use crate::pretty::{CfgPrinter, PrettyPrinter};
        let printer = CfgPrinter::new();
        let cfg_pretty = printer
            .print_to_string(cfg)
            .map_err(|e| format!("Failed to pretty print CFG: {}", e))?;
        std::fs::write(&cfg_pretty_path, cfg_pretty)
            .map_err(|e| format!("Failed to write {}_pretty.txt: {}", prefix, e))?;

        Ok(())
    }

    fn write_scgraph_output(
        &self,
        scg: &crate::sc_graph::SCGraph,
        cfg: &CfgProgram,
        cli: &Cli,
    ) -> Result<(), String> {
        let output_dir = cli.get_output_dir();
        std::fs::create_dir_all(&output_dir)
            .map_err(|e| format!("Failed to create output directory: {}", e))?;

        // Write SC-Graph files
        let scgraph_dump_path = output_dir.join("scgraph_dump.txt");
        let scgraph_pretty_path = output_dir.join("scgraph_pretty.txt");
        let scgraph_dot_path = output_dir.join("scgraph.dot");

        // Write SC-Graph dump
        let scgraph_dump = format!("{:#?}", scg);
        std::fs::write(&scgraph_dump_path, scgraph_dump)
            .map_err(|e| format!("Failed to write scgraph_dump.txt: {}", e))?;

        // Write SC-Graph pretty print
        use crate::pretty::{DotPrinter, PrettyPrinter, SCGraphPrinter};
        let printer = SCGraphPrinter::new();
        let scgraph_pretty = printer
            .print_to_string(scg)
            .map_err(|e| format!("Failed to pretty print SC-Graph: {}", e))?;
        std::fs::write(&scgraph_pretty_path, scgraph_pretty)
            .map_err(|e| format!("Failed to write scgraph_pretty.txt: {}", e))?;

        // Write SC-Graph DOT file using generate_dot method
        let dot_printer = DotPrinter::new();
        let mut dot_content = Vec::new();
        dot_printer
            .generate_dot(scg, cfg, &mut dot_content)
            .map_err(|e| format!("Failed to generate DOT file: {}", e))?;
        std::fs::write(&scgraph_dot_path, dot_content)
            .map_err(|e| format!("Failed to write scgraph.dot: {}", e))?;

        Ok(())
    }

    fn write_verification_output(
        &self,
        boogie_programs: &[crate::verification::Boogie::BoogieProgram],
        cli: &Cli,
    ) -> Result<(), String> {
        let output_dir = cli.get_output_dir();
        let boogie_dir = output_dir.join("Boogie");
        std::fs::create_dir_all(&boogie_dir)
            .map_err(|e| format!("Failed to create Boogie directory: {}", e))?;

        for program in boogie_programs.iter() {
            let file_name = format!("{}.bpl", program.name);
            let file_path = boogie_dir.join(file_name);

            // Write Boogie program to file
            let content = program.to_string();
            std::fs::write(&file_path, content.as_bytes())
                .map_err(|e| format!("Failed to write Boogie file: {}", e))?;
        }

        Ok(())
    }

    fn write_compilation_summary(
        &self,
        ast: &AstProgram,
        cfg: &CfgProgram,
        scg: &crate::sc_graph::SCGraph,
        boogie_programs: &[crate::verification::Boogie::BoogieProgram],
        start: Instant,
        cli: &Cli,
    ) -> Result<(), String> {
        let output_dir = cli.get_output_dir();
        let compilation_time_ms = start.elapsed().as_millis() as u64;

        // Write compilation.log (simple text log)
        let log_path = output_dir.join("compilation.log");
        let log_content = format!(
            "Compilation completed successfully in {} ms\n\
             AST: {} functions, {} tables, {} partitions\n\
             CFG: {} basic blocks\n\
             SC-Graph: {} nodes, {} S-edges, {} C-edges\n\
             Verification: {} Boogie programs generated\n",
            compilation_time_ms,
            ast.functions.len(),
            ast.tables.len(),
            ast.partitions.len(),
            cfg.functions
                .iter()
                .map(|(_, f)| f.blocks.len())
                .sum::<usize>(),
            scg.nodes.len(),
            scg.edges
                .iter()
                .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
                .count(),
            scg.edges
                .iter()
                .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
                .count(),
            boogie_programs.len(),
        );
        std::fs::write(&log_path, log_content)
            .map_err(|e| format!("Failed to write compilation.log: {}", e))?;

        // Write summary.md (markdown summary)
        let summary_path = output_dir.join("summary.md");
        let summary_content = format!(
            "# FMitF Compilation Report\n\n\
             **Input**: {}\n\
             **Output Directory**: {}\n\
             **Compilation Time**: {} ms\n\
             **Status**: ✅ Success\n\n\
             ## Statistics\n\n\
             | Stage | Count |\n\
             |-------|-------|\n\
             | Functions | {} |\n\
             | Tables | {} |\n\
             | Partitions | {} |\n\
             | Basic Blocks | {} |\n\
             | SC-Graph Nodes | {} |\n\
             | S-edges | {} |\n\
             | C-edges | {} |\n\
             | Boogie Programs | {} |\n\n\
             ## Generated Files\n\n\
             - `ast_dump.txt` - AST debug dump\n\
             - `ast_pretty.txt` - Pretty-printed AST\n\
             - `cfg_dump.txt` - CFG debug dump\n\
             - `cfg_pretty.txt` - Pretty-printed CFG\n\
             - `scgraph_dump.txt` - SC-Graph debug dump\n\
             - `scgraph_pretty.txt` - Pretty-printed SC-Graph\n\
             - `scgraph.dot` - GraphViz DOT file\n\
             - `Boogie/*.bpl` - Boogie verification programs\n\
             - `compilation.log` - Compilation log\n",
            cli.input.display(),
            output_dir.display(),
            compilation_time_ms,
            ast.functions.len(),
            ast.tables.len(),
            ast.partitions.len(),
            cfg.functions
                .iter()
                .map(|(_, f)| f.blocks.len())
                .sum::<usize>(),
            scg.nodes.len(),
            scg.edges
                .iter()
                .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
                .count(),
            scg.edges
                .iter()
                .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
                .count(),
            boogie_programs.len(),
        );
        std::fs::write(&summary_path, summary_content)
            .map_err(|e| format!("Failed to write summary.md: {}", e))?;

        Ok(())
    }
}
