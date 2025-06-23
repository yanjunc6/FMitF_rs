// src/cli/stages.rs
use super::{DirectoryOutput, FileOutput, PipelineStage, StageSummary};
use crate::{
    ast::parse_and_analyze,
    optimization::CfgOptimizer,
    pretty::{
        print_cfg, print_program_to_writer, print_sc_graph, CfgFormat, CfgPrintOptions,
        PrintMode as AstPrintMode, PrintOptions as AstPrintOptions, SCGraphFormat,
        SCGraphPrintOptions,
    },
    sc_graph::SCGraph,
    verification::{VerificationManager, VerificationResult},
    AstProgram, AstSpannedError, CfgBuilder, CfgProgram,
};
use std::io::Write;
use std::path::PathBuf;

// Wrapper functions to handle trait bound conversion
fn write_ast_program(
    data: &AstProgram,
    opts: &AstPrintOptions,
    writer: &mut dyn Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut cursor = std::io::Cursor::new(Vec::new());
    print_program_to_writer(data, opts, &mut cursor)?;
    writer.write_all(cursor.get_ref())?;
    Ok(())
}

fn write_cfg_program(
    data: &CfgProgram,
    opts: &CfgPrintOptions,
    writer: &mut dyn Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut cursor = std::io::Cursor::new(Vec::new());
    print_cfg(data, opts, &mut cursor)?;
    writer.write_all(cursor.get_ref())?;
    Ok(())
}

fn write_sc_graph_data(
    sc_graph: &SCGraph,
    cfg_program: &CfgProgram,
    opts: &SCGraphPrintOptions,
    writer: &mut dyn Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut cursor = std::io::Cursor::new(Vec::new());
    print_sc_graph(sc_graph, cfg_program, opts, &mut cursor)?;
    writer.write_all(cursor.get_ref())?;
    Ok(())
}

// AST Stage
pub struct AstStage;

impl PipelineStage for AstStage {
    type Input = String; // source code
    type Output = AstProgram;
    type Error = Vec<AstSpannedError>;

    fn execute(&mut self, source_code: String) -> Result<Self::Output, Self::Error> {
        parse_and_analyze(&source_code)
    }

    fn name(&self) -> &'static str {
        "Frontend analysis (Parsing, Name Resolution, Semantics)"
    }

    fn stage_number(&self) -> usize {
        1
    }
}

impl FileOutput for AstStage {
    type Data = AstProgram;

    fn write_output(
        &self,
        data: &Self::Data,
        writer: &mut dyn Write,
        cli: &super::Cli,
    ) -> Result<(), String> {
        let opts = AstPrintOptions {
            mode: if cli.verbose {
                AstPrintMode::Verbose
            } else {
                AstPrintMode::Summary
            },
            show_spans: cli.show_spans,
        };

        write_ast_program(data, &opts, writer).map_err(|e| format!("Failed to print AST: {}", e))
    }
}

// CFG Stage
pub struct CfgStage;

impl PipelineStage for CfgStage {
    type Input = AstProgram;
    type Output = CfgProgram;
    type Error = String;

    fn execute(&mut self, ast_program: AstProgram) -> Result<Self::Output, Self::Error> {
        CfgBuilder::build_from_program(&ast_program)
            .map(|ctx| ctx.program)
            .map_err(|e| format!("CFG building failed: {}", e))
    }

    fn name(&self) -> &'static str {
        "Building Control Flow Graph (CFG)"
    }

    fn stage_number(&self) -> usize {
        2
    }
}

impl FileOutput for CfgStage {
    type Data = CfgProgram;

    fn write_output(
        &self,
        data: &Self::Data,
        writer: &mut dyn Write,
        cli: &super::Cli,
    ) -> Result<(), String> {
        let cfg_opts = CfgPrintOptions {
            format: if cli.dot {
                CfgFormat::Dot
            } else if cli.verbose {
                CfgFormat::Text
            } else {
                CfgFormat::Summary
            },
            verbose: cli.verbose,
            quiet: cli.quiet,
            show_spans: cli.show_spans,
        };

        write_cfg_program(data, &cfg_opts, writer)
            .map_err(|e| format!("Failed to print CFG: {}", e))
    }
}

// Optimization Stage
pub struct OptimizeStage {
    pub skip_optimization: bool,
}

impl PipelineStage for OptimizeStage {
    type Input = CfgProgram;
    type Output = CfgProgram;
    type Error = String;

    fn execute(&mut self, mut cfg_program: CfgProgram) -> Result<Self::Output, Self::Error> {
        if self.skip_optimization {
            return Ok(cfg_program);
        }

        let optimizer = CfgOptimizer::default_passes();
        let _opt_result = optimizer.optimize_program(&mut cfg_program);

        Ok(cfg_program)
    }

    fn name(&self) -> &'static str {
        "Optimizing Control Flow Graph"
    }

    fn stage_number(&self) -> usize {
        3
    }
}

impl FileOutput for OptimizeStage {
    type Data = CfgProgram;

    fn write_output(
        &self,
        data: &Self::Data,
        writer: &mut dyn Write,
        cli: &super::Cli,
    ) -> Result<(), String> {
        let cfg_opts = CfgPrintOptions {
            format: if cli.dot {
                CfgFormat::Dot
            } else if cli.verbose {
                CfgFormat::Text
            } else {
                CfgFormat::Summary
            },
            verbose: cli.verbose,
            quiet: cli.quiet,
            show_spans: cli.show_spans,
        };

        write_cfg_program(data, &cfg_opts, writer)
            .map_err(|e| format!("Failed to print optimized CFG: {}", e))
    }
}

// SC-Graph Stage
pub struct ScGraphStage;

impl PipelineStage for ScGraphStage {
    type Input = CfgProgram;
    type Output = (CfgProgram, SCGraph);
    type Error = String;

    fn execute(&mut self, cfg_program: CfgProgram) -> Result<Self::Output, Self::Error> {
        let sc_graph = SCGraph::new(&cfg_program);
        Ok((cfg_program, sc_graph))
    }

    fn name(&self) -> &'static str {
        "Building Serializability Conflict Graph (SC-Graph)"
    }

    fn stage_number(&self) -> usize {
        4
    }
}

impl FileOutput for ScGraphStage {
    type Data = (CfgProgram, SCGraph);

    fn write_output(
        &self,
        data: &Self::Data,
        writer: &mut dyn Write,
        cli: &super::Cli,
    ) -> Result<(), String> {
        let (cfg_program, sc_graph) = data;
        let sc_opts = SCGraphPrintOptions {
            format: if cli.dot {
                SCGraphFormat::Dot
            } else if cli.verbose {
                SCGraphFormat::Text
            } else {
                SCGraphFormat::Summary
            },
            verbose: cli.verbose,
            show_spans: cli.show_spans,
        };

        write_sc_graph_data(sc_graph, cfg_program, &sc_opts, writer)
            .map_err(|e| format!("Failed to print SC-Graph: {}", e))
    }
}

impl StageSummary for ScGraphStage {
    type Data = (CfgProgram, SCGraph);

    fn get_summary(&self, data: &Self::Data) -> String {
        let (_, sc_graph) = data;
        let (nodes, s_edges, c_edges) = sc_graph.stats();
        format!("{} nodes, {} S-edges, {} C-edges", nodes, s_edges, c_edges)
    }
}

// Verification Stage
pub struct VerificationStage {
    pub timeout: u32,
    pub boogie_output_dir: Option<PathBuf>, // Added field to store Boogie output directory
}

impl PipelineStage for VerificationStage {
    type Input = (CfgProgram, SCGraph);
    type Output = (CfgProgram, SCGraph, VerificationManager);
    type Error = String;

    fn execute(&mut self, input: Self::Input) -> Result<Self::Output, Self::Error> {
        let (cfg_program, sc_graph) = input;

        // Create verification manager using our new verification module
        let mut verification_manager = VerificationManager::new();

        // Run the commutativity pipeline
        verification_manager.run_commutativity_pipeline(&cfg_program, &sc_graph);

        // If Boogie output directory is specified, save the Boogie files
        if let Some(ref dir) = self.boogie_output_dir {
            verification_manager
                .save_boogie_files(dir)
                .map_err(|e| format!("Failed to save Boogie files: {}", e))?;
        }

        Ok((cfg_program, sc_graph, verification_manager))
    }

    fn name(&self) -> &'static str {
        "Verification and C-edge Pruning"
    }

    fn stage_number(&self) -> usize {
        5
    }
}

impl DirectoryOutput for VerificationStage {
    type Data = (CfgProgram, SCGraph, VerificationManager);

    fn write_to_directory(
        &self,
        _data: &Self::Data,
        dir: &PathBuf,
        cli: &super::Cli,
    ) -> Result<(), String> {
        // Ensure the general output directory for this stage exists
        std::fs::create_dir_all(dir)
            .map_err(|e| format!("Failed to create output directory {:?}: {}", dir, e))?;

        if let Some(boogie_dir) = &self.boogie_output_dir {
            let logger = super::Logger::new(cli.verbose, cli.quiet);
            logger.boogie_files_saved(boogie_dir);
        }

        Ok(())
    }
}

impl StageSummary for VerificationStage {
    type Data = (CfgProgram, SCGraph, VerificationManager);

    fn get_summary(&self, data: &Self::Data) -> String {
        let (_, _, manager) = data;
        let results = &manager.results;
        format!(
            "{} C-edges processed, {} successful",
            results.len(),
            results
                .values()
                .filter(|r| matches!(r, VerificationResult::Success))
                .count()
        )
    }
}

/// Print detailed verification results
pub fn print_verification_results(manager: &VerificationManager, logger: &super::Logger) {
    let total = manager.results.len();
    let successful = manager
        .results
        .values()
        .filter(|r| matches!(r, VerificationResult::Success))
        .count();

    if total == 0 {
        logger.info_positive("No C-edges found for verification.");
        return;
    }

    let success_rate = (successful as f64 / total as f64) * 100.0;
    logger.verification_result(successful, total, success_rate);

    // Use the logger's verification_details method for verbose output
    logger.verification_details(|| {
        manager
            .results
            .iter()
            .map(|(edge, result)| {
                let edge_info = format!("Edge {}→{}", edge.source.index(), edge.target.index());
                match result {
                    crate::verification::execution::VerificationResult::Success => {
                        (edge_info, true, None)
                    }
                    crate::verification::execution::VerificationResult::Failure(msg) => {
                        (edge_info, false, Some(msg.clone()))
                    }
                }
            })
            .collect()
    });
}

/// Check final state after verification
pub fn check_final_state(sc_graph: &SCGraph, logger: &super::Logger) {
    let mixed_cycles = sc_graph.find_mixed_cycles();

    if !mixed_cycles.is_empty() {
        let cycle_strings: Vec<String> = mixed_cycles
            .iter()
            .enumerate()
            .map(|(i, cycle)| {
                let cycle_str: Vec<String> = cycle
                    .iter()
                    .map(|hop_id| format!("H{}", hop_id.index()))
                    .collect();
                format!("Cycle {}: {}", i + 1, cycle_str.join(" -> "))
            })
            .collect();

        logger.mixed_cycles_status(mixed_cycles.len(), Some(cycle_strings));
    } else {
        logger.mixed_cycles_status(0, None);
    }
}
