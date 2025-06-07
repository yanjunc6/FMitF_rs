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
    verification::{AutoVerifier, VerificationResults},
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
    type Output = (CfgProgram, SCGraph, VerificationResults);
    type Error = String;

    fn execute(&mut self, input: Self::Input) -> Result<Self::Output, Self::Error> {
        let (cfg_program, mut sc_graph) = input;

        let mut verifier = AutoVerifier::new().with_timeout(self.timeout); // Create base verifier

        if let Some(ref dir) = self.boogie_output_dir {
            // If an output directory for Boogie files is specified, configure the verifier
            verifier = verifier.with_output_dir(dir.clone());
        }

        let results = verifier
            .verify_and_prune_c_edges(&cfg_program, &mut sc_graph)
            .map_err(|e| format!("Verification error: {}", e))?;

        Ok((cfg_program, sc_graph, results))
    }

    fn name(&self) -> &'static str {
        "Verification and C-edge Pruning"
    }

    fn stage_number(&self) -> usize {
        5
    }
}

impl DirectoryOutput for VerificationStage {
    type Data = (CfgProgram, SCGraph, VerificationResults);

    fn write_to_directory(
        &self,
        _data: &Self::Data,
        dir: &PathBuf, // General output directory for the stage, passed by the pipeline runner
        cli: &super::Cli,
    ) -> Result<(), String> {
        // Ensure the general output directory for this stage exists.
        // AutoVerifier handles creation of self.boogie_output_dir internally if it's set.
        std::fs::create_dir_all(dir)
            .map_err(|e| format!("Failed to create output directory {:?}: {}", dir, e))?;

        if !cli.quiet {
            if let Some(boogie_dir) = &self.boogie_output_dir {
                println!("💾 Boogie files are configured to be saved to: {}", boogie_dir.display());
                // Optionally, note if the general pipeline dir is different, though ideally they align for this stage's main output.
                if boogie_dir != dir {
                    println!("   (Note: General pipeline output directory for this stage is {})", dir.display());
                }
            } else {
                println!("💾 Boogie file output directory not specified. Files will not be saved by the verifier.");
            }
        }

        // The AutoVerifier, if configured with an output directory,
        // should have already written files. This method primarily handles
        // messages and ensures the pipeline's designated 'dir' exists.
        Ok(())
    }
}

impl StageSummary for VerificationStage {
    type Data = (CfgProgram, SCGraph, VerificationResults);

    fn get_summary(&self, data: &Self::Data) -> String {
        let (_, sc_graph, results) = data;
        let mixed_cycles = sc_graph.find_mixed_cycles();

        format!(
            "{}/{} C-edges verified ({:.1}%), {} mixed cycles remain",
            results.verified_count,
            results.total_c_edges,
            results.success_rate() * 100.0,
            mixed_cycles.len()
        )
    }
}

/// Print detailed verification results
pub fn print_verification_results(results: &VerificationResults, cli: &super::Cli) {
    if cli.quiet {
        println!(
            "Verification: {}/{} C-edges verified ({:.1}%)",
            results.verified_count,
            results.total_c_edges,
            results.success_rate() * 100.0
        );
        return;
    }

    println!("✅ Verification completed");
    println!("📊 Results Summary:");
    println!("   Total C-edges analyzed: {}", results.total_c_edges);
    println!(
        "   Successfully verified (pruned): {}",
        results.verified_count
    );
    println!(
        "   Failed verification: {}",
        results.failed_verification.len()
    );
    println!("   Errors encountered: {}", results.errors.len());
    println!("   Success rate: {:.1}%", results.success_rate() * 100.0);

    if cli.verbose {
        if !results.verified_edges.is_empty() {
            println!(
                "\n✅ Verified C-edges (pruned): {:?}",
                results.verified_edges
            );
        }

        if !results.failed_verification.is_empty() {
            println!("\n❌ Failed verification:");
            for (edge_idx, reason) in &results.failed_verification {
                println!("   Edge {}: {}", edge_idx, reason);
            }
        }

        if !results.errors.is_empty() {
            println!("\n⚠️ Errors encountered:");
            for (edge_idx, error) in &results.errors {
                println!("   Edge {}: {}", edge_idx, error);
            }
        }
    } else if !results.errors.is_empty() {
        println!(
            "\n⚠️ {} errors encountered (use --verbose for details)",
            results.errors.len()
        );
    }
}

/// Check final state after verification
pub fn check_final_state(sc_graph: &SCGraph, cli: &super::Cli) {
    let mixed_cycles = sc_graph.find_mixed_cycles();

    if !mixed_cycles.is_empty() {
        println!(
            "\n⚠️ Warning: {} mixed S/C cycles remain after verification",
            mixed_cycles.len()
        );
        if !cli.quiet {
            println!("   This may indicate potential serializability violations");
            if cli.verbose {
                println!("   Remaining cycles:");
                for (i, cycle) in mixed_cycles.iter().enumerate() {
                    let cycle_str: Vec<String> = cycle
                        .iter()
                        .map(|node_id| format!("N{}", node_id.index()))
                        .collect();
                    println!("     Cycle {}: {}", i + 1, cycle_str.join(" -> "));
                }
            }
        }
    } else if !cli.quiet {
        println!("\n✅ No mixed S/C cycles detected - system appears serializable");
    }
}
