use clap::{Parser, ValueEnum};
use std::fs;
use std::io::{stdout, BufWriter, Write};
use std::path::PathBuf;

use FMitF_rs::{
    ast::parse_and_analyze,
    pretty::{
        print_cfg, print_program_to_writer, print_sc_graph, CfgFormat, CfgPrintOptions,
        PrintMode as AstPrintMode, PrintOptions as AstPrintOptions, SCGraphFormat,
        SCGraphPrintOptions,
    },
    sc_graph::SCGraph,
    verification::{AutoVerifier, VerificationError, VerificationResults},
    AstProgram, AstSpannedError, CfgBuilder, CfgProgram,
};

#[derive(Parser)]
#[command(name = "fmitf")]
#[command(about = "A chopped transaction serializability verification tool")]
#[command(version = "0.1.0")]
struct Cli {
    /// Input source file
    input: PathBuf,

    /// Processing mode - each mode includes all previous stages
    #[arg(short = 'm', long = "mode", default_value = "verify")]
    mode: Mode,

    /// Output file for ast/cfg/scgraph modes, or output directory for verify mode
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,

    /// Output directory for Boogie files (verify mode only)
    #[arg(long = "output-dir")]
    output_dir: Option<PathBuf>,

    /// Verbose output
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Show source spans in output
    #[arg(long = "show-spans")]
    show_spans: bool,

    /// Generate DOT output (for cfg and scgraph modes)
    #[arg(long = "dot")]
    dot: bool,

    /// Quiet mode - minimal output
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,

    /// Verification timeout in seconds (verify mode only)
    #[arg(long = "timeout", default_value = "30")]
    timeout: u32,
}

#[derive(ValueEnum, Clone, PartialEq, Debug)]
enum Mode {
    /// Parse and analyze source code (AST stage)
    Ast,
    /// Build Control Flow Graph (includes AST stage)
    Cfg,
    /// Build Serializability Conflict Graph (includes AST + CFG stages)
    Scgraph,
    /// Run verification and pruning (includes all previous stages)
    Verify,
}

/// Represents the compilation pipeline stages
struct Pipeline {
    ast_program: Option<AstProgram>,
    cfg_program: Option<CfgProgram>,
    sc_graph: Option<SCGraph>,
}

impl Pipeline {
    fn new() -> Self {
        Self {
            ast_program: None,
            cfg_program: None,
            sc_graph: None,
        }
    }

    /// Execute the pipeline up to the specified stage
    fn execute_to_stage(
        &mut self,
        source_code: &str,
        target_stage: Mode,
        cli: &Cli,
    ) -> Result<(), String> {
        // Stage 1: AST (always required)
        if self.ast_program.is_none() {
            self.run_ast_stage(source_code, cli)?;
        }

        // Stage 2: CFG (required for cfg, scgraph, verify)
        if matches!(target_stage, Mode::Cfg | Mode::Scgraph | Mode::Verify)
            && self.cfg_program.is_none()
        {
            self.run_cfg_stage(cli)?;
        }

        // Stage 3: SCGraph (required for scgraph, verify)
        if matches!(target_stage, Mode::Scgraph | Mode::Verify) && self.sc_graph.is_none() {
            self.run_scgraph_stage(cli)?;
        }

        Ok(())
    }

    fn run_ast_stage(&mut self, source_code: &str, cli: &Cli) -> Result<(), String> {
        if !cli.quiet {
            println!("⚙️ Stage 1/3: Frontend analysis (Parsing, Name Resolution, Semantics)...");
        }

        match parse_and_analyze(source_code) {
            Ok(program) => {
                if !cli.quiet {
                    println!("✅ AST stage completed successfully");
                }
                self.ast_program = Some(program);
                Ok(())
            }
            Err(errors) => {
                eprintln!("❌ AST stage failed with {} error(s):", errors.len());
                for error in errors {
                    print_spanned_error(&error, source_code);
                }
                Err("AST stage failed".to_string())
            }
        }
    }

    fn run_cfg_stage(&mut self, cli: &Cli) -> Result<(), String> {
        if !cli.quiet {
            println!("⚙️ Stage 2/3: Building Control Flow Graph (CFG)...");
        }

        let ast_program = self
            .ast_program
            .as_ref()
            .ok_or("AST stage must be completed before CFG stage")?;

        match CfgBuilder::build_from_program(ast_program) {
            Ok(cfg_ctx) => {
                if !cli.quiet {
                    println!("✅ CFG stage completed successfully");
                }
                // Extract the CfgProgram from CfgCtx
                // Note: We can't move out of cfg_ctx.program directly, so we need to handle this
                // For now, we'll store the entire context and access program through it
                // This is a limitation of the current Arena-based design
                self.cfg_program = Some(cfg_ctx.program);
                Ok(())
            }
            Err(e) => {
                eprintln!("❌ CFG stage failed: {}", e);
                Err("CFG stage failed".to_string())
            }
        }
    }

    fn run_scgraph_stage(&mut self, cli: &Cli) -> Result<(), String> {
        if !cli.quiet {
            println!("⚙️ Stage 3/3: Building Serializability Conflict Graph (SC-Graph)...");
        }

        let cfg_program = self
            .cfg_program
            .as_ref()
            .ok_or("CFG stage must be completed before SC-Graph stage")?;

        let sc_graph = SCGraph::new(cfg_program);

        if !cli.quiet {
            let (nodes, s_edges, c_edges) = sc_graph.stats();
            println!(
                "✅ SC-Graph stage completed: {} nodes, {} S-edges, {} C-edges",
                nodes, s_edges, c_edges
            );
        }
        self.sc_graph = Some(sc_graph);
        Ok(())
    }

    fn get_ast(&self) -> Result<&AstProgram, &'static str> {
        self.ast_program.as_ref().ok_or("AST stage not completed")
    }

    fn get_cfg(&self) -> Result<&CfgProgram, &'static str> {
        self.cfg_program.as_ref().ok_or("CFG stage not completed")
    }

    fn get_scgraph(&self) -> Result<&SCGraph, &'static str> {
        self.sc_graph.as_ref().ok_or("SC-Graph stage not completed")
    }

    #[allow(dead_code)]
    fn get_scgraph_mut(&mut self) -> Result<&mut SCGraph, &'static str> {
        self.sc_graph.as_mut().ok_or("SC-Graph stage not completed")
    }

    /// Run verification and pruning on the SC-Graph
    fn run_verification(
        &mut self,
        verifier: &mut AutoVerifier,
    ) -> Result<VerificationResults, String> {
        let cfg_program = self.cfg_program.as_ref().ok_or("CFG stage not completed")?;
        let sc_graph = self
            .sc_graph
            .as_mut()
            .ok_or("SC-Graph stage not completed")?;
        verifier
            .verify_and_prune_c_edges(cfg_program, sc_graph)
            .map_err(|e| format!("Verification error: {}", e))
    }
}

fn main() {
    let cli = Cli::parse();

    // Validate CLI arguments
    if let Err(e) = validate_cli(&cli) {
        eprintln!("❌ Invalid arguments: {}", e);
        std::process::exit(1);
    }

    // Read source file
    let source_code = match fs::read_to_string(&cli.input) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("❌ Failed to read file {:?}: {}", cli.input, e);
            std::process::exit(1);
        }
    };

    // Execute pipeline
    let mut pipeline = Pipeline::new();
    if let Err(e) = pipeline.execute_to_stage(&source_code, cli.mode.clone(), &cli) {
        eprintln!("❌ Pipeline execution failed: {}", e);
        std::process::exit(1);
    }

    // Generate output based on requested mode
    let result = match cli.mode {
        Mode::Ast => output_ast(&pipeline, &cli),
        Mode::Cfg => output_cfg(&pipeline, &cli),
        Mode::Scgraph => output_scgraph(&pipeline, &cli),
        Mode::Verify => output_verify(&mut pipeline, &cli),
    };

    if let Err(e) = result {
        eprintln!("❌ Output generation failed: {}", e);
        std::process::exit(1);
    }
}

fn validate_cli(cli: &Cli) -> Result<(), String> {
    // For verify mode, prefer --output-dir over --output for Boogie files
    if cli.mode == Mode::Verify {
        if cli.output.is_some() && cli.output_dir.is_some() {
            return Err("Cannot specify both --output and --output-dir for verify mode. Use --output-dir for Boogie files.".to_string());
        }
    } else {
        // For non-verify modes, --output-dir doesn't make sense
        if cli.output_dir.is_some() {
            return Err("--output-dir is only valid for verify mode".to_string());
        }
    }

    Ok(())
}

fn print_spanned_error(spanned_error: &AstSpannedError, source_code: &str) {
    if let Some(span_value) = &spanned_error.span {
        eprintln!(
            "Error: {:?} at line {}, column {}",
            spanned_error.error, span_value.line, span_value.column
        );
        if let Some(line_content) = source_code.lines().nth(span_value.line.saturating_sub(1)) {
            eprintln!("  |\n{} | {}", span_value.line, line_content);
            eprintln!("  | {}{}", " ".repeat(span_value.column), "^");
        }
    } else {
        eprintln!("Error: {:?}", spanned_error.error);
    }
}

fn get_writer(
    output_path: &Option<PathBuf>,
    quiet: bool,
) -> Result<BufWriter<Box<dyn Write>>, String> {
    match output_path {
        Some(path) => {
            let file = fs::File::create(path)
                .map_err(|e| format!("Failed to create file {:?}: {}", path, e))?;
            if !quiet {
                println!("💾 Output will be written to: {}", path.display());
            }
            Ok(BufWriter::new(Box::new(file)))
        }
        None => Ok(BufWriter::new(Box::new(stdout()))),
    }
}

fn output_ast(pipeline: &Pipeline, cli: &Cli) -> Result<(), String> {
    let ast_program = pipeline.get_ast()?;

    let opts = AstPrintOptions {
        mode: if cli.verbose {
            AstPrintMode::Verbose
        } else {
            AstPrintMode::Summary
        },
        show_spans: cli.show_spans,
    };

    let mut writer = get_writer(&cli.output, cli.quiet)?;
    print_program_to_writer(ast_program, &opts, &mut writer)
        .map_err(|e| format!("Failed to print AST: {}", e))?;
    writer
        .flush()
        .map_err(|e| format!("Failed to flush output: {}", e))?;

    Ok(())
}

fn output_cfg(pipeline: &Pipeline, cli: &Cli) -> Result<(), String> {
    let cfg_program = pipeline.get_cfg()?;

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

    let mut writer = get_writer(&cli.output, cli.quiet)?;
    print_cfg(cfg_program, &cfg_opts, &mut writer)
        .map_err(|e| format!("Failed to print CFG: {}", e))?;
    writer
        .flush()
        .map_err(|e| format!("Failed to flush output: {}", e))?;

    Ok(())
}

fn output_scgraph(pipeline: &Pipeline, cli: &Cli) -> Result<(), String> {
    let cfg_program = pipeline.get_cfg()?;
    let sc_graph = pipeline.get_scgraph()?;

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

    let mut writer = get_writer(&cli.output, cli.quiet)?;
    print_sc_graph(sc_graph, cfg_program, &sc_opts, &mut writer)
        .map_err(|e| format!("Failed to print SC-Graph: {}", e))?;
    writer
        .flush()
        .map_err(|e| format!("Failed to flush output: {}", e))?;

    Ok(())
}

fn output_verify(pipeline: &mut Pipeline, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("🔍 Starting verification process...");
    }

    // Determine output directory for Boogie files
    let boogie_output_dir = cli.output_dir.as_ref().or(cli.output.as_ref());

    // Set up AutoVerifier
    let mut verifier = AutoVerifier::new().with_timeout(cli.timeout);
    if let Some(output_path) = boogie_output_dir {
        verifier = verifier.with_output_dir(output_path);
        if !cli.quiet {
            println!(
                "💾 Boogie files will be saved to: {}",
                output_path.display()
            );
        }
    }

    // Run verification using the pipeline method to avoid borrowing conflicts
    let results = pipeline.run_verification(&mut verifier);

    match results {
        Ok(results) => {
            print_verification_results(&results, cli);
            // Get sc_graph reference for final state check
            let sc_graph = pipeline.get_scgraph()?;
            check_final_state(sc_graph, cli);
            Ok(())
        }
        Err(e) => Err(format!("Verification failed: {}", e)),
    }
}

fn print_verification_results(results: &VerificationResults, cli: &Cli) {
    if cli.quiet {
        // Even in quiet mode, show essential results
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

fn check_final_state(sc_graph: &SCGraph, cli: &Cli) {
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
