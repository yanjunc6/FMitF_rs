use clap::{Parser, ValueEnum};
use std::fs;
use std::path::PathBuf;
use FMitF_rs::{
    format_errors, parse_program, print_dot_graph, print_program, PrintMode, PrintOptions, SCGraph,
    SemanticAnalyzer, AutoVerifier,
};

#[derive(Parser)]
#[command(name = "fmitf")]
#[command(about = "A chopped transaction serializability verification tool")]
#[command(version = "0.1.0")]
struct Cli {
    /// Input TransAct source file
    input: PathBuf,

    /// Output mode
    #[arg(short = 'm', long = "mode", default_value = "scgraph")]
    mode: Mode,

    /// Output file or directory (default: stdout)
    /// For verify mode: directory to save .bpl files
    /// For other modes: output file
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,

    /// Verbose output
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Show source spans in AST output
    #[arg(long = "show-spans")]
    show_spans: bool,

    /// Generate DOT output for scgraph mode
    #[arg(long = "dot")]
    dot: bool,

    /// Quiet mode - minimal output
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,
}

#[derive(ValueEnum, Clone, PartialEq)]
enum Mode {
    /// Show Abstract Syntax Tree
    Ast,
    /// Show Serializability Conflict Graph analysis (default)
    Scgraph,
    /// Run automatic verification and remove verified C edges
    Verify,
}

fn main() {
    let cli = Cli::parse();

    // Read input file
    let source_code = match fs::read_to_string(&cli.input) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", cli.input, e);
            std::process::exit(1);
        }
    };

    // Frontend: Parse + Semantic Analysis
    let program = run_frontend(&source_code, cli.quiet);

    // Generate requested output
    match cli.mode {
        Mode::Ast => output_ast(&program, &cli),
        Mode::Scgraph => output_scgraph(&program, &cli),
        Mode::Verify => output_verify(&program, &cli),
    }
}

fn run_frontend(source_code: &str, quiet: bool) -> FMitF_rs::Program {
    // Parse
    let program = match parse_program(source_code) {
        Ok(prog) => prog,
        Err(errors) => {
            eprintln!("❌ Parse failed!");
            eprintln!("{}", format_errors(&errors, source_code));
            std::process::exit(1);
        }
    };

    // Semantic Analysis
    let mut analyzer = SemanticAnalyzer::new();
    match analyzer.analyze(&program) {
        Ok(()) => {
            if !quiet {
                println!("✅ Frontend analysis passed!");
            }
        }
        Err(errors) => {
            eprintln!("❌ Semantic analysis failed!");
            eprintln!("{}", format_errors(&errors, source_code));
            std::process::exit(1);
        }
    }

    program
}

fn output_ast(program: &FMitF_rs::Program, cli: &Cli) {
    let print_mode = if cli.verbose {
        PrintMode::Verbose
    } else {
        PrintMode::Summary
    };

    match &cli.output {
        Some(file) => {
            // For file output, we'd need to capture print_program output
            // For now, write a placeholder to file
            let content = format!("AST output (mode: {:?}, show_spans: {})\nTODO: Implement actual AST output to file\n", print_mode, cli.show_spans);
            match fs::write(file, content) {
                Ok(()) => {
                    eprintln!("✅ Output written to: {:?}", file);
                }
                Err(e) => {
                    eprintln!("❌ Failed to write to file {:?}: {}", file, e);
                    std::process::exit(1);
                }
            }
        }
        None => {
            // For stdout, directly call print_program
            print_program(
                program,
                &PrintOptions {
                    mode: print_mode,
                    show_spans: cli.show_spans,
                },
            );
        }
    }
}

fn output_scgraph(program: &FMitF_rs::Program, cli: &Cli) {
    // Build SC-Graph
    let sc_graph = SCGraph::new(program);

    if cli.dot {
        // DOT output mode
        output_dot(&sc_graph, &cli);
    } else {
        // Regular scgraph analysis
        let output = format_scgraph_output(&sc_graph, cli.verbose, cli.quiet);
        write_output(&output, &cli.output);
    }
}

fn output_verify(program: &FMitF_rs::Program, cli: &Cli) {
    // Build SC-Graph
    let mut sc_graph = SCGraph::new(program);
    
    if !cli.quiet {
        println!("🔍 Starting verification process...");
    }

    // Show initial SC-Graph state if verbose
    if cli.verbose {
        let initial_output = format_scgraph_output(&sc_graph, true, false);
        println!("📊 Initial SC-Graph State:");
        println!("{}", initial_output);
        println!("{}", "=".repeat(80));
    }

    // Create auto verifier with optional output directory
    let auto_verifier = if let Some(ref output_path) = cli.output {
        if !cli.quiet {
            println!("📁 Saving Boogie files to: {}", output_path.display());
        }
        AutoVerifier::new().with_output_dir(output_path)
    } else {
        AutoVerifier::new()
    };

    // Run verification
    let verification_result = auto_verifier.verify_and_prune_c_edges(program, &mut sc_graph);

    let output = match verification_result {
        Ok(verified_count) => {
            let mut result = String::new();
            
            if !cli.quiet {
                result.push_str("🔧 Verification Results:\n");
                result.push_str(&format!("  ✅ Successfully verified and removed {} C edges\n", verified_count));
                
                if cli.output.is_some() && verified_count > 0 {
                    result.push_str(&format!("  📁 {} Boogie files saved to output directory\n", verified_count));
                }
            }

            // Show final statistics
            let (vertices, s_edges, c_edges) = sc_graph.stats();
            
            if !cli.quiet {
                result.push_str("\n📊 Final SC-Graph Statistics:\n");
                result.push_str(&format!("  Vertices (hops): {}\n", vertices));
                result.push_str(&format!("  Sequential edges: {}\n", s_edges));
                result.push_str(&format!("  Remaining conflict edges: {}\n", c_edges));
                result.push_str(&format!("  Total edges: {}\n", s_edges + c_edges));
            }

            // Show detailed final state if verbose
            if cli.verbose {
                result.push_str("\n🔍 Final Detailed Graph Structure:\n");

                // List vertices with function names
                result.push_str("Vertices:\n");
                for (i, hop) in sc_graph.hops.iter().enumerate() {
                    let function_name = match sc_graph.get_function_name(i) {
                        Some(name) => name.clone(),
                        None => "unknown".to_string(),
                    };
                    result.push_str(&format!(
                        "  {}: {} on {}\n",
                        i, function_name, hop.node.name
                    ));
                }

                // List remaining edges
                result.push_str("\nRemaining Edges:\n");
                if sc_graph.edges.is_empty() {
                    result.push_str("  (no edges remaining)\n");
                } else {
                    for edge in &sc_graph.edges {
                        let edge_symbol = match edge.edge_type {
                            FMitF_rs::EdgeType::S => "S",
                            FMitF_rs::EdgeType::C => "C",
                        };
                        result.push_str(&format!("  {} -- {} ({})\n", edge.v1, edge.v2, edge_symbol));
                    }
                }
            }

            // Analyze remaining mixed cycles
            let mixed_cycles = sc_graph.find_mixed_cycles();
            if mixed_cycles.is_empty() {
                if !cli.quiet {
                    result.push_str("\n✅ No mixed S/C cycles found after verification - SERIALIZABLE!\n");
                }
            } else {
                result.push_str(&format!(
                    "\n⚠️  Found {} remaining mixed cycles (unverified conflicts):\n",
                    mixed_cycles.len()
                ));
                for (i, cycle) in mixed_cycles.iter().enumerate() {
                    // Get vertex indices for this cycle
                    let cycle_indices: Vec<usize> = cycle
                        .iter()
                        .map(|hop| {
                            sc_graph
                                .hops
                                .iter()
                                .position(|h| std::rc::Rc::ptr_eq(h, hop))
                                .unwrap()
                        })
                        .collect();

                    let indices_str = cycle_indices
                        .iter()
                        .map(|idx| idx.to_string())
                        .collect::<Vec<_>>()
                        .join(" → ");

                    result.push_str(&format!(
                        "  {}. {} vertices: {}\n",
                        i + 1,
                        cycle.len(),
                        indices_str
                    ));
                }
                
                if !cli.quiet {
                    result.push_str("\n❌ Program may NOT be serializable due to remaining conflicts!\n");
                }
            }

            // Show verification summary
            if !cli.quiet {
                result.push_str("\n📋 Verification Summary:\n");
                result.push_str(&format!("  • Verified C edges: {}\n", verified_count));
                result.push_str(&format!("  • Remaining C edges: {}\n", c_edges));
                result.push_str(&format!("  • Remaining mixed cycles: {}\n", mixed_cycles.len()));
                
                if cli.output.is_some() && verified_count > 0 {
                    result.push_str(&format!("  • Boogie files saved to: {}\n", cli.output.as_ref().unwrap().display()));
                }
                
                if mixed_cycles.is_empty() && c_edges == 0 {
                    result.push_str("  • Status: ✅ SERIALIZABLE\n");
                } else if c_edges == 0 {
                    result.push_str("  • Status: ✅ POTENTIALLY SERIALIZABLE (no conflicts)\n");
                } else {
                    result.push_str("  • Status: ⚠️  UNKNOWN (unverified conflicts remain)\n");
                }
            }

            result
        }
        Err(e) => {
            format!("❌ Verification failed: {}\n", e)
        }
    };

    // For verify mode with output directory, only print to stdout (don't save text output)
    if cli.mode == Mode::Verify && cli.output.is_some() {
        print!("{}", output);
    } else {
        write_output(&output, &cli.output);
    }
}

fn output_dot(sc_graph: &SCGraph, cli: &Cli) {
    use std::io::Cursor;

    let mut buffer = Cursor::new(Vec::<u8>::new());
    match print_dot_graph(sc_graph, &mut buffer) {
        Ok(()) => {
            let dot_content = String::from_utf8(buffer.into_inner()).unwrap();
            write_output(&dot_content, &cli.output);
        }
        Err(e) => {
            eprintln!("❌ Failed to generate DOT output: {}", e);
            std::process::exit(1);
        }
    }
}

fn format_scgraph_output(sc_graph: &SCGraph, verbose: bool, quiet: bool) -> String {
    let mut output = String::new();

    // Statistics
    let (vertices, s_edges, c_edges) = sc_graph.stats();
    if !quiet {
        output.push_str("📊 SC-Graph Statistics:\n");
        output.push_str(&format!("  Vertices (hops): {}\n", vertices));
        output.push_str(&format!("  Sequential edges: {}\n", s_edges));
        output.push_str(&format!("  Conflict edges: {}\n", c_edges));
        output.push_str(&format!("  Total edges: {}\n", s_edges + c_edges));
    }

    // Verbose mode: list vertices and edges
    if verbose {
        output.push_str("\n🔍 Detailed Graph Structure:\n");

        // List vertices with function names
        output.push_str("Vertices:\n");
        for (i, hop) in sc_graph.hops.iter().enumerate() {
            let function_name = match sc_graph.get_function_name(i) {
                Some(name) => name.clone(),
                None => "unknown".to_string(),
            };
            output.push_str(&format!(
                "  {}: {} on {}\n",
                i, function_name, hop.node.name
            ));
        }

        // List edges
        output.push_str("\nEdges:\n");
        if sc_graph.edges.is_empty() {
            output.push_str("  (no edges)\n");
        } else {
            for edge in &sc_graph.edges {
                let edge_symbol = match edge.edge_type {
                    FMitF_rs::EdgeType::S => "S",
                    FMitF_rs::EdgeType::C => "C",
                };
                output.push_str(&format!("  {} -- {} ({})\n", edge.v1, edge.v2, edge_symbol));
            }
        }
    }

    // Analyze cycles
    let mixed_cycles = sc_graph.find_mixed_cycles();
    if mixed_cycles.is_empty() {
        if !quiet {
            output.push_str("\n✅ No mixed S/C cycles found - potentially serializable!\n");
        }
    } else {
        output.push_str(&format!(
            "\n❌ Found {} mixed cycles (potential serializability violations):\n",
            mixed_cycles.len()
        ));
        for (i, cycle) in mixed_cycles.iter().enumerate() {
            // Get vertex indices for this cycle
            let cycle_indices: Vec<usize> = cycle
                .iter()
                .map(|hop| {
                    sc_graph
                        .hops
                        .iter()
                        .position(|h| std::rc::Rc::ptr_eq(h, hop))
                        .unwrap()
                })
                .collect();

            let indices_str = cycle_indices
                .iter()
                .map(|idx| idx.to_string())
                .collect::<Vec<_>>()
                .join(" → ");

            output.push_str(&format!(
                "  {}. {} vertices: {}\n",
                i + 1,
                cycle.len(),
                indices_str
            ));
        }
    }

    output
}

fn write_output(content: &str, output_file: &Option<PathBuf>) {
    match output_file {
        Some(file) => match fs::write(file, content) {
            Ok(()) => {
                eprintln!("✅ Output written to: {:?}", file);
            }
            Err(e) => {
                eprintln!("❌ Failed to write to file {:?}: {}", file, e);
                std::process::exit(1);
            }
        },
        None => {
            print!("{}", content);
        }
    }
}
