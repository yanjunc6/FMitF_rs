use clap::{Parser, ValueEnum};
use std::fs;
use std::path::PathBuf;
// use std::io::Write; // Required for stdout in output_cfg

// Assuming these are correctly re-exported from your src/lib.rs
use FMitF_rs::{
    ast::parse_and_analyze, // Adjusted for new AST API
    pretty::{print_cfg, print_program, CfgFormat, CfgPrintOptions, PrintMode, PrintOptions},
    // SCGraph and EdgeType are assumed to be unavailable or commented out as per lib.rs indications
    // graph::{SCGraph, EdgeType},
    AstProgram,
    AstSpannedError,
    CfgBuilder,
};

#[derive(Parser)]
#[command(name = "fmitf")]
#[command(about = "A chopped transaction serializability verification tool")]
#[command(version = "0.1.0")]
struct Cli {
    /// Input TransAct source file
    input: PathBuf,

    /// Output mode
    #[arg(short = 'm', long = "mode", default_value = "cfg")]
    // Changed default as SCGraph is out
    mode: Mode,

    /// Output file or directory (default: stdout)
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,

    /// Verbose output
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Show source spans in output
    #[arg(long = "show-spans")]
    show_spans: bool,

    /// Generate DOT output (used for CFG mode)
    #[arg(long = "dot")]
    dot: bool,

    /// Quiet mode - minimal output
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,
}

#[derive(ValueEnum, Clone, PartialEq, Debug)]
enum Mode {
    /// Show Abstract Syntax Tree
    Ast,
    // /// Show Serializability Conflict Graph analysis (default)
    // Scgraph, // Commented out as SCGraph functionality seems unavailable
    /// Run automatic verification and remove verified C edges
    Verify,
    /// Show Control Flow Graph (CFG)
    Cfg,
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

    // Frontend: Parse + Semantic Analysis using the new unified API
    let program = run_frontend(&source_code, &cli);

    // Generate requested output
    match cli.mode {
        Mode::Ast => output_ast(&program, &cli),
        // Mode::Scgraph => output_scgraph(&program, &cli), // SCGraph mode commented out
        Mode::Verify => output_verify(&program, &cli),
        Mode::Cfg => output_cfg(&program, &cli),
    }
}

fn run_frontend(source_code: &str, cli: &Cli) -> AstProgram {
    match parse_and_analyze(source_code) {
        Ok(prog) => {
            if !cli.quiet {
                println!("✅ Frontend analysis passed!");
            }
            prog
        }
        Err(errors) => {
            eprintln!(
                "❌ Frontend analysis failed with {} error(s):",
                errors.len()
            );
            for error in errors {
                print_spanned_error(&error, source_code);
            }
            std::process::exit(1);
        }
    }
}

fn print_spanned_error(spanned_error: &AstSpannedError, source_code: &str) {
    if let Some(span_value) = &spanned_error.span {
        eprintln!(
            "Error: {:?} at line {}, column {}",
            spanned_error.error, span_value.line, span_value.column
        );
        // Optionally print the line from source_code
        if let Some(line_content) = source_code.lines().nth(span_value.line.saturating_sub(1)) {
            eprintln!("  |\n{} | {}", span_value.line, line_content);
            eprintln!("  | {}{}", " ".repeat(span_value.column), "^");
        }
    } else {
        // If there's no span, just print the error.
        eprintln!("Error: {:?}", spanned_error.error);
    }
}

fn output_ast(program: &AstProgram, cli: &Cli) {
    let print_mode = if cli.verbose {
        PrintMode::Verbose
    } else {
        PrintMode::Summary
    };

    let opts = PrintOptions {
        mode: print_mode.clone(),
        show_spans: cli.show_spans,
    };

    match &cli.output {
        Some(file) => {
            // TODO: Implement capture-to-string for print_program or make print_program take a Write trait.
            // For now, this will just inform that AST would be printed, not actually write it to file.
            // Or, we can attempt to use a buffer, but that requires print_program to be flexible.
            // As a placeholder, we'll write a message to the file.
            let content = format!(
                "AST output (mode: {:?}, show_spans: {})\n(Full AST content would be here if file writing was fully supported for AST printer)\n",
                print_mode, cli.show_spans
            );
            if !cli.quiet {
                eprintln!("Note: AST pretty printing to file is currently a placeholder.");
                eprintln!("Printing AST to stdout instead if no file is specified, or placeholder to file.");
            }
            // For demonstration, if you want to see it on stdout when a file is specified:
            // print_program(program, &opts);
            // eprint!("Would write AST to {:?}\n", file);
            match fs::write(file, content) {
                Ok(()) => {
                    if !cli.quiet {
                        eprintln!("✅ (Placeholder) AST output info written to: {:?}", file);
                    }
                }
                Err(e) => {
                    eprintln!("❌ Failed to write to file {:?}: {}", file, e);
                    std::process::exit(1);
                }
            }
        }
        None => {
            // Print directly to stdout
            print_program(program, &opts);
        }
    }
}

fn output_cfg(program: &AstProgram, cli: &Cli) {
    if !cli.quiet {
        println!("⚙️ Building Control Flow Graph (CFG)...");
    }

    let cfg_program = CfgBuilder::build_from_program(program).unwrap_or_else(|e| {
        eprintln!("❌ CFG build failed: {}", e); // Assuming e is String or displays well
        std::process::exit(1);
    });

    let cfg_program = cfg_program.program;

    let cfg_opts = CfgPrintOptions {
        format: if cli.dot {
            CfgFormat::Dot
        } else {
            CfgFormat::Text
        },
        verbose: cli.verbose,
        quiet: cli.quiet,
        show_spans: cli.show_spans,
    };

    match &cli.output {
        Some(file) => match fs::File::create(file) {
            Ok(mut file_handle) => {
                if let Err(e) = print_cfg(&cfg_program, &cfg_opts, &mut file_handle) {
                    eprintln!("❌ Failed to write CFG to file: {}", e);
                    std::process::exit(1);
                }
                if !cli.quiet {
                    eprintln!("✅ CFG output written to: {:?}", file);
                }
            }
            Err(e) => {
                eprintln!("❌ Failed to create file {:?}: {}", file, e);
                std::process::exit(1);
            }
        },
        None => {
            let mut stdout_handle = std::io::stdout();
            if let Err(e) = print_cfg(&cfg_program, &cfg_opts, &mut stdout_handle) {
                eprintln!("❌ Failed to print CFG: {}", e);
                std::process::exit(1);
            }
        }
    }
}

// SCGraph related functionality is commented out as its availability is uncertain.
/*
fn output_scgraph(program: &AstProgram, cli: &Cli) {
    if !cli.quiet {
        println!("⚙️ Building Serializability Conflict Graph (SC-Graph)...");
    }
    // let sc_graph = SCGraph::new(program); // Assuming SCGraph::new exists and works with AstProgram

    // if cli.dot {
    //     output_dot_scgraph(&sc_graph, cli);
    // } else {
    //     let output = format_scgraph_output(&sc_graph, cli.verbose, cli.quiet);
    //     write_output_content(&output, &cli.output, cli.quiet);
    // }
    eprintln!("❌ SC-Graph mode is currently disabled.");
}

fn output_dot_scgraph(sc_graph: &SCGraph, cli: &Cli) {
    // let dot_content = sc_graph.to_dot(); // Assuming a method like this exists
    let dot_content = format!("digraph SCGraph {{\n  // TODO: Implement SCGraph DOT output\n}}");
    write_output_content(&dot_content, &cli.output, cli.quiet);
}

fn format_scgraph_output(sc_graph: &SCGraph, verbose: bool, quiet: bool) -> String {
    let mut output = String::new();
    // ... (Implementation would go here if SCGraph was available) ...
    // This is a placeholder implementation.
    if !quiet {
        output.push_str("📊 SC-Graph (Placeholder Output):\n");
        output.push_str(&format!("  Vertices: {}\n", sc_graph.hops.len())); // Example
        output.push_str(&format!("  Edges: {}\n", sc_graph.edges.len()));   // Example
    }
    if verbose {
        output.push_str("\n🔍 Detailed Graph Structure (Placeholder):\n");
        // ...
    }
    output.push_str("\nAnalysis results (Placeholder)...\n");
    output
}
*/

fn output_verify(_program: &AstProgram, cli: &Cli) {
    if !cli.quiet {
        println!("⚙️ Running verification (Not Implemented)...");
    }
    // TODO: Implement verification once AutoVerifier is updated for new AST/CFG.
    eprintln!("❌ Verification mode not yet implemented for the current AST/CFG structure.");
    if cli.output.is_some() {
        eprintln!("Output file specified but verification is not implemented.");
    }
    // std::process::exit(1); // Optionally exit if it's a critical unimplemented feature
}

// Generic output writing function (replaces the old write_output)
// fn write_output_content(content: &str, output_file: &Option<PathBuf>, quiet: bool) {
//     match output_file {
//         Some(file) => {
//             match fs::write(file, content) {
//                 Ok(()) => {
//                     if !quiet {
//                         eprintln!("✅ Output written to: {:?}", file);
//                     }
//                 }
//                 Err(e) => {
//                     eprintln!("❌ Failed to write to file {:?}: {}", file, e);
//                     std::process::exit(1);
//                 }
//             }
//         }
//         None => {
//             print!("{}", content);
//         }
//     }
// }
