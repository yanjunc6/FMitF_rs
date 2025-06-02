use clap::{Parser, ValueEnum};
use std::fs;
use std::io::{stdout, BufWriter, Write}; // Added BufWriter for potential efficiency
use std::path::PathBuf;

use FMitF_rs::{
    ast::parse_and_analyze,
    pretty::{
        print_cfg,
        print_sc_graph,
        print_program_to_writer, // Add this import
        CfgFormat,
        CfgPrintOptions,
        PrintOptions as AstPrintOptions, // Alias to avoid confusion
        PrintMode as AstPrintMode, // Alias to avoid confusion
        SCGraphFormat,
        SCGraphPrintOptions,
    },
    sc_graph::SCGraph,
    AstProgram, // This is ast::Program
    AstSpannedError,
    CfgBuilder, // The builder struct
    CfgProgram, // The actual CFG program structure
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

    /// Generate DOT output (used for CFG and SCGraph modes)
    #[arg(long = "dot")]
    dot: bool,

    /// Quiet mode - minimal output, suppresses informational messages
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,
}

#[derive(ValueEnum, Clone, PartialEq, Debug)]
enum Mode {
    /// Show Abstract Syntax Tree
    Ast,
    /// Show Control Flow Graph
    Cfg,
    /// Show Serializability Conflict Graph
    Scgraph, // Added
    /// Run automatic verification (Not Implemented)
    Verify,
}

fn main() {
    let cli = Cli::parse();

    let source_code = match fs::read_to_string(&cli.input) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", cli.input, e);
            std::process::exit(1);
        }
    };

    if !cli.quiet {
        println!("⚙️ Running frontend analysis (Parsing, Name Resolution, Semantics)...");
    }
    let ast_program = match parse_and_analyze(&source_code) {
        Ok(prog) => {
            if !cli.quiet {
                println!("✅ Frontend analysis successful.");
            }
            prog
        }
        Err(errors) => {
            eprintln!(
                "❌ Frontend analysis failed with {} error(s):",
                errors.len()
            );
            for error in errors {
                print_spanned_error(&error, &source_code);
            }
            std::process::exit(1);
        }
    };

    // AST output is handled early if that's the selected mode
    if cli.mode == Mode::Ast {
        output_ast(&ast_program, &cli);
        return;
    }

    // For CFG, SCGraph, and Verify modes, we need the CFG
    if !cli.quiet && (cli.mode == Mode::Cfg || cli.mode == Mode::Scgraph || cli.mode == Mode::Verify) {
        println!("⚙️ Building Control Flow Graph (CFG)...");
    }
    let cfg_ctx = match CfgBuilder::build_from_program(&ast_program) {
        Ok(ctx) => {
            if !cli.quiet && (cli.mode == Mode::Cfg || cli.mode == Mode::Scgraph || cli.mode == Mode::Verify) {
                println!("✅ CFG build successful.");
            }
            ctx
        }
        Err(e) => {
            eprintln!("❌ CFG build failed: {}", e);
            std::process::exit(1);
        }
    };
    let cfg_program = &cfg_ctx.program; // Borrow program from CfgCtx

    // Generate requested output based on mode
    match cli.mode {
        Mode::Ast => { /* Handled above */ }
        Mode::Cfg => output_cfg(cfg_program, &cli),
        Mode::Scgraph => output_scgraph(cfg_program, &cli),
        Mode::Verify => output_verify(&ast_program, cfg_program, &cli),
    }
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

// Helper to get a writer (stdout or file)
fn get_writer(output_path: &Option<PathBuf>, quiet: bool) -> Result<BufWriter<Box<dyn Write>>, String> {
    match output_path {
        Some(path) => {
            let file = fs::File::create(path)
                .map_err(|e| format!("Failed to create file {:?}: {}", path, e))?;
            if !quiet {
                eprintln!("✅ Output will be written to: {:?}", path);
            }
            Ok(BufWriter::new(Box::new(file)))
        }
        None => Ok(BufWriter::new(Box::new(stdout()))),
    }
}

fn output_ast(program: &AstProgram, cli: &Cli) {
    let opts = AstPrintOptions {
        mode: if cli.verbose {
            AstPrintMode::Verbose
        } else {
            AstPrintMode::Summary
        },
        show_spans: cli.show_spans,
    };

    match get_writer(&cli.output, cli.quiet) {
        Ok(mut writer) => {
            if let Err(e) = print_program_to_writer(program, &opts, &mut writer) {
                eprintln!("❌ Failed to print AST: {}", e);
                std::process::exit(1);
            }
            writer.flush().expect("Failed to flush writer for AST");
        }
        Err(e) => {
            eprintln!("❌ {}", e);
            std::process::exit(1);
        }
    }
}

fn output_cfg(cfg_program: &CfgProgram, cli: &Cli) {
    let cfg_opts = CfgPrintOptions {
        format: if cli.dot {
            CfgFormat::Dot
        } else if cli.verbose {
            CfgFormat::Text // Verbose text
        } else {
            CfgFormat::Summary
        },
        verbose: cli.verbose, // Pass verbose flag for Text format
        quiet: cli.quiet,
        show_spans: cli.show_spans,
    };

    match get_writer(&cli.output, cli.quiet) {
        Ok(mut writer) => {
            if let Err(e) = print_cfg(cfg_program, &cfg_opts, &mut writer) {
                eprintln!("❌ Failed to print CFG: {}", e);
                std::process::exit(1);
            }
            writer.flush().expect("Failed to flush writer for CFG");
        }
        Err(e) => {
            eprintln!("❌ {}", e);
            std::process::exit(1);
        }
    }
}

fn output_scgraph(cfg_program: &CfgProgram, cli: &Cli) {
    if !cli.quiet {
        println!("⚙️ Building Serializability Conflict Graph (SC-Graph)...");
    }
    let sc_graph = SCGraph::new(cfg_program);
    if !cli.quiet {
        let (nodes, s_edges, c_edges) = sc_graph.stats();
        println!(
            "✅ SC-Graph built: {} nodes, {} S-edges, {} C-edges.",
            nodes, s_edges, c_edges
        );
    }

    let sc_opts = SCGraphPrintOptions {
        format: if cli.dot {
            SCGraphFormat::Dot
        } else if cli.verbose {
            SCGraphFormat::Text // Verbose text
        } else {
            SCGraphFormat::Summary
        },
        verbose: cli.verbose, // Pass verbose flag for Text format
        show_spans: cli.show_spans, // Though spans might not be directly used in SCGraph printer
    };

    match get_writer(&cli.output, cli.quiet) {
        Ok(mut writer) => {
            if let Err(e) = print_sc_graph(&sc_graph, cfg_program, &sc_opts, &mut writer) {
                eprintln!("❌ Failed to print SC-Graph: {}", e);
                std::process::exit(1);
            }
            writer.flush().expect("Failed to flush writer for SC-Graph");
        }
        Err(e) => {
            eprintln!("❌ {}", e);
            std::process::exit(1);
        }
    }
}

fn output_verify(_ast_program: &AstProgram, _cfg_program: &CfgProgram, cli: &Cli) {
    if !cli.quiet {
        println!("⚙️ Running verification (Not Implemented)...");
    }
    // TODO: Implement verification once AutoVerifier is updated.
    // It will likely need the SCGraph as well.
    // let sc_graph = SCGraph::new(cfg_program);
    // let verifier = AutoVerifier::new(...);
    // verifier.verify(&sc_graph, ...);
    eprintln!("❌ Verification mode not yet implemented.");
    if cli.output.is_some() {
        eprintln!("Output file specified, but verification is not implemented.");
    }
}
