use clap::{Parser, ValueEnum};
use FMitF_rs::{parse_program, format_errors, SCGraph, SemanticAnalyzer, print_program, PrintOptions, PrintMode};
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "fmitf")]
#[command(about = "A chopped transaction serializability verification tool")]
#[command(version = "0.1.0")]
struct Cli {
    /// Input TransAct source file
    input: PathBuf,

    /// What to output
    #[arg(short = 'o', long = "output", default_value = "scgraph")]
    output: Output,

    /// Quiet mode - minimal output
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,

    // AST-specific options
    /// AST output mode (only for --output ast)
    #[arg(short = 'm', long = "mode", default_value = "summary")]
    ast_mode: AstMode,

    /// Show source spans in AST (only for --output ast)
    #[arg(long = "show-spans")]
    show_spans: bool,
}

#[derive(ValueEnum, Clone)]
enum Output {
    /// Show Abstract Syntax Tree
    Ast,
    /// Show Serializability Conflict Graph analysis (default)
    Scgraph,
}

#[derive(ValueEnum, Clone)]
enum AstMode {
    /// Summary AST output
    Summary,
    /// Verbose AST output  
    Verbose,
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
    match cli.output {
        Output::Ast => output_ast(&program, &cli),
        Output::Scgraph => output_scgraph(&program, &cli),
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
    let print_mode = match cli.ast_mode {
        AstMode::Summary => PrintMode::Summary,
        AstMode::Verbose => PrintMode::Verbose,
    };
    
    print_program(program, &PrintOptions {
        mode: print_mode,
        show_spans: cli.show_spans,
    });
}

fn output_scgraph(program: &FMitF_rs::Program, cli: &Cli) {
    // Build SC-Graph
    let sc_graph = SCGraph::new(program);
    
    // Always show graph statistics for scgraph output
    let (vertices, s_edges, c_edges) = sc_graph.stats();
    if !cli.quiet {
        println!("📊 SC-Graph Statistics:");
        println!("  Vertices (hops): {}", vertices);
        println!("  Sequential edges: {}", s_edges);
        println!("  Conflict edges: {}", c_edges);
        println!("  Total edges: {}", s_edges + c_edges);
    }
    
    // Analyze cycles
    let mixed_cycles = sc_graph.find_mixed_cycles();
    if mixed_cycles.is_empty() {
        if !cli.quiet {
            println!("✅ No mixed S/C cycles found - potentially serializable!");
        }
    } else {
        println!("❌ Found {} mixed cycles (potential serializability violations):", mixed_cycles.len());
        for (i, cycle) in mixed_cycles.iter().enumerate() {
            let node_names: Vec<String> = cycle.iter()
                .map(|h| h.node.name.clone())
                .collect();
            println!("  {}. {} hops: {}", i + 1, cycle.len(), node_names.join(" → "));
        }
        std::process::exit(1);
    }
}
