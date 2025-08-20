use clap::Parser;
use colored::*;
use std::fs;

use FMitF_rs::cli::{Cli, Compiler};

fn main() {
    let cli = Cli::parse();

    // Handle no-color option using colored::control
    if cli.no_color {
        colored::control::set_override(false);
    }

    // Validate CLI arguments
    if let Err(e) = cli.validate() {
        eprintln!("{} {}", "ERROR:".red().bold(), e.bright_red());
        std::process::exit(1);
    }

    // Read source file
    let source_code = match fs::read_to_string(&cli.input) {
        Ok(content) => content,
        Err(e) => {
            eprintln!(
                "{} Failed to read file {:?}: {}",
                "ERROR:".red().bold(),
                cli.input,
                e.to_string().bright_red()
            );
            std::process::exit(1);
        }
    };

    // Create and run compiler
    let mut compiler = Compiler::new();

    match compiler.compile(&source_code, &cli) {
        Ok(result) => {
            if result.success {
                compiler.print_compilation_complete(result.compilation_time_ms);
            } else {
                compiler.print_compilation_failed(result.compilation_time_ms, 5);
                std::process::exit(1);
            }
        }
        Err(e) => {
            // This should not happen now, but keep as fallback
            eprintln!(
                "{} Compilation failed: {}",
                "ERROR:".red().bold(),
                e.bright_red()
            );
            std::process::exit(1);
        }
    }
}
