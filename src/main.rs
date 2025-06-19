use clap::Parser;
use colored::*;
use std::fs;

use FMitF_rs::cli::{Cli, Pipeline};

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
            eprintln!("{} Failed to read file {:?}: {}", 
                "ERROR:".red().bold(), 
                cli.input, 
                e.to_string().bright_red()
            );
            std::process::exit(1);
        }
    };

    // Create and execute pipeline
    let mut pipeline = Pipeline::new(&cli);
    if let Err(e) = pipeline.execute(source_code, cli.mode.clone(), &cli) {
        eprintln!("{} Pipeline execution failed: {}", 
            "ERROR:".red().bold(), 
            e.bright_red()
        );
        std::process::exit(1);
    }
}
