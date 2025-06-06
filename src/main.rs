use clap::Parser;
use std::fs;

use FMitF_rs::cli::{Cli, Pipeline};

fn main() {
    let cli = Cli::parse();

    // Validate CLI arguments
    if let Err(e) = cli.validate() {
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

    // Create and execute pipeline
    let mut pipeline = Pipeline::new(&cli);
    if let Err(e) = pipeline.execute(source_code, cli.mode.clone(), &cli) {
        eprintln!("❌ Pipeline execution failed: {}", e);
        std::process::exit(1);
    }
}
