//! CLI Module - Simple compiler entry point

pub mod compiler;

use clap::Parser;
use std::path::PathBuf;

use crate::cli::compiler::Compiler;

// ============================================================================
// --- CLI Arguments
// ============================================================================

#[derive(Parser, Debug)]
#[command(name = "fmitf")]
#[command(about = "Formal Methods in Transaction Framework")]
pub struct Args {
    /// Input .transact file
    pub input: PathBuf,

    /// Output directory (optional)
    pub output_dir: Option<PathBuf>,

    /// Number of transaction instances
    #[arg(long, default_value = "2")]
    pub instances: usize,

    /// Skip optimization passes
    #[arg(long)]
    pub no_optimize: bool,

    /// Disable colored output
    #[arg(long)]
    pub no_color: bool,
}

impl Args {
    pub fn output_dir(&self) -> PathBuf {
        self.output_dir
            .clone()
            .unwrap_or_else(|| PathBuf::from("tmp"))
    }
}

// ============================================================================
// --- CLI Entry Point
// ============================================================================

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Create compiler and run pipeline
    let mut compiler = Compiler::new();
    let output_dir = args.output_dir();

    compiler.run_pipeline(&args.input, &output_dir, args.instances)
}

// ============================================================================
// --- Backwards Compatibility
// ============================================================================

/// Old entry point for backwards compatibility
pub fn run_cli() -> Result<(), Box<dyn std::error::Error>> {
    run()
}
