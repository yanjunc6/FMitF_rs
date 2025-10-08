//! CLI Module - Simple compiler entry point

pub mod compiler;
pub mod log;
pub mod stage;
pub mod summary;

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

    /// Loop unroll bound passed to Boogie (must be >= 1)
    #[arg(long, default_value_t = 12, value_parser = clap::value_parser!(u32).range(1..))]
    pub loop_unroll: u32,

    /// Timeout (in seconds) passed to Boogie per verification (must be >= 1)
    #[arg(long, default_value_t = 30, value_parser = clap::value_parser!(u32).range(1..))]
    pub timeout: u32,

    /// Skip optimization passes
    #[arg(long, default_value_t = false)]
    pub no_optimize: bool,

    /// Disable colored output
    #[arg(long, default_value_t = false)]
    pub no_color: bool,

    /// Skip commutative verification stage
    #[arg(long, default_value_t = false)]
    pub no_verify: bool,
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

    compiler.run_pipeline(
        &args.input,
        &output_dir,
        args.instances,
        !args.no_optimize,
        !args.no_verify,
        args.loop_unroll,
        args.timeout,
    )
}

// ============================================================================
// --- Backwards Compatibility
// ============================================================================

/// Old entry point for backwards compatibility
pub fn run_cli() -> Result<(), Box<dyn std::error::Error>> {
    run()
}
