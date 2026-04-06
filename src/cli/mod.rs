//! CLI Module - Simple compiler entry point

pub mod cache_manager;
pub mod compiler;
pub mod data;
pub mod log;
pub mod options;
pub mod stage;
pub mod verification;

use clap::Parser;
use std::path::PathBuf;

use crate::cli::compiler::Compiler;
use crate::cli::options::{CacheOptions, CompilerOptions, SplitOptions};

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

    /// Disable verification result caching
    #[arg(long, default_value_t = false)]
    pub disable_cache: bool,

    /// Cache directory (default: <output_dir>/.verification_cache)
    #[arg(long)]
    pub cache_dir: Option<PathBuf>,

    /// Maximum cache storage in MB
    #[arg(long, default_value_t = 512, value_parser = clap::value_parser!(u64).range(1..))]
    pub cache_max_size: u64,

    /// Clear verification cache before running
    #[arg(long, default_value_t = false)]
    pub clear_cache: bool,

    /// Disable timeout-driven recursive commutativity splitting
    #[arg(long, default_value_t = false)]
    pub disable_split: bool,

    /// Maximum recursive split depth
    #[arg(long, default_value_t = 5, value_parser = clap::value_parser!(u32).range(0..))]
    pub split_max_depth: u32,

    /// Split strategy: min-state | balanced | min-state-balanced
    #[arg(long, default_value = "balanced")]
    pub split_strategy: String,

    /// Print detailed split cut debugging information
    #[arg(long, default_value_t = false)]
    pub split_debug: bool,

    /// Debug mode: enforce splitting to a specific depth for all verifications
    /// (0 = disabled, 1 = split once, 2 = split to depth 2, etc.)
    #[arg(long, default_value_t = 0, value_parser = clap::value_parser!(u32).range(0..))]
    pub debug_enforce_split: u32,
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

    let compiler_options = CompilerOptions {
        instances: args.instances,
        loop_unroll: args.loop_unroll,
        timeout_secs: args.timeout,
        enable_optimization: !args.no_optimize,
        enable_verification: !args.no_verify,
        cache: CacheOptions {
            enabled: !args.disable_cache,
            dir: args
                .cache_dir
                .unwrap_or_else(|| output_dir.join(".verification_cache")),
            max_size_mb: args.cache_max_size,
            clear: args.clear_cache,
        },
        split: SplitOptions {
            enabled: !args.disable_split,
            max_depth: args.split_max_depth,
            strategy: args.split_strategy,
            debug: args.split_debug,
            debug_enforce_split: args.debug_enforce_split,
        },
    };

    compiler.run_pipeline(&args.input, &output_dir, compiler_options)
}

// ============================================================================
// --- Backwards Compatibility
// ============================================================================

/// Old entry point for backwards compatibility
pub fn run_cli() -> Result<(), Box<dyn std::error::Error>> {
    run()
}
