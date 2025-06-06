// src/cli/mod.rs
use clap::{Parser, ValueEnum};
use std::path::PathBuf;

mod output;
mod pipeline;
mod stages;
mod traits;

pub use output::*;
pub use pipeline::*;
pub use stages::*;
pub use traits::*;

#[derive(Parser, Debug)]
#[command(name = "fmitf")]
#[command(about = "A chopped transaction serializability verification tool")]
#[command(version = "0.1.0")]
pub struct Cli {
    /// Input source file
    pub input: PathBuf,

    /// Processing mode - each mode includes all previous stages
    #[arg(short = 'm', long = "mode", default_value = "verify")]
    pub mode: Mode,

    /// Output file for ast/cfg/scgraph modes, or output directory for verify mode
    #[arg(short = 'o', long = "output")]
    pub output: Option<PathBuf>,

    /// Output directory for Boogie files (verify mode only)
    #[arg(long = "output-dir")]
    pub output_dir: Option<PathBuf>,

    /// Verbose output
    #[arg(short = 'v', long = "verbose")]
    pub verbose: bool,

    /// Show source spans in output
    #[arg(long = "show-spans")]
    pub show_spans: bool,

    /// Generate DOT output (for cfg and scgraph modes)
    #[arg(long = "dot")]
    pub dot: bool,

    /// Quiet mode - minimal output
    #[arg(short = 'q', long = "quiet")]
    pub quiet: bool,

    /// Verification timeout in seconds (verify mode only)
    #[arg(long = "timeout", default_value = "30")]
    pub timeout: u32,

    /// Skip optimization passes
    #[arg(long = "no-optimize")]
    pub no_optimize: bool,
}

#[derive(ValueEnum, Clone, PartialEq, Debug)]
pub enum Mode {
    /// Parse and analyze source code (AST stage)
    Ast,
    /// Build Control Flow Graph (includes AST stage)
    Cfg,
    /// Optimize Control Flow Graph (includes AST + CFG stages)
    Optimize,
    /// Build Serializability Conflict Graph (includes AST + CFG + Optimize stages)
    Scgraph,
    /// Run verification and pruning (includes all previous stages)
    Verify,
}

impl Cli {
    pub fn validate(&self) -> Result<(), String> {
        // For verify mode, prefer --output-dir over --output for Boogie files
        if self.mode == Mode::Verify {
            if self.output.is_some() && self.output_dir.is_some() {
                return Err("Cannot specify both --output and --output-dir for verify mode. Use --output-dir for Boogie files.".to_string());
            }
        } else {
            // For non-verify modes, --output-dir doesn't make sense
            if self.output_dir.is_some() {
                return Err("--output-dir is only valid for verify mode".to_string());
            }
        }

        // DOT output is only valid for CFG and SC-Graph modes
        if self.dot && !matches!(self.mode, Mode::Cfg | Mode::Optimize | Mode::Scgraph) {
            return Err(
                "--dot flag is only valid for cfg, optimize, and scgraph modes".to_string(),
            );
        }

        // Show spans is primarily useful for AST mode, but can be used with others
        // We could warn but not error for this one
        if self.show_spans && matches!(self.mode, Mode::Verify) {
            // This is not an error, but spans aren't very useful in verify mode
            // We'll allow it but it won't have much effect
        }

        // Timeout is only meaningful for verify mode
        if self.timeout != 30 && self.mode != Mode::Verify {
            return Err("--timeout is only valid for verify mode".to_string());
        }

        // No-optimize flag is only meaningful for modes that include optimization
        if self.no_optimize && !matches!(self.mode, Mode::Optimize | Mode::Scgraph | Mode::Verify) {
            return Err(
                "--no-optimize is only valid for optimize, scgraph, and verify modes".to_string(),
            );
        }

        // Quiet and verbose are mutually exclusive
        if self.quiet && self.verbose {
            return Err("Cannot use both --quiet and --verbose flags".to_string());
        }

        Ok(())
    }
}
