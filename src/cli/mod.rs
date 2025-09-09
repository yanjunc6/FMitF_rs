// src/cli/mod.rs
use clap::Parser;
use std::path::PathBuf;

mod compiler;
mod logger;
mod output_manager;
// mod verification; // Temporarily disabled due to compilation errors

pub use compiler::*;
pub use logger::Logger;
pub use output_manager::OutputManager;

#[derive(Parser, Debug, Clone)]
#[command(name = "fmitf")]
#[command(about = "A chopped transaction serializability verification compiler")]
#[command(version = "0.1.0")]
pub struct Cli {
    /// Input .transact file
    pub input: PathBuf,

    /// Optional output directory (defaults to input filename without extension)
    pub output_dir: Option<PathBuf>,

    /// Skip optimization passes
    #[arg(long = "no-optimize")]
    pub no_optimize: bool,

    /// Disable colored output
    #[arg(long = "no-color")]
    pub no_color: bool,

    /// Number of instances to create for each transaction (default: 2)
    #[arg(long = "instances", default_value = "2")]
    pub instances: u32,

    /// Verification type to generate (partition, commutative, all, or none). Default: all
    #[arg(long = "verify", default_value = "all")]
    pub verify: String,
}

impl Cli {
    pub fn validate(&self) -> Result<(), String> {
        // Check input file existence
        if !self.input.exists() {
            return Err(format!(
                "Input file does not exist: {}",
                self.input.display()
            ));
        }

        if !self
            .input
            .extension()
            .map_or(false, |ext| ext == "transact")
        {
            return Err("Input file must have .transact extension".to_string());
        }

        // Validate verification type
        match self.verify.as_str() {
            "partition" | "commutative" | "all" | "none" => {}
            _ => {
                return Err(format!(
                "Invalid verification type '{}'. Valid options: partition, commutative, all, none",
                self.verify
            ))
            }
        }

        Ok(())
    }

    /// Get the verification type from CLI argument (simplified for AST-only mode)
    pub fn get_verification_type(&self) -> Option<String> {
        match self.verify.as_str() {
            "partition" => Some("partition".to_string()),
            "commutative" => Some("commutative".to_string()),
            "all" => Some("all".to_string()),
            "none" => None,
            _ => Some("all".to_string()), // Default fallback
        }
    }

    /// Get the output directory, using provided path or creating one from input filename
    pub fn get_output_dir(&self) -> PathBuf {
        match &self.output_dir {
            Some(dir) => dir.clone(),
            None => {
                // Create directory with same name as input file (without extension)
                let input_stem = self.input.file_stem().unwrap_or_default();
                PathBuf::from(input_stem)
            }
        }
    }
}

/// Print ast error using ariadne
pub fn print_ast_spanned_error(
    spanned_error: &crate::AstSpannedError,
    source_code: &str,
    file_name: &str,
) {
    use crate::util::{ErrorReporter, Span, SpannedDiagnostic};

    let mut reporter = ErrorReporter::new();
    reporter.add_source(file_name.to_string(), source_code.to_string());

    let diagnostic = SpannedDiagnostic::new(
        spanned_error.error.clone(),
        spanned_error.span.map(Span::from),
    )
    .with_file_name(file_name.to_string());

    reporter.report_error(&diagnostic);
}
