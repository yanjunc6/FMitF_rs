// src/cli/mod.rs
use clap::Parser;
use std::path::PathBuf;

mod compiler;
mod logger;
// mod verification; // Temporarily disabled due to compilation errors

pub use compiler::*;
pub use logger::Logger;
// pub use output::*; // Not needed anymore - output is handled directly in compiler

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

        Ok(())
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

/// Print ast error
pub fn print_ast_spanned_error(spanned_error: &crate::AstSpannedError, source_code: &str) {
    use colored::*;

    if let Some(span_value) = &spanned_error.span {
        eprintln!(
            "{}: {} at line {}, column {}",
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message(),
            span_value.line.to_string().red(),
            span_value.column.to_string().red()
        );
        if let Some(line_content) = source_code.lines().nth(span_value.line.saturating_sub(1)) {
            eprintln!("   |");
            eprintln!("{} | {}", span_value.line.to_string().red(), line_content);
            eprintln!("   | {}{}", " ".repeat(span_value.column), "^".red().bold());
        }
    } else {
        eprintln!(
            "{}: {}",
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message()
        );
    }
}

/// Print verification error
pub fn print_verification_spanned_error(
    spanned_error: &crate::VerificationSpannedError,
    source_code: &str,
) {
    use colored::*;

    if let Some(span_value) = &spanned_error.span {
        eprintln!(
            "{}: {} at line {}, column {}",
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message(),
            span_value.line.to_string().red(),
            span_value.column.to_string().red()
        );
        if let Some(line_content) = source_code.lines().nth(span_value.line.saturating_sub(1)) {
            eprintln!("   |");
            eprintln!("{} | {}", span_value.line.to_string().red(), line_content);
            eprintln!("   | {}{}", " ".repeat(span_value.column), "^".red().bold());
        }
    } else {
        eprintln!(
            "{}: {}",
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message()
        );
    }
}
