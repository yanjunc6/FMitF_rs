// src/cli/mod.rs
use clap::Parser;
use std::path::PathBuf;

mod compiler;
mod logger;
mod output;
mod verification;

pub use compiler::*;
pub use logger::*;
pub use output::*;
pub use verification::*;

// Re-export error printing function
pub use output::print_spanned_error;

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
