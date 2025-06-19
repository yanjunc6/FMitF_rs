// src/cli/output.rs
use super::{Cli, DirectoryOutput, FileOutput};
use std::fs;
use std::io::{stdout, BufWriter, Write};
use std::path::PathBuf;

pub struct OutputManager;

impl OutputManager {
    /// Get a writer for file output
    pub fn get_file_writer(
        output_path: &Option<PathBuf>,
        logger: &super::Logger,
    ) -> Result<BufWriter<Box<dyn Write>>, String> {
        match output_path {
            Some(path) => {
                let file = fs::File::create(path)
                    .map_err(|e| format!("Failed to create file {:?}: {}", path, e))?;
                logger.file_output(path);
                Ok(BufWriter::new(Box::new(file)))
            }
            None => Ok(BufWriter::new(Box::new(stdout()))),
        }
    }

    /// Handle file output for a stage
    pub fn handle_file_output<T, S>(stage: &S, data: &T, cli: &Cli) -> Result<(), String>
    where
        S: FileOutput<Data = T>,
    {
        let logger = super::Logger::new(cli.verbose, cli.quiet);
        let mut writer = Self::get_file_writer(&cli.output, &logger)?;
        stage.write_output(data, &mut writer, cli)?;
        writer
            .flush()
            .map_err(|e| format!("Failed to flush output: {}", e))?;
        Ok(())
    }

    /// Handle directory output for a stage
    pub fn handle_directory_output<T, S>(stage: &S, data: &T, cli: &Cli) -> Result<(), String>
    where
        S: DirectoryOutput<Data = T>,
    {
        let output_dir = cli
            .output_dir
            .as_ref()
            .or(cli.output.as_ref())
            .ok_or("No output directory specified for directory output")?;

        stage.write_to_directory(data, output_dir, cli)
    }
}

/// Error handling utilities
pub fn print_spanned_error(spanned_error: &crate::AstSpannedError, source_code: &str) {
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
            eprintln!("{} | {}", 
                span_value.line.to_string().red(),
                line_content
            );
            eprintln!("   | {}{}", 
                " ".repeat(span_value.column), 
                "^".red().bold()
            );
        }
    } else {
        eprintln!("{}: {}", 
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message()
        );
    }
}
