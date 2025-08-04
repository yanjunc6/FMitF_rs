// src/cli/output.rs
use super::CompilationResult;
use colored::*;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

pub struct OutputManager;

impl OutputManager {
    pub fn new() -> Self {
        Self
    }

    /// Write directory output (structured output with multiple files)
    pub fn write_directory_output(
        &self,
        result: &CompilationResult,
        dir: &PathBuf,
    ) -> Result<(), String> {
        // Create main directory
        fs::create_dir_all(dir)
            .map_err(|e| format!("Failed to create directory {:?}: {}", dir, e))?;

        // Write compilation log
        let log_path = dir.join("compilation.log");
        self.write_compilation_log(result, &log_path)?;

        // Write main summary
        let summary_path = dir.join("summary.md");
        let mut summary_file = fs::File::create(&summary_path)
            .map_err(|e| format!("Failed to create summary file: {}", e))?;
        self.write_markdown_summary(result, &mut summary_file)
            .map_err(|e| format!("Failed to write summary: {}", e))?;

        // Write console summary
        self.write_console_summary(result);

        println!("Output directory created: {}", dir.display().to_string().bright_blue().underline());
        Ok(())
    }

    /// Write a console summary of the compilation results
    pub fn write_console_summary(&self, result: &CompilationResult) {
        let stats = result.get_stats();

        if result.success {
            println!("{} Compilation successful", "✓".green().bold());
        } else {
            println!("{} Compilation failed", "✗".red().bold());
        }

        println!("  {}  : {} functions, {} tables, {} partitions", 
                 "AST".bright_blue(),
                 stats.functions, stats.tables, stats.partitions);
        
        println!("  {}  : {} basic blocks", 
                 "CFG".bright_blue(),
                 stats.basic_blocks);
        
        println!("  {}: {} nodes, {} S-edges, {} C-edges", 
                 "SC-Graph".bright_blue(),
                 stats.sc_nodes, stats.s_edges, stats.c_edges);

        println!("\nCompilation completed in {}ms", result.compilation_time_ms);
    }

    fn write_markdown_summary(&self, result: &CompilationResult, writer: &mut dyn Write) -> io::Result<()> {
        let stats = result.get_stats();
        
        writeln!(writer, "# FMitF Compilation Report\n")?;
        writeln!(writer, "**Generated:** {}\n", chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC"))?;
        
        writeln!(writer, "## Compilation Summary\n")?;
        writeln!(writer, "- **Status:** {}", if result.success { "✅ Success" } else { "❌ Failed" })?;
        writeln!(writer, "- **Compilation Time:** {}ms", result.compilation_time_ms)?;
        
        writeln!(writer, "\n## Stage Results\n")?;
        writeln!(writer, "| Stage | Result |")?;
        writeln!(writer, "|-------|--------|")?;
        writeln!(writer, "| AST | {} functions, {} tables, {} partitions |", stats.functions, stats.tables, stats.partitions)?;
        writeln!(writer, "| CFG | {} basic blocks |", stats.basic_blocks)?;
        writeln!(writer, "| SC-Graph | {} nodes, {} S-edges, {} C-edges |", stats.sc_nodes, stats.s_edges, stats.c_edges)?;
        
        Ok(())
    }

    fn write_compilation_log(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        let mut file = fs::File::create(path)
            .map_err(|e| format!("Failed to create log file: {}", e))?;
        
        writeln!(file, "FMitF Compilation Log")
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "Generated: {}", chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC"))
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "\nStatus: {}", if result.success { "SUCCESS" } else { "FAILED" })
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "Compilation time: {}ms", result.compilation_time_ms)
            .map_err(|e| format!("Failed to write log: {}", e))?;
        
        let stats = result.get_stats();
        writeln!(file, "\nStage Results:")
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "- AST: {} functions, {} tables, {} partitions", stats.functions, stats.tables, stats.partitions)
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "- CFG: {} basic blocks", stats.basic_blocks)
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "- SC-Graph: {} nodes, {} S-edges, {} C-edges", stats.sc_nodes, stats.s_edges, stats.c_edges)
            .map_err(|e| format!("Failed to write log: {}", e))?;
        
        Ok(())
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
