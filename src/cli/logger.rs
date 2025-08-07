// src/cli/logger.rs
//! Centralized output system with structured verbosity levels and selective color usage

use colored::*;
use std::io::{self, Write};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogLevel {
    /// Only errors and critical failures
    Quiet,
    /// Default level - key progress and results
    Normal,
    /// Detailed information and debug data
    Verbose,
}

impl LogLevel {
    pub fn should_show(self, target: LogLevel) -> bool {
        match (self, target) {
            (LogLevel::Quiet, LogLevel::Quiet) => true,
            (LogLevel::Normal, LogLevel::Quiet | LogLevel::Normal) => true,
            (LogLevel::Verbose, _) => true,
            _ => false,
        }
    }
}

pub struct Logger {
    level: LogLevel,
}

impl Logger {
    pub fn new() -> Self {
        Self {
            level: LogLevel::Normal,
        }
    }

    // Stage progress messages (Normal level)
    pub fn stage_start(&self, stage_num: usize, total: usize, name: &str) {
        if self.level.should_show(LogLevel::Normal) {
            print!(
                "{} {}: ",
                "Stage".bright_blue().bold(),
                format!("{}/{}", stage_num, total).bright_blue().bold()
            );
            print!("({}): ", name.bright_blue());
            io::stdout().flush().unwrap();
        }
    }

    pub fn stage_success(&self) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{}", "OK".green().bold());
        }
    }

    pub fn stage_error(&self, error_count: usize) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} – {} error{} found.",
                "ERROR".red().bold(),
                error_count,
                if error_count == 1 { "" } else { "s" }
            );
        }
    }

    pub fn stage_skipped(&self, reason: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} ({})", "OK".green().bold(), reason.italic());
        }
    }

    pub fn stage_failed(&self, error_message: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{}: {}", "ERROR".red().bold(), error_message);
        }
    }

    pub fn stage_skipped_due_to_error(&self, stage_num: usize, total: usize, name: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} {}: ({}) - {} due to previous errors",
                "Stage".bright_blue().bold(),
                format!("{}/{}", stage_num, total).bright_blue().bold(),
                name.bright_blue(),
                "SKIPPED".yellow().bold()
            );
        }
    }

    // Process status (Normal level)
    pub fn process_start(&self, description: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} {}...",
                "Starting".blue().bold(),
                description.bright_white()
            );
        }
    }

    // File operations (Normal level)
    pub fn file_output(&self, path: &std::path::Path) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} {}",
                "Writing".blue().bold(),
                path.display().to_string().bright_blue().underline()
            );
        }
    }

    pub fn boogie_files_saved(&self, path: &std::path::Path) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} {}",
                "Boogie files saved to:".bright_blue().bold(),
                path.display().to_string().bright_blue().underline()
            );
        }
    }

    // Success messages (Normal level)
    pub fn success(&self, message: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} {}", "SUCCESS:".green().bold(), message.green());
        }
    }

    // Warning messages (Quiet level - always shown except in true quiet)
    pub fn warn(&self, message: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} {}", "Warning:".yellow().bold(), message.bright_yellow());
        }
    }

    // Error messages (Quiet level - always shown)
    pub fn error(&self, message: &str) {
        eprintln!("{} {}", "ERROR:".red().bold(), message.bright_red());
    }

    pub fn error_with_count(&self, message: &str, count: usize) {
        eprintln!(
            "{} {} ({} error{})",
            "ERROR:".red().bold(),
            message.bright_red(),
            count.to_string().red(),
            if count == 1 { "" } else { "s" }
        );
    }

    // Pipeline abortion message
    pub fn abort_pipeline(&self) {
        if self.level.should_show(LogLevel::Normal) {
            println!("Aborting pipeline due to errors.");
        }
    }

    // Detailed information (Verbose level)
    pub fn detail(&self, message: &str) {
        if self.level.should_show(LogLevel::Verbose) {
            println!("  {}", message);
        }
    }

    pub fn detail_item(&self, label: &str, value: &str) {
        if self.level.should_show(LogLevel::Verbose) {
            println!("  {}: {}", label.bright_white(), value);
        }
    }

    // Boogie output recording
    pub fn boogie_output(&self, filename: &str, stdout: &str, stderr: &str, success: bool) {
        if self.level.should_show(LogLevel::Verbose) {
            println!("  Boogie file: {}", filename.bright_blue());
            if success {
                println!("  Status: {}", "Success".green().bold());
            } else {
                println!("  Status: {}", "Failed".red().bold());
            }
            if !stdout.trim().is_empty() {
                println!("  Output:");
                for line in stdout.lines() {
                    println!("    {}", line);
                }
            }
            if !stderr.trim().is_empty() {
                println!("  Errors:");
                for line in stderr.lines() {
                    println!("    {}", line.red());
                }
            }
        }
    }

    // Save boogie output to log (for file recording)
    pub fn record_boogie_result(
        &self,
        log_writer: &mut dyn Write,
        filename: &str,
        stdout: &str,
        stderr: &str,
        success: bool,
    ) -> std::io::Result<()> {
        writeln!(log_writer, "Boogie file: {}", filename)?;
        writeln!(
            log_writer,
            "Status: {}",
            if success { "Success" } else { "Failed" }
        )?;
        if !stdout.trim().is_empty() {
            writeln!(log_writer, "Output:")?;
            for line in stdout.lines() {
                writeln!(log_writer, "  {}", line)?;
            }
        }
        if !stderr.trim().is_empty() {
            writeln!(log_writer, "Errors:")?;
            for line in stderr.lines() {
                writeln!(log_writer, "  {}", line)?;
            }
        }
        writeln!(log_writer, "")?;
        Ok(())
    }

    // Results and summaries (Normal level)
    pub fn result_summary(&self, title: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("\n{}", title.bright_white().bold());
        }
    }

    pub fn result_item(&self, label: &str, value: &str, success: Option<bool>) {
        if self.level.should_show(LogLevel::Normal) {
            let formatted_value = match success {
                Some(true) => value.green(),
                Some(false) => value.red(),
                None => value.normal(),
            };
            println!(" - {}: {}", label, formatted_value);
        }
    }

    // Quiet-only messages for when normal output is suppressed
    pub fn quiet_completion(&self, message: &str) {
        if self.level == LogLevel::Quiet {
            println!("{}", message);
        }
    }

    // Special case: mixed cycles warning (always shown unless truly quiet)
    pub fn cycles_warning(&self, count: usize) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "\n{} {} mixed S/C cycles remain after verification.",
                "Error:".red().bold(),
                count.to_string().bright_yellow()
            );
        }
    }

    pub fn no_cycles(&self) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "No mixed S/C cycles detected - system appears {}.",
                "serializable".green().bold()
            );
        }
    }

    // Special formatting for verification results
    pub fn verification_result(&self, verified: usize, total: usize, success_rate: f64) {
        if self.level == LogLevel::Quiet {
            println!(
                "Verification: {}/{} C-edges verified ({:.1}%)",
                verified, total, success_rate
            );
        } else if self.level.should_show(LogLevel::Normal) {
            println!("Verification Results");
            println!(" - Total C-edges analyzed: {}", total);
            println!(" - Successfully verified: {}", verified);
            println!(" - Success rate: {:.1}%", success_rate);
        }
    }

    // Print detailed verification results
    pub fn verification_details<F>(&self, get_results: F)
    where
        F: Fn() -> Vec<(String, bool, Option<String>)>,
    {
        if self.level.should_show(LogLevel::Verbose) {
            println!("Detailed Results");
            for (edge_info, success, failure_msg) in get_results() {
                if success {
                    println!(" - {}: Verified (commutative)", edge_info);
                } else if let Some(msg) = failure_msg {
                    println!(" - {}: Failed", edge_info);
                    println!("   {}", msg);
                }
            }
        }
    }

    // Handle mixed cycles with appropriate colors
    pub fn mixed_cycles_status(&self, cycle_count: usize, cycles: Option<Vec<String>>) {
        if cycle_count > 0 {
            self.cycles_warning(cycle_count);

            if self.level.should_show(LogLevel::Verbose) {
                if let Some(cycle_strings) = cycles {
                    println!("   Remaining cycles:");
                    for (i, cycle_str) in cycle_strings.iter().enumerate() {
                        println!("     Cycle {}: {}", i + 1, cycle_str);
                    }
                }
            }
        } else {
            self.no_cycles();
        }
    }

    // Compilation lifecycle messages
    pub fn compilation_start(&self, input_path: &std::path::Path) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} {}",
                "Compiling".blue().bold(),
                input_path.display().to_string().bright_blue()
            );
        }
    }

    pub fn compilation_complete(&self, duration_ms: u64) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} Compilation completed in {}ms",
                "✓".green().bold(),
                duration_ms.to_string().bright_white()
            );
        }
    }

    pub fn compilation_failed(&self, duration_ms: u64, failed_stage: usize) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "{} Compilation failed at stage {} after {}ms",
                "✗".red().bold(),
                failed_stage,
                duration_ms.to_string().bright_white()
            );
        }
    }

    // Partition verification results
    pub fn partition_verification_results(
        &self,
        result: &crate::verification::PartitionVerificationResult,
    ) {
        if self.level.should_show(LogLevel::Normal) {
            println!(
                "\n{}",
                "Partition Verification Results".bright_white().bold()
            );

            // Show verified accesses
            println!(
                " - {}: {}",
                "Verified accesses",
                result.verified_accesses.len().to_string().bright_green()
            );

            // Show cross-partition accesses
            if !result.cross_partition_accesses.is_empty() {
                println!(
                    " - {}: {}",
                    "Cross-partition accesses".yellow().bold(),
                    result.cross_partition_accesses.len().to_string().yellow()
                );
            }

            // Show verification errors
            if !result.verification_errors.is_empty() {
                println!(
                    " - {}: {}",
                    "Verification errors".red().bold(),
                    result.verification_errors.len().to_string().red()
                );

                if self.level.should_show(LogLevel::Verbose) {
                    for error in &result.verification_errors {
                        println!("   {} {}", "ERROR:".red().bold(), error.error_message);
                    }
                }
            } else {
                println!(" - {}: {}", "Verification errors", "0".green());
            }

            // Show Boogie files generated
            if result.boogie_files_generated > 0 {
                println!(
                    " - {}: {}",
                    "Boogie files generated",
                    result.boogie_files_generated.to_string().bright_blue()
                );
            }
        }
    }

    // Boogie verification is now handled by the verification CLI module
    // which prints its own results directly
}
