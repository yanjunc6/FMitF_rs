// src/cli/logger.rs
//! Centralized output system with structured verbosity levels and selective color usage

use colored::*;

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
    pub fn new(verbose: bool, quiet: bool) -> Self {
        let level = if quiet {
            LogLevel::Quiet
        } else if verbose {
            LogLevel::Verbose
        } else {
            LogLevel::Normal
        };
        
        Self { level }
    }

    // Stage progress messages (Normal level)
    pub fn stage_start(&self, stage_num: usize, total: usize, name: &str) {
        if self.level.should_show(LogLevel::Normal) {
            print!("{} {}: ", 
                "Stage".bright_blue().bold(),
                format!("{}/{}", stage_num, total).bright_blue().bold()
            );
            print!("({}): ", name.bright_blue());
        }
    }

    pub fn stage_success(&self) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{}", "OK".green().bold());
        }
    }

    pub fn stage_error(&self, error_count: usize) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} – {} error{} found.", 
                "ERROR".red().bold(),
                error_count,
                if error_count == 1 { "" } else { "s" }
            );
        }
    }

    pub fn stage_skipped(&self, reason: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} ({})", 
                "OK".green().bold(),
                reason.italic()
            );
        }
    }

    // Process status (Normal level)
    pub fn process_start(&self, description: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} {}...", 
                "Starting".blue().bold(),
                description.bright_white()
            );
        }
    }

    // File operations (Normal level)
    pub fn file_output(&self, path: &std::path::Path) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} {}", 
                "Writing".blue().bold(),
                path.display().to_string().bright_blue().underline()
            );
        }
    }

    // Success messages (Normal level)
    pub fn success(&self, message: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} {}", 
                "SUCCESS:".green().bold(),
                message.green()
            );
        }
    }

    // Warning messages (Quiet level - always shown except in true quiet)
    pub fn warn(&self, message: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{} {}", 
                "Warning:".yellow().bold(),
                message.bright_yellow()
            );
        }
    }

    // Error messages (Quiet level - always shown)
    pub fn error(&self, message: &str) {
        eprintln!("{} {}", 
            "ERROR:".red().bold(),
            message.bright_red()
        );
    }

    pub fn error_with_count(&self, message: &str, count: usize) {
        eprintln!("{} {} ({} error{})", 
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
            println!("\n{} {} mixed S/C cycles remain after verification", 
                "Warning:".yellow().bold(),
                count.to_string().bright_yellow()
            );
        }
    }

    pub fn no_cycles(&self) {
        if self.level.should_show(LogLevel::Normal) {
            println!("No mixed S/C cycles detected – system appears serializable.");
        }
    }

    // Special formatting for verification results
    pub fn verification_result(&self, verified: usize, total: usize, success_rate: f64) {
        if self.level == LogLevel::Quiet {
            println!("Verification: {}/{} C-edges verified ({:.1}%)", 
                verified, total, success_rate);
        } else if self.level.should_show(LogLevel::Normal) {
            self.result_summary("Verification Results");
            self.result_item("Total C-edges analyzed", &total.to_string(), None);
            self.result_item("Successfully verified", &verified.to_string(), Some(true));
            self.result_item("Success rate", &format!("{:.1}%", success_rate), 
                if success_rate > 80.0 { Some(true) } else if success_rate < 50.0 { Some(false) } else { None });
        }
    }

    // A simple info message for positive cases
    pub fn info_positive(&self, message: &str) {
        if self.level.should_show(LogLevel::Normal) {
            println!("{}", message.bright_white());
        }
    }

    pub fn get_level(&self) -> LogLevel {
        self.level
    }
}
