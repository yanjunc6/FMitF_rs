// src/cli/verification.rs
//! CLI interface for verification operations

use std::path::Path;
use std::process::Command;
use colored::*;

use crate::cfg::CfgProgram;
use crate::verification::{VerificationManager, PartitionVerificationResult};

/// High-level interface for running verification from CLI
pub struct VerificationCli {
    manager: VerificationManager,
}

impl VerificationCli {
    pub fn new() -> Self {
        Self {
            manager: VerificationManager::new(),
        }
    }

    /// Run partition soundness verification and generate all necessary files
    pub fn run_partition_verification(
        &mut self,
        cfg_program: &CfgProgram,
        output_dir: &Path,
    ) -> Result<PartitionVerificationResult, String> {
        // Set up the boogie output directory
        let boogie_dir = output_dir.join("boogie");
        self.manager.partition_verifier.set_boogie_output_dir(
            boogie_dir.to_str().unwrap_or("tmp/boogie").to_string()
        );

        // Run the verification
        let result = self.manager.run_partition_verification(
            cfg_program,
            output_dir.to_str()
        );

        // If Boogie files were generated, try to run Boogie on them
        if result.boogie_files_generated > 0 {
            match self.run_boogie_on_generated_files(&boogie_dir) {
                Ok(boogie_results) => {
                    self.print_boogie_results(&boogie_results);
                }
                Err(e) => {
                    eprintln!("{}: {}", "Warning".yellow().bold(), e);
                }
            }
        }

        Ok(result)
    }

    /// Run Boogie verification on all generated .bpl files
    fn run_boogie_on_generated_files(
        &self,
        boogie_dir: &Path,
    ) -> Result<Vec<BoogieFileResult>, String> {
        use std::fs;

        let mut results = Vec::new();

        // Check if Boogie is available
        if !self.is_boogie_available() {
            return Err("Boogie verifier not found in PATH. Install Boogie to run formal verification.".to_string());
        }

        // Find all .bpl files
        let bpl_files: Vec<_> = fs::read_dir(boogie_dir)
            .map_err(|e| format!("Failed to read Boogie directory: {}", e))?
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.extension()?.to_str()? == "bpl" {
                    Some(path)
                } else {
                    None
                }
            })
            .collect();

        println!("\n{}", "Running Boogie verification...".bright_white().bold());

        // Run Boogie on each file
        for bpl_file in bpl_files {
            let result = self.run_boogie_on_file(&bpl_file);
            results.push(result);
        }

        Ok(results)
    }

    /// Check if Boogie is available in the system PATH
    fn is_boogie_available(&self) -> bool {
        Command::new("boogie")
            .arg("--version")
            .output()
            .is_ok()
    }

    /// Run Boogie on a single .bpl file
    fn run_boogie_on_file(&self, bpl_file: &Path) -> BoogieFileResult {
        let filename = bpl_file.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();

        let output = Command::new("boogie")
            .arg(bpl_file)
            .arg("/timeLimit:30")  // 30 second timeout
            .output();

        match output {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                let success = output.status.success();

                // Parse Boogie output to extract meaningful information
                let (verified, errors) = self.parse_boogie_output(&stdout);

                BoogieFileResult {
                    filename,
                    success,
                    verified_procedures: verified,
                    errors,
                    stdout: stdout.to_string(),
                    stderr: stderr.to_string(),
                }
            }
            Err(e) => BoogieFileResult {
                filename,
                success: false,
                verified_procedures: 0,
                errors: vec![format!("Failed to run Boogie: {}", e)],
                stdout: String::new(),
                stderr: String::new(),
            },
        }
    }

    /// Parse Boogie output to extract verification results
    fn parse_boogie_output(&self, output: &str) -> (usize, Vec<String>) {
        let mut verified_count = 0;
        let mut errors = Vec::new();

        for line in output.lines() {
            if line.contains("Boogie program verifier finished with") {
                if line.contains("0 errors") {
                    // Extract number of verified procedures
                    if let Some(verified_str) = line.split_whitespace()
                        .find(|&word| word.chars().all(|c| c.is_ascii_digit())) 
                    {
                        verified_count = verified_str.parse().unwrap_or(0);
                    }
                }
            } else if line.contains("Error:") || line.contains("assertion violation") {
                errors.push(line.trim().to_string());
            }
        }

        (verified_count, errors)
    }

    /// Print the results of Boogie verification
    fn print_boogie_results(&self, results: &[BoogieFileResult]) {
        let total_files = results.len();
        let successful_files = results.iter().filter(|r| r.success).count();
        let failed_files = total_files - successful_files;

        println!("\n{}", "Boogie Verification Summary".bright_white().bold());
        
        if failed_files == 0 {
            println!(" - {}: {} files verified successfully", 
                "Result".green().bold(), 
                total_files
            );
        } else {
            println!(" - {}: {} successful, {} failed", 
                "Result".red().bold(), 
                successful_files, 
                failed_files
            );
        }

        // Show details for failed files
        for result in results.iter().filter(|r| !r.success) {
            println!("\n{}: {}", "Failed".red().bold(), result.filename);
            for error in &result.errors {
                println!("  {}", error);
            }
        }

        // Show summary for successful files
        let total_verified: usize = results.iter().map(|r| r.verified_procedures).sum();
        if total_verified > 0 {
            println!(" - {}: {} procedures verified", 
                "Total".bright_blue().bold(), 
                total_verified
            );
        }
    }
}

/// Result of running Boogie on a single file
#[derive(Debug)]
pub struct BoogieFileResult {
    pub filename: String,
    pub success: bool,
    pub verified_procedures: usize,
    pub errors: Vec<String>,
    pub stdout: String,
    pub stderr: String,
}
