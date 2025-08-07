// src/cli/verification.rs
//! CLI interface for verification operations

use std::path::Path;
use std::process::Command;

use super::Logger;
use crate::cfg::CfgProgram;
use crate::verification::boogie::parse_boogie_result;
use crate::verification::{
    CommutativityVerificationResult, PartitionVerificationResult, VerificationManager,
};

/// High-level interface for running verification from CLI
pub struct VerificationCli {
    manager: VerificationManager,
    logger: Logger,
}

impl VerificationCli {
    pub fn new() -> Self {
        Self {
            manager: VerificationManager::new(),
            logger: Logger::new(),
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
        self.manager
            .partition_verifier
            .set_boogie_output_dir(boogie_dir.to_str().unwrap_or("tmp/boogie").to_string());

        // Run the verification
        let result = self
            .manager
            .run_partition_verification(cfg_program, output_dir.to_str());

        // If Boogie files were generated, try to run Boogie on them
        if result.boogie_files_generated > 0 {
            match self.run_boogie_on_generated_files(&boogie_dir) {
                Ok(boogie_results) => {
                    // Check if any Boogie verifications failed
                    let failed_results: Vec<_> =
                        boogie_results.iter().filter(|r| !r.success).collect();
                    if !failed_results.is_empty() {
                        // Return error if Boogie verification failed
                        let error_msg =
                            format!("{} partition verification(s) failed", failed_results.len());
                        return Err(error_msg);
                    }
                    // Log detailed results for successful cases
                    self.log_boogie_results(&boogie_results);
                }
                Err(e) => {
                    // Only log warnings, don't print during stage
                    self.logger.detail(&format!("Boogie warning: {}", e));
                }
            }
        }

        Ok(result)
    }

    /// Run commutativity verification across CFG functions using SC-graph edges
    /// Returns error if there are syntax errors, otherwise reports C-edge elimination
    pub fn run_commutativity_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &mut crate::sc_graph::SCGraph,
        output_dir: Option<&str>,
    ) -> Result<CommutativityVerificationResult, String> {
        // Set up Boogie output directory
        let output_path = output_dir
            .map(|d| std::path::Path::new(d))
            .unwrap_or_else(|| std::path::Path::new("tmp"));
        let boogie_dir = output_path.join("boogie");
        self.manager
            .commutativity_verifier
            .set_boogie_output_dir(boogie_dir.to_str().unwrap_or("tmp/boogie").to_string());

        // Run the verification with C-edge elimination
        let result = self
            .manager
            .run_commutativity_verification(cfg_program, sc_graph, output_dir);

        // Convert BoogieResult to BoogieFileResult and log them
        if !result.boogie_results.is_empty() {
            let boogie_file_results: Vec<BoogieFileResult> = result
                .boogie_results
                .iter()
                .map(|(filename, boogie_result)| BoogieFileResult {
                    filename: filename.clone(),
                    success: boogie_result.success,
                    verified_procedures: boogie_result.verified_procedures,
                    errors: boogie_result.errors.clone(),
                    stdout: format!(
                        "Verified procedures: {}, Assertion failures: {}, Runtime: {}ms",
                        boogie_result.verified_procedures,
                        boogie_result.assertion_failures,
                        boogie_result.runtime_ms
                    ),
                    stderr: boogie_result.errors.join("\n"),
                })
                .collect();

            // Log the results for console output
            self.log_boogie_results(&boogie_file_results);
        }

        // Report result based on syntax errors vs assertion failures
        if result.syntax_errors > 0 {
            // Syntax errors are true failures
            return Err(format!(
                "Boogie syntax errors in {} verification(s)",
                result.syntax_errors
            ));
        }

        // If Boogie files were generated, the verification actually ran
        if result.boogie_files_generated > 0 {
            // Log results but don't fail on assertion failures (those are expected for non-commutative functions)
            self.logger.detail(&format!(
                "Commutativity verification completed: {} C-edges eliminated, {} C-edges kept",
                result.c_edges_eliminated, result.c_edges_kept
            ));

            // Also display this information at normal level since it's a key result
            if result.c_edges_eliminated > 0 || result.c_edges_kept > 0 {
                use colored::*;
                println!(
                    "  {} C-edge(s) eliminated, {} C-edge(s) kept",
                    result.c_edges_eliminated.to_string().green(),
                    result.c_edges_kept.to_string().cyan()
                );
            }
        }

        // Output the simplified SC-graph using pretty printer
        if let Some(dir) = output_dir {
            if let Err(e) = self.output_simplified_sc_graph(cfg_program, sc_graph, dir) {
                self.logger
                    .warn(&format!("Failed to output simplified SC-graph: {}", e));
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
            return Err(
                "Boogie verifier not found in PATH. Install Boogie to run formal verification."
                    .to_string(),
            );
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

        // Run Boogie on each file quietly
        for bpl_file in bpl_files {
            let result = self.run_boogie_on_file(&bpl_file);
            results.push(result);
        }

        Ok(results)
    }

    /// Check if Boogie is available in the system PATH
    fn is_boogie_available(&self) -> bool {
        Command::new("boogie").arg("--version").output().is_ok()
    }

    /// Run Boogie on a single .bpl file
    fn run_boogie_on_file(&self, bpl_file: &Path) -> BoogieFileResult {
        let filename = bpl_file
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();

        let output = Command::new("boogie")
            .arg(bpl_file)
            .arg("/timeLimit:30") // 30 second timeout
            .output();

        match output {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                let exit_code = output.status.code().unwrap_or(-1);

                // Use the proper Boogie result parser
                let boogie_result = parse_boogie_result(&stdout, &stderr, exit_code);

                BoogieFileResult {
                    filename,
                    success: boogie_result.success,
                    verified_procedures: boogie_result.verified_procedures,
                    errors: boogie_result.errors,
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

    /// Log the results of Boogie verification (detailed logging only)
    fn log_boogie_results(&self, results: &[BoogieFileResult]) {
        let total_files = results.len();
        let successful_files = results.iter().filter(|r| r.success).count();
        let total_verified: usize = results.iter().map(|r| r.verified_procedures).sum();

        // Log summary details
        self.logger.detail(&format!(
            "Boogie verification: {}/{} files successful, {} procedures verified",
            successful_files, total_files, total_verified
        ));

        // Log details for failed files
        for result in results.iter().filter(|r| !r.success) {
            self.logger.detail(&format!("Failed: {}", result.filename));
            for error in &result.errors {
                self.logger.detail(&format!("  {}", error));
            }
        }
    }

    /// Record Boogie results to the compilation log file
    pub fn record_boogie_results_to_log(
        &self,
        results: &[BoogieFileResult],
        log_file_path: &std::path::Path,
    ) -> Result<(), String> {
        use std::fs::OpenOptions;
        use std::io::Write;

        let mut log_file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(log_file_path)
            .map_err(|e| format!("Failed to open compilation log: {}", e))?;

        writeln!(log_file, "\n=== Boogie Verification Results ===")
            .map_err(|e| format!("Failed to write to log: {}", e))?;
        for result in results {
            self.logger
                .record_boogie_result(
                    &mut log_file,
                    &result.filename,
                    &result.stdout,
                    &result.stderr,
                    result.success,
                )
                .map_err(|e| format!("Failed to record Boogie result: {}", e))?;
        }

        Ok(())
    }

    /// Output the simplified SC-graph using pretty printers
    fn output_simplified_sc_graph(
        &self,
        cfg_program: &CfgProgram,
        sc_graph: &crate::sc_graph::SCGraph,
        output_dir: &str,
    ) -> Result<(), String> {
        use crate::pretty::DotPrinter;
        use std::fs;

        // Generate DOT file for the simplified SC-graph using DotPrinter
        let dot_filename = format!("{}/scgraph_simplified.dot", output_dir);
        let mut dot_file = fs::File::create(&dot_filename)
            .map_err(|e| format!("Failed to create simplified SC-graph DOT file: {}", e))?;

        let dot_printer = DotPrinter::new();
        dot_printer
            .generate_dot(sc_graph, cfg_program, &mut dot_file)
            .map_err(|e| format!("Failed to write simplified SC-graph DOT: {}", e))?;

        // Generate pretty-printed text file
        let pretty_filename = format!("{}/scgraph_simplified.txt", output_dir);
        let pretty_content = self.generate_simplified_pretty_text(cfg_program, sc_graph);
        fs::write(&pretty_filename, pretty_content)
            .map_err(|e| format!("Failed to write simplified SC-graph pretty file: {}", e))?;

        Ok(())
    }

    /// Generate pretty-printed text representation of simplified SC-graph
    fn generate_simplified_pretty_text(
        &self,
        cfg_program: &CfgProgram,
        sc_graph: &crate::sc_graph::SCGraph,
    ) -> String {
        let mut output = String::from("Simplified SC-Graph (after C-edge elimination)\n");
        output.push_str("==============================================\n\n");

        output.push_str(&format!("Nodes: {}\n", sc_graph.nodes.len()));
        for node in &sc_graph.nodes {
            let function_name = &cfg_program.functions[node.function_id].name;
            output.push_str(&format!(
                "  - {} hop {} instance {}\n",
                function_name,
                node.hop_id.index(),
                node.instance
            ));
        }

        output.push_str(&format!("\nEdges: {}\n", sc_graph.edges.len()));
        let s_edges: Vec<_> = sc_graph
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
            .collect();
        let c_edges: Vec<_> = sc_graph
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
            .collect();

        output.push_str(&format!("  S-edges (sequential): {}\n", s_edges.len()));
        for edge in s_edges {
            let source_func = &cfg_program.functions[edge.source.function_id].name;
            let target_func = &cfg_program.functions[edge.target.function_id].name;
            output.push_str(&format!(
                "    {} hop {} inst {} -> {} hop {} inst {}\n",
                source_func,
                edge.source.hop_id.index(),
                edge.source.instance,
                target_func,
                edge.target.hop_id.index(),
                edge.target.instance
            ));
        }

        output.push_str(&format!(
            "  C-edges (conflict, remaining): {}\n",
            c_edges.len()
        ));
        for edge in c_edges {
            let source_func = &cfg_program.functions[edge.source.function_id].name;
            let target_func = &cfg_program.functions[edge.target.function_id].name;
            output.push_str(&format!(
                "    {} hop {} inst {} <-> {} hop {} inst {}\n",
                source_func,
                edge.source.hop_id.index(),
                edge.source.instance,
                target_func,
                edge.target.hop_id.index(),
                edge.target.instance
            ));
        }

        output
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
