use std::collections::{HashMap};
use std::fs;
use std::process::Command;
use crate::sc_graph::Edge;

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Success,
    Failure(String),
}


#[derive(Debug)]
pub struct VerificationExecution {
    results: HashMap<Edge, VerificationResult>,
    original_c_edge_count: usize,
}

impl VerificationExecution {
    pub fn new(original_c_edge_count: usize) -> Self {
        VerificationExecution {
            results: HashMap::new(),
            original_c_edge_count,
        }
    }

    pub fn execute_boogie(
        &mut self,
        edge: Edge,
        boogie_code: String,
    ) -> VerificationResult {
        // 1) Write boogie code to a temporary file
        let temp_file_name = format!("tmp/edge_{}_{}.bpl", edge.source.index(), edge.target.index());
        
        // Create tmp directory if it doesn't exist
        if let Err(e) = fs::create_dir_all("tmp") {
            let error_msg = format!("Failed to create tmp directory: {}", e);
            let result = VerificationResult::Failure(error_msg);
            self.results.insert(edge, result.clone());
            return result;
        }
        
        // Write the Boogie code to the file
        if let Err(e) = fs::write(&temp_file_name, &boogie_code) {
            let error_msg = format!("Failed to write Boogie file {}: {}", temp_file_name, e);
            let result = VerificationResult::Failure(error_msg);
            self.results.insert(edge, result.clone());
            return result;
        }
        
        // 2) Run the boogie verifier with /quiet flag
        let output = Command::new("boogie")
            .arg(&temp_file_name)
            .arg("/quiet")
            .output();
        
        let result = match output {
            Ok(output) => {
                if output.status.success() && output.stdout.is_empty() && output.stderr.is_empty() {
                    // No output means everything is proved
                    VerificationResult::Success
                } else {
                    // If there's output, it tells us where things went wrong
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    let error_msg = if !stdout.is_empty() {
                        stdout.to_string()
                    } else if !stderr.is_empty() {
                        stderr.to_string()
                    } else {
                        format!("Boogie verification failed with exit code: {}", 
                               output.status.code().unwrap_or(-1))
                    };
                    VerificationResult::Failure(error_msg)
                }
            }
            Err(e) => {
                VerificationResult::Failure(format!("Failed to run Boogie: {}", e))
            }
        };
        
        // 4) Store the result and return it
        self.results.insert(edge, result.clone());
        result
    }

    /// Get the verification result for a specific edge
    pub fn get_result(&self, edge: &Edge) -> Option<&VerificationResult> {
        self.results.get(edge)
    }

    /// Get all verification results
    pub fn get_all_results(&self) -> &HashMap<Edge, VerificationResult> {
        &self.results
    }

    /// Check if all verifications were successful
    pub fn all_successful(&self) -> bool {
        self.results.values().all(|result| matches!(result, VerificationResult::Success))
    }

    /// Get count of successful verifications
    pub fn success_count(&self) -> usize {
        self.results.values()
            .filter(|result| matches!(result, VerificationResult::Success))
            .count()
    }

    /// Get count of failed verifications
    pub fn failure_count(&self) -> usize {
        self.results.values()
            .filter(|result| matches!(result, VerificationResult::Failure(_)))
            .count()
    }

    /// Get total number of verifications attempted
    pub fn total_count(&self) -> usize {
        self.results.len()
    }

    /// Get the original C-edge count before verification
    pub fn original_c_edge_count(&self) -> usize {
        self.original_c_edge_count
    }

    /// Save Boogie files to a specific directory for testing
    pub fn save_boogie_files(&mut self, output_dir: &std::path::Path) -> Result<(), String> {
        // Create the output directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(output_dir) {
            return Err(format!("Failed to create output directory: {}", e));
        }
        
        println!("Boogie files saved to: {}", output_dir.display());
        Ok(())
    }

    /// Save a single Boogie file for an edge
    pub fn save_boogie_file(
        &self,
        edge: &Edge,
        boogie_code: &str,
        output_dir: &std::path::Path,
    ) -> Result<(), String> {
        let filename = format!("edge_{}_{}.bpl", edge.source.index(), edge.target.index());
        let file_path = output_dir.join(filename);
        
        std::fs::write(&file_path, boogie_code)
            .map_err(|e| format!("Failed to write Boogie file {}: {}", file_path.display(), e))?;
        
        Ok(())
    }
}