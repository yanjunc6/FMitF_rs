use std::fs;
use std::process::Command;
use crate::sc_graph::Edge;

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Success,
    Failure(String),
}


#[derive(Debug)]
pub struct VerificationExecution;

impl VerificationExecution {
    pub fn execute_boogie(
        &self,
        edge: Edge,
        boogie_code: String,
    ) -> VerificationResult {
        // 1) Write boogie code to a temporary file
        let temp_file_name = format!("tmp/edge_{}_{}.bpl", edge.source.index(), edge.target.index());
        
        // Create tmp directory if it doesn't exist
        if let Err(e) = fs::create_dir_all("tmp") {
            let error_msg = format!("Failed to create tmp directory: {}", e);
            return VerificationResult::Failure(error_msg);
        }
        
        
        // Write the Boogie code to the file
        if let Err(e) = fs::write(&temp_file_name, &boogie_code) {
            let error_msg = format!("Failed to write Boogie file {}: {}", temp_file_name, e);
            return VerificationResult::Failure(error_msg);
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
        
        result
    }
}