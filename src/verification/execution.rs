use std::process::Command;
use std::path::Path;

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Success,
    Failure(String),
}


#[derive(Debug)]
pub struct VerificationExecution;

impl VerificationExecution {
    pub fn execute_boogie<P: AsRef<Path>>(
        &self,
        file_path: P,
    ) -> VerificationResult {

        // Run the boogie verifier with /quiet flag
        let output = Command::new("boogie")
            .arg(file_path.as_ref())
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