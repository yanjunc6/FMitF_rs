use std::fmt;

#[derive(Debug, Clone)]
pub struct BoogieResult {
    pub success: bool,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    pub runtime_ms: u64,
    pub verified_procedures: usize,
}

impl Default for BoogieResult {
    fn default() -> Self {
        Self {
            success: false,
            errors: Vec::new(),
            warnings: Vec::new(),
            runtime_ms: 0,
            verified_procedures: 0,
        }
    }
}

impl fmt::Display for BoogieResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.success {
            writeln!(f, "✓ Verification succeeded")?;
        } else {
            writeln!(f, "✗ Verification failed")?;
        }

        if !self.errors.is_empty() {
            writeln!(f, "Errors:")?;
            for error in &self.errors {
                writeln!(f, "  - {}", error)?;
            }
        }

        if !self.warnings.is_empty() {
            writeln!(f, "Warnings:")?;
            for warning in &self.warnings {
                writeln!(f, "  - {}", warning)?;
            }
        }

        writeln!(f, "Verified procedures: {}", self.verified_procedures)?;
        writeln!(f, "Runtime: {}ms", self.runtime_ms)?;
        Ok(())
    }
}

pub fn parse_boogie_result(output: &str, stderr: &str, exit_code: i32) -> BoogieResult {
    let mut result = BoogieResult::default();

    // Parse exit code
    result.success = exit_code == 0;

    // Parse output for timing information
    if let Some(line) = output.lines().find(|line| line.contains("ms")) {
        if let Some(time_str) = line
            .split_whitespace()
            .find(|word| word.ends_with("ms"))
            .and_then(|s| s.strip_suffix("ms"))
        {
            if let Ok(time) = time_str.parse::<u64>() {
                result.runtime_ms = time;
            }
        }
    }

    // Parse final summary line: "Boogie program verifier finished with X verified, Y error"
    for line in output.lines() {
        if line.contains("Boogie program verifier finished with") {
            // Extract verified count
            if let Some(verified_part) = line.split("with").nth(1) {
                if let Some(verified_str) = verified_part.split_whitespace().next() {
                    if let Ok(count) = verified_str.parse::<usize>() {
                        result.verified_procedures = count;
                    }
                }
            }

            // Check for errors in the summary
            if line.contains("1 error") || line.contains("errors") && !line.contains("0 error") {
                result.success = false;
            }
        }
    }

    // Parse errors and warnings from both stdout and stderr
    let combined = format!("{}\n{}", output, stderr);

    for line in combined.lines() {
        let line = line.trim();

        // Boogie error patterns
        if line.contains("Error:") {
            if line.contains("this assertion could not be proved") {
                result
                    .errors
                    .push("Assertion violation: Partition soundness check failed".to_string());
            } else if line.contains("postcondition might not hold") {
                result
                    .errors
                    .push(format!("Postcondition violation: {}", line));
            } else if line.contains("precondition might not hold") {
                result
                    .errors
                    .push(format!("Precondition violation: {}", line));
            } else {
                result.errors.push(line.to_string());
            }
        } else if line.contains("warning:") || line.contains("Warning:") {
            result.warnings.push(line.to_string());
        }
    }

    result
}
