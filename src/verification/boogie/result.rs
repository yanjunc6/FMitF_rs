use std::fmt;

#[derive(Debug, Clone)]
pub enum BoogieFailureType {
    SyntaxError,
    AssertionFailure, // Expected for non-commutative functions
    OtherError,
}

#[derive(Debug, Clone)]
pub struct BoogieResult {
    pub success: bool,
    pub failure_type: Option<BoogieFailureType>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    pub runtime_ms: u64,
    pub verified_procedures: usize,
    pub assertion_failures: usize,
}

impl Default for BoogieResult {
    fn default() -> Self {
        Self {
            success: false,
            failure_type: None,
            errors: Vec::new(),
            warnings: Vec::new(),
            runtime_ms: 0,
            verified_procedures: 0,
            assertion_failures: 0,
        }
    }
}

impl fmt::Display for BoogieResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.success {
            writeln!(f, "✓ Verification succeeded")?;
        } else {
            match &self.failure_type {
                Some(BoogieFailureType::AssertionFailure) => {
                    writeln!(
                        f,
                        "✗ Non-commutative (assertion failures: {})",
                        self.assertion_failures
                    )?;
                }
                Some(BoogieFailureType::SyntaxError) => {
                    writeln!(f, "✗ Syntax error in generated Boogie code")?;
                }
                Some(BoogieFailureType::OtherError) => {
                    writeln!(f, "✗ Verification failed (other error)")?;
                }
                None => {
                    writeln!(f, "✗ Verification failed")?;
                }
            }
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

    // Count assertion failures and determine failure type
    let mut assertion_count = 0;
    let mut has_syntax_error = false;
    let mut has_other_error = false;

    // Parse errors and warnings from both stdout and stderr
    let combined = format!("{}\n{}", output, stderr);

    for line in combined.lines() {
        let line = line.trim();

        // Check for syntax/parse errors
        if line.contains("parse errors detected")
            || line.contains("name resolution errors detected")
            || line.contains("expected")
            || line.contains("syntax error")
        {
            has_syntax_error = true;
            result.errors.push(line.to_string());
        }
        // Check for assertion failures
        else if line.contains("Error: this assertion could not be proved") {
            assertion_count += 1;
        }
        // Other Boogie errors
        else if line.contains("Error:") {
            if line.contains("undeclared identifier") {
                has_syntax_error = true;
                result.errors.push(line.to_string());
            } else if line.contains("postcondition might not hold") {
                result
                    .errors
                    .push(format!("Postcondition violation: {}", line));
                has_other_error = true;
            } else if line.contains("precondition might not hold") {
                result
                    .errors
                    .push(format!("Precondition violation: {}", line));
                has_other_error = true;
            } else {
                result.errors.push(line.to_string());
                has_other_error = true;
            }
        } else if line.contains("warning:") || line.contains("Warning:") {
            result.warnings.push(line.to_string());
        }
    }

    result.assertion_failures = assertion_count;

    // Determine failure type
    if !result.success {
        if has_syntax_error {
            result.failure_type = Some(BoogieFailureType::SyntaxError);
        } else if assertion_count > 0 && !has_other_error {
            result.failure_type = Some(BoogieFailureType::AssertionFailure);
        } else {
            result.failure_type = Some(BoogieFailureType::OtherError);
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
        }
    }

    result
}

/// Parse Boogie result when using /quiet flag - simpler logic
/// With /quiet: if there's any output, it's warnings/errors
/// If no output: verification succeeded
pub fn parse_boogie_result_quiet(output: &str, stderr: &str, _exit_code: i32) -> BoogieResult {
    let mut result = BoogieResult::default();

    let combined_output = format!("{}\n{}", output, stderr).trim().to_string();

    // With /quiet flag, if there's no output, everything is fine (success)
    if combined_output.is_empty() {
        result.success = true;
        result.verified_procedures = 1; // Assume at least one procedure was verified
        return result;
    }

    // If there's output, there are warnings/errors
    result.success = false;

    // Check for assertion failures (expected for non-commutative functions)
    if combined_output.contains("Error: this assertion could not be proved")
        || combined_output.contains("assertion violation")
    {
        result.failure_type = Some(BoogieFailureType::AssertionFailure);
        result.assertion_failures = 1;
    }
    // Check for syntax/name resolution errors (not acceptable)
    else if combined_output.contains("name resolution errors detected")
        || combined_output.contains("parse errors detected")
        || combined_output.contains("undeclared identifier")
        || combined_output.contains("syntax error")
    {
        result.failure_type = Some(BoogieFailureType::SyntaxError);
    }
    // Other errors
    else {
        result.failure_type = Some(BoogieFailureType::OtherError);
    }

    // Store all error lines
    for line in combined_output.lines() {
        if !line.trim().is_empty() {
            result.errors.push(line.trim().to_string());
        }
    }

    result
}
