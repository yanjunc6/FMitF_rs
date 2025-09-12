use colored::*;
use std::time::{Duration, Instant};

/// Execute a compilation stage with proper error handling and formatting.
/// Returns Ok(T) on success, Err(E) on failure.
pub fn execute_stage<F, T, E>(
    stage_name: &str,
    current: usize,
    total: usize,
    action: F,
) -> Result<T, E>
where
    F: FnOnce() -> Result<T, E>,
    E: std::fmt::Display,
{
    // Print the stage start message with proper formatting
    // [Stage 1/5] in blue, stage name in bold green
    println!(
        "{} {} ...",
        format!("[Stage {}/{}]", current, total).blue(),
        stage_name
    );

    let start = Instant::now();
    let result = action();
    let duration = start.elapsed();

    match result {
        Ok(value) => {
            // Success: show "Done" in green with duration
            println!(
                "{} {}",
                "Done.".green(),
                format_duration(duration).truecolor(128, 128, 128)
            );
            Ok(value)
        }
        Err(error) => {
            // Error: show the error details first, then "Error" with duration
            let error_msg = format!("{}", error);

            // Print the "Error:" prefix with the message and duration on the same line
            println!(
                "{} {}    {}",
                "Error:".red(),
                error_msg,
                format_duration(duration).truecolor(128, 128, 128)
            );

            Err(error)
        }
    }
}

/// Formats a `Duration` into a human-readable string (e.g., ms, μs, ns).
fn format_duration(duration: Duration) -> String {
    let nanos = duration.as_nanos();
    if nanos >= 1_000_000 {
        format!("{}ms", nanos / 1_000_000)
    } else if nanos >= 1_000 {
        format!("{}μs", nanos / 1_000)
    } else {
        format!("{}ns", nanos)
    }
}
