use colored::*;
use std::time::{Duration, Instant};

/// Tuple of (result, duration_ms) for stage execution
pub type StageResult<T> = (T, f64);

/// Execute a compilation stage with proper error handling and formatting.
/// Returns Ok((T, duration_ms)) on success, Err(E) on failure.
pub fn execute_stage<F, T, E>(
    stage_name: &str,
    current: usize,
    total: usize,
    action: F,
) -> Result<StageResult<T>, E>
where
    F: FnOnce() -> Result<T, E>,
    E: std::fmt::Display,
{
    // Print the stage start message on its own line so progress bars own subsequent lines.
    println!(
        "{} {}",
        format!("[Stage {}/{}]", current, total).blue(),
        stage_name
    );

    let start = Instant::now();
    let result = action();
    let duration = start.elapsed();
    let duration_ms = duration.as_secs_f64() * 1000.0;

    match result {
        Ok(value) => {
            // Success: show "Done" in green with duration
            println!(
                "{} {}",
                "Done.".green(),
                format_duration(duration).truecolor(128, 128, 128)
            );
            Ok((value, duration_ms))
        }
        Err(error) => {
            // Error: show the error details to stderr, then "Error" with duration
            let error_msg = format!("{}", error);
            eprintln!("{}", error_msg);

            // Print the "Error:" prefix with duration on the same line
            println!(
                "{} {}",
                "Error.".red(),
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
