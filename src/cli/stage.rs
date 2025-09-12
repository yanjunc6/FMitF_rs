use colored::*;
use std::io::{self, Write};
use std::time::{Duration, Instant};

/// A helper to wrap a compilation stage, printing its status and duration.
pub fn execute_stage<F, R>(stage_name: &str, current: usize, total: usize, action: F) -> R
where
    F: FnOnce() -> R,
{
    // Print the stage start message, e.g., "[Stage 1/1] Frontend ... "
    print!(
        "[{prefix} {current}/{total}] {name} ... ",
        prefix = "Stage".bold(),
        current = current,
        total = total,
        name = stage_name
    );
    // Flush stdout to ensure the message appears before the stage runs
    io::stdout().flush().unwrap();

    let start = Instant::now();
    let result = action();
    let duration = start.elapsed();

    // Print the stage completion message, e.g., "Done 1ms"
    println!(
        "{status} {duration}",
        status = "Done".green(),
        duration = format_duration(duration).truecolor(128, 128, 128)
    );

    result
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
