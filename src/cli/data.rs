//! Data collection and benchmarking module
//!
//! This module provides comprehensive data collection for benchmarking purposes.
//! It tracks verification times, memory usage, Boogie outputs, and more for each C-edge.

use colored::*;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Memory statistics parsed from /usr/bin/time -v output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryStats {
    /// Maximum resident set size (KB)
    pub max_rss_kb: Option<u64>,
    /// Average resident set size (KB)
    pub avg_rss_kb: Option<u64>,
    /// Minor page faults
    pub minor_page_faults: Option<u64>,
    /// Major page faults
    pub major_page_faults: Option<u64>,
    /// Voluntary context switches
    pub voluntary_context_switches: Option<u64>,
    /// Involuntary context switches
    pub involuntary_context_switches: Option<u64>,
    /// User time (seconds)
    pub user_time_secs: Option<f64>,
    /// System time (seconds)
    pub system_time_secs: Option<f64>,
    /// Percent of CPU
    pub cpu_percent: Option<u64>,
    /// Elapsed wall clock time (seconds)
    pub elapsed_secs: Option<f64>,
}

impl Default for MemoryStats {
    fn default() -> Self {
        Self {
            max_rss_kb: None,
            avg_rss_kb: None,
            minor_page_faults: None,
            major_page_faults: None,
            voluntary_context_switches: None,
            involuntary_context_switches: None,
            user_time_secs: None,
            system_time_secs: None,
            cpu_percent: None,
            elapsed_secs: None,
        }
    }
}

/// Verification result type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum VerificationResult {
    /// Verification passed (UNSAT - no counterexample found)
    Pass,
    /// Verification failed (SAT - counterexample found)
    Error,
    /// Verification timed out
    Timeout,
    /// Boogie compilation failed
    CompilationError,
}

/// Information about a single C-edge verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CEdgeVerificationData {
    pub source_function_id: usize,
    pub source_instance: u32,
    pub source_hop_id: usize,
    pub target_function_id: usize,
    pub target_instance: u32,
    pub target_hop_id: usize,
    /// Wall-clock duration of verification
    pub duration_ms: f64,
    /// Verification result
    pub result: VerificationResult,
    /// Whether this edge was eliminated by verification
    pub eliminated: bool,
    /// Memory and resource statistics from /usr/bin/time -v
    pub memory_stats: MemoryStats,
    /// Raw Boogie stdout
    pub boogie_stdout: String,
    /// Raw Boogie stderr
    pub boogie_stderr: String,
    /// Boogie file name
    pub boogie_file: String,
}

/// Summary statistics for the entire run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkSummary {
    pub function_count: usize,
    pub hop_count: usize,
    pub instance_count: usize,
    pub boogie_loop_unroll: usize,
    pub boogie_timeout_secs: usize,
    pub sc_c_edges: usize,
    pub simplified_sc_c_edges: usize,
    pub verification_total: usize,
    pub verification_pass: usize,
    pub verification_errors: usize,
    pub verification_timeouts: usize,
    pub boogie_compile_failures: usize,
}

/// Complete benchmark data for output to JSON
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkData {
    /// Summary statistics
    pub summary: BenchmarkSummary,
    /// Per-edge verification data
    pub c_edge_verifications: Vec<CEdgeVerificationData>,
    /// Timestamp of the run
    pub timestamp: String,
    /// Input file name
    pub input_file: String,
}

impl BenchmarkData {
    /// Create a new BenchmarkData instance
    pub fn new(input_file: String) -> Self {
        Self {
            summary: BenchmarkSummary {
                function_count: 0,
                hop_count: 0,
                instance_count: 0,
                boogie_loop_unroll: 0,
                boogie_timeout_secs: 0,
                sc_c_edges: 0,
                simplified_sc_c_edges: 0,
                verification_total: 0,
                verification_pass: 0,
                verification_errors: 0,
                verification_timeouts: 0,
                boogie_compile_failures: 0,
            },
            c_edge_verifications: Vec::new(),
            timestamp: chrono::Local::now().to_rfc3339(),
            input_file,
        }
    }

    /// Write benchmark data to JSON file
    pub fn write_to_file(&self, output_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(output_path, json)?;
        Ok(())
    }
}

/// Parse /usr/bin/time -v output from stderr
pub fn parse_time_output(stderr: &str) -> MemoryStats {
    let mut stats = MemoryStats::default();

    for line in stderr.lines() {
        let line = line.trim();
        if let Some(colon_pos) = line.find(':') {
            let key = line[..colon_pos].trim();
            let value = line[colon_pos + 1..].trim();

            match key {
                "Maximum resident set size (kbytes)" => {
                    stats.max_rss_kb = value.parse().ok();
                }
                "Average resident set size (kbytes)" => {
                    stats.avg_rss_kb = value.parse().ok();
                }
                "Minor (reclaiming a frame) page faults" => {
                    stats.minor_page_faults = value.parse().ok();
                }
                "Major (requiring I/O) page faults" => {
                    stats.major_page_faults = value.parse().ok();
                }
                "Voluntary context switches" => {
                    stats.voluntary_context_switches = value.parse().ok();
                }
                "Involuntary context switches" => {
                    stats.involuntary_context_switches = value.parse().ok();
                }
                "User time (seconds)" => {
                    stats.user_time_secs = value.parse().ok();
                }
                "System time (seconds)" => {
                    stats.system_time_secs = value.parse().ok();
                }
                "Percent of CPU this job got" => {
                    // Remove trailing '%' if present
                    let cleaned = value.trim_end_matches('%');
                    stats.cpu_percent = cleaned.parse().ok();
                }
                "Elapsed (wall clock) time (h:mm:ss or m:ss)" => {
                    stats.elapsed_secs = parse_elapsed_time(value);
                }
                _ => {}
            }
        }
    }

    stats
}

/// Parse elapsed time string like "0:01.23" or "1:02:03.45" to seconds
fn parse_elapsed_time(time_str: &str) -> Option<f64> {
    let parts: Vec<&str> = time_str.split(':').collect();
    match parts.len() {
        1 => {
            // Just seconds: "1.23"
            time_str.parse().ok()
        }
        2 => {
            // mm:ss.ss
            let minutes: f64 = parts[0].parse().ok()?;
            let seconds: f64 = parts[1].parse().ok()?;
            Some(minutes * 60.0 + seconds)
        }
        3 => {
            // h:mm:ss.ss
            let hours: f64 = parts[0].parse().ok()?;
            let minutes: f64 = parts[1].parse().ok()?;
            let seconds: f64 = parts[2].parse().ok()?;
            Some(hours * 3600.0 + minutes * 60.0 + seconds)
        }
        _ => None,
    }
}

/// Data collector for tracking verification information
pub struct DataCollector {
    data: BenchmarkData,
}

impl DataCollector {
    /// Create a new DataCollector
    pub fn new(input_file: String, instances: usize) -> Self {
        let mut data = BenchmarkData::new(input_file);
        data.summary.instance_count = instances;
        Self { data }
    }

    /// Set basic configuration
    pub fn set_config(&mut self, loop_unroll: usize, timeout_secs: usize) {
        self.data.summary.boogie_loop_unroll = loop_unroll;
        self.data.summary.boogie_timeout_secs = timeout_secs;
    }

    /// Set program statistics
    pub fn set_program_stats(&mut self, function_count: usize, hop_count: usize) {
        self.data.summary.function_count = function_count;
        self.data.summary.hop_count = hop_count;
    }

    /// Set SC-graph statistics
    pub fn set_sc_stats(&mut self, sc_c_edges: usize, simplified_sc_c_edges: usize) {
        self.data.summary.sc_c_edges = sc_c_edges;
        self.data.summary.simplified_sc_c_edges = simplified_sc_c_edges;
    }

    /// Add a C-edge verification result
    pub fn add_c_edge_verification(&mut self, edge_data: CEdgeVerificationData) {
        self.data.summary.verification_total += 1;
        match edge_data.result {
            VerificationResult::Pass => self.data.summary.verification_pass += 1,
            VerificationResult::Error => self.data.summary.verification_errors += 1,
            VerificationResult::Timeout => self.data.summary.verification_timeouts += 1,
            VerificationResult::CompilationError => {
                self.data.summary.boogie_compile_failures += 1
            }
        }
        self.data.c_edge_verifications.push(edge_data);
    }

    /// Mark edges as eliminated based on SC-graph comparison
    /// Mark edges as eliminated based on SC-graph comparison
    pub fn mark_eliminated_edges(
        &mut self,
        eliminated_edges: &std::collections::HashSet<(usize, u32, usize, usize, u32, usize)>,
    ) {
        for edge in &mut self.data.c_edge_verifications {
            let key = (
                edge.source_function_id,
                edge.source_instance,
                edge.source_hop_id,
                edge.target_function_id,
                edge.target_instance,
                edge.target_hop_id,
            );
            edge.eliminated = eliminated_edges.contains(&key);
        }
    }

    /// Get the collected data
    pub fn data(&self) -> &BenchmarkData {
        &self.data
    }

    /// Write data to JSON file
    pub fn write_to_file(&self, output_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        self.data.write_to_file(output_path)
    }

    /// Format summary for display
    pub fn format_summary(&self) -> String {
        let s = &self.data.summary;
        let header = "Summary".blue().to_string();
        let term = |label: &str| label.truecolor(128, 128, 128).to_string();

        let basic = format!(
            "  {} {}  {} {}  {} {}",
            term("Functions:"),
            s.function_count,
            term("Hops:"),
            s.hop_count,
            term("Instances:"),
            s.instance_count
        );

        let sc = format!(
            "  {} {}  {} {}",
            term("C-edges (original):"),
            s.sc_c_edges,
            term("C-edges (simplified):"),
            s.simplified_sc_c_edges
        );

        let boogie = format!(
            "  {} {}  {} {}s",
            term("Boogie loopUnroll:"),
            s.boogie_loop_unroll,
            term("Boogie timeLimit:"),
            s.boogie_timeout_secs
        );

        let verification = format!(
            "  {} {}  {} {}  {} {}  {} {}  {} {}",
            term("Verifications:"),
            s.verification_total,
            term("Pass:"),
            s.verification_pass,
            term("Errors:"),
            s.verification_errors,
            term("Timeouts:"),
            s.verification_timeouts,
            term("Boogie failures:"),
            s.boogie_compile_failures
        );

        vec![header, basic, sc, boogie, verification].join("\n")
    }

    /// Format summary for plain text (logging)
    pub fn format_summary_plain(&self) -> String {
        let s = &self.data.summary;
        let basic = format!(
            "  Functions: {}  Hops: {}  Instances: {}",
            s.function_count, s.hop_count, s.instance_count
        );
        let sc = format!(
            "  C-edges (original): {}  C-edges (simplified): {}",
            s.sc_c_edges, s.simplified_sc_c_edges
        );
        let boogie = format!(
            "  Boogie loopUnroll: {}  Boogie timeLimit: {}s",
            s.boogie_loop_unroll, s.boogie_timeout_secs
        );
        let verification = format!(
            "  Verifications: {}  Pass: {}  Errors: {}  Timeouts: {}  Boogie failures: {}",
            s.verification_total,
            s.verification_pass,
            s.verification_errors,
            s.verification_timeouts,
            s.boogie_compile_failures
        );

        vec![basic, sc, boogie, verification].join("\n")
    }

    /// Plot verification time histogram
    pub fn plot_verification_histogram(
        &self,
        output_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use plotters::prelude::*;

        if self.data.c_edge_verifications.is_empty() {
            return Ok(()); // Nothing to plot
        }

        let root = BitMapBackend::new(output_path, (1200, 800)).into_drawing_area();
        root.fill(&WHITE)?;

        // Separate timeout and non-timeout verifications
        let mut durations_ms: Vec<f64> = Vec::new();
        let mut timeout_count = 0;

        for info in &self.data.c_edge_verifications {
            if info.result == VerificationResult::Timeout {
                timeout_count += 1;
            } else {
                durations_ms.push(info.duration_ms);
            }
        }

        // Calculate range excluding timeouts
        let max_time = durations_ms
            .iter()
            .fold(0.0f64, |a, &b| if b > a { b } else { a });
        let min_time = durations_ms
            .iter()
            .fold(max_time, |a, &b| if b < a { b } else { a });

        // Create 20 bins for non-timeout durations
        let num_bins = 20;
        let bin_width = if max_time > min_time {
            (max_time - min_time) / num_bins as f64
        } else {
            1.0
        };
        let mut bins = vec![0; num_bins];

        for &duration in &durations_ms {
            let bin_idx = ((duration - min_time) / bin_width) as usize;
            let bin_idx = bin_idx.min(num_bins - 1);
            bins[bin_idx] += 1;
        }

        let max_count_regular = *bins.iter().max().unwrap_or(&1);
        let max_count = max_count_regular.max(timeout_count);

        // Calculate extended range to include timeout bin
        let timeout_bin_start = max_time + bin_width * 2.0;
        let timeout_bin_end = timeout_bin_start + bin_width;
        let chart_max_x = if timeout_count > 0 {
            timeout_bin_end
        } else {
            max_time + bin_width
        };

        let mut chart = ChartBuilder::on(&root)
            .caption(
                "C-Edge Verification Time Distribution",
                ("sans-serif", 40).into_font(),
            )
            .margin(20)
            .x_label_area_size(60)
            .y_label_area_size(60)
            .build_cartesian_2d(min_time..chart_max_x, 0..(max_count + max_count / 10))?;

        chart
            .configure_mesh()
            .x_desc("Verification Time (ms)")
            .y_desc("Number of C-Edges")
            .x_label_formatter(&|x| {
                if timeout_count > 0 && *x >= timeout_bin_start && *x < timeout_bin_end {
                    "Timeout".to_string()
                } else {
                    format!("{:.0}", x)
                }
            })
            .y_label_formatter(&|y| format!("{}", y))
            .draw()?;

        // Draw regular bins
        chart.draw_series(bins.iter().enumerate().map(|(i, &count)| {
            let x_start = min_time + (i as f64) * bin_width;
            let x_end = x_start + bin_width;
            Rectangle::new([(x_start, 0), (x_end, count)], BLUE.mix(0.7).filled())
        }))?;

        // Draw timeout bin if there are timeouts
        if timeout_count > 0 {
            chart.draw_series(std::iter::once(Rectangle::new(
                [(timeout_bin_start, 0), (timeout_bin_end, timeout_count)],
                RED.mix(0.5).filled(),
            )))?;

            // Add label for timeout bin
            chart.draw_series(std::iter::once(Text::new(
                "Timeout",
                (
                    timeout_bin_start + bin_width / 2.0,
                    (timeout_count as f64 * 1.05) as i32,
                ),
                ("sans-serif", 20).into_font(),
            )))?;
        }

        root.present()?;
        Ok(())
    }
}

/// Emit summary to console and logger
pub fn emit_summary(collector: &DataCollector, logger: &mut super::log::Logger) {
    println!("{}", collector.format_summary());
    let _ = logger.block("Summary:", &collector.format_summary_plain());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_elapsed_time() {
        assert_eq!(parse_elapsed_time("1.23"), Some(1.23));
        assert_eq!(parse_elapsed_time("1:30.50"), Some(90.50));
        assert_eq!(parse_elapsed_time("1:02:03.45"), Some(3723.45));
    }

    #[test]
    fn test_parse_time_output() {
        let output = r#"
        Command being timed: "boogie test.bpl"
        User time (seconds): 1.23
        System time (seconds): 0.45
        Percent of CPU this job got: 95%
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:01.78
        Maximum resident set size (kbytes): 123456
        Average resident set size (kbytes): 98765
        Minor (reclaiming a frame) page faults: 1000
        Major (requiring I/O) page faults: 10
        Voluntary context switches: 50
        Involuntary context switches: 5
        "#;

        let stats = parse_time_output(output);
        assert_eq!(stats.max_rss_kb, Some(123456));
        assert_eq!(stats.user_time_secs, Some(1.23));
        assert_eq!(stats.system_time_secs, Some(0.45));
        assert_eq!(stats.cpu_percent, Some(95));
        assert_eq!(stats.elapsed_secs, Some(1.78));
    }
}
