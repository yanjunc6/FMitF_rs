use colored::*;

use super::log::Logger;

/// Information about a C-edge verification
#[derive(Debug, Clone)]
pub struct CEdgeVerificationInfo {
    pub source_function_id: usize,
    pub source_instance: u32,
    pub source_hop_id: usize,
    pub target_function_id: usize,
    pub target_instance: u32,
    pub target_hop_id: usize,
    pub duration: std::time::Duration,
    pub is_timeout: bool,
    pub eliminated: bool,
}

#[derive(Default)]
pub struct RunSummary {
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
    pub c_edge_verifications: Vec<CEdgeVerificationInfo>,
}

impl RunSummary {
    pub fn new(instances: usize) -> Self {
        Self {
            instance_count: instances,
            c_edge_verifications: Vec::new(),
            ..Default::default()
        }
    }

    /// Plot verification time histogram using plotters
    /// Generates a histogram with title, axis labels, and grid on the PNG
    pub fn plot_verification_histogram(
        &self,
        output_path: &std::path::Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use plotters::prelude::*;

        if self.c_edge_verifications.is_empty() {
            return Ok(()); // Nothing to plot
        }

        let root = BitMapBackend::new(output_path, (1200, 800)).into_drawing_area();
        root.fill(&WHITE)?;

        // Collect durations in milliseconds for better readability
        let durations_ms: Vec<f64> = self
            .c_edge_verifications
            .iter()
            .map(|info| info.duration.as_secs_f64() * 1000.0)
            .collect();

        let max_time = durations_ms
            .iter()
            .fold(0.0f64, |a, &b| if b > a { b } else { a });
        let min_time = durations_ms
            .iter()
            .fold(max_time, |a, &b| if b < a { b } else { a });

        // Create 20 bins
        let num_bins = 20;
        let bin_width = (max_time - min_time) / num_bins as f64;
        let mut bins = vec![0; num_bins];

        for &duration in &durations_ms {
            let bin_idx = ((duration - min_time) / bin_width) as usize;
            let bin_idx = bin_idx.min(num_bins - 1);
            bins[bin_idx] += 1;
        }

        let max_count = *bins.iter().max().unwrap_or(&1);

        let mut chart = ChartBuilder::on(&root)
            .caption(
                "C-Edge Verification Time Distribution",
                ("sans-serif", 40).into_font(),
            )
            .margin(20)
            .x_label_area_size(60)
            .y_label_area_size(60)
            .build_cartesian_2d(
                min_time..(max_time + bin_width),
                0..(max_count + max_count / 10),
            )?;

        chart
            .configure_mesh()
            .x_desc("Verification Time (ms)")
            .y_desc("Number of C-Edges")
            .x_label_formatter(&|x| format!("{:.0}", x))
            .y_label_formatter(&|y| format!("{}", y))
            .draw()?;

        chart.draw_series(bins.iter().enumerate().map(|(i, &count)| {
            let x_start = min_time + (i as f64) * bin_width;
            let x_end = x_start + bin_width;
            Rectangle::new([(x_start, 0), (x_end, count)], BLUE.mix(0.7).filled())
        }))?;

        root.present()?;
        Ok(())
    }

    pub fn format(&self) -> String {
        let header = "Summary".blue().to_string();
        let term = |label: &str| label.truecolor(128, 128, 128).to_string();

        let basic = format!(
            "  {} {}  {} {}  {} {}",
            term("Functions:"),
            self.function_count,
            term("Hops:"),
            self.hop_count,
            term("Instances:"),
            self.instance_count
        );

        let sc = format!(
            "  {} {}  {} {}",
            term("C-edges (original):"),
            self.sc_c_edges,
            term("C-edges (simplified):"),
            self.simplified_sc_c_edges
        );

        let boogie = format!(
            "  {} {}  {} {}s",
            term("Boogie loopUnroll:"),
            self.boogie_loop_unroll,
            term("Boogie timeLimit:"),
            self.boogie_timeout_secs
        );

        let verification = format!(
            "  {} {}  {} {}  {} {}  {} {}  {} {}",
            term("Verifications:"),
            self.verification_total,
            term("Pass:"),
            self.verification_pass,
            term("Errors:"),
            self.verification_errors,
            term("Timeouts:"),
            self.verification_timeouts,
            term("Boogie failures:"),
            self.boogie_compile_failures
        );

        vec![header, basic, sc, boogie, verification].join("\n")
    }

    pub fn format_plain(&self) -> String {
        let basic = format!(
            "  Functions: {}  Hops: {}  Instances: {}",
            self.function_count, self.hop_count, self.instance_count
        );
        let sc = format!(
            "  C-edges (original): {}  C-edges (simplified): {}",
            self.sc_c_edges, self.simplified_sc_c_edges
        );
        let boogie = format!(
            "  Boogie loopUnroll: {}  Boogie timeLimit: {}s",
            self.boogie_loop_unroll, self.boogie_timeout_secs
        );
        let verification = format!(
            "  Verifications: {}  Pass: {}  Errors: {}  Timeouts: {}  Boogie failures: {}",
            self.verification_total,
            self.verification_pass,
            self.verification_errors,
            self.verification_timeouts,
            self.boogie_compile_failures
        );

        vec![basic, sc, boogie, verification].join("\n")
    }
}

pub fn emit_summary(summary: &RunSummary, logger: &mut Logger) {
    println!("{}", summary.format());
    let _ = logger.block("Summary:", summary.format_plain());
}
