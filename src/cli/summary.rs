use colored::*;

use super::log::Logger;

#[derive(Default)]
pub struct RunSummary {
    pub function_count: usize,
    pub hop_count: usize,
    pub instance_count: usize,
    pub sc_c_edges: usize,
    pub simplified_sc_c_edges: usize,
    pub verification_total: usize,
    pub verification_pass: usize,
    pub verification_errors: usize,
    pub verification_timeouts: usize,
    pub boogie_compile_failures: usize,
}

impl RunSummary {
    pub fn new(instances: usize) -> Self {
        Self {
            instance_count: instances,
            ..Default::default()
        }
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

        vec![header, basic, sc, verification].join("\n")
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
        let verification = format!(
            "  Verifications: {}  Pass: {}  Errors: {}  Timeouts: {}  Boogie failures: {}",
            self.verification_total,
            self.verification_pass,
            self.verification_errors,
            self.verification_timeouts,
            self.boogie_compile_failures
        );

        vec![basic, sc, verification].join("\n")
    }
}

pub fn emit_summary(summary: &RunSummary, logger: &mut Logger) {
    println!("{}", summary.format());
    let _ = logger.block("Summary:", summary.format_plain());
}
