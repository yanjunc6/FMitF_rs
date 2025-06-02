use crate::cfg::CfgProgram;
use crate::sc_graph::{Edge, EdgeType, SCGraph};
use crate::verification::{
    generate_verification_boogie_code, VerificationUnit, VerificationUnitBuilder,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

pub struct AutoVerifier {
    save_boogie_files: bool,
    output_dir: Option<PathBuf>,
    boogie_timeout: u32, // timeout in seconds
}

impl AutoVerifier {
    pub fn new() -> Self {
        Self {
            save_boogie_files: false,
            output_dir: None,
            boogie_timeout: 30, // 30 second default timeout
        }
    }

    pub fn with_output_dir<P: AsRef<Path>>(mut self, output_dir: P) -> Self {
        self.save_boogie_files = true;
        self.output_dir = Some(output_dir.as_ref().to_path_buf());
        self
    }

    pub fn with_timeout(mut self, timeout_seconds: u32) -> Self {
        self.boogie_timeout = timeout_seconds;
        self
    }

    /// Automatically verify all C edges in the SCGraph and remove those that pass verification
    pub fn verify_and_prune_c_edges(
        &self,
        cfg_program: &CfgProgram,
        scgraph: &mut SCGraph,
    ) -> Result<VerificationResults, VerificationError> {
        let mut results = VerificationResults::new();
        let mut edges_to_remove = Vec::new();

        // Create output directory if needed
        if let Some(ref dir) = self.output_dir {
            std::fs::create_dir_all(dir).map_err(|e| {
                VerificationError::IoError(format!("Failed to create output directory: {}", e))
            })?;
        }

        // Find all C edges
        for (i, edge) in scgraph.edges.iter().enumerate() {
            if matches!(edge.edge_type, EdgeType::C) {
                results.total_c_edges += 1;

                match self.verify_c_edge(cfg_program, scgraph, edge, i) {
                    Ok(true) => {
                        edges_to_remove.push(i);
                        results.verified_count += 1;
                        results.verified_edges.push(i);
                    }
                    Ok(false) => {
                        results
                            .failed_verification
                            .push((i, "Verification failed".to_string()));
                    }
                    Err(e) => {
                        results.errors.push((i, e.to_string()));
                        // Continue with other edges even if one fails
                    }
                }
            }
        }

        // Remove verified C edges (in reverse order to maintain indices)
        for &idx in edges_to_remove.iter().rev() {
            scgraph.edges.remove(idx);
        }

        Ok(results)
    }

    /// Verify a single C edge
    fn verify_c_edge(
        &self,
        cfg_program: &CfgProgram,
        scgraph: &SCGraph,
        edge: &Edge,
        edge_index: usize,
    ) -> Result<bool, VerificationError> {
        // Build verification unit
        let verification_builder = VerificationUnitBuilder::new(cfg_program, scgraph);
        let verification_unit = verification_builder.build(edge).ok_or_else(|| {
            VerificationError::BuildError("Failed to build verification unit".to_string())
        })?;

        // Generate Boogie code
        let boogie_code = generate_verification_boogie_code(&verification_unit, cfg_program);

        // Save Boogie file if output directory is specified
        if self.save_boogie_files {
            self.save_boogie_file(&boogie_code, &verification_unit, edge_index, cfg_program)?;
        }

        // Run Boogie verification
        self.run_boogie_verification(&boogie_code)
    }

    /// Save Boogie file with a reasonable name
    fn save_boogie_file(
        &self,
        boogie_code: &str,
        verification_unit: &VerificationUnit,
        edge_index: usize,
        cfg_program: &CfgProgram,
    ) -> Result<(), VerificationError> {
        if let Some(ref output_dir) = self.output_dir {
            // Get function names for filename
            let func1_name = &cfg_program.functions[verification_unit.func1_id].name;
            let func2_name = &cfg_program.functions[verification_unit.func2_id].name;
            let node_name = &cfg_program.nodes[verification_unit.conflict_node].name;

            // Sanitize names for filesystem
            let sanitize = |s: &str| s.replace(['/', '\\', ':', '*', '?', '"', '<', '>', '|'], "_");

            let filename = format!(
                "edge_{:03}_{}_{}_on_{}.bpl",
                edge_index,
                sanitize(func1_name),
                sanitize(func2_name),
                sanitize(node_name)
            );

            let file_path = output_dir.join(&filename);

            // Add header comment to the Boogie file
            let mut content = String::new();
            content.push_str(&format!("// Verification for C edge {}\n", edge_index));
            content.push_str(&format!("// Functions: {} vs {}\n", func1_name, func2_name));
            content.push_str(&format!("// Conflicting node: {}\n", node_name));
            content.push_str(&format!(
                "// Hop IDs: {} vs {}\n",
                verification_unit.hop1_id.index(),
                verification_unit.hop2_id.index()
            ));
            content.push_str(&format!(
                "// Generated at: {}\n",
                chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC")
            ));
            content.push_str("//\n\n");
            content.push_str(boogie_code);

            std::fs::write(&file_path, content).map_err(|e| {
                VerificationError::IoError(format!(
                    "Failed to write Boogie file {}: {}",
                    file_path.display(),
                    e
                ))
            })?;
        }

        Ok(())
    }

    /// Run Boogie verification and return true if verification passes
    fn run_boogie_verification(&self, boogie_code: &str) -> Result<bool, VerificationError> {
        // Create a temporary file for Boogie input
        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join(format!("verification_{}.bpl", std::process::id()));

        // Write Boogie code to temporary file
        fs::write(&temp_file, boogie_code)
            .map_err(|e| VerificationError::IoError(format!("Failed to write temp file: {}", e)))?;

        // Run Boogie with timeout
        let output = Command::new("boogie")
            .arg(&temp_file)
            .arg("/nologo") // Suppress logo
            .arg(format!("/timeLimit:{}", self.boogie_timeout)) // Set time limit
            .arg("/vcsCores:1") // Use single core for deterministic results
            .output()
            .map_err(|e| {
                VerificationError::BoogieError(format!("Failed to execute Boogie: {}", e))
            })?;

        // Clean up temporary file
        let _ = fs::remove_file(&temp_file);

        // Parse Boogie output
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        // Check if Boogie verification succeeded
        let verification_passed = self.parse_boogie_result(&stdout, &stderr)?;

        Ok(verification_passed)
    }

    /// Parse Boogie output to determine if verification succeeded
    fn parse_boogie_result(&self, stdout: &str, stderr: &str) -> Result<bool, VerificationError> {
        // Check for errors in stderr first
        if !stderr.is_empty() && stderr.contains("Error") {
            return Err(VerificationError::BoogieError(format!(
                "Boogie stderr: {}",
                stderr
            )));
        }

        // Parse stdout for verification results
        if stdout.contains("Boogie program verifier finished with") {
            // Look for specific verification results
            if stdout.contains("0 errors") {
                return Ok(true); // Verification succeeded
            } else if stdout.contains("error") || stdout.contains("Error") {
                return Ok(false); // Verification failed but Boogie ran successfully
            }
        }

        // Check for timeout
        if stdout.contains("timeout") || stdout.contains("Timeout") {
            return Err(VerificationError::BoogieError(
                "Boogie verification timeout".to_string(),
            ));
        }

        // Check for inconclusive results
        if stdout.contains("inconclusive") {
            return Ok(false); // Treat inconclusive as failed verification
        }

        // If we can't parse the result, treat it as an error
        Err(VerificationError::BoogieError(format!(
            "Could not parse Boogie output. Stdout: {}, Stderr: {}",
            stdout, stderr
        )))
    }
}

#[derive(Debug)]
pub struct VerificationResults {
    pub total_c_edges: usize,
    pub verified_count: usize,
    pub verified_edges: Vec<usize>,
    pub failed_verification: Vec<(usize, String)>,
    pub errors: Vec<(usize, String)>,
}

impl VerificationResults {
    pub fn new() -> Self {
        Self {
            total_c_edges: 0,
            verified_count: 0,
            verified_edges: Vec::new(),
            failed_verification: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn success_rate(&self) -> f64 {
        if self.total_c_edges == 0 {
            1.0
        } else {
            self.verified_count as f64 / self.total_c_edges as f64
        }
    }
}

#[derive(Debug)]
pub enum VerificationError {
    IoError(String),
    BoogieError(String),
    BuildError(String),
}

impl std::fmt::Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VerificationError::IoError(msg) => write!(f, "IO Error: {}", msg),
            VerificationError::BoogieError(msg) => write!(f, "Boogie Error: {}", msg),
            VerificationError::BuildError(msg) => write!(f, "Build Error: {}", msg),
        }
    }
}

impl std::error::Error for VerificationError {}

impl Default for AutoVerifier {
    fn default() -> Self {
        Self::new()
    }
}
