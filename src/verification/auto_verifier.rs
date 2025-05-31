// use crate::ast::*;
// use crate::verification::*;
// use std::fs;
// use std::path::Path;
// use std::rc::Rc;

// pub struct AutoVerifier {
//     // Add option to save Boogie files
//     save_boogie_files: bool,
//     output_dir: Option<String>,
// }

// impl AutoVerifier {
//     pub fn new() -> Self {
//         Self {
//             save_boogie_files: false,
//             output_dir: None,
//         }
//     }

//     pub fn with_output_dir<P: AsRef<Path>>(mut self, output_dir: P) -> Self {
//         self.save_boogie_files = true;
//         self.output_dir = Some(output_dir.as_ref().to_string_lossy().to_string());
//         self
//     }

//     /// Automatically verify all C edges in the SCGraph and remove those that pass verification
//     pub fn verify_and_prune_c_edges(
//         &self,
//         program: &Program,
//         scgraph: &mut SCGraph,
//     ) -> Result<usize, VerificationError> {
//         let mut verified_count = 0;
//         let mut edges_to_remove = Vec::new();

//         // Create output directory if needed
//         if let Some(ref dir) = self.output_dir {
//             std::fs::create_dir_all(dir)
//                 .map_err(|e| VerificationError::IoError(format!("Failed to create output directory: {}", e)))?;
//         }

//         // Find all C edges
//         for (i, edge) in scgraph.edges.iter().enumerate() {
//             if matches!(edge.edge_type, EdgeType::C) {
//                 if self.verify_c_edge(program, scgraph, edge, i)? {
//                     edges_to_remove.push(i);
//                     verified_count += 1;
//                 }
//             }
//         }

//         // Remove verified C edges (in reverse order to maintain indices)
//         for &idx in edges_to_remove.iter().rev() {
//             scgraph.edges.remove(idx);
//         }

//         Ok(verified_count)
//     }

//     /// Verify a single C edge
//     fn verify_c_edge(
//         &self,
//         program: &Program,
//         scgraph: &SCGraph,
//         edge: &Edge,
//         edge_index: usize,
//     ) -> Result<bool, VerificationError> {
//         // Build verification unit
//         let verification_unit = build_verification_unit(program, scgraph, edge)
//             .ok_or_else(|| VerificationError::BuildError("Failed to build verification unit".to_string()))?;

//         // Generate Boogie code
//         let boogie_code = generate_verification_boogie_code(&verification_unit, program);

//         // Save Boogie file if output directory is specified
//         if self.save_boogie_files {
//             self.save_boogie_file(&boogie_code, &verification_unit, edge_index)?;
//         }

//         // Run Boogie verification
//         self.run_boogie_verification(&boogie_code)
//     }

//     /// Save Boogie file with a reasonable name
//     fn save_boogie_file(
//         &self,
//         boogie_code: &str,
//         verification_unit: &VerificationUnit,
//         edge_index: usize,
//     ) -> Result<(), VerificationError> {
//         if let Some(ref output_dir) = self.output_dir {
//             // Create a reasonable filename based on the verification unit
//             let func1_name = &verification_unit.func1.name;
//             let func2_name = &verification_unit.func2.name;
//             let node_name = &verification_unit.node.name;
            
//             // Sanitize names for filesystem
//             let sanitize = |s: &str| s.replace(['/', '\\', ':', '*', '?', '"', '<', '>', '|'], "_");
            
//             let filename = format!(
//                 "edge_{:03}_{}_{}_on_{}.bpl",
//                 edge_index,
//                 sanitize(func1_name),
//                 sanitize(func2_name),
//                 sanitize(node_name)
//             );
            
//             let file_path = Path::new(output_dir).join(&filename);
            
//             // Add header comment to the Boogie file
//             let mut content = String::new();
//             content.push_str(&format!("// Verification for C edge {}\n", edge_index));
//             content.push_str(&format!("// Functions: {} vs {}\n", func1_name, func2_name));
//             content.push_str(&format!("// Conflicting node: {}\n", node_name));
//             content.push_str(&format!("// Hop indices: {} vs {}\n", verification_unit.idx1, verification_unit.idx2));
//             content.push_str(&format!("// Generated at: {}\n", chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC")));
//             content.push_str("//\n\n");
//             content.push_str(boogie_code);
            
//             std::fs::write(&file_path, content)
//                 .map_err(|e| VerificationError::IoError(format!("Failed to write Boogie file {}: {}", file_path.display(), e)))?;
//         }
        
//         Ok(())
//     }

//     /// Run Boogie verification and return true if verification passes
//     fn run_boogie_verification(&self, boogie_code: &str) -> Result<bool, VerificationError> {
//         use std::process::Command;
//         use std::fs;

//         // Write Boogie code to temporary file
//         let temp_file = "/tmp/verification.bpl";
//         fs::write(temp_file, boogie_code)
//             .map_err(|e| VerificationError::IoError(e.to_string()))?;

//         // Run Boogie
//         let output = Command::new("boogie")
//             .arg(temp_file)
//             .output()
//             .map_err(|e| VerificationError::BoogieError(e.to_string()))?;

//         // Clean up
//         let _ = fs::remove_file(temp_file);

//         // Check if verification succeeded (no errors reported)
//         Ok(output.status.success() && 
//            !String::from_utf8_lossy(&output.stdout).contains("Error"))
//     }
// }

// #[derive(Debug)]
// pub enum VerificationError {
//     IoError(String),
//     BoogieError(String),
//     BuildError(String),
// }

// impl std::fmt::Display for VerificationError {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         match self {
//             VerificationError::IoError(msg) => write!(f, "IO Error: {}", msg),
//             VerificationError::BoogieError(msg) => write!(f, "Boogie Error: {}", msg),
//             VerificationError::BuildError(msg) => write!(f, "Build Error: {}", msg),
//         }
//     }
// }

// impl std::error::Error for VerificationError {}