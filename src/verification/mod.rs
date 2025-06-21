use crate::cfg::CfgProgram;
use crate::sc_graph::{SCGraph, EdgeType, Edge};
use std::collections::HashMap;
use std::path::{Path, PathBuf};


pub mod boogie_file_manager;
pub mod commutativity_check;
pub mod code_generation;
pub mod interleaving;
pub mod execution;
pub use execution::{VerificationExecution, VerificationResult};


use boogie_file_manager::{BoogieFile, BoogieFileManager};


/// The main verification interface - handles all verification operations
pub struct VerificationManager {
    pub boogie_files: Vec<BoogieFile>, // Store generated Boogie files with their names
    pub results: HashMap<Edge, VerificationResult>, // Store results of verification
    pub temp_file_paths: Vec<PathBuf>, // Track temporary files for cleanup
}

impl VerificationManager {
    pub fn new(

    ) -> Self {       
        Self { 
            boogie_files: Vec::new(),
            results: HashMap::new(),
            temp_file_paths: Vec::new(),
        }
    }

    /// Run the verification pipeline and return owned results
    pub fn run_commutativity_pipeline(&mut self, cfg: &CfgProgram, sc_graph: &SCGraph) {
        // Get all C-edges (commutativity edges) from the SC graph
        let c_edges: Vec<_> = sc_graph.edges.iter()
            .filter(|edge| edge.edge_type == EdgeType::C)
            .collect();
        
        let execution = VerificationExecution;
        
        // Process each C-edge
        for edge in c_edges {
            // 1) Create a VerificationUnit for this C-edge
            let verification_unit = commutativity_check::create_verification_unit(
                edge.clone(),
                cfg,
                sc_graph,
            );
            
            // 2) Generate Boogie code for this unit
            let boogie_code = code_generation::generate_boogie_for_unit(&verification_unit);
            
            // 3) Generate filename using the unit (better naming)
            let filename = BoogieFileManager::generate_filename(&verification_unit, cfg);
            
            // 4) Create BoogieFile entry
            let boogie_file = BoogieFile {
                filename,
                code: boogie_code,
            };
            
            // Store the Boogie file for potential output
            self.boogie_files.push(boogie_file.clone());
            
            // 5) Write temporary file for verification
            match BoogieFileManager::write_temp_file(&boogie_file) {
                Ok(temp_path) => {
                    // Track temporary file for cleanup
                    self.temp_file_paths.push(temp_path.clone());
                    
                    // 6) Execute the Boogie verification
                    let result = execution.execute_boogie(&temp_path);
                    self.results.insert(edge.clone(), result);
                }
                Err(e) => {
                    let result = VerificationResult::Failure(e);
                    self.results.insert(edge.clone(), result);
                }
            }
        }
        
        // 7) Clean up temporary files
        self.cleanup_temp_files();
    }
    
    /// Clean up temporary files
    fn cleanup_temp_files(&mut self) {
        BoogieFileManager::cleanup_files(&self.temp_file_paths);
        self.temp_file_paths.clear();
    }

    /// Save Boogie files to a directory
    pub fn save_boogie_files(&self, output_dir: &Path) -> Result<(), String> {
        BoogieFileManager::write_files(&self.boogie_files, output_dir)?;
        println!("Boogie files saved to: {}", output_dir.display());
        Ok(())
    }
}

impl Drop for VerificationManager {
    fn drop(&mut self) {
        // Ensure cleanup happens even if the user doesn't call cleanup_temp_files explicitly
        self.cleanup_temp_files();
    }
}