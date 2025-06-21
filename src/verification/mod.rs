use crate::cfg::CfgProgram;
use crate::sc_graph::{SCGraph, EdgeType, Edge};
use std::collections::HashMap;
use std::path::Path;

pub mod commutativity_check;
pub mod code_generation;
pub mod interleaving;
pub mod execution;
pub use execution::{VerificationExecution, VerificationResult};

/// The main verification interface - handles all verification operations
pub struct VerificationManager {
    pub boogie_codes: HashMap<Edge, String>, // Store generated Boogie code for potential file output
    pub results: HashMap<Edge, VerificationResult>, // Store results of verification
}

impl VerificationManager {
    pub fn new(

    ) -> Self {       
        Self { 
            boogie_codes: HashMap::new(),
            results: HashMap::new(),
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
            
            // Store the Boogie code for potential file output
            self.boogie_codes.insert(edge.clone(), boogie_code.clone());
            
            // 3) Execute the Boogie verification and store the result
            execution.execute_boogie(edge.clone(), boogie_code);
        }
    }

    /// Save Boogie files to a directory
    pub fn save_boogie_files(&self, output_dir: &Path) -> Result<(), String> {
        // Create the output directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(output_dir) {
            return Err(format!("Failed to create output directory: {}", e));
        }
        
        // Save each Boogie file
        for (edge, boogie_code) in &self.boogie_codes {
            let filename = format!("edge_{}_{}.bpl", edge.source.index(), edge.target.index());
            let file_path = output_dir.join(filename);
            
            std::fs::write(&file_path, boogie_code)
                .map_err(|e| format!("Failed to write Boogie file {}: {}", file_path.display(), e))?;
        }
        
        println!("Boogie files saved to: {}", output_dir.display());
        Ok(())
    }
}