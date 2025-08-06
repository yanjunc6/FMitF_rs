use std::collections::HashMap;

use crate::cfg::{self, CfgProgram, HopId};

pub mod errors;
pub mod boogie;
pub mod partition_soundness;

use self::boogie::BoogieProgram;
use self::errors::VerificationError;
use self::partition_soundness::PartitionSoundnessUnit;
/// What the outside world (CLI, tests) has to call.
pub struct Verifier {
    /// The verification units to run.
    pub units: Vec<Box<dyn VerificationUnit>>,

    /// A map of function IDs to their names for diagnostics.
    pub results: HashMap<cfg::FunctionId, Vec<VerificationError>>,
}

impl Verifier {
    pub fn new() -> Self {
        Self {
            units: Vec::new(),
            results: HashMap::new(),
        }
    }

    /// Add a partition soundness verification unit for the given function
    pub fn add_partition_soundness(&mut self, function_id: cfg::FunctionId) {
        self.units.push(Box::new(PartitionSoundnessUnit { function_id }));
    }

    /// Run all verification units and collect results
    pub fn verify(&mut self, cfg_program: &CfgProgram) -> VerificationResult {
        let mut all_errors = Vec::new();
        let mut successful = true;

        for unit in &self.units {
            // Generate Boogie program for this unit
            let _boogie_program = unit.generate(cfg_program);
            
            // For now, we'll just collect the generated programs
            // Later we'll add actual Boogie verification
            match unit.property() {
                Property::PartitionSoundness { function } => {
                    // Store any errors found during generation
                    if let Some(errors) = self.results.get(&function) {
                        all_errors.extend(errors.iter().cloned());
                        if !errors.is_empty() {
                            successful = false;
                        }
                    }
                }
                Property::Commutativity { .. } => {
                    // TODO: Implement commutativity verification
                }
            }
        }

        VerificationResult {
            successful,
            errors: all_errors,
        }
    }
}

/// Enum used in user-facing diagnostics.
#[derive(Debug, Clone)]
pub enum Property {
    PartitionSoundness {
        function: cfg::FunctionId,
    },
    Commutativity {
        function_a: cfg::FunctionId,
        function_b: cfg::FunctionId,
        hops_a: Vec<HopId>,
        hops_b: Vec<HopId>,
    }
}

/// All verification units conform to this interface.
pub trait VerificationUnit {
    /// Human-readable name used in logs
    fn property(&self) -> Property;

    fn generate(
        &self,
        cfg_program: &CfgProgram,
    ) -> BoogieProgram;
}

pub struct VerificationResult {
    pub successful: bool,
    pub errors: Vec<VerificationError>,
}

/// High-level interface for running different types of verification
pub struct VerificationManager {
    pub partition_verifier: PartitionVerifier,
}

impl VerificationManager {
    pub fn new() -> Self {
        Self {
            partition_verifier: PartitionVerifier::new(),
        }
    }

    /// Run partition soundness verification on all transaction functions
    pub fn run_partition_verification(
        &mut self, 
        cfg_program: &CfgProgram,
        output_dir: Option<&str>
    ) -> PartitionVerificationResult {
        self.partition_verifier.verify_all_functions(cfg_program, output_dir)
    }
}

/// Manages partition soundness verification specifically
pub struct PartitionVerifier {
    pub boogie_output_dir: Option<String>,
}

impl PartitionVerifier {
    pub fn new() -> Self {
        Self {
            boogie_output_dir: None,
        }
    }

    pub fn set_boogie_output_dir(&mut self, dir: String) {
        self.boogie_output_dir = Some(dir);
    }

    /// Verify partition soundness for all transaction functions
    pub fn verify_all_functions(
        &self, 
        cfg_program: &CfgProgram,
        _output_dir: Option<&str>
    ) -> PartitionVerificationResult {
        let mut functions_verified = 0;
        let mut functions_failed = 0;
        let mut boogie_files_generated = 0;
        let mut errors = Vec::new();

        // Find all transaction functions (not partition functions)
        for (function_id, function_cfg) in cfg_program.functions.iter() {
            if function_cfg.function_type == crate::cfg::FunctionType::Transaction {
                functions_verified += 1;

                // Create verification unit for this function
                let unit = PartitionSoundnessUnit { function_id };
                
                // Generate Boogie program
                match self.generate_and_save_boogie(&unit, cfg_program) {
                    Ok(_) => {
                        boogie_files_generated += 1;
                    }
                    Err(err) => {
                        functions_failed += 1;
                        errors.push(err);
                    }
                }
            }
        }

        PartitionVerificationResult {
            functions_verified,
            functions_failed,
            boogie_files_generated,
            errors,
        }
    }

    fn generate_and_save_boogie(
        &self,
        unit: &PartitionSoundnessUnit,
        cfg_program: &CfgProgram
    ) -> Result<(), VerificationError> {
        // Generate the Boogie program
        let boogie_program = unit.generate(cfg_program);
        
        // Save to file if output directory is set
        if let Some(ref output_dir) = self.boogie_output_dir {
            let function_name = &cfg_program.functions[unit.function_id].name;
            let filename = format!("{}/partition_soundness_{}.bpl", output_dir, function_name);
            
            // Create directory if it doesn't exist
            if let Some(parent) = std::path::Path::new(&filename).parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    VerificationError::BoogieGenerationError {
                        function_name: function_name.clone(),
                        message: format!("Failed to create output directory: {}", e),
                    }
                })?;
            }

            // Write Boogie program to file
            let boogie_text = format!("{}", boogie_program);
            std::fs::write(&filename, boogie_text).map_err(|e| {
                VerificationError::BoogieGenerationError {
                    function_name: function_name.clone(),
                    message: format!("Failed to write Boogie file: {}", e),
                }
            })?;
        }

        Ok(())
    }
}

/// Result of partition verification process
#[derive(Debug)]
pub struct PartitionVerificationResult {
    pub functions_verified: usize,
    pub functions_failed: usize,
    pub boogie_files_generated: usize,
    pub errors: Vec<VerificationError>,
}

