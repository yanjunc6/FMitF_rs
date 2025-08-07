use std::collections::HashMap;

use crate::cfg::{self, CfgProgram, HopId};

pub mod boogie;
pub mod commutativity;
pub mod errors;
pub mod partition_soundness;

use self::boogie::{BoogieFailureType, BoogieProgram, BoogieResult};
use self::commutativity::CommutativityUnit;
use self::errors::VerificationError;
use self::partition_soundness::PartitionSoundnessUnit;

/// Result of a single commutativity unit verification
#[derive(Debug, Clone)]
pub enum CommutativityUnitResult {
    Commutative,    // Verification succeeded - eliminate C-edge
    NonCommutative, // Assertion failures - keep C-edge
    SyntaxError,    // Syntax/parse errors - stage failure
}

/// Detailed verification result for C-edge elimination
#[derive(Debug)]
pub struct DetailedCommutativityResult {
    pub verifications_attempted: usize,
    pub boogie_files_generated: usize,
    pub errors: Vec<VerificationError>,
    pub unit_results: Vec<CommutativityUnitResult>,
    pub syntax_errors: usize,
    pub boogie_results: Vec<(String, BoogieResult)>, // (filename, result) pairs
}
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
        self.units
            .push(Box::new(PartitionSoundnessUnit { function_id }));
    }

    /// Add a commutativity verification unit for the given hop slices
    pub fn add_commutativity(
        &mut self,
        function_a: cfg::FunctionId,
        function_b: cfg::FunctionId,
        hops_a: Vec<HopId>,
        hops_b: Vec<HopId>,
    ) {
        self.units.push(Box::new(CommutativityUnit::new(
            function_a, function_b, hops_a, hops_b,
        )));
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
    },
}

/// All verification units conform to this interface.
pub trait VerificationUnit {
    /// Human-readable name used in logs
    fn property(&self) -> Property;

    fn generate(&self, cfg_program: &CfgProgram) -> BoogieProgram;
}

pub struct VerificationResult {
    pub successful: bool,
    pub errors: Vec<VerificationError>,
}

/// High-level interface for running different types of verification
pub struct VerificationManager {
    pub partition_verifier: PartitionVerifier,
    pub commutativity_verifier: CommutativityVerifier,
}

impl VerificationManager {
    pub fn new() -> Self {
        Self {
            partition_verifier: PartitionVerifier::new(),
            commutativity_verifier: CommutativityVerifier::new(),
        }
    }

    /// Run partition soundness verification on all transaction functions
    pub fn run_partition_verification(
        &mut self,
        cfg_program: &CfgProgram,
        output_dir: Option<&str>,
    ) -> PartitionVerificationResult {
        // Automatically generate partition verification units for all transaction functions
        let mut verifier = Verifier::new();

        for (function_id, function_cfg) in cfg_program.functions.iter() {
            if function_cfg.function_type == crate::cfg::FunctionType::Transaction {
                verifier.add_partition_soundness(function_id);
            }
        }

        self.partition_verifier
            .verify_units(&verifier.units, cfg_program, output_dir)
    }

    /// Run commutativity verification on C-edges and eliminate commutative ones
    pub fn run_commutativity_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &mut crate::sc_graph::SCGraph,
        output_dir: Option<&str>,
    ) -> CommutativityVerificationResult {
        // Automatically generate commutativity verification units for all C-edges
        let mut verifier = Verifier::new();
        let mut edge_to_unit_map = std::collections::HashMap::new();

        for (edge_idx, edge) in sc_graph.edges.iter().enumerate() {
            if edge.edge_type == crate::sc_graph::EdgeType::C {
                // For simplicity, create commutativity verification between just these two hops
                let hops_a = vec![edge.source.hop_id];
                let hops_b = vec![edge.target.hop_id];

                let unit_idx = verifier.units.len();
                verifier.add_commutativity(
                    edge.source.function_id,
                    edge.target.function_id,
                    hops_a,
                    hops_b,
                );
                edge_to_unit_map.insert(edge_idx, unit_idx);
            }
        }

        let detailed_result = self.commutativity_verifier.verify_units_for_elimination(
            &verifier.units,
            cfg_program,
            output_dir,
        );

        // Based on verification results, eliminate commutative C-edges
        let mut edges_to_remove = Vec::new();
        let mut c_edges_eliminated = 0;
        let mut c_edges_kept = 0;

        for (edge_idx, _unit_idx) in edge_to_unit_map {
            if edge_idx < detailed_result.unit_results.len() {
                let unit_result = &detailed_result.unit_results[edge_idx];
                match unit_result {
                    CommutativityUnitResult::Commutative => {
                        edges_to_remove.push(edge_idx);
                        c_edges_eliminated += 1;
                    }
                    CommutativityUnitResult::NonCommutative => {
                        c_edges_kept += 1;
                    }
                    CommutativityUnitResult::SyntaxError => {
                        // Keep the edge but this is an error condition
                        c_edges_kept += 1;
                    }
                }
            }
        }

        // Remove edges in reverse order to maintain indices
        edges_to_remove.sort_by(|a, b| b.cmp(a));
        for edge_idx in edges_to_remove {
            sc_graph.edges.remove(edge_idx);
        }

        CommutativityVerificationResult {
            verifications_attempted: detailed_result.verifications_attempted,
            verifications_failed: detailed_result.syntax_errors, // Only syntax errors count as failures
            boogie_files_generated: detailed_result.boogie_files_generated,
            errors: detailed_result.errors,
            c_edges_eliminated,
            c_edges_kept,
            syntax_errors: detailed_result.syntax_errors,
            boogie_results: detailed_result.boogie_results,
        }
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

    /// Verify partition soundness using provided verification units
    pub fn verify_units(
        &self,
        units: &[Box<dyn VerificationUnit>],
        cfg_program: &CfgProgram,
        _output_dir: Option<&str>,
    ) -> PartitionVerificationResult {
        let mut functions_verified = 0;
        let mut functions_failed = 0;
        let mut boogie_files_generated = 0;
        let mut errors = Vec::new();

        for unit in units {
            if let Property::PartitionSoundness { function: _ } = unit.property() {
                functions_verified += 1;

                // Generate Boogie program
                match self.generate_and_save_boogie_partition(unit.as_ref(), cfg_program) {
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

    fn generate_and_save_boogie_partition(
        &self,
        unit: &dyn VerificationUnit,
        cfg_program: &CfgProgram,
    ) -> Result<(), VerificationError> {
        // Generate the Boogie program
        let boogie_program = unit.generate(cfg_program);

        // Get function name for the file
        let function_name = if let Property::PartitionSoundness { function } = unit.property() {
            &cfg_program.functions[function].name
        } else {
            return Err(VerificationError::BoogieGenerationError {
                function_name: "unknown".to_string(),
                message: "Invalid verification unit for partition soundness".to_string(),
            });
        };

        // Save to file if output directory is set
        if let Some(ref output_dir) = self.boogie_output_dir {
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

/// Manages commutativity verification specifically
pub struct CommutativityVerifier {
    pub boogie_output_dir: Option<String>,
}

impl CommutativityVerifier {
    pub fn new() -> Self {
        Self {
            boogie_output_dir: None,
        }
    }

    pub fn set_boogie_output_dir(&mut self, dir: String) {
        self.boogie_output_dir = Some(dir);
    }

    /// Get the filename for a Boogie unit (for logging purposes)
    fn get_boogie_filename(&self, unit: &dyn VerificationUnit) -> String {
        // Extract function names from the unit
        if let Property::Commutativity {
            function_a,
            function_b,
            ..
        } = unit.property()
        {
            // Use function IDs in debug format
            format!("commutativity_{:?}_{:?}.bpl", function_a, function_b)
        } else {
            "unknown_commutativity.bpl".to_string()
        }
    }

    /// Verify commutativity using provided verification units
    pub fn verify_units(
        &self,
        units: &[Box<dyn VerificationUnit>],
        cfg_program: &CfgProgram,
        _output_dir: Option<&str>,
    ) -> CommutativityVerificationResult {
        let mut verifications_attempted = 0;
        let mut verifications_failed = 0;
        let mut boogie_files_generated = 0;
        let mut errors = Vec::new();

        for unit in units {
            if let Property::Commutativity { .. } = unit.property() {
                verifications_attempted += 1;

                // Generate Boogie program
                match self.generate_and_save_commutativity_boogie_unit(unit.as_ref(), cfg_program) {
                    Ok(_) => {
                        boogie_files_generated += 1;
                    }
                    Err(err) => {
                        verifications_failed += 1;
                        errors.push(err);
                    }
                }
            }
        }

        CommutativityVerificationResult {
            verifications_attempted,
            verifications_failed,
            boogie_files_generated,
            errors,
            c_edges_eliminated: 0,
            c_edges_kept: 0,
            syntax_errors: 0,
            boogie_results: vec![], // Empty for the basic verify_units method
        }
    }

    /// Verify commutativity units and categorize results for C-edge elimination
    pub fn verify_units_for_elimination(
        &self,
        units: &[Box<dyn VerificationUnit>],
        cfg_program: &CfgProgram,
        _output_dir: Option<&str>,
    ) -> DetailedCommutativityResult {
        let mut verifications_attempted = 0;
        let mut boogie_files_generated = 0;
        let mut errors = Vec::new();
        let mut unit_results = Vec::new();
        let mut syntax_errors = 0;
        let mut boogie_results = Vec::new();

        for unit in units {
            if let Property::Commutativity { .. } = unit.property() {
                verifications_attempted += 1;

                // Generate Boogie program and save to file
                match self.generate_and_save_commutativity_boogie_unit(unit.as_ref(), cfg_program) {
                    Ok(_) => {
                        boogie_files_generated += 1;

                        // Run Boogie verification and categorize the result
                        let boogie_result =
                            self.run_boogie_verification(unit.as_ref(), cfg_program);
                        match boogie_result {
                            Some(result) => {
                                // Store the boogie result for logging
                                let filename = self.get_boogie_filename(unit.as_ref());
                                boogie_results.push((filename, result.clone()));

                                if result.success {
                                    unit_results.push(CommutativityUnitResult::Commutative);
                                } else {
                                    match result.failure_type {
                                        Some(BoogieFailureType::AssertionFailure) => {
                                            unit_results
                                                .push(CommutativityUnitResult::NonCommutative);
                                        }
                                        Some(BoogieFailureType::SyntaxError) => {
                                            unit_results.push(CommutativityUnitResult::SyntaxError);
                                            syntax_errors += 1;
                                        }
                                        _ => {
                                            unit_results.push(CommutativityUnitResult::SyntaxError);
                                            syntax_errors += 1;
                                        }
                                    }
                                }
                            }
                            None => {
                                unit_results.push(CommutativityUnitResult::SyntaxError);
                                syntax_errors += 1;
                            }
                        }
                    }
                    Err(err) => {
                        errors.push(err);
                        unit_results.push(CommutativityUnitResult::SyntaxError);
                        syntax_errors += 1;
                    }
                }
            }
        }

        DetailedCommutativityResult {
            verifications_attempted,
            boogie_files_generated,
            errors,
            unit_results,
            syntax_errors,
            boogie_results,
        }
    }

    /// Run Boogie verification and return the parsed result
    fn run_boogie_verification(
        &self,
        unit: &dyn VerificationUnit,
        cfg_program: &CfgProgram,
    ) -> Option<BoogieResult> {
        // Get the file path for the generated Boogie file
        let (func_a_name, func_b_name) = if let Property::Commutativity {
            function_a,
            function_b,
            ..
        } = unit.property()
        {
            (
                &cfg_program.functions[function_a].name,
                &cfg_program.functions[function_b].name,
            )
        } else {
            return None;
        };

        if let Some(ref output_dir) = self.boogie_output_dir {
            let filename = format!(
                "{}/commutativity_{}_{}.bpl",
                output_dir, func_a_name, func_b_name
            );

            // Run Boogie process with /quiet flag - only shows warnings and errors
            match std::process::Command::new("boogie")
                .arg("/quiet")
                .arg(&filename)
                .output()
            {
                Ok(output) => {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    let exit_code = output.status.code().unwrap_or(-1);

                    Some(boogie::parse_boogie_result_quiet(
                        &stdout, &stderr, exit_code,
                    ))
                }
                Err(_) => None,
            }
        } else {
            None
        }
    }

    fn generate_and_save_commutativity_boogie_unit(
        &self,
        unit: &dyn VerificationUnit,
        cfg_program: &CfgProgram,
    ) -> Result<(), VerificationError> {
        // Generate the Boogie program
        let boogie_program = unit.generate(cfg_program);

        // Get function names for the file
        let (func_a_name, func_b_name) = if let Property::Commutativity {
            function_a,
            function_b,
            ..
        } = unit.property()
        {
            (
                &cfg_program.functions[function_a].name,
                &cfg_program.functions[function_b].name,
            )
        } else {
            return Err(VerificationError::BoogieGenerationError {
                function_name: "unknown".to_string(),
                message: "Invalid verification unit for commutativity".to_string(),
            });
        };

        // Save to file if output directory is set
        if let Some(ref output_dir) = self.boogie_output_dir {
            let filename = format!(
                "{}/commutativity_{}_{}.bpl",
                output_dir, func_a_name, func_b_name
            );

            // Create directory if it doesn't exist
            if let Some(parent) = std::path::Path::new(&filename).parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    VerificationError::BoogieGenerationError {
                        function_name: format!("{}_{}", func_a_name, func_b_name),
                        message: format!("Failed to create output directory: {}", e),
                    }
                })?;
            }

            // Write Boogie program to file
            let boogie_text = format!("{}", boogie_program);
            std::fs::write(&filename, boogie_text).map_err(|e| {
                VerificationError::BoogieGenerationError {
                    function_name: format!("{}_{}", func_a_name, func_b_name),
                    message: format!("Failed to write Boogie file: {}", e),
                }
            })?;
        }

        Ok(())
    }
}

/// Result of commutativity verification process
#[derive(Debug)]
pub struct CommutativityVerificationResult {
    pub verifications_attempted: usize,
    pub verifications_failed: usize,
    pub boogie_files_generated: usize,
    pub errors: Vec<VerificationError>,
    pub c_edges_eliminated: usize,
    pub c_edges_kept: usize,
    pub syntax_errors: usize,
    pub boogie_results: Vec<(String, BoogieResult)>, // (filename, result) pairs
}
