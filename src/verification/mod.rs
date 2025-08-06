pub mod partition_verification;

#[cfg(test)]
pub mod partition_verification_test;

pub use partition_verification::{
    PartitionVerifier, 
    PartitionVerificationResult, 
    DetailedTableAccess,
    CrossPartitionAccess,
    PartitionVerificationError
};

use crate::cfg::CfgProgram;

/// The main verification interface - handles partition verification operations
pub struct VerificationManager {
    pub partition_verifier: PartitionVerifier,
}

impl VerificationManager {
    pub fn new() -> Self {
        Self {
            partition_verifier: PartitionVerifier::new(),
        }
    }

    /// Run the partition verification pipeline
    pub fn run_partition_verification(
        &mut self,
        cfg_program: &CfgProgram,
        output_dir: Option<&str>,
    ) -> PartitionVerificationResult {
        // Set output directory for Boogie files if provided
        if let Some(dir) = output_dir {
            let boogie_dir = format!("{}/boogie", dir);
            self.partition_verifier.set_boogie_output_dir(boogie_dir);
        }

        // Run the verification
        self.partition_verifier.run_verification(cfg_program)
    }
}
