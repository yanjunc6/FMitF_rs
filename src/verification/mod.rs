pub mod partition_verification;

pub use partition_verification::{
    PartitionVerificationResult, 
    PartitionVerifier,
    DetailedTableAccess,
    CrossPartitionAccess,
    PartitionVerificationError,
};

/// Simple verification manager that orchestrates partition verification
pub struct VerificationManager {
    partition_verifier: PartitionVerifier,
}

impl VerificationManager {
    pub fn new() -> Self {
        Self {
            partition_verifier: PartitionVerifier::new(),
        }
    }

    pub fn run_partition_verification(
        &mut self,
        cfg_program: &crate::cfg::CfgProgram,
        sc_graph: &crate::sc_graph::SCGraph,
        output_dir: Option<&str>,
    ) -> PartitionVerificationResult {
        if let Some(dir) = output_dir {
            self.partition_verifier.set_boogie_output_dir(dir.to_string());
        }
        
        self.partition_verifier.run_verification(cfg_program, sc_graph)
    }
}
