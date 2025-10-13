pub mod Boogie;
mod base_generator;
pub mod commutative;
pub mod errors;
pub mod partition;
mod scope;
mod strategy;
pub mod verify_result_process;

use crate::cfg::Program;
use crate::sc_graph::SCGraph;
use errors::Results;

// pub use errors::{SpannedError, VerificationError};

/// Types of verification to generate (currently only Commutative is active)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerificationType {
    /// Slice commutativity verification
    Commutative,
    /// Hop partition consistency verification
    HopPartition,
}

/// Simple verification manager for Boogie generation (commutative-only)
pub struct VerificationManager {}

impl VerificationManager {
    pub fn new() -> Self {
        VerificationManager {}
    }

    /// Generate verification programs based on type
    pub fn generate_verification_programs(
        &self,
        cfg_program: &Program,
        sc_graph: &SCGraph,
        verification_type: VerificationType,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        match verification_type {
            VerificationType::Commutative => {
                let mut commutative_manager = commutative::CommutativeVerificationManager::new();
                commutative_manager.generate_commutative_verification(cfg_program, sc_graph)
            }
            VerificationType::HopPartition => {
                let mut partition_manager = partition::HopPartitionVerificationManager::new();
                partition_manager.generate_partition_verification(cfg_program)
            }
        }
    }
}
