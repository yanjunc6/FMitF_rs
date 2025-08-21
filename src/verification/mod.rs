pub mod Boogie;
pub mod commutative;
pub mod errors;
pub mod partition;

use crate::cfg::CfgProgram;
use errors::Results;

pub use errors::{SpannedError, VerificationError};

/// Types of verification to generate
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerificationType {
    /// Partition function consistency verification
    Partition,
    /// Slice commutativity verification  
    Commutative,
    /// Generate both verification types
    All,
}

/// Simple verification manager for Boogie generation
pub struct VerificationManager {}

impl VerificationManager {
    pub fn new() -> Self {
        VerificationManager {}
    }

    /// Generate verification programs based on type
    pub fn generate_verification_programs(
        &self,
        cfg_program: &CfgProgram,
        verification_type: VerificationType,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let partition_manager = partition::PartitionVerificationManager {};
        let commutative_manager = commutative::CommutativeVerificationManager {};
        match verification_type {
            VerificationType::Partition => {
                partition_manager.generate_partition_verification(cfg_program)
            }
            VerificationType::Commutative => {
                commutative_manager.generate_commutative_verification(cfg_program)
            }
            VerificationType::All => {
                let mut all_programs = Vec::new();
                all_programs
                    .extend(partition_manager.generate_partition_verification(cfg_program)?);
                all_programs
                    .extend(commutative_manager.generate_commutative_verification(cfg_program)?);
                Ok(all_programs)
            }
        }
    }
}
