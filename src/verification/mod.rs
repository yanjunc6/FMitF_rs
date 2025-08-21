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
    /// P-1: Correct PartitionNode Placement verification
    NodePlacement,
    /// P-2: Slice Commutativity verification  
    SliceCommutativity,
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
            VerificationType::NodePlacement => {
                partition_manager.generate_p1_verification(cfg_program)
            }
            VerificationType::SliceCommutativity => {
                commutative_manager.generate_p2_verification(cfg_program)
            }
            VerificationType::All => {
                let mut all_programs = Vec::new();
                all_programs.extend(partition_manager.generate_p1_verification(cfg_program)?);
                all_programs.extend(commutative_manager.generate_p2_verification(cfg_program)?);
                Ok(all_programs)
            }
        }
    }
}
