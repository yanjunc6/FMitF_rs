use crate::ast::Span;
use serde::{Deserialize, Serialize};

pub type Results<T> = Result<T, Vec<SpannedError>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpannedError {
    pub error: VerificationError,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VerificationError {
    StringConstantNotSupported,
    ArrayConstantNotSupported,
    IncrementDecrementNotSupported,
    HopNotFoundInFunction,
    PartitionFunctionArgumentInconsistency {
        partition_function_id: usize,
        function_id: usize,
        table_id: usize,
    },
    SliceCommutativityViolation {
        hop_id_1: usize,
        hop_id_2: usize,
    },
    SpecialInterleavingNonEquivalence {
        hop_id_1: usize,
        hop_id_2: usize,
    },
}

impl std::fmt::Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type(), self.message())
    }
}

impl std::error::Error for VerificationError {}

impl VerificationError {
    /// Get the error type name for display purposes.
    pub fn error_type(&self) -> &'static str {
        match self {
            VerificationError::StringConstantNotSupported => "StringConstantNotSupported",
            VerificationError::ArrayConstantNotSupported => "ArrayConstantNotSupported",
            VerificationError::IncrementDecrementNotSupported => "IncrementDecrementNotSupported",
            VerificationError::HopNotFoundInFunction => "HopNotFoundInFunction",
            VerificationError::PartitionFunctionArgumentInconsistency { .. } => {
                "PartitionFunctionArgumentInconsistency"
            }
            VerificationError::SliceCommutativityViolation { .. } => "SliceCommutativityViolation",
            VerificationError::SpecialInterleavingNonEquivalence { .. } => {
                "SpecialInterleavingNonEquivalence"
            }
        }
    }

    /// Get the error message without the type prefix.
    pub fn message(&self) -> String {
        match self {
            VerificationError::StringConstantNotSupported => {
                "String constants are not supported".to_string()
            }
            VerificationError::ArrayConstantNotSupported => {
                "Array constants are not supported".to_string()
            }
            VerificationError::IncrementDecrementNotSupported => {
                "Increment/decrement operators are not supported".to_string()
            }
            VerificationError::HopNotFoundInFunction => "Hop not found in any function".to_string(),
            VerificationError::PartitionFunctionArgumentInconsistency {
                partition_function_id,
                function_id,
                table_id,
            } => {
                format!(
                    "Function {} (table {} with partition {}) has different arguments in the same hop, violating single-node constraint",
                    function_id, table_id, partition_function_id
                )
            }
            VerificationError::SliceCommutativityViolation { hop_id_1, hop_id_2 } => {
                format!(
                    "Slice commutativity violation: interleaving between hop {} and hop {} produces different result than both special orderings",
                    hop_id_1, hop_id_2
                )
            }
            VerificationError::SpecialInterleavingNonEquivalence { hop_id_1, hop_id_2 } => {
                format!(
                    "Special interleavings non-equivalence: hop {} → hop {} and hop {} → hop {} produce different results, slices are not commutative",
                    hop_id_1, hop_id_2, hop_id_2, hop_id_1
                )
            }
        }
    }
}
