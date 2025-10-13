use crate::util::{CompilerError, CompilerErrorKind};
use serde::{Deserialize, Serialize};

pub type Results<T> = Result<T, Vec<CompilerError>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VerificationErrorKind {
    UnsupportedConstruct {
        construct: String,
        details: String,
    },
    HopNotFound,
    PartitionFunctionInconsistency {
        function_name: String,
        table_name: String,
        partition_function_name: String,
        hop_index: usize,
    },
    NotYetImplemented(String),
}

impl std::fmt::Display for VerificationErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type(), self.message())
    }
}

impl std::error::Error for VerificationErrorKind {}

impl CompilerErrorKind for VerificationErrorKind {
    fn code(&self) -> &'static str {
        match self {
            VerificationErrorKind::UnsupportedConstruct { .. } => "V001",
            VerificationErrorKind::HopNotFound => "V002",
            VerificationErrorKind::PartitionFunctionInconsistency { .. } => "V003",
            VerificationErrorKind::NotYetImplemented(_) => "V999",
        }
    }

    fn help(&self) -> Option<&str> {
        match self {
            VerificationErrorKind::UnsupportedConstruct { .. } => {
                Some("This language feature is not supported by the verification backend.")
            }
            VerificationErrorKind::HopNotFound => {
                Some("A 'hop' statement was expected but not found in the function body.")
            }
            VerificationErrorKind::PartitionFunctionInconsistency { .. } => {
                Some("The arguments to the partition function must be consistent across all calls.")
            }
            VerificationErrorKind::NotYetImplemented(_) => {
                Some("This feature is not yet implemented in the verification backend.")
            }
        }
    }
}

impl VerificationErrorKind {
    /// Get the error type name for display purposes.
    pub fn error_type(&self) -> &'static str {
        match self {
            VerificationErrorKind::UnsupportedConstruct { .. } => "UnsupportedConstruct",
            VerificationErrorKind::HopNotFound => "HopNotFound",
            VerificationErrorKind::PartitionFunctionInconsistency { .. } => {
                "PartitionFunctionInconsistency"
            }
            VerificationErrorKind::NotYetImplemented(_) => "NotYetImplemented",
        }
    }

    /// Get the error message without the type prefix.
    pub fn message(&self) -> String {
        match self {
            VerificationErrorKind::UnsupportedConstruct { construct, details } => {
                format!("Unsupported construct '{}': {}", construct, details)
            }
            VerificationErrorKind::HopNotFound => "Hop not found in any function".to_string(),
            VerificationErrorKind::PartitionFunctionInconsistency {
                function_name,
                table_name,
                partition_function_name,
                hop_index,
            } => format!(
                "Function '{}' uses table '{}' within hop #{} and partition function '{}' with inconsistent arguments",
                function_name, table_name, hop_index, partition_function_name
            ),
            VerificationErrorKind::NotYetImplemented(feature) => {
                format!("Feature not yet implemented in verification: {}", feature)
            }
        }
    }
}
