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
        function_name: String,
        table_name: String,
        partition_function_name: String,
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
                function_name,
                table_name,
                partition_function_name,
            } => format!(
                "Function '{}' uses table '{}' with partition function '{}' with inconsistent arguments",
                function_name, table_name, partition_function_name
            ),
        }
    }
}
