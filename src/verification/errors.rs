use crate::ast::Span;

pub type Results<T> = Result<T, Vec<SpannedError>>;

#[derive(Debug, Clone)]
pub struct SpannedError {
    pub error: VerificationError,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum VerificationError {
    StringConstantNotSupported,
    ArrayConstantNotSupported,
    IncrementDecrementNotSupported,
    HopNotFoundInFunction,
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
        }
    }
}
