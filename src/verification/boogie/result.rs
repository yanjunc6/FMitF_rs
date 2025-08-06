use super::super::VerificationResult;

pub struct BoogieResult {
    /// Whether the verification was successful.
    pub successful: bool,

    /// The message from boogie
    pub message: String,
}

pub fn parse_boogie_result(
    bresult: BoogieResult,
) -> VerificationResult {
    VerificationResult {
        successful: bresult.successful,
        errors: Vec::new(), // TODO: Parse actual errors from Boogie output
    }
}