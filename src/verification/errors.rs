#[derive(Debug, Clone)]
pub enum VerificationError {
    Timeout,
    PartitionSoundnessViolation {
        function_name: String,
        hop_id: usize,
        conflicting_keys: Vec<String>,
        message: String,
    },
    BoogieGenerationError {
        function_name: String,
        message: String,
    },
    BoogieExecutionError {
        message: String,
    },
}