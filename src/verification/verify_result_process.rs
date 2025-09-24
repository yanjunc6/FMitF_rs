use crate::cfg::CfgProgram;
use crate::sc_graph::{update_scgraph_for_commutative_errors, SCGraph};
use crate::verification::errors::{SpannedError, VerificationError};
use crate::verification::Boogie::BoogieError;

/// Result processor for verification results
pub struct VerifyResultProcessor;

impl VerifyResultProcessor {
    /// Process Boogie verification errors to create user-friendly messages
    pub fn process_boogie_errors(
        cfg_program: &CfgProgram,
        scgraph: SCGraph,
        boogie_errors: Vec<BoogieError>,
    ) -> (Vec<SpannedError>, SCGraph) {
        let mut verification_errors = Vec::new();
        let mut verified_not_commutative_pairs = Vec::new();

        for error in boogie_errors {
            match error {
                BoogieError::PartitionFunctionInconsistency {
                    function_id,
                    table_id,
                    partition_function_id,
                    span,
                } => {
                    // Get function name from CFG by finding the item at the given index
                    let function_name = cfg_program
                        .functions
                        .iter()
                        .nth(function_id)
                        .map(|(_, f)| f.name.clone())
                        .unwrap_or_else(|| format!("function_{}", function_id));

                    // Get table name from CFG by finding the item at the given index
                    let table_name = cfg_program
                        .tables
                        .iter()
                        .nth(table_id)
                        .map(|(_, t)| t.name.clone())
                        .unwrap_or_else(|| format!("table_{}", table_id));

                    // Get partition function name from CFG by finding the item at the given index
                    let partition_name = cfg_program
                        .functions
                        .iter()
                        .nth(partition_function_id)
                        .map(|(_, p)| p.name.clone())
                        .unwrap_or_else(|| format!("partition_{}", partition_function_id));

                    verification_errors.push(SpannedError {
                        error: VerificationError::PartitionFunctionInconsistency {
                            function_name,
                            table_name,
                            partition_function_name: partition_name,
                        },
                        span,
                    });
                }
                BoogieError::SliceCommutativityViolation { hop_id_1, hop_id_2 } => {
                    verified_not_commutative_pairs.push((hop_id_1, hop_id_2));
                }
                BoogieError::SpecialInterleavingNonEquivalence { hop_id_1, hop_id_2 } => {
                    verified_not_commutative_pairs.push((hop_id_1, hop_id_2));
                }
            }
        }

        // Update SC graph by removing verified commutative pairs
        let simplified_scgraph =
            update_scgraph_for_commutative_errors(scgraph, verified_not_commutative_pairs);

        (verification_errors, simplified_scgraph)
    }
}
