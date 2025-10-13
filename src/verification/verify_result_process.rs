use crate::cfg::Program as CfgProgram;
use crate::sc_graph::{update_scgraph_for_commutative_errors, SCGraph};
use crate::util::{CompilerError, Span};
use crate::verification::errors::VerificationErrorKind;
use crate::verification::Boogie::BoogieError;

/// Result processor for verification results
pub struct VerifyResultProcessor;

impl VerifyResultProcessor {
    /// Process Boogie verification errors to create user-friendly messages
    pub fn process_boogie_errors(
        cfg_program: &CfgProgram,
        scgraph: SCGraph,
        boogie_errors: Vec<BoogieError>,
    ) -> (Vec<CompilerError>, SCGraph) {
        let mut verification_errors: Vec<CompilerError> = Vec::new();
        let mut verified_not_commutative_pairs = Vec::new();

        for error in boogie_errors {
            match error {
                BoogieError::PartitionFunctionInconsistency {
                    function_id,
                    table_id,
                    partition_function_id,
                    hop_id,
                    span,
                } => {
                    // Get function name from CFG by finding the item at the given index
                    let function_name = cfg_program
                        .functions
                        .iter()
                        .find_map(|(id, f)| (id.index() == function_id).then(|| f.name.clone()))
                        .unwrap_or_else(|| format!("function_{}", function_id));

                    // Get table name from CFG by finding the item at the given index
                    let table_name = cfg_program
                        .tables
                        .iter()
                        .find_map(|(id, t)| (id.index() == table_id).then(|| t.name.clone()))
                        .unwrap_or_else(|| format!("table_{}", table_id));

                    // Get partition function name from CFG by finding the item at the given index
                    let partition_name = cfg_program
                        .functions
                        .iter()
                        .find_map(|(id, p)| {
                            (id.index() == partition_function_id).then(|| p.name.clone())
                        })
                        .unwrap_or_else(|| format!("partition_{}", partition_function_id));

                    let hop_index = hop_id;

                    let span = span.unwrap_or(Span::new(0, 0, "<boogie>"));
                    verification_errors.push(CompilerError::new(
                        VerificationErrorKind::PartitionFunctionInconsistency {
                            function_name,
                            table_name,
                            partition_function_name: partition_name,
                            hop_index,
                        },
                        span,
                    ));
                }
                BoogieError::SliceCommutativityViolation { node_1, node_2 } => {
                    verified_not_commutative_pairs.push((node_1, node_2));
                }
                BoogieError::SpecialInterleavingNonEquivalence { node_1, node_2 } => {
                    verified_not_commutative_pairs.push((node_1, node_2));
                }
                BoogieError::SpecialInterleavingTimeout { node_1, node_2 } => {
                    verified_not_commutative_pairs.push((node_1, node_2));
                }
            }
        }

        // Update SC graph by removing verified commutative pairs
        let simplified_scgraph =
            update_scgraph_for_commutative_errors(scgraph, verified_not_commutative_pairs);

        (verification_errors, simplified_scgraph)
    }
}
