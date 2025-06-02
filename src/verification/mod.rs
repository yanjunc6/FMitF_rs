use crate::cfg::{FunctionId, HopId, NodeId, TableId, VarId};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct VerificationUnit {
    pub conflict_node: NodeId,
    pub func1_id: FunctionId,
    pub hop1_id: HopId,
    pub func2_id: FunctionId,
    pub hop2_id: HopId,
    pub relevant_tables: Vec<TableId>,
    pub live_vars_at_hop1_exit: HashSet<VarId>,
    pub live_vars_at_hop2_exit: HashSet<VarId>,
}

mod verification_unit_builder;
pub use verification_unit_builder::VerificationUnitBuilder;

mod verification_logic;
pub use verification_logic::VerificationPlan;

mod boogie_codegen;
pub use boogie_codegen::BoogieCodeGenerator;

mod boogie;
pub use boogie::generate_verification_boogie_code;

mod auto_verifier;
pub use auto_verifier::{AutoVerifier, VerificationError, VerificationResults};
