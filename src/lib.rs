pub mod ast;
pub mod cfg;
pub mod graph;
pub mod pretty;
pub mod verification;

// Re-export AST functionality
pub use ast::ast_builder::{build_program_from_pair};
pub use ast::semantics_analysis::analyze_program;

// Re-export pretty printing
pub use pretty::ast_printer::{print_program, PrintOptions, PrintMode};
pub use pretty::cfg_printer::{print_cfg, CfgPrintOptions, CfgFormat};

// Re-export CFG functionality with prefixed names to avoid conflicts
pub use cfg::cfg_builder::{CfgCtx, CfgBuilder};
pub use cfg::{NodeId as CfgNodeId, TableId as CfgTableId, FieldId as CfgFieldId,
              FunctionId as CfgFunctionId, HopId as CfgHopId, BasicBlockId, VariableId};

// Re-export graph functionality
// pub use graph::{SCGraph, EdgeType};

// Re-export verification (when enabled)
// pub use verification::{AutoVerifier, VerificationUnit, build_verification_unit, generate_verification_boogie_code};
