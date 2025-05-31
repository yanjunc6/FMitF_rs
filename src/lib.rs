pub mod ast;
pub mod cfg;
pub mod graph;
pub mod pretty;
pub mod verification;

// Re-export AST functionality
pub use ast::{parse_and_analyze, Program as AstProgram, Span as AstSpan};
pub use ast::{AstError, Results as AstResults, SpannedError as AstSpannedError};

// Re-export pretty printing
pub use pretty::ast_printer::{print_program, PrintMode, PrintOptions};
pub use pretty::cfg_printer::{print_cfg, CfgFormat, CfgPrintOptions};

// Re-export CFG functionality
pub use cfg::CfgProgram;
pub use cfg::{
    BasicBlockId as CfgBasicBlockId, CfgBuilder, FieldId as CfgFieldId,
    FunctionId as CfgFunctionId, HopId as CfgHopId, NodeId as CfgNodeId, TableId as CfgTableId,
    VarId as CfgVarId,
};

// Re-export graph functionality (currently commented out)
// pub use graph::{SCGraph, EdgeType};

// Re-export verification (currently commented out)
// pub use verification::{AutoVerifier, VerificationUnit, build_verification_unit, generate_verification_boogie_code};
