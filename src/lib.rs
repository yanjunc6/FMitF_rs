#![allow(non_snake_case)]

pub mod ast;
pub mod cfg;
pub mod cli;
pub mod dataflow;
pub mod optimization;
pub mod pretty;
// pub mod runtime;
pub mod sc_graph;
pub mod verification;

// Re-export AST functionality
pub use ast::{parse_and_analyze, Program as AstProgram, Span as AstSpan};
pub use ast::{AstError, Results as AstResults, SpannedError as AstSpannedError};

// Re-export pretty printing
pub use pretty::{print_program, PrintMode, PrintOptions};

// Re-export CFG functionality
pub use cfg::CfgProgram; // This is the main CFG structure
pub use cfg::{
    BasicBlockId as CfgBasicBlockId, CfgBuilder, FieldId as CfgFieldId,
    FunctionId as CfgFunctionId, HopId as CfgHopId, TableId as CfgTableId, VarId as CfgVarId,
};

// Re-export optimization functionality
pub use optimization::CfgOptimizer;

// Re-export SC-Graph functionality
pub use sc_graph::{EdgeType as SCGraphEdgeType, SCGraph};

// Re-export verification functionality
pub use verification::{SpannedError as VerificationSpannedError, VerificationManager};
