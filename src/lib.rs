#![allow(non_snake_case)]

pub mod ast;
pub mod cfg;
pub mod cli;
pub mod dataflow;
pub mod optimization;
pub mod pretty;
pub mod sc_graph;
pub mod verification;
pub mod runtime;
// Re-export AST functionality
pub use ast::{parse_and_analyze, Program as AstProgram, Span as AstSpan};
pub use ast::{AstError, Results as AstResults, SpannedError as AstSpannedError};

// Re-export pretty printing
pub use pretty::ast_printer::{print_program, PrintMode, PrintOptions};
pub use pretty::cfg_printer::{print_cfg, CfgFormat, CfgPrintOptions};
pub use pretty::sc_graph_printer::{print_sc_graph, SCGraphFormat, SCGraphPrintOptions};

// Re-export CFG functionality
pub use cfg::CfgProgram; // This is the main CFG structure
pub use cfg::{
    BasicBlockId as CfgBasicBlockId, CfgBuilder, FieldId as CfgFieldId,
    FunctionId as CfgFunctionId, HopId as CfgHopId, NodeId as CfgNodeId, TableId as CfgTableId,
    VarId as CfgVarId,
};

// Re-export SC-Graph functionality
pub use sc_graph::{EdgeType as SCGraphEdgeType, SCGraph};

// Re-export verification (currently commented out)
// pub use verification::{AutoVerifier, VerificationError, VerificationOptions};
pub use optimization::CfgOptimizer;
