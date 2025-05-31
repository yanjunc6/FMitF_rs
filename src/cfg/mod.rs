use id_arena::{Arena, Id};
use std::collections::HashMap;

pub mod cfg_builder;

// Re-export types from AST that we keep
pub use crate::ast::{TypeName, ReturnType, UnaryOp, BinaryOp, Span, NodeDef, 
                     TableDeclaration, FieldDeclaration};

// Arena-based IDs
pub type NodeId = Id<NodeDef>;
pub type TableId = Id<Table>;
pub type FieldId = Id<FieldDeclaration>;
pub type FunctionId = Id<FunctionCfg>;
pub type HopId = Id<Hop>;
pub type BasicBlockId = Id<BasicBlock>;
pub type VariableId = Id<VariableDecl>; // Unified variable ID

/// A table declaration with its fields
#[derive(Debug, Clone)]
pub struct Table {
    pub name: String,
    pub node_id: NodeId,
    pub fields: Vec<FieldId>,
    pub primary_key: FieldId,
    pub span: Span,
}

/// Context containing all global definitions
#[derive(Debug)]
pub struct CfgCtx {
    pub nodes: Arena<NodeDef>,
    pub tables: Arena<Table>,
    pub fields: Arena<FieldDeclaration>,
    pub functions: Arena<FunctionCfg>,
    
    /// Node lookup by name
    pub node_map: HashMap<String, NodeId>,
    /// Table lookup by name
    pub table_map: HashMap<String, TableId>,
    /// Function lookup by name
    pub function_map: HashMap<String, FunctionId>,
}

/// A function's control flow graph
#[derive(Debug)]
pub struct FunctionCfg {
    /// Function metadata
    pub name: String,
    pub parameters: Vec<VariableId>, // Function parameters
    pub return_type: ReturnType,
    pub span: Span,
    
    pub hops: Arena<Hop>,
    pub blocks: Arena<BasicBlock>,
    /// Arena for ALL variables in this function (both function-scoped and hop-scoped)
    pub variables: Arena<VariableDecl>,
    
    /// Entry hop of the function
    pub entry_hop: Option<HopId>,
    
    /// Hop execution order
    pub hop_order: Vec<HopId>,
    
    /// Variable lookup by name within this function
    pub variable_map: HashMap<String, VariableId>,
}

/// Information about a hop - execution on a specific node
#[derive(Debug)]
pub struct Hop {
    /// Which node this hop executes on
    pub node_id: NodeId,
    
    /// Entry block for this hop
    pub entry_block: Option<BasicBlockId>,
    
    /// All blocks in this hop
    pub blocks: Vec<BasicBlockId>,
    
    /// Source span for debugging
    pub span: Span,
}

/// Basic block in the control flow graph
#[derive(Debug)]
pub struct BasicBlock {
    /// Statements in this block
    pub statements: Vec<Statement>,
    
    /// How this block terminates
    pub terminator: Option<Terminator>,
    
    /// Which hop this block belongs to
    pub hop_id: Option<HopId>,
    
    /// Successor blocks (computed from terminator)
    pub successors: Vec<BasicBlockId>,
    
    /// Predecessor blocks
    pub predecessors: Vec<BasicBlockId>,
    
    /// Source span for debugging
    pub span: Span,
}

/// Variable scope determines where the variable can be accessed
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableScope {
    /// Function-scoped: available across all hops in the function
    Function,
    /// Hop-scoped: only available within the specific hop
    Hop(HopId),
}

/// Unified variable declaration
#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub name: String,
    pub ty: TypeName,
    pub scope: VariableScope,
    pub is_param: bool, // Whether this is a function parameter
    pub span: Span,
}

/// Statements in basic blocks
#[derive(Debug, Clone)]
pub enum Statement {
    /// Variable assignment: variable = rvalue
    Assign {
        variable: VariableId,
        rvalue: Rvalue,
        span: Span,
    },
    
    /// Table field assignment
    TableAssign {
        table_id: TableId,
        primary_key: Operand,
        field_id: FieldId,
        value: Operand,
        span: Span,
    },
    
    /// Variable declaration with initialization
    DeclareVariable {
        variable: VariableId,
        init: Rvalue,
        span: Span,
    },
}

/// Right-hand side of assignments
#[derive(Debug, Clone)]
pub enum Rvalue {
    /// Simple operand use
    Use(Operand),
    
    /// Table field access
    TableAccess {
        table_id: TableId,
        primary_key: Box<Operand>,
        field_id: FieldId,
    },
    
    /// Unary operation
    UnaryOp {
        op: UnaryOp,
        operand: Box<Operand>,
    },
    
    /// Binary operation
    BinaryOp {
        op: BinaryOp,
        left: Box<Operand>,
        right: Box<Operand>,
    },
}

/// Operands (values that can be used directly)
#[derive(Debug, Clone)]
pub enum Operand {
    /// Variable reference (scope is determined by the variable's declaration)
    Variable(VariableId),
    
    /// Constant value
    Constant(Constant),
}

/// Constants
#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

/// Block terminators
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Jump to another block within same hop
    Goto(BasicBlockId),
    
    /// Conditional branch within same hop
    Branch {
        condition: Operand,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
    },
    
    /// Return from function
    Return(Option<Operand>),
    
    /// Abort execution
    Abort,
    
    /// Hop transition - control flow moving to a different node
    /// Only function-scoped variables can be accessed across hops
    HopTransition {
        next_hop: HopId,
        target_block: BasicBlockId,
    },
    
    /// Function exit (implicit return for void functions)
    FunctionExit,
}

/// Loop context for break/continue resolution during CFG construction
#[derive(Debug, Clone)]
pub struct LoopContext {
    /// Where 'continue' jumps to (loop header)
    pub continue_target: BasicBlockId,
    
    /// Where 'break' jumps to (loop exit)  
    pub break_target: BasicBlockId,
    
    /// The hop this loop is in
    pub hop_id: HopId,
}

/// Analysis data that can be computed on the CFG
#[derive(Debug, Default)]
pub struct AnalysisData {
    /// Dominator tree - maps each block to its immediate dominator
    pub dominators: HashMap<BasicBlockId, BasicBlockId>,
    
    /// Reaching definitions per block
    pub reaching_defs: HashMap<BasicBlockId, HashMap<VariableId, BasicBlockId>>,
    
    /// Node-to-node data flow for distributed analysis
    pub cross_node_flow: HashMap<(NodeId, NodeId), Vec<TableId>>,
    
    /// Which tables are accessed in each hop
    pub hop_table_access: HashMap<HopId, Vec<TableId>>,
    
    /// Which function-scoped variables are live at hop boundaries
    pub hop_live_variables: HashMap<HopId, Vec<VariableId>>,
}
