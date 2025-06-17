use id_arena::{Arena, Id};

pub use crate::ast::{BinaryOp, ReturnType, Span, TypeName, UnaryOp};

mod cfg_builder;
pub use cfg_builder::CfgBuilder;

// Core ID types
pub type NodeId = Id<NodeInfo>;
pub type TableId = Id<TableInfo>;
pub type FieldId = Id<FieldInfo>;
pub type FunctionId = Id<FunctionCfg>;
pub type HopId = Id<HopCfg>;
pub type BasicBlockId = Id<BasicBlock>;
pub type VarId = Id<Variable>;

/// Core CFG Program structure, never clone this structure
#[derive(Debug)]
pub struct CfgProgram {
    // Arena for storing various components
    pub nodes: Arena<NodeInfo>,
    pub tables: Arena<TableInfo>,
    pub fields: Arena<FieldInfo>,
    pub functions: Arena<FunctionCfg>,

    // Root collections - public for iteration
    pub root_nodes: Vec<NodeId>,
    pub root_tables: Vec<TableId>,
    pub root_functions: Vec<FunctionId>,
}

#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub name: String,
    pub tables: Vec<TableId>,
}

#[derive(Debug, Clone)]
pub struct TableInfo {
    pub name: String,
    pub node_id: NodeId,
    pub fields: Vec<FieldId>,
    pub primary_keys: Vec<FieldId>,  // Changed from single primary_key to multiple primary_keys
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub ty: TypeName,
    pub table_id: Option<TableId>, // Can be None during initial CFG construction, set later
    pub is_primary: bool,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: TypeName,
    pub is_parameter: bool,
}

/// Function CFG, never clone it
#[derive(Debug)]
pub struct FunctionCfg {
    pub name: String,
    pub return_type: ReturnType,
    pub span: Span,

    pub variables: Arena<Variable>,
    pub parameters: Vec<VarId>,

    pub hops: Arena<HopCfg>,
    pub blocks: Arena<BasicBlock>,

    pub entry_hop: Option<HopId>, // Set after all hops are allocated
    pub hop_order: Vec<HopId>,
}

/// Hop - execution on a specific node
#[derive(Debug)]
pub struct HopCfg {
    pub node_id: NodeId,
    pub entry_block: Option<BasicBlockId>, // Set after its basic block is created
    pub blocks: Vec<BasicBlockId>,
    pub span: Span,
}

/// Basic block
#[derive(Debug)]
pub struct BasicBlock {
    pub hop_id: HopId,
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign {
        var: VarId,
        rvalue: Rvalue,
        span: Span,
    },
    TableAssign {
        table: TableId,
        pk_field: FieldId,
        pk_value: Operand,
        field: FieldId,
        value: Operand,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Rvalue {
    Use(Operand),
    TableAccess {
        table: TableId,
        pk_field: FieldId,
        pk_value: Operand,
        field: FieldId,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Operand,
    },
    BinaryOp {
        op: BinaryOp,
        left: Operand,
        right: Operand,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Var(VarId),
    Const(Constant),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Int(i64),
    Float(ordered_float::OrderedFloat<f64>),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Terminator {
    Goto(BasicBlockId),
    Branch {
        condition: Operand,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
    },
    Return(Option<Operand>),
    Abort,
    HopExit {
        next_hop: Option<HopId>,
    },
}
