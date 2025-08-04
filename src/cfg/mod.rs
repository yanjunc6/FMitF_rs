use id_arena::{Arena, Id};

pub use crate::ast::{BinaryOp, ReturnType, Span, TypeName, UnaryOp};

mod cfg_builder;
pub use cfg_builder::CfgBuilder;

// Core ID types
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
    pub tables: Arena<TableInfo>,
    pub fields: Arena<FieldInfo>,
    pub functions: Arena<FunctionCfg>,
    pub variables: Arena<Variable>, // Unified: global constants, parameters, locals

    // Root collections - public for iteration
    pub root_tables: Vec<TableId>,
    pub root_functions: Vec<FunctionId>, // Contains both partitions and transactions
    pub root_variables: Vec<VarId>,      // Global constants/variables
}

impl Default for CfgProgram {
    fn default() -> Self {
        Self {
            tables: Arena::new(),
            fields: Arena::new(),
            functions: Arena::new(),
            variables: Arena::new(),
            root_tables: Vec::new(),
            root_functions: Vec::new(),
            root_variables: Vec::new(),
        }
    }
}

/// Function type distinguishing partitions from transactions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionType {
    Partition,   // Partition functions (always return int)
    Transaction, // Regular transaction functions
}

/// Function implementation status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionImplementation {
    Abstract, // Abstract/virtual function (no implementation)
    Concrete, // Has actual implementation
}

#[derive(Debug, Clone)]
pub struct TableInfo {
    pub name: String,
    pub fields: Vec<FieldId>,
    pub primary_keys: Vec<FieldId>, // Changed from single primary_key to multiple primary_keys
    pub partition_function: FunctionId, // Partition function for this table
    pub partition_fields: Vec<FieldId>, // Fields used as partition parameters
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
    pub kind: VariableKind, // SSA: all variables are immutable once assigned
    pub value: Option<Constant>, // For constants with known values (including computed from expressions)
    pub span: Span,
}

/// In SSA form, all variables are immutable once assigned
/// This distinguishes different sources of variables
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableKind {
    Global,    // Global constant (from AST constants)
    Parameter, // Function parameter
    Local,     // Local variable (SSA: assigned exactly once)
    Temporary, // Temporary variable from expressions (SSA: assigned exactly once)
}

/// Function CFG, never clone it
#[derive(Debug)]
pub struct FunctionCfg {
    pub name: String,
    pub function_type: FunctionType, // Whether this is a partition or transaction
    pub implementation: FunctionImplementation, // Whether this is abstract or concrete
    pub return_type: ReturnType,
    pub span: Span,

    pub parameters: Vec<VarId>, // Parameters (stored in program.variables)
    pub local_variables: Vec<VarId>, // Local variables (stored in program.variables)

    pub hops: Arena<HopCfg>,
    pub blocks: Arena<BasicBlock>,

    pub entry_hop: Option<HopId>, // Set after all hops are allocated, None for abstract functions
    pub hop_order: Vec<HopId>,    // Empty for abstract functions
}

/// Hop - execution on a specific node
#[derive(Debug)]
pub struct HopCfg {
    pub entry_block: Option<BasicBlockId>, // Set after its basic block is created
    pub blocks: Vec<BasicBlockId>,
    pub span: Span,
}

/// Basic block with unified control flow representation
#[derive(Debug)]
pub struct BasicBlock {
    pub hop_id: HopId,
    pub statements: Vec<Statement>,
    pub span: Span,
    pub predecessors: Vec<ControlFlowEdge>, // Incoming edges
    pub successors: Vec<ControlFlowEdge>,   // Outgoing edges
}

/// Represents a control flow edge between basic blocks
#[derive(Debug, Clone)]
pub struct ControlFlowEdge {
    pub from: BasicBlockId,
    pub to: BasicBlockId,
    pub edge_type: EdgeType,
}

/// Type of control flow edge, unified representation for all control flow
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdgeType {
    /// Unconditional jump (goto, hop exit to next hop)
    Unconditional,
    /// Conditional jump taken when condition is true
    ConditionalTrue { condition: Operand },
    /// Conditional jump taken when condition is false  
    ConditionalFalse { condition: Operand },
    /// Return from function with optional value
    Return { value: Option<Operand> },
    /// Abort execution (no successor)
    Abort,
    /// Exit current hop and jump to next hop
    HopExit { next_hop: Option<HopId> },
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
        pk_fields: Vec<FieldId>,
        pk_values: Vec<Operand>,
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
        pk_fields: Vec<FieldId>,
        pk_values: Vec<Operand>,
        field: FieldId,
    },
    ArrayAccess {
        array: Operand,
        index: Operand,
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
    Array(Vec<Constant>), // Support for array literals
}
