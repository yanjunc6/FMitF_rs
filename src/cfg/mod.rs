use id_arena::{Arena, Id};

pub use crate::ast::{BinaryOp, ReturnType, Span, TypeName, UnaryOp};

mod cfg_builder;
pub use cfg_builder::CfgBuilder;
pub mod cfg_api;

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
    // Arena for storing various components - package-private to allow cfg_builder access
    pub(crate) tables: Arena<TableInfo>,
    pub(crate) fields: Arena<FieldInfo>,
    pub(crate) functions: Arena<FunctionCfg>,
    pub(crate) variables: Arena<Variable>, // Unified: global constants, parameters, locals
    pub(crate) hops: Arena<HopCfg>,        // Centralized hop storage
    pub(crate) blocks: Arena<BasicBlock>,  // Centralized basic block storage

    // Root collections - package-private to allow cfg_builder access
    pub(crate) root_tables: Vec<TableId>,
    pub(crate) root_functions: Vec<FunctionId>, // Contains both partitions and transactions
    pub(crate) root_variables: Vec<VarId>,      // Global constants/variables
}

impl Default for CfgProgram {
    fn default() -> Self {
        Self {
            tables: Arena::new(),
            fields: Arena::new(),
            functions: Arena::new(),
            variables: Arena::new(),
            hops: Arena::new(),
            blocks: Arena::new(),
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
    pub(crate) name: String,
    pub(crate) function_type: FunctionType, // Whether this is a partition or transaction
    pub(crate) implementation: FunctionImplementation, // Whether this is abstract or concrete
    pub(crate) return_type: ReturnType,
    pub(crate) span: Span,

    pub(crate) parameters: Vec<VarId>, // Parameters (stored in program.variables)
    pub(crate) local_variables: Vec<VarId>, // Local variables (stored in program.variables)

    pub(crate) hops: Vec<HopId>, // References to hops in program.hops
    pub(crate) blocks: Vec<BasicBlockId>, // References to blocks in program.blocks

    pub(crate) entry_hop: Option<HopId>, // Set after all hops are allocated, None for abstract functions
    pub(crate) hop_order: Vec<HopId>,    // Empty for abstract functions
}

/// Hop - execution on a specific node
#[derive(Debug)]
pub struct HopCfg {
    pub(crate) entry_block: Option<BasicBlockId>, // Set after its basic block is created
    pub(crate) blocks: Vec<BasicBlockId>,
    pub(crate) span: Span,
}

/// Basic block with unified control flow representation
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub(crate) hop_id: HopId,
    pub(crate) statements: Vec<Statement>,
    pub(crate) span: Span,
    pub(crate) predecessors: Vec<ControlFlowEdge>, // Incoming edges
    pub(crate) successors: Vec<ControlFlowEdge>,   // Outgoing edges
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
        lvalue: LValue,
        rvalue: Rvalue,
        span: Span,
    },
}

/// Left-hand side values for assignments - unified representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LValue {
    /// Variable assignment: var = value
    Variable { var: VarId },
    /// Array element assignment: array[index] = value  
    ArrayElement { array: VarId, index: Operand },
    /// Table field assignment: table[pk_values].field = value
    TableField {
        table: TableId,
        pk_fields: Vec<FieldId>,
        pk_values: Vec<Operand>,
        field: FieldId,
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
