//! cfg/mod.rs
//!
//! Defines the Control Flow Graph (CFG) Intermediate Representation.
//!
//! This version is updated to include explicit global constants, a more robust
//! representation for user-defined types, and the `HopExit` terminator. The design
//! adheres to an lvalue/rvalue instruction format and correctly models generic functions.

use crate::util::Span;
use id_arena::{Arena, Id};
use std::collections::HashMap;

pub mod cfg_api;
pub mod cfg_builder;

// ============================================================================
// --- Core ID Types
// ============================================================================

pub type FunctionId = Id<Function>;
pub type HopId = Id<Hop>;
pub type BasicBlockId = Id<BasicBlock>;
pub type VariableId = Id<Variable>;
pub type GlobalConstId = Id<GlobalConst>;
pub type TypeId = Id<Type>;
pub type UserDefinedTypeId = Id<UserDefinedType>;
pub type TableId = Id<Table>;
pub type FieldId = Id<TableField>;
pub type GenericParamId = Id<GenericParam>;

// ============================================================================
// --- Program Root
// ============================================================================

/// The root structure for the entire CFG. It owns all arenas for CFG components.
#[derive(Debug, Default)]
pub struct Program {
    // --- Arenas for all CFG nodes ---
    pub functions: Arena<Function>,
    pub hops: Arena<Hop>,
    pub basic_blocks: Arena<BasicBlock>,
    pub variables: Arena<Variable>,
    pub global_consts: Arena<GlobalConst>,
    pub types: Arena<Type>,
    pub user_defined_types: Arena<UserDefinedType>,
    pub tables: Arena<Table>,
    pub table_fields: Arena<TableField>,
    pub generic_params: Arena<GenericParam>,

    // --- Name-to-ID Mappings for unique items ---
    pub types_map: HashMap<String, UserDefinedTypeId>,
    pub tables_map: HashMap<String, TableId>,
    pub global_consts_map: HashMap<String, GlobalConstId>,

    // --- Root Collections ---
    pub all_tables: Vec<TableId>,
    pub all_transactions: Vec<FunctionId>,
    pub all_functions: Vec<FunctionId>,
}

// ============================================================================
// --- Global Constants
// ============================================================================

/// Represents a top-level, immutable constant value.
#[derive(Debug, Clone)]
pub struct GlobalConst {
    pub name: String,
    pub ty: TypeId,
    pub init: ConstantValue,
}

// ============================================================================
// --- Type System
// ============================================================================

/// Represents a polytype (e.g., `∀T. (T) -> T`). Used for generic function signatures.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeScheme {
    pub quantified_params: Vec<GenericParamId>,
    pub ty: TypeId,
}

/// Represents a resolved monotype in the CFG.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Primitive(PrimitiveType),
    Function {
        param_types: Vec<TypeId>,
        return_type: Box<TypeId>,
    },
    List(TypeId),
    /// An instantiation of a user-defined type (e.g., a struct).
    Declared {
        type_id: UserDefinedTypeId,
        args: Vec<TypeId>,
    },
    Table(TableId),
    Row {
        table_id: TableId,
    },
    GenericParam(GenericParamId),
    Void,
}

/// Represents the declaration of a user-defined type, like a struct.
#[derive(Debug, Clone)]
pub struct UserDefinedType {
    pub name: String,
    pub generic_params: Vec<GenericParamId>,
    pub decorators: Vec<Decorator>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    String,
}

// ============================================================================
// --- Functions, Variables, and Generics
// ============================================================================

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub signature: TypeScheme,
    pub kind: FunctionKind,
    pub params: Vec<VariableId>,
    pub assumptions: Vec<FunctionId>,
    pub entry_block: Option<BasicBlockId>,
    pub all_blocks: Vec<BasicBlockId>,
    pub decorators: Vec<Decorator>,

    // transaction only
    pub entry_hop: Option<HopId>,
    pub hops: Vec<HopId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    Function,
    Transaction,
    Partition,
    Lambda,
    Operator,
    Invariant,
    Assumption,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: TypeId,
    pub kind: VariableKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableKind {
    Parameter,
    Local,
    Temporary,
    Lambda(FunctionId), // Captured variable from enclosing scope
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
}

// ============================================================================
// --- Hops, Basic Blocks, Instructions, and Terminators
// ============================================================================
#[derive(Debug, Clone)]
pub struct Hop {
    pub function_id: FunctionId,
    pub entry_block: Option<BasicBlockId>,
    pub blocks: Vec<BasicBlockId>,
    pub decorators: Vec<Decorator>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub hop_id: HopId,
    pub function_id: FunctionId,
    pub predecessors: Vec<BasicBlockId>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionKind {
    Assign {
        dest: VariableId,
        src: Operand,
    },
    BinaryOp {
        dest: VariableId,
        op: BinaryOp,
        left: Operand,
        right: Operand,
    },
    UnaryOp {
        dest: VariableId,
        op: UnaryOp,
        operand: Operand,
    },
    Call {
        dest: Option<VariableId>,
        func: FunctionId,
        args: Vec<Operand>,
    },
    TableGet {
        dest: VariableId,
        table: TableId,
        keys: Vec<Operand>,
        field: Option<FieldId>,
    },
    TableSet {
        table: TableId,
        keys: Vec<Operand>,
        field: Option<FieldId>,
        value: Operand,
    },
    Assert {
        condition: Operand,
        message: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Jump(BasicBlockId),
    Branch {
        condition: Operand,
        if_true: BasicBlockId,
        if_false: BasicBlockId,
    },
    Return(Option<Operand>),
    /// Exit the current execution "hop" and jump to the entry block of the next one.
    HopExit {
        next_hop: HopId,
    },
    Abort,
}

// ============================================================================
// --- Operations, Operands, and Constants
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Variable(VariableId),
    Constant(ConstantValue),
    Global(GlobalConstId),
    Table(TableId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Int(i64),
    Float(ordered_float::OrderedFloat<f64>),
    Bool(bool),
    String(String),
}

impl ConstantValue {
    pub fn as_int(&self) -> Option<i64> {
        match self {
            ConstantValue::Int(i) => Some(*i),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    ModInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    EqInt,
    NeqInt,
    LtInt,
    LeqInt,
    GtInt,
    GeqInt,
    EqFloat,
    NeqFloat,
    LtFloat,
    LeqFloat,
    GtFloat,
    GeqFloat,
    And,
    Or,
    Eq,
    Neq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    NegInt,
    NegFloat,
    NotBool,
}

// ============================================================================
// --- Tables and Fields
// ============================================================================

#[derive(Debug, Clone)]
pub struct Table {
    pub name: String,
    pub schedule_key_fields: Vec<FieldId>, // subset of primary key fields, used for scheduling/partitioning
    pub primary_key_fields: Vec<FieldId>,
    pub other_fields: Vec<FieldId>,
    pub node_partition: Option<FunctionId>,
    pub node_partition_args: Vec<FieldId>,
    pub invariants: Vec<FunctionId>,
}

#[derive(Debug, Clone)]
pub struct TableField {
    pub name: String,
    pub field_type: TypeId,
    pub table_id: TableId,
}

// ============================================================================
// --- Others
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decorator {
    pub name: String,
}
