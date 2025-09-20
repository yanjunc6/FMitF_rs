//! cfg/mod.rs
//!
//! Defines the Control Flow Graph (CFG) Intermediate Representation.
//!
//! This version is updated to include explicit global constants, a more robust
//! representation for user-defined types, and the `HopExit` terminator. The design
//! adheres to an lvalue/rvalue instruction format and correctly models generic functions.

use id_arena::{Arena, Id};
use std::collections::HashMap;

// ============================================================================
// --- Core ID Types
// ============================================================================

pub type FunctionId = Id<Function>;
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
    pub basic_blocks: Arena<BasicBlock>,
    pub variables: Arena<Variable>,
    pub global_consts: Arena<GlobalConst>,
    pub types: Arena<Type>,
    pub user_defined_types: Arena<UserDefinedType>,
    pub tables: Arena<Table>,
    pub table_fields: Arena<TableField>,
    pub generic_params: Arena<GenericParam>,

    // --- Name-to-ID Mappings for unique items ---
    pub functions_map: HashMap<String, FunctionId>,
    pub types_map: HashMap<String, UserDefinedTypeId>,
    pub tables_map: HashMap<String, TableId>,
    pub global_consts_map: HashMap<String, GlobalConstId>,

    // --- Root Collections ---
    pub all_tables: Vec<TableId>,
    pub all_transactions: Vec<FunctionId>,
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
        return_type: TypeId,
    },
    List(TypeId),
    /// An instantiation of a user-defined type (e.g., a struct).
    Declared {
        type_id: UserDefinedTypeId,
        args: Vec<TypeId>,
    },
    Table(TableId),
    Row { table_id: TableId },
    GenericParam(GenericParamId),
    Void,
}

/// Represents the declaration of a user-defined type, like a struct.
#[derive(Debug, Clone)]
pub struct UserDefinedType {
    pub name: String,
    pub generic_params: Vec<GenericParamId>,
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
    pub entry_block: Option<BasicBlockId>,
    pub blocks: Vec<BasicBlockId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    Function,
    Transaction,
    Partition,
    Lambda,
    Invariant,
    Builtin,
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
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
}

// ============================================================================
// --- Basic Blocks, Instructions, and Terminators
// ============================================================================

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub function_id: FunctionId,
    pub predecessors: Vec<BasicBlockId>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Assign { dest: VariableId, src: Operand },
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
    Assert { condition: Operand, message: String },
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
    HopExit { next_block: BasicBlockId },
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Int(i64),
    Float(ordered_float::OrderedFloat<f64>),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    AddInt, SubInt, MulInt, DivInt, ModInt,
    AddFloat, SubFloat, MulFloat, DivFloat,
    EqInt, NeqInt, LtInt, LeqInt, GtInt, GeqInt,
    EqFloat, NeqFloat, LtFloat, LeqFloat, GtFloat, GeqFloat,
    And, Or,
    Eq, Neq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub primary_key_fields: Vec<FieldId>,
    pub other_fields: Vec<FieldId>,
    pub node_partition: FunctionId,
    pub invariants: Vec<FunctionId>,
}

#[derive(Debug, Clone)]
pub struct TableField {
    pub name: String,
    pub field_type: TypeId,
    pub table_id: TableId,
}
