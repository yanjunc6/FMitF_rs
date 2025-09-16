//! cfg/mod.rs
//!
//! Defines the Control Flow Graph (CFG) Intermediate Representation.
//!
//! The CFG is a low-level, explicit representation of the program, designed for
//! optimization and code generation. It is completely decoupled from the AST.
//!
//! Key Characteristics:
//! - **AST Independent**: No references to `ast` nodes or types.
//! - **Unique Naming**: All functions, tables, and user-defined types have unique names.
//!   Overloaded functions from the AST are resolved into unique variants (e.g., `foo#1`, `foo#2`).
//! - **Explicit Operations**: Arithmetic and logical operators are converted into explicit
//!   `BinaryOp` or `UnaryOp` instructions, with distinct versions for different types (e.g., `AddInt`, `AddFloat`).
//! - **Structured Control Flow**: Code is organized into `BasicBlock`s. Each block contains a
//!   linear sequence of `Instruction`s and ends with a `Terminator` that dictates all control flow.
//! - **Self-Contained Type System**: A comprehensive type system to describe all values,
//!   including structs, tables, and lists, without referencing the AST.
//! - **Hoisted Lambdas**: Lambda expressions are transformed into unique, named functions.

use id_arena::{Arena, Id};
use std::collections::HashMap;

// ============================================================================
// --- Core ID Types
// ============================================================================

pub type FunctionId = Id<Function>;
pub type BasicBlockId = Id<BasicBlock>;
pub type VariableId = Id<Variable>;
pub type TypeId = Id<Type>;
pub type TableId = Id<Table>;
pub type FieldId = Id<TableField>;

// ============================================================================
// --- Program Root
// ============================================================================

/// The root structure for the entire CFG.
/// It owns all the arenas for CFG components and provides maps for name-based lookups.
#[derive(Debug, Default)]
pub struct Program {
    // --- Arenas for all CFG nodes ---
    pub functions: Arena<Function>,
    pub basic_blocks: Arena<BasicBlock>,
    pub variables: Arena<Variable>,
    pub types: Arena<Type>,
    pub tables: Arena<Table>,
    pub table_fields: Arena<TableField>,

    // --- Name-to-ID Mappings for unique items ---
    /// Maps unique function names to their `FunctionId`.
    pub functions_map: HashMap<String, FunctionId>,
    /// Maps unique user-defined type names to their `TypeId`.
    pub types_map: HashMap<String, TypeId>,
    /// Maps unique table names to their `TableId`.
    pub tables_map: HashMap<String, TableId>,

    // --- Root Collections ---
    /// A collection of all defined tables, for easy iteration.
    pub all_tables: Vec<TableId>,
    /// A list of all functions marked as transactions, serving as program entry points.
    pub all_transactions: Vec<FunctionId>,
}

// ============================================================================
// --- Type System
// ============================================================================

/// Represents a resolved type in the CFG.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// A primitive type like `int`, `float`, or `bool`.
    Primitive(PrimitiveType),
    /// A function signature, detailing parameter and return types.
    Function {
        param_types: Vec<TypeId>,
        return_type: TypeId,
    },
    /// A list containing elements of a specific type.
    List(TypeId),
    /// A user-defined structure with type arguments.
    Declared { name: String, args: Vec<TypeId> },
    /// A type representing a the whole table.
    Table(TableId),
    /// A type representing a single row in a table.
    Row(TableId),
    /// The `void` type, representing the absence of a value.
    Void,
}

/// The set of primitive types supported by the CFG.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    String,
}

// ============================================================================
// --- Functions and Variables
// ============================================================================

/// Represents a callable unit in the CFG.
#[derive(Debug, Clone)]
pub struct Function {
    /// The unique name of the function (e.g., "my_func#1", "#lambda_1").
    pub name: String,
    /// The `TypeId` pointing to the function's signature (`Type::Function`).
    pub signature: TypeId,
    /// The kind of the function, distinguishing its origin and purpose.
    pub kind: FunctionKind,
    /// The parameters of the function.
    pub params: Vec<VariableId>,
    /// A list of boolean-returning functions representing preconditions (`assume`).
    pub assumptions: Vec<FunctionId>,
    /// The entry point to the function's control flow graph. `None` for abstract/builtin functions.
    pub entry_block: Option<BasicBlockId>,
    /// A list of all basic blocks that constitute this function's body.
    pub blocks: Vec<BasicBlockId>,
}

/// Distinguishes the origin and purpose of a function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    /// A standard, user-defined function.
    Function,
    /// A top-level transaction, serving as an entry point.
    Transaction,
    /// A function used to determine data placement for a table.
    Partition,
    /// A function generated from a lambda expression.
    Lambda,
    /// A function generated from an `assume` clause.
    Assumption,
    /// A function generated from a table `invariant`.
    Invariant,
    /// A built-in function provided by the language (e.g., for list operations, row get/set).
    Builtin,
}

/// Represents a variable in the CFG, which can be a parameter, a local, or a temporary.
#[derive(Debug, Clone)]
pub struct Variable {
    /// The `TypeId` of the value this variable holds.
    pub var_type: TypeId,
    /// The kind of the variable.
    pub kind: VariableKind,
    /// The name of the variable (e.g., "x", "_t1"). Guaranteed to be present.
    pub name: String,
}

/// The kind of a variable, indicating its scope and origin.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableKind {
    Parameter,
    Local,
    Temporary,
}

// ============================================================================
// --- Basic Blocks, Instructions, and Terminators
// ============================================================================

/// A sequence of instructions that executes linearly, ending with a single control-flow `Terminator`.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// The ID of the function this block belongs to.
    pub function_id: FunctionId,
    /// The list of basic blocks that can jump to this one.
    pub predecessors: Vec<BasicBlockId>,
    /// The list of non-control-flow instructions in this block.
    pub instructions: Vec<Instruction>,
    /// The control-flow instruction that ends this block.
    pub terminator: Terminator,
}

/// A single, non-control-flow operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// A binary operation (e.g., `a + b`).
    BinaryOp {
        dest: VariableId,
        op: BinaryOp,
        left: Operand,
        right: Operand,
    },
    /// A unary operation (e.g., `-x`).
    UnaryOp {
        dest: VariableId,
        op: UnaryOp,
        operand: Operand,
    },
    /// A function call. All non-primitive operations are handled by this.
    Call {
        dest: Option<VariableId>,
        func: FunctionId,
        args: Vec<Operand>,
    },
    /// Reads a field from a table row identified by its primary key(s).
    TableGet {
        dest: VariableId,
        table: TableId,
        keys: Vec<Operand>,     // ordered by primary key fields
        field: Option<FieldId>, // None means get the whole row
    },
    /// Writes to a field in a table row identified by its primary key(s).
    TableSet {
        table: TableId,
        keys: Vec<Operand>,     // ordered by primary key fields
        field: Option<FieldId>, // None means set the whole row
        value: Operand,
    },
    /// Halts execution if the condition is false.
    Assert { condition: Operand, message: String },
}

/// A control-flow instruction that terminates a `BasicBlock`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    /// Unconditionally jump to another basic block.
    Jump(BasicBlockId),
    /// Conditionally branch to one of two basic blocks based on an operand.
    Branch {
        condition: Operand,
        if_true: BasicBlockId,
        if_false: BasicBlockId,
    },
    /// Return from the current function, optionally with a value.
    Return(Option<Operand>),
    /// Exit the current execution "hop" and jump to the entry block of the next one.
    HopExit { next_block: BasicBlockId },
    /// Abort the entire transaction.
    Abort,
}

// ============================================================================
// --- Operations, Operands, and Constants
// ============================================================================

/// An operand to an instruction, which can be a variable or a compile-time constant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Variable(VariableId),
    Constant(ConstantValue),
}

/// A compile-time constant value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Int(i64),
    Float(ordered_float::OrderedFloat<f64>),
    Bool(bool),
    String(String),
    Void,
}

/// Explicit binary operators, distinguishing between integer and float versions.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    EqFloat,
    Eq,
    NeqInt,
    NeqFloat,
    Neq,
    LtInt,
    LeqInt,
    GtInt,
    GeqInt,
    LtFloat,
    LeqFloat,
    GtFloat,
    GeqFloat,
    And,
    Or,
}

/// Explicit unary operators.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    NegInt,
    NegFloat,
    NotBool,
}

// ============================================================================
// --- Tables and Fields
// ============================================================================

/// Represents a table definition in the CFG.
#[derive(Debug, Clone)]
pub struct Table {
    /// The unique name of the table.
    pub name: String,
    /// An ordered list of fields that constitute the primary key.
    pub primary_key_fields: Vec<FieldId>,
    /// A list of all non-primary-key fields in the table.
    pub other_fields: Vec<FieldId>,
    /// A function that determines the physical placement of a row.
    pub node_partition: FunctionId,
    /// A list of boolean-returning functions representing data integrity constraints.
    pub invariants: Vec<FunctionId>,
}

/// Represents a single field (column) within a table.
#[derive(Debug, Clone)]
pub struct TableField {
    pub name: String,
    /// The `TypeId` of the data stored in this field.
    pub field_type: TypeId,
    /// The `TableId` of the table this field belongs to.
    pub table_id: TableId,
}
