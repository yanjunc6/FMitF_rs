use crate::util::Span;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
mod Boogie_printer;
pub mod gen_Boogie;

#[derive(Debug, Clone)]
pub struct BoogieProgram {
    pub name: String, // program name, usually the input file name without extension
    pub global_vars: HashMap<String, BoogieVarDecl>, // global variables, var_name -> var_decl
    pub other_declarations: Vec<String>, // axiom, function, types; for global var, const: use global_vars
    pub global_string_literals: HashMap<String, BoogieVarDecl>, // string literal -> global string literals
    pub procedures: Vec<BoogieProcedure>,                       // procedures
}

#[derive(Debug, Clone)]
pub struct BoogieVarDecl {
    pub var_name: String,
    pub var_type: BoogieType,
    pub is_const: bool, // only global var can be const
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BoogieType {
    Int,
    Real,
    Bool,
    Map(Vec<Box<BoogieType>>, Box<BoogieType>), // :[type1]type2, or :[type1][type2]type3
    // A type constructor with zero or more type parameters, e.g.,
    //   Parametric { name: "List", args: [Int] }  ->  List int
    //   Parametric { name: "Pair", args: [Int, Bool] } -> Pair int bool
    Parametric { name: String, args: Vec<BoogieType> },
}

#[derive(Debug, Clone)]
pub struct BoogieProcedure {
    pub name: String,
    pub params: Vec<BoogieVarDecl>,
    pub local_vars: Vec<BoogieVarDecl>, // local variables declared at beginning of procedure
    pub modifies: Vec<String>,          // modified global variables
    pub lines: Vec<BoogieLine>,         // lines in the procedure
                                        // We never use returns for procedures for now
}

#[derive(Debug, Clone)]
pub enum BoogieLine {
    Comment(String),                  // // string
    Label(String),                    // label:
    Goto(String),                     // goto label;
    Assign(String, BoogieExpr),       // var := expr ;
    Assert(BoogieExpr, ErrorMessage), // assert {:msg "ErrorMessage"} expr;
    Assume(BoogieExpr),               // assume expr;
    Havoc(String),                    // havoc var;
    If {
        // only used for conditional jump in cfg
        cond: BoogieExpr,
        then_body: Vec<Box<BoogieLine>>,
        else_body: Vec<Box<BoogieLine>>,
    },
}

/// Node identifier for verification errors
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationNodeId {
    pub function_id: usize,
    pub instance: u32,
    pub hop_id: usize,
}

/// Boogie-specific verification errors that get embedded in Boogie assertions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BoogieError {
    PartitionFunctionInconsistency {
        partition_function_id: usize,
        function_id: usize,
        hop_id: usize,
        table_id: usize,
        span: Option<Span>,
    },
    SliceCommutativityViolation {
        node_1: VerificationNodeId,
        node_2: VerificationNodeId,
    },
    SpecialInterleavingNonEquivalence {
        node_1: VerificationNodeId,
        node_2: VerificationNodeId,
    },
    SpecialInterleavingTimeout {
        node_1: VerificationNodeId,
        node_2: VerificationNodeId,
    },
}
/// Attach an error message with structured error information to an assertion.
#[derive(Debug, Clone)]
pub struct ErrorMessage {
    pub boogie_error: BoogieError,
}

impl ErrorMessage {
    /// Serialize the error to S-expression format with proper quote escaping for Boogie
    pub fn to_boogie_string(&self) -> String {
        let sexpr = serde_lexpr::to_string(&self.boogie_error)
            .unwrap_or_else(|_| format!("{:?}", self.boogie_error));
        // Escape quotes for Boogie grammar: " becomes \"
        sexpr.replace('"', "\\\"")
    }
}

impl BoogieError {
    /// Parse BoogieError from escaped S-expression string
    pub fn from_boogie_string(escaped_sexpr: &str) -> Option<BoogieError> {
        // First, unescape the string: \" becomes "
        let unescaped = escaped_sexpr.replace("\\\"", "\"");

        // Try to parse as S-expression
        match serde_lexpr::from_str::<BoogieError>(&unescaped) {
            Ok(error) => Some(error),
            Err(_) => None,
        }
    }
}

/// A complete expression node.
#[derive(Debug, Clone)]
pub struct BoogieExpr {
    pub kind: BoogieExprKind,
}

/// ────────────────────────── Expression kinds ──────────────────────────
#[derive(Debug, Clone)]
pub enum BoogieExprKind {
    // Scalars
    Var(String),
    IntConst(i64),
    RealConst(f64),
    BoolConst(bool),

    // Function calls
    FunctionCall {
        name: String,
        args: Vec<BoogieExpr>,
    },

    // Arithmetic / Boolean
    BinOp(Box<BoogieExpr>, BoogieBinOp, Box<BoogieExpr>),
    UnOp(BoogieUnOp, Box<BoogieExpr>),

    // ───────────────  Map operations  ───────────────
    //
    // • Select  M[k1][k2]…   → MapSelect { base: M, indices: [k1,k2,…] }
    // • Store   M[k1,k2 := v] → MapStore  { base: M, indices: [k1,k2], value: v }
    //
    //   For nested maps you simply nest `MapStore` / `MapSelect`
    //   inside each other; nothing more is needed.
    MapSelect {
        base: Box<BoogieExpr>,    // the map being indexed
        indices: Vec<BoogieExpr>, // one or more keys
    },

    MapStore {
        base: Box<BoogieExpr>,    // the map to update
        indices: Vec<BoogieExpr>, // the key(s) to update
        value: Box<BoogieExpr>,   // the new element
    },

    Quantifier {
        kind: BoogieQuantifierKind,
        bound_vars: Vec<(String, BoogieType)>,
        body: Box<BoogieExpr>,
    },
}

/// Binary operators you already supported
#[derive(Debug, Clone)]
pub enum BoogieBinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge, // comparisons
    And,
    Or,
    Imp, // Boolean connectives
}

/// Optional: unary operators (negation, logical not, etc.)
#[derive(Debug, Clone)]
pub enum BoogieUnOp {
    Neg, // arithmetic  −e
    Not, // Boolean     !e
}

#[derive(Debug, Clone, Copy)]
pub enum BoogieQuantifierKind {
    Forall,
    Exists,
}
