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
    Map(Vec<Box<BoogieType>>, Box<BoogieType>), //:[type1]type2, or :[type1][type2]type3, all the BoogieType here must be flattened simple types.
    UserDefined(String),
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

/// Attach an optional user-defined message to an assertion.
#[derive(Debug, Clone)]
pub struct ErrorMessage {
    pub msg: String, // We might want to parse this later, for now it's just a string
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
