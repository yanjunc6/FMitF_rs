use std::fmt;
mod result;

pub use result::{parse_boogie_result, BoogieResult};#[derive(Debug, Clone)]
pub struct BoogieProgram {
    pub declarations: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Var(VarDecl),
    Procedure(ProcedureDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Int,
    Real,
    Bool,
    Str,
    Map(Box<Ty>, Box<Ty>),
}

impl Ty {
    pub fn int() -> Self  { Ty::Int  }
    pub fn real() -> Self { Ty::Real }
    pub fn bool() -> Self { Ty::Bool }
    pub fn str() -> Self  { Ty::Str  }
    pub fn map(k: Ty, v: Ty) -> Self { Ty::Map(Box::new(k), Box::new(v)) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Ident,
    pub ty:   Ty,
}

#[derive(Debug, Clone)]
pub struct ProcedureDecl {
    pub name: Ident,
    pub params: Vec<VarDecl>,
    pub locals: Vec<VarDecl>,
    pub modifies: Vec<Ident>,
    pub body:   Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label: Label,
    pub stmts: Vec<Stmt>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(pub String);

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(Expr, Expr),
    Havoc(Expr),
    Assert(Expr),
    Assume(Expr),
    Comment(String),
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(Vec<Label>),
    If { cond: Expr, then_lbl: Label, else_lbl: Label },
    Return,
    Abort,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Ident),
    Const(Constant),
    Unary { op: UnOp, e: Box<Expr> },
    Binary { op: BinOp, l: Box<Expr>, r: Box<Expr> },
    MapSelect { map: Box<Expr>, index: Box<Expr> },
    MapStore  { map: Box<Expr>, index: Box<Expr>, val: Box<Expr> },
    Forall { binders: Vec<(Ident, Ty)>, body: Box<Expr> },
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    Real(f64),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp  { Not, Neg }

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add, Sub, Mul, Div,
    Lt, Lte, Gt, Gte,
    Eq, Neq,
    And, Or,
}

impl fmt::Display for BoogieProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for decl in &self.declarations {
            writeln!(f, "{}", decl)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decl::Var(v) => write!(f, "var {}: {};", v.name.0, v.ty),
            Decl::Procedure(p) => write!(f, "{}", p),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::Real => write!(f, "real"),
            Ty::Bool => write!(f, "bool"),
            Ty::Str => write!(f, "string"),
            Ty::Map(k, v) => write!(f, "[{}]{}", k, v),
        }
    }
}

impl fmt::Display for ProcedureDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "procedure {}(", self.name.0)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", param.name.0, param.ty)?;
        }
        write!(f, ")")?;
        
        if !self.modifies.is_empty() {
            write!(f, "\n  modifies ")?;
            for (i, var) in self.modifies.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{}", var.0)?;
            }
            write!(f, ";")?;
        }
        
        for local in &self.locals {
            writeln!(f, "\n  var {}: {};", local.name.0, local.ty)?;
        }
        
        writeln!(f, "\n{{")?;
        for block in &self.body {
            writeln!(f, "{}", block)?;
        }
        writeln!(f, "}}")?;
        
        Ok(())
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.label.0)?;
        for stmt in &self.stmts {
            writeln!(f, "  {}", stmt)?;
        }
        writeln!(f, "  {}", self.terminator)?;
        Ok(())
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Assign(lhs, rhs) => write!(f, "{} := {};", lhs, rhs),
            Stmt::Havoc(e) => write!(f, "havoc {};", e),
            Stmt::Assert(e) => write!(f, "assert {};", e),
            Stmt::Assume(e) => write!(f, "assume {};", e),
            Stmt::Comment(c) => write!(f, "// {}", c),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Goto(labels) => {
                write!(f, "goto ")?;
                for (i, label) in labels.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", label.0)?;
                }
                write!(f, ";")
            }
            Terminator::If { cond, then_lbl, else_lbl } => {
                write!(f, "if ({}) goto {}; else goto {};", cond, then_lbl.0, else_lbl.0)
            }
            Terminator::Return => write!(f, "return;"),
            Terminator::Abort => write!(f, "abort;"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(ident) => write!(f, "{}", ident.0),
            Expr::Const(c) => write!(f, "{}", c),
            Expr::Unary { op, e } => write!(f, "({} {})", op, e),
            Expr::Binary { op, l, r } => write!(f, "({} {} {})", l, op, r),
            Expr::MapSelect { map, index } => write!(f, "{}[{}]", map, index),
            Expr::MapStore { map, index, val } => write!(f, "{}[{} := {}]", map, index, val),
            Expr::Forall { binders, body } => {
                write!(f, "(forall ")?;
                for (i, (ident, ty)) in binders.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", ident.0, ty)?;
                }
                write!(f, " :: {})", body)
            }
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Real(r) => write!(f, "{}", r),
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Not => write!(f, "!"),
            UnOp::Neg => write!(f, "-"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Lte => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Gte => write!(f, ">="),
            BinOp::Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
        }
    }
}

