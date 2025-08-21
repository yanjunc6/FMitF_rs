use crate::verification::Boogie::{
    BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine, BoogieProcedure, BoogieProgram,
    BoogieType, BoogieUnOp, BoogieVarDecl,
};
use std::fmt::{self, Display, Formatter};

// ──────────────────  Main Program Structure  ──────────────────
impl Display for BoogieProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Write other declarations (axioms, functions, types)
        for decl in &self.other_declarations {
            writeln!(f, "{}", decl)?;
        }

        // Write global variable declarations
        for var_decl in self.global_vars.values() {
            writeln!(f, "{}", var_decl)?;
        }

        // Write global string literal constants
        for var_decl in self.global_string_literals.values() {
            writeln!(f, "{}", var_decl)?;
        }

        // Write procedures
        for procedure in &self.procedures {
            writeln!(f, "{}", procedure)?;
        }

        Ok(())
    }
}

// ──────────────────  Variable Declarations  ──────────────────
impl Display for BoogieVarDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_const {
            write!(f, "const {} : {};", self.var_name, self.var_type)
        } else {
            write!(f, "var {} : {};", self.var_name, self.var_type)
        }
    }
}

// ──────────────────  Types  ──────────────────
impl Display for BoogieType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BoogieType::Int => write!(f, "int"),
            BoogieType::Real => write!(f, "real"),
            BoogieType::Bool => write!(f, "bool"),
            BoogieType::Map(domains, range) => {
                // Print each domain as “[dom]”
                for dom in domains {
                    if let BoogieType::Map(_, _) = **dom {
                        assert!(false, "Type must be simple for domain");
                    };
                    write!(f, "[{}]", dom)?; // `[int]`, `[real]`, …
                }
                // Finally print the range
                write!(f, "{}", range)
            }
            BoogieType::UserDefined(name) => write!(f, "{}", name),
        }
    }
}

// ──────────────────  Procedures  ──────────────────
impl Display for BoogieProcedure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Procedure header
        write!(f, "procedure {}", self.name)?;

        // Parameters
        if !self.params.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}: {}", param.var_name, param.var_type)?;
            }
            write!(f, ")")?;
        } else {
            write!(f, "()")?;
        }
        writeln!(f)?;

        // Modifies clause
        if !self.modifies.is_empty() {
            write!(f, "modifies ")?;
            for (i, var) in self.modifies.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", var)?;
            }
            writeln!(f, ";")?;
        }

        // Procedure body
        writeln!(f, "{{")?;
        for line in &self.lines {
            write!(f, "{}", line)?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}

// ──────────────────  Statements/Lines  ──────────────────
impl Display for BoogieLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BoogieLine::Comment(text) => writeln!(f, "  // {}", text),
            BoogieLine::Label(name) => writeln!(f, "  {}:", name),
            BoogieLine::Goto(tgt) => writeln!(f, "    goto {};", tgt),
            BoogieLine::Assign(lhs, rhs) => writeln!(f, "    {} := {};", lhs, rhs),
            BoogieLine::Assert(e, msg) => writeln!(f, "    assert {{:msg \"{}\"}} {};", msg.msg, e),
            BoogieLine::Assume(e) => writeln!(f, "    assume {};", e),
            BoogieLine::If {
                cond,
                then_body,
                else_body,
            } => {
                writeln!(f, "    if ({}) {{", cond)?;
                for line in then_body {
                    write!(f, "  {}", line)?;
                }
                if !else_body.is_empty() {
                    writeln!(f, "    }} else {{")?;
                    for line in else_body {
                        write!(f, "  {}", line)?;
                    }
                }
                writeln!(f, "    }}")?;
                Ok(())
            }
        }
    }
}

// ──────────────────  Expressions  ──────────────────
impl Display for BoogieExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Display for BoogieExprKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // ───── scalars ─────
            BoogieExprKind::Var(name) => write!(f, "{}", name),
            BoogieExprKind::IntConst(n) => write!(f, "{}", n),
            BoogieExprKind::RealConst(r) => write!(f, "{}", r),
            BoogieExprKind::BoolConst(b) => write!(f, "{}", b),

            // ───── function call ─────
            BoogieExprKind::FunctionCall { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }

            // ───── unary ─────
            BoogieExprKind::UnOp(op, e) => write!(f, "{}{}", op, wrap(e)),

            // ───── binary ─────
            BoogieExprKind::BinOp(lhs, op, rhs) => write!(f, "{} {} {}", wrap(lhs), op, wrap(rhs)),

            // ───── map select ─────
            BoogieExprKind::MapSelect { base, indices } => {
                write!(
                    f,
                    "{}[{}]",
                    base,
                    indices
                        .iter()
                        .map(|e| format!("{}", e))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }

            // ───── map store ─────
            BoogieExprKind::MapStore {
                base,
                indices,
                value,
            } => {
                write!(
                    f,
                    "{}[{} := {}]",
                    base,
                    indices
                        .iter()
                        .map(|e| format!("{}", e))
                        .collect::<Vec<_>>()
                        .join(", "),
                    value
                )
            }
        }
    }
}

// Helpers ---------------------------------------------------------------

/// Parenthesise every expression that is *not* an atomic variable/constant,
/// to avoid precedence headaches.  A tiny “good enough” heuristic.
fn wrap(e: &BoogieExpr) -> String {
    match e.kind {
        BoogieExprKind::Var(_)
        | BoogieExprKind::IntConst(_)
        | BoogieExprKind::RealConst(_)
        | BoogieExprKind::BoolConst(_)
        | BoogieExprKind::MapSelect { .. } => format!("{}", e), // safe to emit bare
        _ => format!("({})", e), // wrap everything else
    }
}

/// Pretty print binary and unary operator tokens
impl Display for BoogieBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BoogieBinOp::*;
        write!(
            f,
            "{}",
            match self {
                Add => "+",
                Sub => "-",
                Mul => "*",
                Div => "/",
                Eq => "==",
                Ne => "!=",
                Lt => "<",
                Le => "<=",
                Gt => ">",
                Ge => ">=",
                And => "&&",
                Or => "||",
                Imp => "==>",
            }
        )
    }
}
impl Display for BoogieUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BoogieUnOp::*;
        write!(
            f,
            "{}",
            match self {
                Neg => "-",
                Not => "!",
            }
        )
    }
}
