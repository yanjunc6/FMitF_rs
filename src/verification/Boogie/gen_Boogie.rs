use super::{
    BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieProcedure, BoogieProgram, BoogieType,
    BoogieUnOp, BoogieVarDecl,
};
use crate::cfg::{self, BinaryOp, ConstantValue, Operand, Program as CfgProgram, UnaryOp};
use crate::verification::errors::Results;
use std::collections::HashMap;
/// Helper functions for generating Boogie programs from CFG programs
/// Focus on building blocks for Boogie generation with prefix support
#[derive(Debug, Clone)]
pub struct BoogieProgramGenerator {
    pub program: BoogieProgram,
    pub current_procedure_index: Option<usize>, // index of current procedure being worked on
}

impl BoogieProgramGenerator {
    /// Create a new generator from a CFG program
    pub fn from_cfg(name: String, cfg_program: &CfgProgram) -> Self {
        let mut generator = BoogieProgramGenerator {
            program: BoogieProgram {
                name,
                global_vars: HashMap::new(),
                other_declarations: Vec::new(),
                global_string_literals: HashMap::new(),
                procedures: Vec::new(),
            },
            current_procedure_index: None,
        };
        generator.gen_string_axioms();
        generator.gen_list_axioms();
        generator.gen_global_constants(cfg_program);
        generator.gen_table_variables(cfg_program);
        generator
    }

    /// Set the current procedure being worked on by index
    pub fn set_current_procedure(&mut self, procedure_index: usize) {
        self.current_procedure_index = Some(procedure_index);
    }

    /// Clear the current procedure reference
    pub fn clear_current_procedure(&mut self) {
        self.current_procedure_index = None;
    }

    /// Get a reference to the current procedure being worked on
    pub fn get_current_procedure(&self) -> Option<&BoogieProcedure> {
        self.current_procedure_index
            .and_then(|i| self.program.procedures.get(i))
    }

    /// Get a mutable reference to the current procedure being worked on
    pub fn get_current_procedure_mut(&mut self) -> Option<&mut BoogieProcedure> {
        self.current_procedure_index
            .and_then(move |i| self.program.procedures.get_mut(i))
    }

    /// Generate string axioms and add to other_declarations
    pub fn gen_string_axioms(&mut self) {
        let mut decls = Vec::new();
        // Type and operators
        decls.push("type String;".to_string());
        decls.push("const empty: String;".to_string());
        decls.push("function Concat(x: String, y: String): String;".to_string());
        // Generic to-string for any type T
        decls.push("function str<a>(x: a): String;".to_string());

        // Axioms (with correct triggers)
        decls.push(
            "axiom (forall s: String :: {Concat(empty, s)} Concat(empty, s) == s);".to_string(),
        );
        decls.push(
            "axiom (forall s: String :: {Concat(s, empty)} Concat(s, empty) == s);".to_string(),
        );
        // Injectivity of str<T> (for each instantiation of T)
        decls.push(
            "axiom (forall<a> x: a, y: a :: {str(x), str(y)} str(x) == str(y) ==> x == y);"
                .to_string(),
        );
        // str<T>(x) is never the empty string (uniform for all T)
        decls.push("axiom (forall<a> x: a :: {str(x)} str(x) != empty);".to_string());

        self.program.other_declarations.extend(decls);
    }

    /// Generate polymorphic list type and axioms similar to list.bpl
    pub fn gen_list_axioms(&mut self) {
        let mut decls = Vec::new();
        // Type
        decls.push("type List a;".to_string());
        // Constructors
        decls.push("function Cons<a>(x:a, t:List a) returns (List a);".to_string());
        decls.push("function Nil<a>(w:a) returns (List a);".to_string());
        // Ops
        decls.push("function length<a>(l:List a) returns (int);".to_string());
        decls.push("function get<a>(l:List a, i:int) returns (a);".to_string());
        // Axioms
        decls.push("axiom (forall<a> u:a, v:a :: Nil(u) == Nil(v));".to_string());
        decls.push("axiom (forall<a> w:a :: length(Nil(w)) == 0);".to_string());
        decls.push(
            "axiom (forall<a> x:a, t:List a :: length(Cons(x, t)) == 1 + length(t));".to_string(),
        );
        decls.push("axiom (forall<a> x:a, t:List a :: get(Cons(x, t), 0) == x);".to_string());
        decls.push(
            "axiom (forall<a> x:a, t:List a, i:int ::  0 < i && i < length(Cons(x, t)) ==> get(Cons(x, t), i) == get(t, i - 1));"
                .to_string(),
        );
        decls.push("axiom (forall<a> l:List a :: length(l) >= 0);".to_string());

        self.program.other_declarations.extend(decls);
    }

    /// Helper function to generate a global constant name for a string literal
    /// For string literal "abc", generates "L_abc"
    /// Uses $ prefix for unicode escapes to ensure uniqueness
    fn gen_string_literal_name(literal: &str) -> String {
        // Sanitize the string for use as identifier
        let sanitized: String = literal
            .chars()
            .map(|c| match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => c.to_string(),
                _ => format!("$u{:04X}", c as u32), // Use $uXXXX for unicode escapes
            })
            .collect();

        format!("L_{}", sanitized)
    }

    /// Add a string literal to the global string literals map
    pub fn add_string_literal(&mut self, literal: &str) -> String {
        if literal.is_empty() {
            return "empty".to_string();
        }

        let const_name = Self::gen_string_literal_name(literal);

        if !self
            .program
            .global_string_literals
            .contains_key(&const_name)
        {
            let var_decl = BoogieVarDecl {
                var_name: const_name.clone(),
                var_type: BoogieType::Parametric {
                    name: "String".to_string(),
                    args: vec![],
                },
                is_const: true,
            };
            self.program
                .global_string_literals
                .insert(const_name.clone(), var_decl);
        }

        const_name
    }

    /// Generate global constants from CFG program
    fn gen_global_constants(&mut self, cfg_program: &CfgProgram) {
        for (_, constant) in &cfg_program.global_consts {
            let var_name = constant.name.clone();
            let var_type = BoogieProgramGenerator::convert_type_id(cfg_program, &constant.ty);
            let decl = BoogieVarDecl {
                var_name: var_name.clone(),
                var_type,
                is_const: true,
            };
            self.program.global_vars.insert(var_name, decl);
        }
    }

    /// Generate global table variables from CFG program
    fn gen_table_variables(&mut self, cfg_program: &CfgProgram) {
        for (_, table) in &cfg_program.tables {
            for field_id in table
                .primary_key_fields
                .iter()
                .chain(table.other_fields.iter())
            {
                let field = &cfg_program.table_fields[*field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
                let var_type = BoogieProgramGenerator::gen_table_field_type(cfg_program, *field_id);
                let decl = BoogieVarDecl {
                    var_name: var_name.clone(),
                    var_type,
                    is_const: false,
                };
                self.program.global_vars.insert(var_name, decl);
            }
        }
    }

    pub fn convert_type_id(program: &CfgProgram, type_id: &cfg::TypeId) -> BoogieType {
        let ty = &program.types[*type_id];
        Self::convert_type(program, ty)
    }

    /// Convert CFG TypeName to BoogieType
    pub fn convert_type(program: &CfgProgram, ty: &cfg::Type) -> BoogieType {
        match ty {
            cfg::Type::Primitive(p) => match p {
                cfg::PrimitiveType::Int => BoogieType::Int,
                cfg::PrimitiveType::Float => BoogieType::Real,
                cfg::PrimitiveType::Bool => BoogieType::Bool,
                cfg::PrimitiveType::String => BoogieType::Parametric {
                    name: "String".to_string(),
                    args: vec![],
                },
            },
            cfg::Type::List(elem_ty) => {
                let boogie_base_ty = Self::convert_type_id(program, elem_ty);
                // Represent list as a parametric List type rather than a map
                BoogieType::Parametric {
                    name: "List".to_string(),
                    args: vec![boogie_base_ty],
                }
            }
            cfg::Type::Declared { type_id, .. } => {
                let udt = &program.user_defined_types[*type_id];
                BoogieType::Parametric {
                    name: udt.name.clone(),
                    args: vec![],
                }
            }
            cfg::Type::Table(table_id) => {
                let table = &program.tables[*table_id];
                BoogieType::Parametric {
                    name: table.name.clone(),
                    args: vec![],
                }
            }
            cfg::Type::Row { table_id } => {
                let table = &program.tables[*table_id];
                // Represent a row as a parametric Row<Table>
                BoogieType::Parametric {
                    name: "Row".to_string(),
                    args: vec![BoogieType::Parametric {
                        name: table.name.clone(),
                        args: vec![],
                    }],
                }
            }
            cfg::Type::GenericParam(p) => {
                let param = &program.generic_params[*p];
                BoogieType::Parametric {
                    name: param.name.clone(),
                    args: vec![],
                }
            }
            cfg::Type::Void => BoogieType::Parametric {
                name: "()".to_string(),
                args: vec![],
            },
            cfg::Type::Function { .. } => {
                // Represent function as an opaque parametric marker
                BoogieType::Parametric {
                    name: "function".to_string(),
                    args: vec![],
                }
            }
        }
    }

    /// Generate table field variable name
    pub fn gen_table_field_var_name(table_name: &str, field_name: &str) -> String {
        format!("{}_{}", table_name, field_name)
    }

    /// Generate Boogie type for a table field (used for both global variables and local snapshots)
    pub fn gen_table_field_type(
        cfg_program: &CfgProgram,
        field_id: crate::cfg::FieldId,
    ) -> BoogieType {
        let field = &cfg_program.table_fields[field_id];
        let table = &cfg_program.tables[field.table_id];
        let key_types: Vec<Box<BoogieType>> = table
            .primary_key_fields
            .iter()
            .map(|f_id| {
                let key_field = &cfg_program.table_fields[*f_id];
                Box::new(Self::convert_type_id(cfg_program, &key_field.field_type))
            })
            .collect();
        let value_type = Box::new(Self::convert_type_id(cfg_program, &field.field_type));
        BoogieType::Map(key_types, value_type)
    }

    /// Helper function to add a local variable to current procedure if it doesn't exist
    /// Returns true if variable was added, false if it already existed
    pub fn ensure_local_variable_exists(&mut self, var_name: &str, var_type: BoogieType) -> bool {
        if let Some(proc) = self.get_current_procedure_mut() {
            if !proc.local_vars.iter().any(|v| v.var_name == var_name)
                && !proc.params.iter().any(|v| v.var_name == var_name)
            {
                proc.local_vars.push(BoogieVarDecl {
                    var_name: var_name.to_string(),
                    var_type,
                    is_const: false,
                });
                return true;
            }
        }
        false
    }

    /// Convert CFG operand to Boogie expression.
    /// Note: This does not handle scoped names. The caller (BaseVerificationGenerator) should provide scoped names.
    pub fn convert_operand(
        &mut self,
        _cfg_program: &CfgProgram,
        operand: &Operand,
        // The name of the variable is passed in directly by the caller
        var_name: String,
    ) -> Results<BoogieExpr> {
        match operand {
            Operand::Variable(_) => Ok(BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            }),
            Operand::Constant(constant) => self.convert_constant(constant),
            Operand::Global(_) => Ok(BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            }),
        }
    }

    /// Convert CFG constant to Boogie expression
    pub fn convert_constant(&mut self, constant: &ConstantValue) -> Results<BoogieExpr> {
        match constant {
            ConstantValue::Int(i) => Ok(BoogieExpr {
                kind: BoogieExprKind::IntConst(*i),
            }),
            ConstantValue::Float(r) => Ok(BoogieExpr {
                kind: BoogieExprKind::RealConst(r.into_inner()),
            }),
            ConstantValue::Bool(b) => Ok(BoogieExpr {
                kind: BoogieExprKind::BoolConst(*b),
            }),
            ConstantValue::String(s) => {
                let const_name = self.add_string_literal(s);
                Ok(BoogieExpr {
                    kind: BoogieExprKind::Var(const_name),
                })
            }
        }
    }

    /// Convert CFG binary operation to Boogie binary operation
    pub fn convert_binary_op(op: &BinaryOp) -> BoogieBinOp {
        match op {
            BinaryOp::AddInt | BinaryOp::AddFloat => BoogieBinOp::Add,
            BinaryOp::SubInt | BinaryOp::SubFloat => BoogieBinOp::Sub,
            BinaryOp::MulInt | BinaryOp::MulFloat => BoogieBinOp::Mul,
            BinaryOp::DivInt | BinaryOp::DivFloat => BoogieBinOp::Div,
            BinaryOp::EqInt | BinaryOp::EqFloat | BinaryOp::Eq => BoogieBinOp::Eq,
            BinaryOp::NeqInt | BinaryOp::NeqFloat | BinaryOp::Neq => BoogieBinOp::Ne,
            BinaryOp::LtInt | BinaryOp::LtFloat => BoogieBinOp::Lt,
            BinaryOp::LeqInt | BinaryOp::LeqFloat => BoogieBinOp::Le,
            BinaryOp::GtInt | BinaryOp::GtFloat => BoogieBinOp::Gt,
            BinaryOp::GeqInt | BinaryOp::GeqFloat => BoogieBinOp::Ge,
            BinaryOp::And => BoogieBinOp::And,
            BinaryOp::Or => BoogieBinOp::Or,
            _ => panic!("Unsupported binary operator"),
        }
    }

    pub fn convert_unary_op(&self, op: &UnaryOp) -> Results<BoogieUnOp> {
        match op {
            UnaryOp::NotBool => Ok(BoogieUnOp::Not),
            UnaryOp::NegInt | UnaryOp::NegFloat => Ok(BoogieUnOp::Neg),
        }
    }
}
