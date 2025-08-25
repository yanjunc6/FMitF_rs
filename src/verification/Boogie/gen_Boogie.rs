use super::{
    BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine, BoogieProcedure, BoogieProgram,
    BoogieType, BoogieUnOp, BoogieVarDecl, ErrorMessage,
};
use crate::cfg::{
    BasicBlockId, BinaryOp, CfgProgram, Constant, EdgeType, FunctionCfg, FunctionId, LValue,
    Operand, RValue, Statement, TypeName, UnaryOp, VarId, VariableKind,
};
use crate::verification::errors::{Results, SpannedError, VerificationError};
use std::collections::HashMap;

/// Helper functions for generating Boogie programs from CFG programs
/// Focus on building blocks for Boogie generation with prefix support
#[derive(Debug, Clone)]
pub struct BoogieProgramGenerator {
    pub program: BoogieProgram,
    pub current_procedure_index: Option<usize>, // index of current procedure being worked on
}

impl BoogieProgramGenerator {
    /// Create a new generator with an empty Boogie program
    pub fn new(name: String) -> Self {
        BoogieProgramGenerator {
            program: BoogieProgram {
                name,
                global_vars: HashMap::new(),
                other_declarations: Vec::new(),
                global_string_literals: HashMap::new(),
                procedures: Vec::new(),
            },
            current_procedure_index: None,
        }
    }

    /// Create a new generator with a provided Boogie program
    pub fn with_program(program: BoogieProgram) -> Self {
        BoogieProgramGenerator {
            program,
            current_procedure_index: None,
        }
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
            .map(|idx| &self.program.procedures[idx])
    }

    /// Get a mutable reference to the current procedure being worked on
    pub fn get_current_procedure_mut(&mut self) -> Option<&mut BoogieProcedure> {
        if let Some(idx) = self.current_procedure_index {
            Some(&mut self.program.procedures[idx])
        } else {
            None
        }
    }

    /// Generate string axioms and add to other_declarations
    pub fn gen_string_axioms(&mut self) {
        let string_axioms = vec!["// --------------------------
// Type and Operators
// --------------------------

type String;

const empty: String;

function Concat(x: String, y: String): String;
function IntToString(i: int): String;
function RealToString(r: real): String;
function BoolToString(b: bool): String;


// --------------------------
// Axioms (with correct triggers)
// --------------------------

// Identity of empty for concat
axiom (forall s: String :: {Concat(empty, s)} Concat(empty, s) == s);
axiom (forall s: String :: {Concat(s, empty)} Concat(s, empty) == s);

// Injectivity of IntToString
axiom (forall i: int, j: int ::
{ IntToString(i), IntToString(j) }
IntToString(i) == IntToString(j) ==> i == j);

// Injectivity of RealToString
axiom (forall x: real, y: real ::
{ RealToString(x), RealToString(y) }
RealToString(x) == RealToString(y) ==> x == y);

// Injectivity of BoolToString
axiom (forall x: bool, y: bool ::
{ BoolToString(x), BoolToString(y) }
BoolToString(x) == BoolToString(y) ==> x == y);

// Conversions never yield empty
axiom (forall i: int :: {IntToString(i)} IntToString(i) != empty);
axiom (forall r: real :: {RealToString(r)} RealToString(r) != empty);
axiom (forall b: bool :: {BoolToString(b)} BoolToString(b) != empty);"
            .to_string()];

        self.program.other_declarations.extend(string_axioms);
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
                var_type: BoogieType::UserDefined("String".to_string()),
                is_const: true,
            };
            self.program
                .global_string_literals
                .insert(const_name.clone(), var_decl);
        }

        const_name
    }

    /// Generate global constants from CFG program
    pub fn gen_global_constants(&mut self, cfg_program: &CfgProgram) {
        for &var_id in &cfg_program.root_variables {
            let var = &cfg_program.variables[var_id];
            if var.kind == VariableKind::Global {
                let boogie_type = Self::convert_type(&var.ty);
                let var_decl = BoogieVarDecl {
                    var_name: var.name.clone(),
                    var_type: boogie_type,
                    is_const: true,
                };
                self.program.global_vars.insert(var.name.clone(), var_decl);
            }
        }
    }

    /// Generate global table variables from CFG program
    pub fn gen_table_variables(&mut self, cfg_program: &CfgProgram) {
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];

            // For each non-primary field, generate a map variable
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                if !field.is_primary {
                    let var_name = Self::gen_table_field_var_name(&table.name, &field.name);
                    let map_type = Self::gen_table_field_type(cfg_program, table_id, field_id);

                    let var_decl = BoogieVarDecl {
                        var_name: var_name.clone(),
                        var_type: map_type,
                        is_const: false,
                    };
                    self.program.global_vars.insert(var_name, var_decl);
                }
            }
        }
    }

    /// Convert CFG TypeName to BoogieType
    pub fn convert_type(ty: &TypeName) -> BoogieType {
        match ty {
            TypeName::Int => BoogieType::Int,
            TypeName::Float => BoogieType::Real,
            TypeName::Bool => BoogieType::Bool,
            TypeName::String => BoogieType::UserDefined("String".to_string()),
            TypeName::Table(s) => {
                panic!("Table type '{}' should not be used directly in Boogie", s);
            }
            TypeName::Array {
                element_type,
                size: _,
                size_expr: _,
            } => {
                // Flatten nested arrays by counting dimensions recursively
                let (final_element_type, dimensions) = Self::flatten_array_type(element_type);

                // Create int indices for each dimension
                let mut key_types = Vec::new();
                for _ in 0..dimensions {
                    key_types.push(Box::new(BoogieType::Int));
                }
                key_types.push(Box::new(BoogieType::Int)); // Add one for this array level

                let final_boogie_type = Box::new(Self::convert_type(&final_element_type));
                BoogieType::Map(key_types, final_boogie_type)
            }
        }
    }

    /// Helper function to flatten nested array types and count total dimensions
    fn flatten_array_type(ty: &TypeName) -> (TypeName, usize) {
        match ty {
            TypeName::Array {
                element_type,
                size: _,
                size_expr: _,
            } => {
                let (inner_type, inner_dims) = Self::flatten_array_type(element_type);
                (inner_type, inner_dims + 1)
            }
            _ => (ty.clone(), 0),
        }
    }

    /// Generate table field variable name
    pub fn gen_table_field_var_name(table_name: &str, field_name: &str) -> String {
        format!("{}_{}", table_name, field_name)
    }

    /// Generate Boogie type for a table field (used for both global variables and local snapshots)
    pub fn gen_table_field_type(
        cfg_program: &CfgProgram,
        table_id: crate::cfg::TableId,
        field_id: crate::cfg::FieldId,
    ) -> BoogieType {
        let table = &cfg_program.tables[table_id];
        let field = &cfg_program.fields[field_id];

        let key_types: Vec<Box<BoogieType>> = table
            .primary_keys
            .iter()
            .map(|&pk_id| Box::new(Self::convert_type(&cfg_program.fields[pk_id].ty)))
            .collect();
        let value_type = Box::new(Self::convert_type(&field.ty));

        BoogieType::Map(key_types, value_type)
    }

    /// Helper function to check if a variable exists in current procedure's scope
    /// Returns true if found, false if not found
    fn check_variable_exists_in_procedure(&self, var_name: &str) -> bool {
        // Check global variables first
        if self.program.global_vars.contains_key(var_name) {
            return true;
        }

        // Check current procedure parameters and local variables
        if let Some(current_proc) = self.get_current_procedure() {
            // Check parameters
            for param in &current_proc.params {
                if param.var_name == var_name {
                    return true;
                }
            }
            // Check local variables
            for local_var in &current_proc.local_vars {
                if local_var.var_name == var_name {
                    return true;
                }
            }
        }

        false
    }

    /// Helper function to add a local variable to current procedure if it doesn't exist
    /// Returns true if variable was added, false if it already existed
    fn ensure_local_variable_exists(&mut self, var_name: &str, var_type: BoogieType) -> bool {
        if self.check_variable_exists_in_procedure(var_name) {
            return false; // Variable already exists
        }

        // Add to current procedure's local variables
        if let Some(current_proc) = self.get_current_procedure_mut() {
            let var_decl = BoogieVarDecl {
                var_name: var_name.to_string(),
                var_type,
                is_const: false,
            };
            current_proc.local_vars.push(var_decl);
            return true; // Variable was added
        }

        false // No current procedure set
    }

    /// Add a local variable to the current procedure
    /// This is for cases where generators need to create additional local variables
    pub fn add_local_var(&mut self, var_name: &str, var_type: BoogieType) -> bool {
        self.ensure_local_variable_exists(var_name, var_type)
    }

    /// Generate static variable name without adding to local variables (for procedure parameters)
    pub fn gen_var_name_static(
        cfg_program: &CfgProgram,
        var_id: VarId,
        prefix: Option<&str>,
    ) -> String {
        let var = &cfg_program.variables[var_id];
        let base_name = match var.kind {
            VariableKind::Global => var.name.clone(),
            VariableKind::Parameter => format!("param_{}", var.name),
            VariableKind::Local => format!("local_{}", var.name),
            VariableKind::Temporary => format!("{}", var.name),
        };

        if let Some(prefix) = prefix {
            format!("{}_{}", prefix, base_name)
        } else {
            base_name
        }
    }

    /// Generate variable name for a CFG variable with optional prefix
    /// Only automatically adds variables to local_vars for Local and Temporary variable kinds
    pub fn gen_var_name(
        &mut self,
        cfg_program: &CfgProgram,
        var_id: VarId,
        prefix: Option<&str>,
    ) -> String {
        let var = &cfg_program.variables[var_id];
        let var_name = Self::gen_var_name_static(cfg_program, var_id, prefix);

        // For Local and Temporary variables, ensure they are declared in current procedure
        match var.kind {
            VariableKind::Local | VariableKind::Temporary | VariableKind::Parameter => {
                let boogie_type = Self::convert_type(&var.ty);
                self.ensure_local_variable_exists(&var_name, boogie_type);
            }
            VariableKind::Global => {
                // Global and parameter variables should already be declared, just check they exist
                // If they don't exist, it's likely a programming error, but we'll still return the name
            }
        }

        var_name
    }

    /// Convert CFG operand to Boogie expression with optional prefix
    pub fn convert_operand(
        &mut self,
        cfg_program: &CfgProgram,
        operand: &Operand,
        prefix: Option<&str>,
    ) -> Results<BoogieExpr> {
        match operand {
            Operand::Var(var_id) => {
                let var_name = self.gen_var_name(cfg_program, *var_id, prefix);
                Ok(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                })
            }
            Operand::Const(constant) => self.convert_constant(constant),
        }
    }

    /// Convert CFG constant to Boogie expression
    pub fn convert_constant(&mut self, constant: &Constant) -> Results<BoogieExpr> {
        match constant {
            Constant::Int(i) => Ok(BoogieExpr {
                kind: BoogieExprKind::IntConst(*i),
            }),
            Constant::Float(f) => Ok(BoogieExpr {
                kind: BoogieExprKind::RealConst(f.into_inner()),
            }),
            Constant::Bool(b) => Ok(BoogieExpr {
                kind: BoogieExprKind::BoolConst(*b),
            }),
            Constant::String(s) => {
                // Generate the expected string literal constant name
                let const_name = self.add_string_literal(s);
                Ok(BoogieExpr {
                    kind: BoogieExprKind::Var(const_name),
                })
            }
            Constant::Array(_) => Err(vec![SpannedError {
                error: VerificationError::ArrayConstantNotSupported,
                span: None,
            }]),
        }
    }

    /// Convert CFG binary operation to Boogie binary operation
    pub fn convert_binary_op(op: &BinaryOp) -> BoogieBinOp {
        match op {
            BinaryOp::Add => BoogieBinOp::Add,
            BinaryOp::Sub => BoogieBinOp::Sub,
            BinaryOp::Mul => BoogieBinOp::Mul,
            BinaryOp::Div => BoogieBinOp::Div,
            BinaryOp::Lt => BoogieBinOp::Lt,
            BinaryOp::Lte => BoogieBinOp::Le,
            BinaryOp::Gt => BoogieBinOp::Gt,
            BinaryOp::Gte => BoogieBinOp::Ge,
            BinaryOp::Eq => BoogieBinOp::Eq,
            BinaryOp::Neq => BoogieBinOp::Ne,
            BinaryOp::And => BoogieBinOp::And,
            BinaryOp::Or => BoogieBinOp::Or,
            BinaryOp::Concat => panic!("Concat should be handled as function call, not binary op"),
        }
    }

    /// Convert CFG operand to Boogie expression, automatically converting to string if needed
    pub fn convert_operand_to_string(
        &mut self,
        cfg_program: &CfgProgram,
        operand: &Operand,
        prefix: Option<&str>,
    ) -> Results<BoogieExpr> {
        match operand {
            Operand::Var(var_id) => {
                let var = &cfg_program.variables[*var_id];
                let var_name = self.gen_var_name(cfg_program, *var_id, prefix);

                match var.ty {
                    TypeName::String => Ok(BoogieExpr {
                        kind: BoogieExprKind::Var(var_name),
                    }),
                    TypeName::Int => Ok(BoogieExpr {
                        kind: BoogieExprKind::FunctionCall {
                            name: "IntToString".to_string(),
                            args: vec![BoogieExpr {
                                kind: BoogieExprKind::Var(var_name),
                            }],
                        },
                    }),
                    TypeName::Float => Ok(BoogieExpr {
                        kind: BoogieExprKind::FunctionCall {
                            name: "RealToString".to_string(),
                            args: vec![BoogieExpr {
                                kind: BoogieExprKind::Var(var_name),
                            }],
                        },
                    }),
                    TypeName::Bool => Ok(BoogieExpr {
                        kind: BoogieExprKind::FunctionCall {
                            name: "BoolToString".to_string(),
                            args: vec![BoogieExpr {
                                kind: BoogieExprKind::Var(var_name),
                            }],
                        },
                    }),
                    _ => {
                        todo!("Implement conversion for other types")
                    }
                }
            }
            Operand::Const(constant) => match constant {
                Constant::String(s) => {
                    let const_name = self.add_string_literal(s);
                    Ok(BoogieExpr {
                        kind: BoogieExprKind::Var(const_name),
                    })
                }
                Constant::Int(i) => Ok(BoogieExpr {
                    kind: BoogieExprKind::FunctionCall {
                        name: "IntToString".to_string(),
                        args: vec![BoogieExpr {
                            kind: BoogieExprKind::IntConst(*i),
                        }],
                    },
                }),
                Constant::Float(f) => Ok(BoogieExpr {
                    kind: BoogieExprKind::FunctionCall {
                        name: "RealToString".to_string(),
                        args: vec![BoogieExpr {
                            kind: BoogieExprKind::RealConst(f.into_inner()),
                        }],
                    },
                }),
                Constant::Bool(b) => Ok(BoogieExpr {
                    kind: BoogieExprKind::FunctionCall {
                        name: "BoolToString".to_string(),
                        args: vec![BoogieExpr {
                            kind: BoogieExprKind::BoolConst(*b),
                        }],
                    },
                }),
                Constant::Array(_) => Err(vec![SpannedError {
                    error: VerificationError::ArrayConstantNotSupported,
                    span: None,
                }]),
            },
        }
    }

    /// Infer the type of an operand for concatenation logic
    pub fn infer_operand_type(
        &self,
        cfg_program: &CfgProgram,
        operand: &Operand,
    ) -> Option<TypeName> {
        match operand {
            Operand::Var(var_id) => Some(cfg_program.variables[*var_id].ty.clone()),
            Operand::Const(constant) => {
                match constant {
                    Constant::Int(_) => Some(TypeName::Int),
                    Constant::Float(_) => Some(TypeName::Float),
                    Constant::Bool(_) => Some(TypeName::Bool),
                    Constant::String(_) => Some(TypeName::String),
                    Constant::Array(_) => None, // Arrays not fully supported yet
                }
            }
        }
    }
    pub fn convert_unary_op(op: &UnaryOp) -> Results<BoogieUnOp> {
        match op {
            UnaryOp::Not => Ok(BoogieUnOp::Not),
            UnaryOp::Neg => Ok(BoogieUnOp::Neg),
            UnaryOp::PreIncrement
            | UnaryOp::PostIncrement
            | UnaryOp::PreDecrement
            | UnaryOp::PostDecrement => Err(vec![SpannedError {
                error: VerificationError::IncrementDecrementNotSupported,
                span: None,
            }]),
        }
    }

    /// Convert CFG RValue to Boogie expression with optional prefix
    pub fn convert_rvalue(
        &mut self,
        cfg_program: &CfgProgram,
        rvalue: &RValue,
        prefix: Option<&str>,
    ) -> Results<BoogieExpr> {
        match rvalue {
            RValue::Use(operand) => self.convert_operand(cfg_program, operand, prefix),
            RValue::TableAccess {
                table,
                pk_values,
                field,
                ..
            } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let var_name = Self::gen_table_field_var_name(&table_info.name, &field_info.name);

                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                });

                let mut indices = Vec::new();
                let mut errors = Vec::new();

                for operand in pk_values {
                    match self.convert_operand(cfg_program, operand, prefix) {
                        Ok(expr) => indices.push(expr),
                        Err(mut errs) => errors.append(&mut errs),
                    }
                }

                if !errors.is_empty() {
                    return Err(errors);
                }

                Ok(BoogieExpr {
                    kind: BoogieExprKind::MapSelect {
                        base: base_expr,
                        indices,
                    },
                })
            }
            RValue::ArrayAccess { array, index } => {
                let array_expr = self.convert_operand(cfg_program, array, prefix)?;
                let index_expr = self.convert_operand(cfg_program, index, prefix)?;

                Ok(BoogieExpr {
                    kind: BoogieExprKind::MapSelect {
                        base: Box::new(array_expr),
                        indices: vec![index_expr],
                    },
                })
            }
            RValue::UnaryOp { op, operand } => {
                let boogie_op = Self::convert_unary_op(op)?;
                let operand_expr = self.convert_operand(cfg_program, operand, prefix)?;

                Ok(BoogieExpr {
                    kind: BoogieExprKind::UnOp(boogie_op, Box::new(operand_expr)),
                })
            }
            RValue::BinaryOp { op, left, right } => {
                let left_expr = self.convert_operand(cfg_program, left, prefix)?;
                let right_expr = self.convert_operand(cfg_program, right, prefix)?;

                match op {
                    BinaryOp::Concat => {
                        // Handle string concatenation with type conversion
                        let left_converted =
                            self.convert_operand_to_string(cfg_program, left, prefix)?;
                        let right_converted =
                            self.convert_operand_to_string(cfg_program, right, prefix)?;

                        Ok(BoogieExpr {
                            kind: BoogieExprKind::FunctionCall {
                                name: "Concat".to_string(),
                                args: vec![left_converted, right_converted],
                            },
                        })
                    }
                    BinaryOp::Div => {
                        // Determine whether to use regular division or integer division
                        // based on the types of the operands
                        let left_type = self.infer_operand_type(cfg_program, left);
                        let right_type = self.infer_operand_type(cfg_program, right);
                        
                        // Use integer division if both operands are integers
                        let boogie_op = match (left_type, right_type) {
                            (Some(TypeName::Int), Some(TypeName::Int)) => BoogieBinOp::IntDiv,
                            _ => BoogieBinOp::Div, // Default to regular division for floats or mixed types
                        };

                        Ok(BoogieExpr {
                            kind: BoogieExprKind::BinOp(
                                Box::new(left_expr),
                                boogie_op,
                                Box::new(right_expr),
                            ),
                        })
                    }
                    _ => {
                        let boogie_op = Self::convert_binary_op(op);
                        Ok(BoogieExpr {
                            kind: BoogieExprKind::BinOp(
                                Box::new(left_expr),
                                boogie_op,
                                Box::new(right_expr),
                            ),
                        })
                    }
                }
            }
        }
    }

    /// Convert CFG assignment statement to Boogie assignment with optional prefix
    pub fn convert_assignment(
        &mut self,
        cfg_program: &CfgProgram,
        lvalue: &LValue,
        rvalue: &RValue,
        prefix: Option<&str>,
    ) -> Results<BoogieLine> {
        let rvalue_expr = self.convert_rvalue(cfg_program, rvalue, prefix)?;

        match lvalue {
            LValue::Variable { var } => {
                let var_name = self.gen_var_name(cfg_program, *var, prefix);
                Ok(BoogieLine::Assign(var_name, rvalue_expr))
            }
            LValue::ArrayElement { array, index } => {
                let array_name = self.gen_var_name(cfg_program, *array, prefix);
                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(array_name.clone()),
                });
                let index_expr = self.convert_operand(cfg_program, index, prefix)?;

                let store_expr = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: base_expr,
                        indices: vec![index_expr],
                        value: Box::new(rvalue_expr),
                    },
                };

                Ok(BoogieLine::Assign(array_name, store_expr))
            }
            LValue::TableField {
                table,
                pk_values,
                field,
                ..
            } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let var_name = Self::gen_table_field_var_name(&table_info.name, &field_info.name);

                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name.clone()),
                });

                let mut indices = Vec::new();
                let mut errors = Vec::new();

                for operand in pk_values {
                    match self.convert_operand(cfg_program, operand, prefix) {
                        Ok(expr) => indices.push(expr),
                        Err(mut errs) => errors.append(&mut errs),
                    }
                }

                if !errors.is_empty() {
                    return Err(errors);
                }

                let store_expr = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: base_expr,
                        indices,
                        value: Box::new(rvalue_expr),
                    },
                };

                Ok(BoogieLine::Assign(var_name, store_expr))
            }
        }
    }

    /// Convert CFG statement to Boogie lines with optional prefix
    pub fn convert_statement(
        &mut self,
        cfg_program: &CfgProgram,
        stmt: &Statement,
        prefix: Option<&str>,
    ) -> Results<Vec<BoogieLine>> {
        match stmt {
            Statement::Assign {
                lvalue,
                rvalue,
                span,
            } => {
                let assignment = self.convert_assignment(cfg_program, lvalue, rvalue, prefix);
                match assignment {
                    Ok(line) => Ok(vec![line]),
                    Err(errors) => Err(errors
                        .into_iter()
                        .map(|e| SpannedError {
                            error: e.error,
                            span: Some(span.clone()),
                        })
                        .collect()),
                }
            }
        }
    }

    /// Generate procedure parameters from function parameters with optional prefix
    pub fn gen_procedure_params(
        cfg_program: &CfgProgram,
        function: &FunctionCfg,
        prefix: Option<&str>,
    ) -> Vec<BoogieVarDecl> {
        function
            .parameters
            .iter()
            .map(|&var_id| {
                let var = &cfg_program.variables[var_id];
                BoogieVarDecl {
                    var_name: Self::gen_var_name_static(cfg_program, var_id, prefix),
                    var_type: Self::convert_type(&var.ty),
                    is_const: false,
                }
            })
            .collect()
    }

    /// Convert BoogieType to string representation
    pub fn boogie_type_to_string(ty: &BoogieType) -> String {
        match ty {
            BoogieType::Int => "int".to_string(),
            BoogieType::Real => "real".to_string(),
            BoogieType::Bool => "bool".to_string(),
            BoogieType::UserDefined(name) => name.clone(),
            BoogieType::Map(key_types, value_type) => {
                let key_strs: Vec<String> = key_types
                    .iter()
                    .map(|kt| Self::boogie_type_to_string(kt))
                    .collect();
                let value_str = Self::boogie_type_to_string(value_type);
                format!("[{}]{}", key_strs.join("]["), value_str)
            }
        }
    }

    /// Generate base Boogie program with common elements (constants, globals, axioms, tables)
    /// Returns a BoogieProgram that can be used as a template for individual functions
    pub fn gen_base_program(cfg_program: &CfgProgram) -> Results<BoogieProgram> {
        let mut generator = BoogieProgramGenerator::new("base_program".to_string());

        // Generate string axioms if needed
        generator.gen_string_axioms();

        // Generate global constants
        generator.gen_global_constants(cfg_program);

        // Generate table variables
        generator.gen_table_variables(cfg_program);

        Ok(generator.program)
    }

    /// Add a Boogie assertion to the current procedure
    pub fn add_assertion_to_current_procedure(
        &mut self,
        condition: BoogieExpr,
        error_msg: ErrorMessage,
    ) {
        if let Some(proc) = self.get_current_procedure_mut() {
            proc.lines.push(BoogieLine::Assert(condition, error_msg));
        }
    }

    /// Add a Boogie comment to the current procedure  
    pub fn add_comment_to_current_procedure(&mut self, comment: String) {
        if let Some(proc) = self.get_current_procedure_mut() {
            proc.lines.push(BoogieLine::Comment(comment));
        }
    }

    /// Add any Boogie line to the current procedure
    pub fn add_line_to_current_procedure(&mut self, line: BoogieLine) {
        if let Some(proc) = self.get_current_procedure_mut() {
            proc.lines.push(line);
        }
    }

    /// Add multiple Boogie lines to the current procedure
    pub fn add_lines_to_current_procedure(&mut self, lines: Vec<BoogieLine>) {
        if let Some(proc) = self.get_current_procedure_mut() {
            proc.lines.extend(lines);
        }
    }

    /// Generate boolean conjunction (AND) of multiple expressions
    /// Returns true if expressions is empty, otherwise combines all with AND
    pub fn gen_conjunction(expressions: Vec<BoogieExpr>) -> BoogieExpr {
        if expressions.is_empty() {
            return BoogieExpr {
                kind: BoogieExprKind::BoolConst(true),
            };
        }

        if expressions.len() == 1 {
            return expressions.into_iter().next().unwrap();
        }

        let mut result = expressions[0].clone();
        for expr in expressions.iter().skip(1) {
            result = BoogieExpr {
                kind: BoogieExprKind::BinOp(
                    Box::new(result),
                    BoogieBinOp::And,
                    Box::new(expr.clone()),
                ),
            };
        }

        result
    }

    /// Generate boolean disjunction (OR) of multiple expressions
    /// Returns false if expressions is empty, otherwise combines all with OR
    pub fn gen_disjunction(expressions: Vec<BoogieExpr>) -> BoogieExpr {
        if expressions.is_empty() {
            return BoogieExpr {
                kind: BoogieExprKind::BoolConst(false),
            };
        }

        if expressions.len() == 1 {
            return expressions.into_iter().next().unwrap();
        }

        let mut result = expressions[0].clone();
        for expr in expressions.iter().skip(1) {
            result = BoogieExpr {
                kind: BoogieExprKind::BinOp(
                    Box::new(result),
                    BoogieBinOp::Or,
                    Box::new(expr.clone()),
                ),
            };
        }

        result
    }

    /// Collect modified globals using hop-level table_mod_ref analysis
    /// Returns list of global variable names that are modified by the function
    pub fn collect_modified_globals_from_dataflow(
        cfg_program: &CfgProgram,
        function_id: FunctionId,
    ) -> Vec<String> {
        use crate::dataflow::{analyze_table_mod_ref, AccessType};

        let function = &cfg_program.functions[function_id];
        let analysis_results = analyze_table_mod_ref(function, cfg_program);

        let mut modified_globals = std::collections::HashSet::new();

        // Collect all table accesses from all basic blocks
        for block_exit in analysis_results.block_exit.values() {
            if let Some(accesses) = block_exit.as_set() {
                for access in accesses {
                    if access.access_type == AccessType::Write {
                        let table = &cfg_program.tables[access.table];
                        let field = &cfg_program.fields[access.field];
                        let var_name = Self::gen_table_field_var_name(&table.name, &field.name);
                        modified_globals.insert(var_name);
                    }
                }
            }
        }

        modified_globals.into_iter().collect()
    }

    /// add suffix and prefix
    pub fn add_suffix_prefix_helper(
        name: &str,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) -> String {
        let mut result = name.to_string();
        if let Some(prefix) = prefix {
            result = format!("{}_{}", prefix, result);
        }
        if let Some(suffix) = suffix {
            result = format!("{}_{}", result, suffix);
        }
        result
    }

    /// Generate a unique label name for a basic block with optional prefix and suffix  
    pub fn gen_basic_block_label(
        block_id: BasicBlockId,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) -> String {
        let label = format!("block{}", block_id.index());
        Self::add_suffix_prefix_helper(&label, prefix, suffix)
    }

    /// Generate function start label
    pub fn gen_function_start_label(
        function_name: &str,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) -> String {
        let label = format!("{}_start", function_name);
        Self::add_suffix_prefix_helper(&label, prefix, suffix)
    }

    /// Generate function end label
    pub fn gen_function_end_label(
        function_name: &str,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) -> String {
        let label = format!("{}_end", function_name);
        Self::add_suffix_prefix_helper(&label, prefix, suffix)
    }

    /// Generate function return label,
    /// should be placed at the same place as function end if the function is generated fully
    pub fn gen_function_return_label(
        function_name: &str,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) -> String {
        let label = format!("{}_return", function_name);
        Self::add_suffix_prefix_helper(&label, prefix, suffix)
    }

    /// Generate function abort label,
    /// should be placed at the same place as function end if the function is generated fully
    pub fn gen_function_abort_label(
        function_name: &str,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) -> String {
        let label = format!("{}_abort", function_name);
        Self::add_suffix_prefix_helper(&label, prefix, suffix)
    }

    /// Generate goto edges for basic block control flow
    pub fn gen_basic_block_edges(
        &mut self,
        lines: &mut Vec<BoogieLine>,
        cfg_program: &CfgProgram,
        block_id: BasicBlockId,
        function_name: &str,
        prefix: Option<&str>,
        suffix: Option<&str>,
    ) {
        let block = &cfg_program.blocks[block_id];

        // If no successors, this is a terminal block - no gotos needed
        if block.successors.is_empty() {
            return;
        }

        for edge in &block.successors {
            match &edge.edge_type {
                EdgeType::Unconditional => {
                    // Generate goto to target block
                    let target_label = Self::gen_basic_block_label(edge.to, prefix, suffix);
                    lines.push(BoogieLine::Goto(target_label));
                }
                EdgeType::ConditionalTrue { condition } => {
                    // Generate conditional goto: if (condition) goto target
                    let target_label = Self::gen_basic_block_label(edge.to, prefix, suffix);
                    // Convert condition operand to Boogie expression
                    match self.convert_operand(cfg_program, condition, None) {
                        Ok(condition_expr) => {
                            lines.push(BoogieLine::If {
                                cond: condition_expr,
                                then_body: vec![Box::new(BoogieLine::Goto(target_label))],
                                else_body: vec![], // Empty else body
                            });
                        }
                        Err(_) => {
                            // If condition conversion fails, generate unconditional goto as fallback
                            lines.push(BoogieLine::Goto(target_label));
                        }
                    }
                }
                EdgeType::ConditionalFalse { condition } => {
                    // Generate conditional goto: if (!condition) goto target
                    let target_label = Self::gen_basic_block_label(edge.to, prefix, suffix);
                    // Convert condition operand to Boogie expression and negate it
                    match self.convert_operand(cfg_program, condition, None) {
                        Ok(condition_expr) => {
                            let negated_condition = BoogieExpr {
                                kind: BoogieExprKind::UnOp(
                                    BoogieUnOp::Not,
                                    Box::new(condition_expr),
                                ),
                            };
                            lines.push(BoogieLine::If {
                                cond: negated_condition,
                                then_body: vec![Box::new(BoogieLine::Goto(target_label))],
                                else_body: vec![], // Empty else body
                            });
                        }
                        Err(_) => {
                            // If condition conversion fails, generate unconditional goto as fallback
                            lines.push(BoogieLine::Goto(target_label));
                        }
                    }
                }
                EdgeType::HopExit { next_hop } => {
                    if let Some(next_hop_id) = next_hop {
                        // Generate goto to next hop's entry block
                        let next_hop = &cfg_program.hops[*next_hop_id];
                        if let Some(entry_block) = next_hop.entry_block {
                            let target_label =
                                Self::gen_basic_block_label(entry_block, prefix, suffix);
                            lines.push(BoogieLine::Goto(target_label));
                        }
                    } else {
                        // HopExit with no next hop - goto function end
                        let end_label = Self::gen_function_end_label(function_name, prefix, suffix);
                        lines.push(BoogieLine::Goto(end_label));
                    }
                }
                EdgeType::Return { value: _ } => {
                    // Return statement - goto function return label
                    let return_label =
                        Self::gen_function_return_label(function_name, prefix, suffix);
                    lines.push(BoogieLine::Goto(return_label));
                }
                EdgeType::Abort => {
                    // Abort statement - goto function abort label
                    let abort_label = Self::gen_function_abort_label(function_name, prefix, suffix);
                    lines.push(BoogieLine::Goto(abort_label));
                }
            }
        }
    }
}

impl Default for BoogieProgramGenerator {
    fn default() -> Self {
        Self::new("program".to_string())
    }
}
