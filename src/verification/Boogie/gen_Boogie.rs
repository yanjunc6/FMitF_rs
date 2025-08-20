use super::{
    BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine, BoogieProcedure, BoogieProgram,
    BoogieType, BoogieUnOp, BoogieVarDecl,
};
use crate::cfg::{
    BinaryOp, CfgProgram, Constant, FunctionCfg, FunctionId, FunctionType, LValue, Operand, RValue,
    Statement, TypeName, UnaryOp, VarId, VariableKind,
};
use crate::verification::errors::{Results, SpannedError, VerificationError};
use std::collections::HashMap;

/// Helper functions for generating Boogie programs from CFG programs
/// Focus on building blocks for Boogie generation with prefix support
#[derive(Debug, Clone)]
pub struct BoogieProgramGenerator {
    pub program: BoogieProgram,
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
        }
    }

    /// Create a new generator with a provided Boogie program
    pub fn with_program(program: BoogieProgram) -> Self {
        BoogieProgramGenerator { program }
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

// Conversions never yield empty
axiom (forall i: int :: {IntToString(i)} IntToString(i) != empty);
axiom (forall r: real :: {RealToString(r)} RealToString(r) != empty);"
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
                    let key_types: Vec<Box<BoogieType>> = table
                        .primary_keys
                        .iter()
                        .map(|&pk_id| Box::new(Self::convert_type(&cfg_program.fields[pk_id].ty)))
                        .collect();
                    let value_type = Box::new(Self::convert_type(&field.ty));

                    let map_type = BoogieType::Map(key_types, value_type);
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

    /// Generate variable name for a CFG variable with optional prefix
    pub fn gen_var_name(cfg_program: &CfgProgram, var_id: VarId, prefix: Option<&str>) -> String {
        let var = &cfg_program.variables[var_id];
        let base_name = match var.kind {
            VariableKind::Global => var.name.clone(),
            VariableKind::Parameter => format!("param_{}", var.name),
            VariableKind::Local => format!("local_{}", var.name),
            VariableKind::Temporary => format!("temp_{}", var.name),
        };

        if let Some(prefix) = prefix {
            format!("{}_{}", prefix, base_name)
        } else {
            base_name
        }
    }

    /// Generate a unique label name for a basic block with optional prefix
    pub fn gen_block_label(
        function_name: &str,
        hop_index: usize,
        block_index: usize,
        prefix: Option<&str>,
    ) -> String {
        let base_label = format!("{}_hop{}_block{}", function_name, hop_index, block_index);
        if let Some(prefix) = prefix {
            format!("{}_{}", prefix, base_label)
        } else {
            base_label
        }
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
                let var_name = Self::gen_var_name(cfg_program, *var_id, prefix);
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
        }
    }

    /// Convert CFG unary operation to Boogie unary operation
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
                let boogie_op = Self::convert_binary_op(op);
                let left_expr = self.convert_operand(cfg_program, left, prefix)?;
                let right_expr = self.convert_operand(cfg_program, right, prefix)?;

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
                let var_name = Self::gen_var_name(cfg_program, *var, prefix);
                Ok(BoogieLine::Assign(var_name, rvalue_expr))
            }
            LValue::ArrayElement { array, index } => {
                let array_name = Self::gen_var_name(cfg_program, *array, prefix);
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
                    var_name: Self::gen_var_name(cfg_program, var_id, prefix),
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

    /// Generate a complete Boogie program from a CFG program
    pub fn gen_complete_program(&mut self, cfg_program: &CfgProgram) -> Results<()> {
        // Generate string axioms if needed
        self.gen_string_axioms();

        // Generate global constants
        self.gen_global_constants(cfg_program);

        // Generate table variables
        self.gen_table_variables(cfg_program);

        // Generate procedures for transaction functions only
        let mut all_errors = Vec::new();

        for &function_id in &cfg_program.root_functions {
            let function = &cfg_program.functions[function_id];
            if function.function_type == FunctionType::Transaction {
                match self.gen_function_to_boogie_template(cfg_program, function_id, None) {
                    Ok(procedure) => self.program.procedures.push(procedure),
                    Err(mut errors) => all_errors.append(&mut errors),
                }
            }
        }

        if all_errors.is_empty() {
            Ok(())
        } else {
            Err(all_errors)
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

    /// Template: Generate Boogie lines from a single hop with prefix support
    pub fn gen_hop_to_boogie_template(
        &mut self,
        cfg_program: &CfgProgram,
        hop_id: crate::cfg::HopId,
        prefix: Option<&str>,
    ) -> Results<Vec<BoogieLine>> {
        let mut lines = Vec::new();
        let mut all_errors = Vec::new();
        let hop = &cfg_program.hops[hop_id];

        lines.push(BoogieLine::Comment(format!("Hop template")));

        // Generate labels and statements for each block in the hop
        for (block_index, &block_id) in hop.blocks.iter().enumerate() {
            let block = &cfg_program.blocks[block_id];
            let label = Self::gen_block_label("template", 0, block_index, prefix);
            lines.push(BoogieLine::Label(label));

            // Convert statements
            for stmt in &block.statements {
                match self.convert_statement(cfg_program, stmt, prefix) {
                    Ok(boogie_stmts) => lines.extend(boogie_stmts),
                    Err(mut errors) => all_errors.append(&mut errors),
                }
            }
        }

        if all_errors.is_empty() {
            Ok(lines)
        } else {
            Err(all_errors)
        }
    }

    /// Template: Generate complete Boogie procedure from a CFG function with prefix
    pub fn gen_function_to_boogie_template(
        &mut self,
        cfg_program: &CfgProgram,
        function_id: FunctionId,
        prefix: Option<&str>,
    ) -> Results<BoogieProcedure> {
        let function = &cfg_program.functions[function_id];
        let params = Self::gen_procedure_params(cfg_program, function, prefix);

        let mut lines = Vec::new();
        let mut all_errors = Vec::new();

        lines.push(BoogieLine::Comment(format!(
            "Template function: {}",
            function.name
        )));

        // Generate hops
        for (hop_index, &hop_id) in function.hops.iter().enumerate() {
            let hop = &cfg_program.hops[hop_id];
            lines.push(BoogieLine::Comment(format!("Hop {}", hop_index)));

            for (block_index, &block_id) in hop.blocks.iter().enumerate() {
                let block = &cfg_program.blocks[block_id];
                let label = Self::gen_block_label(&function.name, hop_index, block_index, prefix);
                lines.push(BoogieLine::Label(label));

                for stmt in &block.statements {
                    match self.convert_statement(cfg_program, stmt, prefix) {
                        Ok(boogie_stmts) => lines.extend(boogie_stmts),
                        Err(mut errors) => all_errors.append(&mut errors),
                    }
                }
            }
        }

        if !all_errors.is_empty() {
            return Err(all_errors);
        }

        let procedure_name = if let Some(prefix) = prefix {
            format!("{}_{}", prefix, function.name)
        } else {
            function.name.clone()
        };

        Ok(BoogieProcedure {
            name: procedure_name,
            params,
            modifies: vec![],
            lines,
        })
    }
}

impl Default for BoogieProgramGenerator {
    fn default() -> Self {
        Self::new("program".to_string())
    }
}
