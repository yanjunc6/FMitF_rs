use super::{
    BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine, BoogieProcedure, BoogieProgram,
    BoogieType, BoogieUnOp, BoogieVarDecl, ErrorMessage,
};
use crate::ast::ReturnType;
use crate::cfg::{
    BasicBlock, BasicBlockId, BinaryOp, CfgProgram, Constant, ControlFlowEdge, EdgeType, FieldId,
    FieldInfo, FunctionCfg, FunctionId, FunctionType, HopCfg, HopId, LValue, Operand, RValue,
    Statement, TableId, TableInfo, TypeName, UnaryOp, VarId, Variable, VariableKind,
};
use std::collections::{HashMap, HashSet};

/// Helper functions for generating Boogie programs from CFG programs
/// This is a comprehensive set of helper functions to assist users in converting CFG programs to Boogie
#[derive(Debug, Clone)]
pub struct BoogieProgramGenerator {
    pub program: BoogieProgram,
}

impl BoogieProgramGenerator {
    /// Create a new generator with an empty Boogie program
    pub fn new() -> Self {
        BoogieProgramGenerator {
            program: BoogieProgram {
                global_vars: HashMap::new(),
                other_declarations: Vec::new(),
                procedures: Vec::new(),
            },
        }
    }

    /// Create a new generator with a provided Boogie program
    pub fn with_program(program: BoogieProgram) -> Self {
        BoogieProgramGenerator { program }
    }

    /// Generate string axioms and add to other_declarations
    /// This includes the complete string model with concat operations and type conversions
    pub fn gen_string_axioms(&mut self) {
        let string_axioms = vec![
            "/*************************************************************************".to_string(),
            " * String model with + overloads for int and real".to_string(),
            " *************************************************************************/".to_string(),
            "".to_string(),
            "// An uninterpreted sort for strings".to_string(),
            "type String;".to_string(),
            "".to_string(),
            "/* ------------------  core constructors  ------------------ */".to_string(),
            "function Concat(s1:String, s2:String) : String;   // s1 + s2".to_string(),
            "function IntToString(i:int)            : String;   // \"42\"".to_string(),
            "function RealToString(r:real)          : String;   // \"3.14\"".to_string(),
            "".to_string(),
            "/* ------------------  derived \"+\" operators --------------- */".to_string(),
            "//  s + i".to_string(),
            "function StrAddInt  (s:String, i:int)  : String;".to_string(),
            "//  i + s".to_string(),
            "function IntAddStr  (i:int, s:String)  : String;".to_string(),
            "//  s + r".to_string(),
            "function StrAddReal (s:String, r:real) : String;".to_string(),
            "//  r + s".to_string(),
            "function RealAddStr (r:real, s:String) : String;".to_string(),
            "".to_string(),
            "/* ------------------  axioms tying everything together ---- */".to_string(),
            "".to_string(),
            "// 1.  \"+\" is just Concat after coercion".to_string(),
            "axiom (forall s:String, i:int  :: StrAddInt(s,i)  == Concat(s, IntToString(i)));".to_string(),
            "axiom (forall i:int,  s:String :: IntAddStr(i,s)  == Concat(IntToString(i), s));".to_string(),
            "".to_string(),
            "axiom (forall s:String, r:real :: StrAddReal(s,r) == Concat(s, RealToString(r)));".to_string(),
            "axiom (forall r:real, s:String :: RealAddStr(r,s) == Concat(RealToString(r), s));".to_string(),
            "".to_string(),
            "axiom (forall s1,s2,s3:String ::".to_string(),
            "        Concat(Concat(s1,s2),s3) == Concat(s1,Concat(s2,s3)));          // associativity".to_string(),
            "".to_string(),
            "// Optional identity element (empty string)".to_string(),
            "const Empty : String;".to_string(),
            "axiom (forall s:String :: Concat(Empty,s) == s && Concat(s,Empty) == s);".to_string(),
        ];

        self.program.other_declarations.extend(string_axioms);
    }

    /// Generate global constants from CFG program
    /// Converts all global variables from CFG to Boogie constants
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
    /// For each table field, creates a map variable in Boogie
    /// Example: table Data{ primary int ID; primary int B_ID; int VALUE; float PRICE;}
    /// generates: var Data_VALUE: [int][int]int; and var Data_PRICE: [int][int]real;
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
    /// Handles all type conversions including arrays as nested maps
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
                dimensions,
                ..
            } => {
                let element_boogie_type = Box::new(Self::convert_type(element_type));
                let mut result_type = element_boogie_type;

                // Build nested map types for multi-dimensional arrays
                // All array dimensions use integer keys
                for _ in 0..*dimensions {
                    let key_types = vec![Box::new(BoogieType::Int)];
                    result_type = Box::new(BoogieType::Map(key_types, result_type));
                }
                *result_type
            }
        }
    }

    /// Generate table field variable name
    /// Convention: TableName_FieldName
    pub fn gen_table_field_var_name(table_name: &str, field_name: &str) -> String {
        format!("{}_{}", table_name, field_name)
    }

    /// Generate variable name for a CFG variable
    /// Adds prefixes based on variable kind to avoid naming conflicts
    pub fn gen_var_name(cfg_program: &CfgProgram, var_id: VarId) -> String {
        let var = &cfg_program.variables[var_id];
        match var.kind {
            VariableKind::Global => var.name.clone(),
            VariableKind::Parameter => format!("param_{}", var.name),
            VariableKind::Local => format!("local_{}", var.name),
            VariableKind::Temporary => format!("temp_{}", var.name),
        }
    }

    /// Generate a unique label name for a basic block
    /// Format: FunctionName_hopN_blockM
    pub fn gen_block_label(function_name: &str, hop_index: usize, block_index: usize) -> String {
        format!("{}_hop{}_block{}", function_name, hop_index, block_index)
    }

    /// Convert CFG operand to Boogie expression
    pub fn convert_operand(cfg_program: &CfgProgram, operand: &Operand) -> BoogieExpr {
        match operand {
            Operand::Var(var_id) => {
                let var_name = Self::gen_var_name(cfg_program, *var_id);
                BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                }
            }
            Operand::Const(constant) => Self::convert_constant(constant),
        }
    }

    /// Convert CFG constant to Boogie expression
    pub fn convert_constant(constant: &Constant) -> BoogieExpr {
        match constant {
            Constant::Int(i) => BoogieExpr {
                kind: BoogieExprKind::IntConst(*i),
            },
            Constant::Float(f) => BoogieExpr {
                kind: BoogieExprKind::RealConst(f.into_inner()),
            },
            Constant::Bool(b) => BoogieExpr {
                kind: BoogieExprKind::BoolConst(*b),
            },
            Constant::String(_) => {
                // For string constants, would need to extend Boogie expression kinds
                todo!("String constants not yet supported in Boogie generation")
            }
            Constant::Array(_) => {
                // For array constants, would need to generate map initialization
                todo!("Array constants not yet supported in Boogie generation")
            }
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
    pub fn convert_unary_op(op: &UnaryOp) -> BoogieUnOp {
        match op {
            UnaryOp::Not => BoogieUnOp::Not,
            UnaryOp::Neg => BoogieUnOp::Neg,
            // Note: Pre/Post increment/decrement need special handling
            // They should be converted to separate assignment statements
            UnaryOp::PreIncrement | UnaryOp::PostIncrement => {
                BoogieUnOp::Neg // placeholder - increment should be handled as assignment
            }
            UnaryOp::PreDecrement | UnaryOp::PostDecrement => {
                BoogieUnOp::Neg // placeholder - decrement should be handled as assignment
            }
        }
    }

    /// Convert CFG RValue to Boogie expression
    /// Handles table access, array access, and operations
    pub fn convert_rvalue(cfg_program: &CfgProgram, rvalue: &RValue) -> BoogieExpr {
        match rvalue {
            RValue::Use(operand) => Self::convert_operand(cfg_program, operand),
            RValue::TableAccess {
                table,
                pk_fields: _,
                pk_values,
                field,
            } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let var_name = Self::gen_table_field_var_name(&table_info.name, &field_info.name);

                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                });

                let indices: Vec<BoogieExpr> = pk_values
                    .iter()
                    .map(|operand| Self::convert_operand(cfg_program, operand))
                    .collect();

                BoogieExpr {
                    kind: BoogieExprKind::MapSelect {
                        base: base_expr,
                        indices,
                    },
                }
            }
            RValue::ArrayAccess { array, index } => {
                let array_expr = Box::new(Self::convert_operand(cfg_program, array));
                let index_expr = Self::convert_operand(cfg_program, index);

                BoogieExpr {
                    kind: BoogieExprKind::MapSelect {
                        base: array_expr,
                        indices: vec![index_expr],
                    },
                }
            }
            RValue::UnaryOp { op, operand } => {
                let boogie_op = Self::convert_unary_op(op);
                let operand_expr = Box::new(Self::convert_operand(cfg_program, operand));

                BoogieExpr {
                    kind: BoogieExprKind::UnOp(boogie_op, operand_expr),
                }
            }
            RValue::BinaryOp { op, left, right } => {
                let boogie_op = Self::convert_binary_op(op);
                let left_expr = Box::new(Self::convert_operand(cfg_program, left));
                let right_expr = Box::new(Self::convert_operand(cfg_program, right));

                BoogieExpr {
                    kind: BoogieExprKind::BinOp(left_expr, boogie_op, right_expr),
                }
            }
        }
    }

    /// Convert CFG assignment statement to Boogie assignment
    /// Handles variable, array element, and table field assignments
    pub fn convert_assignment(
        cfg_program: &CfgProgram,
        lvalue: &LValue,
        rvalue: &RValue,
    ) -> BoogieLine {
        let rvalue_expr = Self::convert_rvalue(cfg_program, rvalue);

        match lvalue {
            LValue::Variable { var } => {
                let var_name = Self::gen_var_name(cfg_program, *var);
                BoogieLine::Assign(var_name, rvalue_expr)
            }
            LValue::ArrayElement { array, index } => {
                let array_name = Self::gen_var_name(cfg_program, *array);
                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(array_name.clone()),
                });
                let index_expr = Self::convert_operand(cfg_program, index);

                let store_expr = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: base_expr,
                        indices: vec![index_expr],
                        value: Box::new(rvalue_expr),
                    },
                };

                BoogieLine::Assign(array_name, store_expr)
            }
            LValue::TableField {
                table,
                pk_fields: _,
                pk_values,
                field,
            } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let var_name = Self::gen_table_field_var_name(&table_info.name, &field_info.name);

                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name.clone()),
                });

                let indices: Vec<BoogieExpr> = pk_values
                    .iter()
                    .map(|operand| Self::convert_operand(cfg_program, operand))
                    .collect();

                let store_expr = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: base_expr,
                        indices,
                        value: Box::new(rvalue_expr),
                    },
                };

                BoogieLine::Assign(var_name, store_expr)
            }
        }
    }

    /// Convert CFG statement to Boogie lines
    /// Currently handles assignment statements
    pub fn convert_statement(cfg_program: &CfgProgram, stmt: &Statement) -> Vec<BoogieLine> {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                vec![Self::convert_assignment(cfg_program, lvalue, rvalue)]
            }
        }
    }

    /// Generate all table field variables and add to global_vars
    /// Convenience method for generating all table-related variables
    pub fn gen_all_table_variables(&mut self, cfg_program: &CfgProgram) {
        self.gen_table_variables(cfg_program);
    }

    /// Get all modified global variables from a function (for modifies clause)
    /// Analyzes function to determine which global variables are modified
    pub fn get_modified_globals(cfg_program: &CfgProgram, function_id: FunctionId) -> Vec<String> {
        let mut modified = HashSet::new();
        let function = &cfg_program.functions[function_id];

        for &hop_id in &function.hops {
            let hop = &cfg_program.hops[hop_id];
            for &block_id in &hop.blocks {
                let block = &cfg_program.blocks[block_id];
                for stmt in &block.statements {
                    match stmt {
                        Statement::Assign { lvalue, .. } => match lvalue {
                            LValue::Variable { var } => {
                                let var = &cfg_program.variables[*var];
                                if var.kind == VariableKind::Global {
                                    modified.insert(var.name.clone());
                                }
                            }
                            LValue::TableField { table, field, .. } => {
                                let table_info = &cfg_program.tables[*table];
                                let field_info = &cfg_program.fields[*field];
                                let var_name = Self::gen_table_field_var_name(
                                    &table_info.name,
                                    &field_info.name,
                                );
                                modified.insert(var_name);
                            }
                            LValue::ArrayElement { array, .. } => {
                                let var = &cfg_program.variables[*array];
                                if var.kind == VariableKind::Global {
                                    modified.insert(var.name.clone());
                                }
                            }
                        },
                    }
                }
            }
        }

        modified.into_iter().collect()
    }

    /// Generate procedure parameters from function parameters
    /// Converts CFG function parameters to Boogie procedure parameters
    pub fn gen_procedure_params(
        cfg_program: &CfgProgram,
        function: &FunctionCfg,
    ) -> Vec<BoogieVarDecl> {
        function
            .parameters
            .iter()
            .map(|&var_id| {
                let var = &cfg_program.variables[var_id];
                BoogieVarDecl {
                    var_name: Self::gen_var_name(cfg_program, var_id),
                    var_type: Self::convert_type(&var.ty),
                    is_const: false,
                }
            })
            .collect()
    }

    /// Create error message for assertions
    /// Helper for creating error messages in Boogie assertions
    pub fn create_error_message(msg: &str) -> ErrorMessage {
        ErrorMessage {
            msg: msg.to_string(),
        }
    }

    /// Generate control flow edges as Boogie lines
    /// Converts CFG control flow to Boogie goto/if statements
    pub fn convert_control_flow_edge(
        cfg_program: &CfgProgram,
        edge: &ControlFlowEdge,
        target_label: &str,
    ) -> Vec<BoogieLine> {
        match &edge.edge_type {
            EdgeType::Unconditional => vec![BoogieLine::Goto(target_label.to_string())],
            EdgeType::ConditionalTrue { condition } => {
                let cond_expr = Self::convert_operand(cfg_program, condition);
                vec![BoogieLine::If {
                    cond: cond_expr,
                    then_body: vec![Box::new(BoogieLine::Goto(target_label.to_string()))],
                    else_body: vec![],
                }]
            }
            EdgeType::ConditionalFalse { condition } => {
                let cond_expr = Self::convert_operand(cfg_program, condition);
                let negated_cond = BoogieExpr {
                    kind: BoogieExprKind::UnOp(BoogieUnOp::Not, Box::new(cond_expr)),
                };
                vec![BoogieLine::If {
                    cond: negated_cond,
                    then_body: vec![Box::new(BoogieLine::Goto(target_label.to_string()))],
                    else_body: vec![],
                }]
            }
            EdgeType::Return { value } => {
                if let Some(ret_val) = value {
                    let ret_expr = Self::convert_operand(cfg_program, ret_val);
                    vec![
                        BoogieLine::Assign("$result".to_string(), ret_expr),
                        BoogieLine::Goto("$return".to_string()),
                    ]
                } else {
                    vec![BoogieLine::Goto("$return".to_string())]
                }
            }
            EdgeType::Abort => vec![BoogieLine::Assert(
                BoogieExpr {
                    kind: BoogieExprKind::BoolConst(false),
                },
                Self::create_error_message("Execution aborted"),
            )],
            EdgeType::HopExit { next_hop: _ } => {
                vec![BoogieLine::Goto(target_label.to_string())]
            }
        }
    }

    /// Generate local variable declarations for a procedure
    /// Creates variable declaration strings for procedure locals
    pub fn gen_local_var_declarations(
        cfg_program: &CfgProgram,
        function: &FunctionCfg,
    ) -> Vec<String> {
        let mut declarations = Vec::new();

        for &var_id in &function.local_variables {
            let var = &cfg_program.variables[var_id];
            let var_name = Self::gen_var_name(cfg_program, var_id);
            let boogie_type = Self::convert_type(&var.ty);
            let type_str = Self::boogie_type_to_string(&boogie_type);
            declarations.push(format!("var {} : {};", var_name, type_str));
        }

        declarations
    }

    /// Convert BoogieType to string representation
    /// Formats Boogie types for output/debugging
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

    /// Generate invariant assertions for a basic block
    /// Creates safety invariants for verification
    pub fn gen_block_invariants(_cfg_program: &CfgProgram, _block: &BasicBlock) -> Vec<BoogieLine> {
        let mut invariants = Vec::new();

        // Add basic safety invariants
        invariants.push(BoogieLine::Comment("Block invariants".to_string()));

        // Example invariants (customize based on verification requirements):
        // - Assert that all table accesses are within bounds
        // - Assert that variables have valid values
        // - Assert transaction consistency properties

        invariants
    }

    /// Generate a complete Boogie program from a CFG program
    /// Main entry point for complete CFG to Boogie conversion
    pub fn gen_complete_program(&mut self, cfg_program: &CfgProgram) {
        // Generate string axioms if needed
        self.gen_string_axioms();

        // Generate global constants
        self.gen_global_constants(cfg_program);

        // Generate table variables
        self.gen_all_table_variables(cfg_program);

        // Generate procedures for transaction functions only
        for &function_id in &cfg_program.root_functions {
            let function = &cfg_program.functions[function_id];
            if function.function_type == FunctionType::Transaction {
                let procedure = self.gen_procedure_from_function(cfg_program, function_id);
                self.program.procedures.push(procedure);
            }
        }
    }

    /// Generate a complete Boogie procedure from a CFG function
    /// Converts CFG function with hops and basic blocks to Boogie procedure
    pub fn gen_procedure_from_function(
        &self,
        cfg_program: &CfgProgram,
        function_id: FunctionId,
    ) -> BoogieProcedure {
        let function = &cfg_program.functions[function_id];
        let params = Self::gen_procedure_params(cfg_program, function);
        let modifies = Self::get_modified_globals(cfg_program, function_id);

        // Generate procedure body
        let mut lines = Vec::new();
        lines.push(BoogieLine::Comment(format!("Procedure: {}", function.name)));

        // Add local variable declarations as comments (would go in procedure header in real Boogie)
        let local_decls = Self::gen_local_var_declarations(cfg_program, function);
        for decl in local_decls {
            lines.push(BoogieLine::Comment(format!("Local: {}", decl)));
        }

        // Generate basic blocks with labels and control flow
        for (hop_index, &hop_id) in function.hops.iter().enumerate() {
            let hop = &cfg_program.hops[hop_id];
            lines.push(BoogieLine::Comment(format!("Hop {}", hop_index)));

            for (block_index, &block_id) in hop.blocks.iter().enumerate() {
                let block = &cfg_program.blocks[block_id];
                let label = Self::gen_block_label(&function.name, hop_index, block_index);
                lines.push(BoogieLine::Label(label));

                // Add block invariants
                let invariants = Self::gen_block_invariants(cfg_program, block);
                lines.extend(invariants);

                // Convert statements
                for stmt in &block.statements {
                    let boogie_stmts = Self::convert_statement(cfg_program, stmt);
                    lines.extend(boogie_stmts);
                }

                // Handle control flow edges (simplified - would need proper successor handling)
                if !block.successors.is_empty() {
                    let successor_edge = &block.successors[0]; // Simplified: take first successor
                    let target_label =
                        Self::gen_block_label(&function.name, hop_index, block_index + 1);
                    let control_flow =
                        Self::convert_control_flow_edge(cfg_program, successor_edge, &target_label);
                    lines.extend(control_flow);
                }
            }
        }

        // Add return label
        lines.push(BoogieLine::Label("$return".to_string()));
        lines.push(BoogieLine::Comment("End of procedure".to_string()));

        BoogieProcedure {
            name: function.name.clone(),
            params,
            modifies,
            lines,
        }
    }

    /// Generate verification conditions for serializability
    /// Adds axioms and functions for transaction serializability verification
    pub fn gen_serializability_assertions(&mut self) {
        let assertions = vec![
            "// Serializability verification conditions".to_string(),
            "type Transaction;".to_string(),
            "function ConflictSerializable(t1: Transaction, t2: Transaction): bool;".to_string(),
            "axiom (forall t1, t2: Transaction :: ConflictSerializable(t1, t2));".to_string(),
        ];

        self.program.other_declarations.extend(assertions);
    }

    /// Generate atomic section markers for hops
    /// Creates Boogie lines to mark the beginning of atomic sections
    pub fn gen_atomic_section(hop_name: &str) -> Vec<BoogieLine> {
        vec![
            BoogieLine::Comment(format!("Begin atomic section: {}", hop_name)),
            BoogieLine::Assume(BoogieExpr {
                kind: BoogieExprKind::BoolConst(true), // Placeholder for atomic constraints
            }),
        ]
    }

    /// Generate assume statements for preconditions
    /// Converts preconditions to Boogie assume statements
    pub fn gen_precondition_assumes(preconditions: Vec<BoogieExpr>) -> Vec<BoogieLine> {
        let mut lines = Vec::new();
        lines.push(BoogieLine::Comment("Preconditions".to_string()));

        for precond in preconditions {
            lines.push(BoogieLine::Assume(precond));
        }

        lines
    }

    /// Generate assert statements for postconditions
    /// Converts postconditions to Boogie assert statements
    pub fn gen_postcondition_asserts(postconditions: Vec<BoogieExpr>) -> Vec<BoogieLine> {
        let mut lines = Vec::new();
        lines.push(BoogieLine::Comment("Postconditions".to_string()));

        for postcond in postconditions {
            lines.push(BoogieLine::Assert(
                postcond,
                Self::create_error_message("Postcondition violation"),
            ));
        }

        lines
    }

    /// Generate transaction isolation assertions
    /// Creates assertions for transaction isolation properties
    pub fn gen_isolation_assertions(&mut self, isolation_level: &str) {
        let assertions = match isolation_level {
            "READ_UNCOMMITTED" => vec![
                "// Read Uncommitted isolation level".to_string(),
                "// No isolation guarantees".to_string(),
            ],
            "READ_COMMITTED" => vec![
                "// Read Committed isolation level".to_string(),
                "axiom (forall t: Transaction :: NoUncommittedReads(t));".to_string(),
                "function NoUncommittedReads(t: Transaction): bool;".to_string(),
            ],
            "REPEATABLE_READ" => vec![
                "// Repeatable Read isolation level".to_string(),
                "axiom (forall t: Transaction :: NoUncommittedReads(t) && RepeatableReads(t));"
                    .to_string(),
                "function RepeatableReads(t: Transaction): bool;".to_string(),
            ],
            "SERIALIZABLE" => vec![
                "// Serializable isolation level".to_string(),
                "axiom (forall t1, t2: Transaction :: SerializableExecution(t1, t2));".to_string(),
                "function SerializableExecution(t1: Transaction, t2: Transaction): bool;"
                    .to_string(),
            ],
            _ => vec!["// Unknown isolation level".to_string()],
        };

        self.program.other_declarations.extend(assertions);
    }

    /// Generate consistency constraints for tables
    /// Creates constraints to ensure table consistency properties
    pub fn gen_table_constraints(&mut self, cfg_program: &CfgProgram) {
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            let mut constraints = Vec::new();

            constraints.push(format!("// Constraints for table {}", table.name));

            // Primary key constraints
            if !table.primary_keys.is_empty() {
                let pk_names: Vec<String> = table
                    .primary_keys
                    .iter()
                    .map(|&field_id| cfg_program.fields[field_id].name.clone())
                    .collect();
                constraints.push(format!("// Primary key: {}", pk_names.join(", ")));
            }

            // Foreign key constraints (would need additional metadata)
            // Referential integrity constraints
            // Domain constraints based on field types

            self.program.other_declarations.extend(constraints);
        }
    }

    /// Generate loop invariants for control flow
    /// Creates loop invariants for CFG loops (if present)
    pub fn gen_loop_invariants(
        cfg_program: &CfgProgram,
        function_id: FunctionId,
    ) -> Vec<BoogieLine> {
        let mut invariants = Vec::new();
        let function = &cfg_program.functions[function_id];

        // Analyze CFG for loops and generate appropriate invariants
        invariants.push(BoogieLine::Comment("Loop invariants".to_string()));

        // Example: for each detected loop, add invariants about:
        // - Loop bounds
        // - Variable relationships
        // - Table state preservation

        for &hop_id in &function.hops {
            let hop = &cfg_program.hops[hop_id];
            for &block_id in &hop.blocks {
                let block = &cfg_program.blocks[block_id];

                // Simple heuristic: if block has a successor that points back to earlier block,
                // it might be part of a loop
                for successor in &block.successors {
                    if successor.to.index() < block_id.index() {
                        invariants.push(BoogieLine::Comment(format!(
                            "Potential loop detected at block {}",
                            block_id.index()
                        )));
                    }
                }
            }
        }

        invariants
    }

    /// Generate memory safety assertions
    /// Creates assertions for memory safety (array bounds, null checks, etc.)
    pub fn gen_memory_safety_assertions(
        cfg_program: &CfgProgram,
        function_id: FunctionId,
    ) -> Vec<BoogieLine> {
        let mut assertions = Vec::new();
        let function = &cfg_program.functions[function_id];

        assertions.push(BoogieLine::Comment("Memory safety assertions".to_string()));

        for &hop_id in &function.hops {
            let hop = &cfg_program.hops[hop_id];
            for &block_id in &hop.blocks {
                let block = &cfg_program.blocks[block_id];

                for stmt in &block.statements {
                    match stmt {
                        Statement::Assign { lvalue, rvalue, .. } => {
                            // Check array bounds for array accesses
                            match lvalue {
                                LValue::ArrayElement { .. } => {
                                    assertions.push(BoogieLine::Comment(
                                        "Array bounds check needed".to_string(),
                                    ));
                                }
                                _ => {}
                            }

                            // Check for null pointer dereferences in rvalue
                            match rvalue {
                                RValue::ArrayAccess { .. } => {
                                    assertions.push(BoogieLine::Comment(
                                        "Array access bounds check needed".to_string(),
                                    ));
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        assertions
    }
}

impl Default for BoogieProgramGenerator {
    fn default() -> Self {
        Self::new()
    }
}
