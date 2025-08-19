use super::{
    BoogieProgram, BoogieProcedure, BoogieVarDecl, BoogieType, BoogieLine, BoogieExpr, 
    BoogieExprKind, BoogieBinOp, BoogieUnOp, ErrorMessage,
};
use crate::cfg::{
    CfgProgram, FunctionId, VarId, TypeName, FunctionCfg, Statement, 
    LValue, RValue, Operand, Constant, FunctionType, VariableKind, BinaryOp, UnaryOp
};
use std::collections::HashMap;

/// Helper functions for generating Boogie programs from CFG programs
/// Focus on building blocks for Boogie generation with prefix support
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
            }
        }
    }

    /// Create a new generator with a provided Boogie program
    pub fn with_program(program: BoogieProgram) -> Self {
        BoogieProgramGenerator { program }
    }

    /// Generate string axioms and add to other_declarations
    pub fn gen_string_axioms(&mut self) {
        let string_axioms = vec![
            "// String model".to_string(),
            "type String;".to_string(),
            "function Concat(s1:String, s2:String) : String;".to_string(),
            "function IntToString(i:int) : String;".to_string(),
            "function RealToString(r:real) : String;".to_string(),
            "const Empty : String;".to_string(),
        ];
        
        self.program.other_declarations.extend(string_axioms);
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
                    let key_types: Vec<Box<BoogieType>> = table.primary_keys
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
            },
            TypeName::Array { element_type, .. } => {
                let element_boogie_type = Box::new(Self::convert_type(element_type));
                let key_types = vec![Box::new(BoogieType::Int)];
                BoogieType::Map(key_types, element_boogie_type)
            }
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
    pub fn gen_block_label(function_name: &str, hop_index: usize, block_index: usize, prefix: Option<&str>) -> String {
        let base_label = format!("{}_hop{}_block{}", function_name, hop_index, block_index);
        if let Some(prefix) = prefix {
            format!("{}_{}", prefix, base_label)
        } else {
            base_label
        }
    }

    /// Convert CFG operand to Boogie expression with optional prefix
    pub fn convert_operand(cfg_program: &CfgProgram, operand: &Operand, prefix: Option<&str>) -> BoogieExpr {
        match operand {
            Operand::Var(var_id) => {
                let var_name = Self::gen_var_name(cfg_program, *var_id, prefix);
                BoogieExpr {
                    kind: BoogieExprKind::Var(var_name)
                }
            },
            Operand::Const(constant) => Self::convert_constant(constant)
        }
    }

    /// Convert CFG constant to Boogie expression
    pub fn convert_constant(constant: &Constant) -> BoogieExpr {
        match constant {
            Constant::Int(i) => BoogieExpr {
                kind: BoogieExprKind::IntConst(*i)
            },
            Constant::Float(f) => BoogieExpr {
                kind: BoogieExprKind::RealConst(f.into_inner())
            },
            Constant::Bool(b) => BoogieExpr {
                kind: BoogieExprKind::BoolConst(*b)
            },
            Constant::String(_) => {
                // For string constants
                todo!("String constants not yet supported in Boogie generation")
            },
            Constant::Array(_) => {
                // For array constants
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
            UnaryOp::PreIncrement | UnaryOp::PostIncrement => BoogieUnOp::Neg, // placeholder
            UnaryOp::PreDecrement | UnaryOp::PostDecrement => BoogieUnOp::Neg, // placeholder
        }
    }

    /// Convert CFG RValue to Boogie expression with optional prefix
    pub fn convert_rvalue(cfg_program: &CfgProgram, rvalue: &RValue, prefix: Option<&str>) -> BoogieExpr {
        match rvalue {
            RValue::Use(operand) => Self::convert_operand(cfg_program, operand, prefix),
            RValue::TableAccess { table, pk_values, field, .. } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let var_name = Self::gen_table_field_var_name(&table_info.name, &field_info.name);
                
                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name)
                });
                
                let indices: Vec<BoogieExpr> = pk_values.iter()
                    .map(|operand| Self::convert_operand(cfg_program, operand, prefix))
                    .collect();
                
                BoogieExpr {
                    kind: BoogieExprKind::MapSelect { base: base_expr, indices }
                }
            },
            RValue::ArrayAccess { array, index } => {
                let array_expr = Box::new(Self::convert_operand(cfg_program, array, prefix));
                let index_expr = Self::convert_operand(cfg_program, index, prefix);
                
                BoogieExpr {
                    kind: BoogieExprKind::MapSelect {
                        base: array_expr,
                        indices: vec![index_expr]
                    }
                }
            },
            RValue::UnaryOp { op, operand } => {
                let boogie_op = Self::convert_unary_op(op);
                let operand_expr = Box::new(Self::convert_operand(cfg_program, operand, prefix));
                
                BoogieExpr {
                    kind: BoogieExprKind::UnOp(boogie_op, operand_expr)
                }
            },
            RValue::BinaryOp { op, left, right } => {
                let boogie_op = Self::convert_binary_op(op);
                let left_expr = Box::new(Self::convert_operand(cfg_program, left, prefix));
                let right_expr = Box::new(Self::convert_operand(cfg_program, right, prefix));
                
                BoogieExpr {
                    kind: BoogieExprKind::BinOp(left_expr, boogie_op, right_expr)
                }
            },
        }
    }

    /// Convert CFG assignment statement to Boogie assignment with optional prefix
    pub fn convert_assignment(cfg_program: &CfgProgram, lvalue: &LValue, rvalue: &RValue, prefix: Option<&str>) -> BoogieLine {
        let rvalue_expr = Self::convert_rvalue(cfg_program, rvalue, prefix);
        
        match lvalue {
            LValue::Variable { var } => {
                let var_name = Self::gen_var_name(cfg_program, *var, prefix);
                BoogieLine::Assign(var_name, rvalue_expr)
            },
            LValue::ArrayElement { array, index } => {
                let array_name = Self::gen_var_name(cfg_program, *array, prefix);
                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(array_name.clone())
                });
                let index_expr = Self::convert_operand(cfg_program, index, prefix);
                
                let store_expr = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: base_expr,
                        indices: vec![index_expr],
                        value: Box::new(rvalue_expr)
                    }
                };
                
                BoogieLine::Assign(array_name, store_expr)
            },
            LValue::TableField { table, pk_values, field, .. } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let var_name = Self::gen_table_field_var_name(&table_info.name, &field_info.name);
                
                let base_expr = Box::new(BoogieExpr {
                    kind: BoogieExprKind::Var(var_name.clone())
                });
                
                let indices: Vec<BoogieExpr> = pk_values.iter()
                    .map(|operand| Self::convert_operand(cfg_program, operand, prefix))
                    .collect();
                
                let store_expr = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: base_expr,
                        indices,
                        value: Box::new(rvalue_expr)
                    }
                };
                
                BoogieLine::Assign(var_name, store_expr)
            }
        }
    }

    /// Convert CFG statement to Boogie lines with optional prefix
    pub fn convert_statement(cfg_program: &CfgProgram, stmt: &Statement, prefix: Option<&str>) -> Vec<BoogieLine> {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                vec![Self::convert_assignment(cfg_program, lvalue, rvalue, prefix)]
            }
        }
    }

    /// Generate procedure parameters from function parameters with optional prefix
    pub fn gen_procedure_params(cfg_program: &CfgProgram, function: &FunctionCfg, prefix: Option<&str>) -> Vec<BoogieVarDecl> {
        function.parameters.iter().map(|&var_id| {
            let var = &cfg_program.variables[var_id];
            BoogieVarDecl {
                var_name: Self::gen_var_name(cfg_program, var_id, prefix),
                var_type: Self::convert_type(&var.ty),
                is_const: false,
            }
        }).collect()
    }

    /// Convert BoogieType to string representation
    pub fn boogie_type_to_string(ty: &BoogieType) -> String {
        match ty {
            BoogieType::Int => "int".to_string(),
            BoogieType::Real => "real".to_string(),
            BoogieType::Bool => "bool".to_string(),
            BoogieType::UserDefined(name) => name.clone(),
            BoogieType::Map(key_types, value_type) => {
                let key_strs: Vec<String> = key_types.iter()
                    .map(|kt| Self::boogie_type_to_string(kt))
                    .collect();
                let value_str = Self::boogie_type_to_string(value_type);
                format!("[{}]{}", key_strs.join("]["), value_str)
            }
        }
    }

    /// Generate a complete Boogie program from a CFG program
    pub fn gen_complete_program(&mut self, cfg_program: &CfgProgram) {
        // Generate string axioms if needed
        self.gen_string_axioms();
        
        // Generate global constants
        self.gen_global_constants(cfg_program);
        
        // Generate table variables  
        self.gen_table_variables(cfg_program);
        
        // Generate procedures for transaction functions only
        for &function_id in &cfg_program.root_functions {
            let function = &cfg_program.functions[function_id];
            if function.function_type == FunctionType::Transaction {
                let procedure = self.gen_procedure_from_function(cfg_program, function_id, None);
                self.program.procedures.push(procedure);
            }
        }
    }

    /// Generate a complete Boogie procedure from a CFG function with optional prefix
    pub fn gen_procedure_from_function(&self, cfg_program: &CfgProgram, function_id: FunctionId, prefix: Option<&str>) -> BoogieProcedure {
        let function = &cfg_program.functions[function_id];
        let params = Self::gen_procedure_params(cfg_program, function, prefix);
        let modifies = vec![]; // Simplified - no modifies tracking for now
        
        // Generate procedure body
        let mut lines = Vec::new();
        lines.push(BoogieLine::Comment(format!("Function: {}", function.name)));
        
        // For now, just add a placeholder return
        lines.push(BoogieLine::Comment("Procedure body goes here".to_string()));
        
        let procedure_name = if let Some(prefix) = prefix {
            format!("{}_{}", prefix, function.name)
        } else {
            function.name.clone()
        };
        
        BoogieProcedure {
            name: procedure_name,
            params,
            modifies,
            lines,
        }
    }

    /// Template: Generate Boogie lines from a single hop with prefix support
    pub fn gen_hop_to_boogie_template(cfg_program: &CfgProgram, hop_id: crate::cfg::HopId, prefix: Option<&str>) -> Vec<BoogieLine> {
        let mut lines = Vec::new();
        let hop = &cfg_program.hops[hop_id];
        
        lines.push(BoogieLine::Comment(format!("Hop template")));
        
        // Generate labels and statements for each block in the hop
        for (block_index, &block_id) in hop.blocks.iter().enumerate() {
            let block = &cfg_program.blocks[block_id];
            let label = Self::gen_block_label("template", 0, block_index, prefix);
            lines.push(BoogieLine::Label(label));
            
            // Convert statements
            for stmt in &block.statements {
                let boogie_stmts = Self::convert_statement(cfg_program, stmt, prefix);
                lines.extend(boogie_stmts);
            }
        }
        
        lines
    }

    /// Template: Generate complete Boogie procedure from a CFG function with prefix
    pub fn gen_function_to_boogie_template(cfg_program: &CfgProgram, function_id: FunctionId, prefix: Option<&str>) -> BoogieProcedure {
        let function = &cfg_program.functions[function_id];
        let params = Self::gen_procedure_params(cfg_program, function, prefix);
        
        let mut lines = Vec::new();
        lines.push(BoogieLine::Comment(format!("Template function: {}", function.name)));
        
        // Generate hops
        for (hop_index, &hop_id) in function.hops.iter().enumerate() {
            let hop = &cfg_program.hops[hop_id];
            lines.push(BoogieLine::Comment(format!("Hop {}", hop_index)));
            
            for (block_index, &block_id) in hop.blocks.iter().enumerate() {
                let block = &cfg_program.blocks[block_id];
                let label = Self::gen_block_label(&function.name, hop_index, block_index, prefix);
                lines.push(BoogieLine::Label(label));
                
                for stmt in &block.statements {
                    let boogie_stmts = Self::convert_statement(cfg_program, stmt, prefix);
                    lines.extend(boogie_stmts);
                }
            }
        }
        
        let procedure_name = if let Some(prefix) = prefix {
            format!("{}_{}", prefix, function.name)
        } else {
            function.name.clone()
        };
        
        BoogieProcedure {
            name: procedure_name,
            params,
            modifies: vec![],
            lines,
        }
    }
}

impl Default for BoogieProgramGenerator {
    fn default() -> Self {
        Self::new()
    }
}
