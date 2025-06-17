use crate::cfg::{
    BinaryOp, CfgProgram, Constant, FunctionId as CfgFunctionId, HopId as CfgHopId, Operand, Rvalue, Statement, Terminator, TypeName, UnaryOp, VarId};
use crate::verification::verification_logic::VerificationPlan;
use std::fmt::Write;
use std::collections::{BTreeMap, HashSet};

/// Pure Boogie code generation - no verification logic
pub struct BoogieCodeGenerator;

impl BoogieCodeGenerator {
    pub fn new() -> Self {
        Self
    }

    /// Generate Boogie code from a verification plan
    pub fn generate_code(&self, plan: &VerificationPlan, cfg: &CfgProgram) -> String {
        let mut w = String::new();

        // Header comments
        self.write_header(&mut w, plan, cfg);

        // Table declarations (global variables)
        self.write_table_declarations(&mut w, plan, cfg);

        // Hop procedures
        self.write_hop_procedures(&mut w, plan, cfg);

        // Main procedure
        self.write_main_procedure(&mut w, plan, cfg);

        w
    }

    fn write_header(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        let func1_name = &cfg.functions[plan.func1_id].name;
        let func2_name = &cfg.functions[plan.func2_id].name;

        writeln!(w, "// Boogie verification code").unwrap();
        writeln!(
            w,
            "// Function 1: {} (ID: {})",
            func1_name,
            plan.func1_id.index()
        )
        .unwrap();
        writeln!(
            w,
            "// Function 2: {} (ID: {})",
            func2_name,
            plan.func2_id.index()
        )
        .unwrap();
        writeln!(w, "").unwrap();
    }

    fn write_table_declarations(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "// Table declarations").unwrap();

        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            let table_name = &table.name;

            // Get primary key type
            let pk_field = &cfg.fields[table.primary_key];
            let pk_type = self.type_to_boogie(&pk_field.ty);

            // Declare each non-primary field as a map from primary key
            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    let field_type = self.type_to_boogie(&field.ty);
                    writeln!(
                        w,
                        "var {}_{}: [{}]{};",
                        table_name, field.name, pk_type, field_type
                    )
                    .unwrap();
                }
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_hop_procedures(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "// Hop procedures").unwrap();

        // Generate procedures for function 1 hops
        for &hop_id in &plan.relevant_hops_f1 {
            self.write_hop_procedure(w, plan.func1_id, hop_id, cfg);
        }

        // Generate procedures for function 2 hops
        for &hop_id in &plan.relevant_hops_f2 {
            self.write_hop_procedure(w, plan.func2_id, hop_id, cfg);
        }

        writeln!(w, "").unwrap();
    }

    fn write_hop_procedure(
        &self,
        w: &mut String,
        func_id: CfgFunctionId,
        hop_id: CfgHopId,
        cfg: &CfgProgram,
    ) {
        let func = &cfg.functions[func_id];
        let hop = &func.hops[hop_id];

        let proc_name = format!("{}_Hop{}", func.name, hop_id.index());

        // --- MODIFIED PARAMETER LOGIC ---
        let mut input_params: BTreeMap<VarId, (String, String)> = BTreeMap::new();
        for param_var_id in &func.parameters { // Iterate over VarIds in parameters
            let var_info = &func.variables[*param_var_id];
            // The type of the parameter is var_info.ty
            input_params.insert(*param_var_id, (var_info.name.clone(), self.type_to_boogie(&var_info.ty)));
        }

        // --- LOCAL VARIABLE LOGIC (Revised) ---
        let mut local_vars_to_declare: BTreeMap<VarId, (String, String)> = BTreeMap::new();
        let mut defined_in_hop: HashSet<VarId> = HashSet::new();

        // First pass: find all defined vars in this hop
        for &block_id in &hop.blocks {
            let block = &func.blocks[block_id];
            for stmt in &block.statements {
                if let Statement::Assign { var, .. } = stmt {
                    defined_in_hop.insert(*var);
                }
            }
        }

        // Determine local variables to declare
        for &var_id in &defined_in_hop {
            if !input_params.contains_key(&var_id) { // If it's defined in hop AND not a function parameter
                let var_info = &func.variables[var_id];
                local_vars_to_declare.entry(var_id).or_insert_with(|| (var_info.name.clone(), self.type_to_boogie(&var_info.ty)));
            }
        }
        // --- END OF MODIFIED LOGIC ---
        
        let params_str_vec: Vec<String> = input_params.values().map(|(name, ty)| format!("{}: {}", name, ty)).collect();
        let local_vars_decl_str_vec: Vec<String> = local_vars_to_declare.values().map(|(name, ty)| format!("{}: {}", name, ty)).collect();

        // Collect return values
        let mut returns_str_vec = Vec::new();
        for &block_id in &hop.blocks {
            let block = &func.blocks[block_id];
            if let Terminator::Return(Some(Operand::Var(var_id))) = &block.terminator {
                let var_info = &func.variables[*var_id]; // Corrected: Access variable info using VarId as index
                returns_str_vec.push(format!("{}: {}", var_info.name, self.type_to_boogie(&var_info.ty)));
                break; 
            }
        }

        // Collect modifies clauses
        let mut modifies_set = HashSet::new();
        for &block_id in &hop.blocks {
            let block = &func.blocks[block_id];
            for stmt in &block.statements {
                if let Statement::TableAssign { table, field, .. } = stmt {
                    let table_name = &cfg.tables[*table].name;
                    let field_name = &cfg.fields[*field].name;
                    modifies_set.insert(format!("{}_{}", table_name, field_name));
                }
            }
        }
        let modifies_str_vec: Vec<String> = modifies_set.into_iter().collect();


        // Write procedure signature
        write!(w, "procedure {{:inline 1}} {}(", proc_name).unwrap();
        if !params_str_vec.is_empty() {
            write!(w, "{}", params_str_vec.join(", ")).unwrap();
        }
        write!(w, ")").unwrap();

        if !returns_str_vec.is_empty() {
            write!(w, " returns ({})", returns_str_vec.join(", ")).unwrap();
        }
        writeln!(w, "").unwrap();

        if !modifies_str_vec.is_empty() {
            writeln!(w, "  modifies {};", modifies_str_vec.join(", ")).unwrap();
        }

        writeln!(w, "{{").unwrap();

        // Declare local variables
        for local_var_decl in &local_vars_decl_str_vec {
            writeln!(w, "  var {};", local_var_decl).unwrap();
        }
        if !local_vars_decl_str_vec.is_empty() {
            writeln!(w, "").unwrap();
        }

        // Generate hop body
        for &block_id in &hop.blocks {
            let block = &func.blocks[block_id];
            for stmt in &block.statements {
                self.write_statement(w, stmt, func, cfg);
            }
        }

        writeln!(w, "}}").unwrap();
        writeln!(w, "").unwrap();
    }

    fn write_main_procedure(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "procedure main()").unwrap();
        
        let mut modifies_vars = Vec::new();
        // Iterate over relevant tables specified in the plan for modifies clauses
        for &table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[table_id];
            // Each field of the table is a separate global variable in Boogie
            for &field_id in &table_info.fields {
                if field_id != table_info.primary_key { // Primary key itself is not a map, but its type is used in map keys
                    let field_info = &cfg.fields[field_id];
                    modifies_vars.push(format!("{}_{}", table_info.name, field_info.name));
                }
            }
        }

        if !modifies_vars.is_empty() {
            writeln!(w, "  modifies {};", modifies_vars.join(", ")).unwrap();
        }
        writeln!(w, "{{").unwrap();

        // --- Local variable declarations ---
        writeln!(w, "  // Local variables for verification").unwrap();
        let mut local_vars: BTreeMap<String, String> = BTreeMap::new(); // name -> type_str

        // 1. State-saving variables for global table fields
        for &table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[table_id];
            let pk_field_info = &cfg.fields[table_info.primary_key];
            let pk_boogie_type = self.type_to_boogie(&pk_field_info.ty);

            for &field_id in &table_info.fields {
                if field_id != table_info.primary_key {
                    let field_info = &cfg.fields[field_id];
                    let field_boogie_type = self.type_to_boogie(&field_info.ty);
                    let map_var_name = format!("{}_{}", table_info.name, field_info.name);
                    let boogie_map_type = format!("[{}] {}", pk_boogie_type, field_boogie_type);

                    local_vars.insert(format!("{}_init", map_var_name), boogie_map_type.clone());
                    local_vars.insert(format!("{}_serial1", map_var_name), boogie_map_type.clone());
                    local_vars.insert(format!("{}_serial2", map_var_name), boogie_map_type.clone());
                }
            }
        }

        // 2. Collect all functions involved in the plan to get their parameters.
        let mut involved_func_ids: HashSet<CfgFunctionId> = HashSet::new();
        involved_func_ids.insert(plan.func1_id);
        involved_func_ids.insert(plan.func2_id);
        // Interleavings in the plan use these functions too.

        // Add parameters of these functions to local_vars.
        let mut param_names_for_havoc: HashSet<String> = HashSet::new();
        for func_id in involved_func_ids {
            let func_cfg = &cfg.functions[func_id];
            for param_var_id in &func_cfg.parameters {
                let var_info = &func_cfg.variables[*param_var_id];
                local_vars.insert(var_info.name.clone(), self.type_to_boogie(&var_info.ty));
                param_names_for_havoc.insert(var_info.name.clone());
            }
        }

        // Write out the declarations for main's local variables
        for (name, ty) in &local_vars {
            writeln!(w, "  var {}: {};", name, ty).unwrap();
        }
        writeln!(w).unwrap();

        // --- Havoc initial state ---
        writeln!(w, "  // Havoc initial state").unwrap();
        // Havoc global table fields (which are Boogie global vars)
        for &table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[table_id];
            for &field_id in &table_info.fields {
                if field_id != table_info.primary_key {
                    let field_info = &cfg.fields[field_id];
                    writeln!(w, "  havoc {}_{};", table_info.name, field_info.name).unwrap();
                }
            }
        }
        // Havoc only the collected parameters
        for name in &param_names_for_havoc {
            writeln!(w, "  havoc {};", name).unwrap();
        }
        writeln!(w).unwrap();

        // Save initial state
        self.write_save_initial_state(w, plan, cfg);

        // Execute serial orders
        self.write_serial_executions(w, plan, cfg);

        // Execute interleavings and assert equivalence
        self.write_interleaving_executions(w, plan, cfg);

        writeln!(w, "}}").unwrap();
    }

    fn write_initial_havoc(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "  // Havoc initial state").unwrap();

        // Havoc table variables
        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    writeln!(w, "  havoc {}_{};", table.name, field.name).unwrap();
                }
            }
        }

        // Havoc function variables
        for (var_id, (func_id, _)) in &plan.global_vars {
            let func = &cfg.functions[*func_id];
            if let Some(var_info) = func.variables.get(*var_id) {
                writeln!(w, "  havoc {};", var_info.name).unwrap();
            }
        }

        writeln!(w, "").unwrap();
    }

    fn write_save_initial_state(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "  // Save initial state").unwrap();

        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    let var_name = format!("{}_{}", table.name, field.name);
                    writeln!(w, "  {}_init := {};", var_name, var_name).unwrap();
                }
            }
        }

        writeln!(w, "").unwrap();
    }

    fn write_serial_executions(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        let func1_name = &cfg.functions[plan.func1_id].name;
        let func2_name = &cfg.functions[plan.func2_id].name;

        writeln!(
            w,
            "  // Serial execution 1: {} then {}",
            func1_name, func2_name
        )
        .unwrap();
        self.write_reset_state(w, plan, cfg);
        self.write_execute_function_hops(w, plan.func1_id, &plan.relevant_hops_f1, cfg);
        self.write_execute_function_hops(w, plan.func2_id, &plan.relevant_hops_f2, cfg);
        self.write_save_serial_state(w, plan, cfg, "serial1");

        writeln!(
            w,
            "  // Serial execution 2: {} then {}",
            func2_name, func1_name
        )
        .unwrap();
        self.write_reset_state(w, plan, cfg);
        self.write_execute_function_hops(w, plan.func2_id, &plan.relevant_hops_f2, cfg);
        self.write_execute_function_hops(w, plan.func1_id, &plan.relevant_hops_f1, cfg);
        self.write_save_serial_state(w, plan, cfg, "serial2");

        writeln!(w, "").unwrap();
    }

    fn write_interleaving_executions(
        &self,
        w: &mut String,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        for (i, interleaving) in plan.interleavings.iter().enumerate() {
            writeln!(w, "  // Interleaving {}", i + 1).unwrap();
            self.write_reset_state(w, plan, cfg);

            for &(func_id, hop_id) in interleaving {
                let hop_func_cfg = &cfg.functions[func_id];
                let proc_name = format!("{}_Hop{}", hop_func_cfg.name, hop_id.index());

                // --- MODIFIED ARGUMENT LOGIC ---
                let mut call_arg_names: Vec<String> = Vec::new();
                for param_var_id in &hop_func_cfg.parameters { // Iterate over VarIds in parameters
                    let var_info = &hop_func_cfg.variables[*param_var_id];
                    call_arg_names.push(var_info.name.clone());
                }
                let args_str = call_arg_names.join(", ");
                // --- END OF MODIFIED ARGUMENT LOGIC ---

                writeln!(w, "  call {}({});", proc_name, args_str).unwrap();
            }

            // Assert equivalence to one of the serial executions
            self.write_serializability_assertion(w, plan, cfg);
        }
    }

    fn write_reset_state(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    let var_name = format!("{}_{}", table.name, field.name);
                    writeln!(w, "  {} := {}_init;", var_name, var_name).unwrap();
                }
            }
        }
    }

    fn write_execute_function_hops(
        &self,
        w: &mut String,
        func_id: CfgFunctionId,
        hops: &[CfgHopId],
        cfg: &CfgProgram,
    ) {
        let func_cfg = &cfg.functions[func_id];
        for &hop_id in hops {
            let proc_name = format!("{}_Hop{}", func_cfg.name, hop_id.index());
            
            // --- MODIFIED ARGUMENT LOGIC ---
            let mut call_arg_names: Vec<String> = Vec::new();
            for param_var_id in &func_cfg.parameters { // Iterate over VarIds in parameters
                let var_info = &func_cfg.variables[*param_var_id];
                call_arg_names.push(var_info.name.clone());
            }
            let args_str = call_arg_names.join(", ");
            // --- END OF MODIFIED ARGUMENT LOGIC ---

            writeln!(w, "  call {}({});", proc_name, args_str).unwrap();
        }
    }

    fn write_save_serial_state(
        &self,
        w: &mut String,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
        suffix: &str,
    ) {
        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    let var_name = format!("{}_{}", table.name, field.name);
                    writeln!(w, "  {}_{} := {};", var_name, suffix, var_name).unwrap();
                }
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_serializability_assertion(
        &self,
        w: &mut String,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        writeln!(w, "  // Assert serializability").unwrap();

        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            let pk_type = self.type_to_boogie(&cfg.fields[table.primary_key].ty);

            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    let var_name = format!("{}_{}", table.name, field.name);

                    writeln!(
                        w,
                        "  assert (forall k: {} :: {}[k] == {}_serial1[k] || {}[k] == {}_serial2[k]);",
                        pk_type, var_name, var_name, var_name, var_name
                    ).unwrap();
                }
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_statement(
        &self,
        w: &mut String,
        stmt: &Statement,
        func: &crate::cfg::FunctionCfg,
        cfg: &CfgProgram,
    ) {
        match stmt {
            Statement::Assign { var, rvalue, .. } => {
                let var_name = &func.variables[*var].name;
                let rvalue_str = self.rvalue_to_boogie(rvalue, func, cfg);
                writeln!(w, "  {} := {};", var_name, rvalue_str).unwrap();
            }
            Statement::TableAssign {
                table,
                field,
                pk_value,
                value,
                ..
            } => {
                let table_name = &cfg.tables[*table].name;
                let field_name = &cfg.fields[*field].name;
                let pk_str = self.operand_to_boogie(pk_value, func, cfg);
                let value_str = self.operand_to_boogie(value, func, cfg);
                writeln!(
                    w,
                    "  {}_{}[{}] := {};",
                    table_name, field_name, pk_str, value_str
                )
                .unwrap();
            }
        }
    }

    fn rvalue_to_boogie(
        &self,
        rvalue: &Rvalue,
        func: &crate::cfg::FunctionCfg,
        cfg: &CfgProgram,
    ) -> String {
        match rvalue {
            Rvalue::Use(operand) => self.operand_to_boogie(operand, func, cfg),
            Rvalue::TableAccess {
                table,
                field,
                pk_value,
                ..
            } => {
                let table_name = &cfg.tables[*table].name;
                let field_name = &cfg.fields[*field].name;
                let pk_str = self.operand_to_boogie(pk_value, func, cfg);
                format!("{}_{}[{}]", table_name, field_name, pk_str)
            }
            Rvalue::UnaryOp { op, operand } => {
                let op_str = self.unary_op_to_boogie(op);
                let operand_str = self.operand_to_boogie(operand, func, cfg);
                format!("{}{}", op_str, operand_str)
            }
            Rvalue::BinaryOp { op, left, right } => {
                let left_str = self.operand_to_boogie(left, func, cfg);
                let right_str = self.operand_to_boogie(right, func, cfg);
                let op_str = self.binary_op_to_boogie(op);
                format!("({} {} {})", left_str, op_str, right_str)
            }
        }
    }

    // Ensure this is the only definition of visit_rvalue_operands
    fn visit_rvalue_operands<F>(&self, rvalue: &Rvalue, visitor: &mut F)
    where F: FnMut(&Operand) {
        match rvalue {
            Rvalue::Use(op) => visitor(op),
            Rvalue::TableAccess { pk_value, .. } => visitor(pk_value),
            Rvalue::UnaryOp { operand, .. } => visitor(operand),
            Rvalue::BinaryOp { left, right, .. } => {
                visitor(left);
                visitor(right);
            }
        }
    }

    fn operand_to_boogie(
        &self,
        operand: &Operand,
        func: &crate::cfg::FunctionCfg,
        _cfg: &CfgProgram,
    ) -> String {
        match operand {
            Operand::Var(var_id) => func.variables[*var_id].name.clone(),
            Operand::Const(constant) => self.constant_to_boogie(constant),
        }
    }

    fn constant_to_boogie(&self, constant: &Constant) -> String {
        match constant {
            Constant::Int(i) => i.to_string(),
            Constant::Float(f) => f.to_string(),
            Constant::Bool(b) => b.to_string(),
            Constant::String(s) => format!("\"{}\"", s),
        }
    }

    fn type_to_boogie(&self, ty: &TypeName) -> String {
        match ty {
            TypeName::Int => "int".to_string(),
            TypeName::Float => "real".to_string(),
            TypeName::Bool => "bool".to_string(),
            TypeName::String => "string".to_string(),
        }
    }

    fn unary_op_to_boogie(&self, op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        }
    }

    fn binary_op_to_boogie(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}
