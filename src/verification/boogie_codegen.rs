use crate::cfg::{
    BinaryOp, CfgProgram, Constant, FunctionId as CfgFunctionId, HopId as CfgHopId, Operand,
    Rvalue, Statement, Terminator, TypeName, UnaryOp,
};
use crate::verification::verification_logic::VerificationPlan;
use std::fmt::Write;

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

        // Generate procedure name
        let proc_name = format!("{}_Hop{}", func.name, hop_id.index());

        // Collect parameters, return values, and modifies clauses
        let mut params = Vec::new();
        let mut returns = Vec::new();
        let mut modifies = Vec::new();
        let mut local_vars = Vec::new();

        // Analyze what this hop does
        for &block_id in &hop.blocks {
            let block = &func.blocks[block_id];

            // Check for return statements to determine return type
            match &block.terminator {
                Terminator::Return(Some(operand)) => {
                    if let Operand::Var(var_id) = operand {
                        let var_info = &func.variables[*var_id];
                        let var_type = self.type_to_boogie(&var_info.ty);
                        returns.push(format!("{}: {}", var_info.name, var_type));
                    }
                }
                _ => {}
            }

            // Analyze statements for modifies and local variables
            for stmt in &block.statements {
                match stmt {
                    Statement::Assign { var, .. } => {
                        let var_info = &func.variables[*var];
                        if var_info.is_parameter {
                            // This is a parameter, add to params if not already there
                            let param_str =
                                format!("{}: {}", var_info.name, self.type_to_boogie(&var_info.ty));
                            if !params.contains(&param_str) {
                                params.push(param_str);
                            }
                        } else {
                            // This is a local variable
                            let local_str =
                                format!("{}: {}", var_info.name, self.type_to_boogie(&var_info.ty));
                            if !local_vars.contains(&local_str) {
                                local_vars.push(local_str);
                            }
                        }
                    }
                    Statement::TableAssign { table, field, .. } => {
                        let table_name = &cfg.tables[*table].name;
                        let field_name = &cfg.fields[*field].name;
                        let modify_str = format!("{}_{}", table_name, field_name);
                        if !modifies.contains(&modify_str) {
                            modifies.push(modify_str);
                        }
                    }
                }
            }
        }

        // Write procedure signature
        write!(w, "procedure {}(", proc_name).unwrap();
        if !params.is_empty() {
            write!(w, "{}", params.join(", ")).unwrap();
        }
        write!(w, ")").unwrap();

        if !returns.is_empty() {
            write!(w, " returns ({})", returns.join(", ")).unwrap();
        }
        writeln!(w, "").unwrap();

        if !modifies.is_empty() {
            writeln!(w, "  modifies {};", modifies.join(", ")).unwrap();
        }

        writeln!(w, "{{").unwrap();

        // Declare local variables
        for local_var in &local_vars {
            writeln!(w, "  var {};", local_var).unwrap();
        }
        if !local_vars.is_empty() {
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

        // Collect all modifiable variables
        let mut modifies = Vec::new();

        // Add table variables
        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    modifies.push(format!("{}_{}", table.name, field.name));
                }
            }
        }

        if !modifies.is_empty() {
            writeln!(w, "  modifies {};", modifies.join(", ")).unwrap();
        }

        writeln!(w, "{{").unwrap();

        // Local variables for storing intermediate results and function variables
        self.write_main_locals(w, plan, cfg);

        // Havoc initial state
        self.write_initial_havoc(w, plan, cfg);

        // Save initial state
        self.write_save_initial_state(w, plan, cfg);

        // Execute serial orders
        self.write_serial_executions(w, plan, cfg);

        // Execute interleavings and assert equivalence
        self.write_interleaving_executions(w, plan, cfg);

        writeln!(w, "}}").unwrap();
    }

    fn write_main_locals(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "  // Local variables for verification").unwrap();

        // Variables to store initial and final states
        for &table_id in &plan.relevant_tables {
            let table = &cfg.tables[table_id];
            let pk_type = self.type_to_boogie(&cfg.fields[table.primary_key].ty);

            for &field_id in &table.fields {
                if field_id != table.primary_key {
                    let field = &cfg.fields[field_id];
                    let field_type = self.type_to_boogie(&field.ty);
                    let var_name = format!("{}_{}", table.name, field.name);

                    writeln!(w, "  var {}_init: [{}]{};", var_name, pk_type, field_type).unwrap();
                    writeln!(
                        w,
                        "  var {}_serial1: [{}]{};",
                        var_name, pk_type, field_type
                    )
                    .unwrap();
                    writeln!(
                        w,
                        "  var {}_serial2: [{}]{};",
                        var_name, pk_type, field_type
                    )
                    .unwrap();
                }
            }
        }

        // Function variables (parameters and globals from the plan)
        for (var_id, (func_id, type_name)) in &plan.global_vars {
            let func = &cfg.functions[*func_id];
            if let Some(var_info) = func.variables.get(*var_id) {
                let var_type = self.type_to_boogie(type_name);
                writeln!(w, "  var {}: {};", var_info.name, var_type).unwrap();
            }
        }

        writeln!(w, "").unwrap();
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
                let func_name = &cfg.functions[func_id].name;
                let proc_name = format!("{}_Hop{}", func_name, hop_id.index());
                writeln!(w, "  call {}();", proc_name).unwrap();
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
        let func_name = &cfg.functions[func_id].name;
        for &hop_id in hops {
            let proc_name = format!("{}_Hop{}", func_name, hop_id.index());
            writeln!(w, "  call {}();", proc_name).unwrap();
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
