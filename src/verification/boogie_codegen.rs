use crate::cfg::{
    BinaryOp, CfgProgram, Constant, FieldId, FunctionId as CfgFunctionId, HopId as CfgHopId,
    Operand, Rvalue, Statement, TableId, TypeName, UnaryOp, VarId,
};
use crate::verification::verification_logic::VerificationPlan;
use std::collections::HashSet;
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

        // Variable declarations
        self.write_variable_declarations(&mut w, plan, cfg);

        // Table declarations
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

    fn write_variable_declarations(
        &self,
        w: &mut String,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        writeln!(w, "// Global Variable Declarations").unwrap();
        for (var_id, (func_ctx_id, ty)) in &plan.global_vars {
            let base_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            let b_type = self.boogie_type_name(ty);
            writeln!(w, "var {}: {};", base_name, b_type).unwrap();
            writeln!(w, "var {}_init: {};", base_name, b_type).unwrap();
            writeln!(w, "var {}_serial1: {};", base_name, b_type).unwrap();
            writeln!(w, "var {}_serial2: {};", base_name, b_type).unwrap();
        }
        writeln!(w, "").unwrap();
    }

    fn write_table_declarations(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        writeln!(w, "// Table Declarations").unwrap();
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            let pk_type = "int"; // Simplified

            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }

                let field_info = &cfg.fields[*field_id];
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                let value_type = self.boogie_type_name(&field_info.ty);

                writeln!(w, "var {}: [{}]{};", map_name, pk_type, value_type).unwrap();
                writeln!(w, "var {}_init: [{}]{};", map_name, pk_type, value_type).unwrap();
                writeln!(w, "var {}_serial1: [{}]{};", map_name, pk_type, value_type).unwrap();
                writeln!(w, "var {}_serial2: [{}]{};", map_name, pk_type, value_type).unwrap();
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_hop_procedures(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        self.write_function_hops(w, plan.func1_id, &plan.relevant_hops_f1, plan, cfg);
        self.write_function_hops(w, plan.func2_id, &plan.relevant_hops_f2, plan, cfg);
    }

    fn write_function_hops(
        &self,
        w: &mut String,
        func_id: CfgFunctionId,
        hop_ids: &[CfgHopId],
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        let func_cfg = &cfg.functions[func_id];

        for hop_id in hop_ids {
            let hop_cfg = &func_cfg.hops[*hop_id];
            writeln!(w, "procedure Hop_{}_{}()", func_id.index(), hop_id.index()).unwrap();

            // Generate modifies clause
            let modifies = self.collect_modifies_for_hop(hop_cfg, func_cfg, plan, cfg);
            if !modifies.is_empty() {
                write!(w, "  modifies ").unwrap();
                for (i, name) in modifies.iter().enumerate() {
                    write!(w, "{}", name).unwrap();
                    if i < modifies.len() - 1 {
                        write!(w, ", ").unwrap();
                    }
                }
                writeln!(w, ";").unwrap();
            }

            writeln!(w, "{{").unwrap();

            // Generate procedure body
            for block_id in &hop_cfg.blocks {
                let block = &func_cfg.blocks[*block_id];
                for stmt in &block.statements {
                    self.write_statement(w, "  ", stmt, func_id, plan, cfg);
                }
            }

            writeln!(w, "}}").unwrap();
            writeln!(w, "").unwrap();
        }
    }

    fn write_main_procedure(&self, w: &mut String, plan: &VerificationPlan, cfg: &CfgProgram) {
        let indent = "  ";

        // Collect modifies for main
        let main_modifies = self.collect_modifies_for_main(plan, cfg);

        write!(w, "procedure main()").unwrap();
        if !main_modifies.is_empty() {
            writeln!(w).unwrap(); // Newline before modifies if it exists
            write!(w, "  modifies ").unwrap();
            for (i, name) in main_modifies.iter().enumerate() {
                write!(w, "{}", name).unwrap();
                if i < main_modifies.len() - 1 {
                    write!(w, ", ").unwrap();
                }
            }
            writeln!(w, ";").unwrap(); // Add semicolon and newline at the end of the modifies clause
        }
        writeln!(w, "{{").unwrap();

        // Havoc initial state
        self.write_havoc_section(w, indent, plan, cfg);

        // Store initial state
        self.write_store_initial_section(w, indent, plan, cfg);

        // Serial executions
        self.write_serial_executions(w, indent, plan, cfg);

        // Interleaved executions with assertions
        self.write_interleaved_executions(w, indent, plan, cfg);

        writeln!(w, "}}").unwrap();
    }

    fn collect_modifies_for_main(&self, plan: &VerificationPlan, cfg: &CfgProgram) -> Vec<String> {
        let mut modifies_set = HashSet::new();

        // Global variables and their variants
        for (var_id, (func_ctx_id, _)) in &plan.global_vars {
            let base_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            modifies_set.insert(base_name.clone());
            modifies_set.insert(format!("{}_init", base_name));
            modifies_set.insert(format!("{}_serial1", base_name));
            modifies_set.insert(format!("{}_serial2", base_name));
        }

        // Table maps and their variants
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                modifies_set.insert(map_name.clone());
                modifies_set.insert(format!("{}_init", map_name));
                modifies_set.insert(format!("{}_serial1", map_name));
                modifies_set.insert(format!("{}_serial2", map_name));
            }
        }
        // Sort for deterministic output, then collect into Vec
        let mut sorted_modifies: Vec<String> = modifies_set.into_iter().collect();
        sorted_modifies.sort();
        sorted_modifies
    }

    fn write_havoc_section(
        &self,
        w: &mut String,
        indent: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        writeln!(w, "{}// Havoc initial state", indent).unwrap();
        for (var_id, (func_ctx_id, _)) in &plan.global_vars {
            let var_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            writeln!(w, "{}havoc {};", indent, var_name).unwrap();
        }
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                writeln!(w, "{}havoc {};", indent, map_name).unwrap();
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_store_initial_section(
        &self,
        w: &mut String,
        indent: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        writeln!(w, "{}// Store initial state", indent).unwrap();
        for (var_id, (func_ctx_id, _)) in &plan.global_vars {
            let var_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            writeln!(w, "{}{}_init := {};", indent, var_name, var_name).unwrap();
        }
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                writeln!(w, "{}{}_init := {};", indent, map_name, map_name).unwrap();
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_serial_executions(
        &self,
        w: &mut String,
        indent: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        for (i, (func1_id, hops1, func2_id, hops2)) in plan.serial_orders.iter().enumerate() {
            let suffix = if i == 0 { "serial1" } else { "serial2" };
            writeln!(w, "{}// Serial Execution {}", indent, suffix).unwrap();

            self.write_reset_to_initial(w, indent, plan, cfg);

            // Execute first function's hops
            for hop_id in hops1 {
                writeln!(
                    w,
                    "{}call Hop_{}_{}();",
                    indent,
                    func1_id.index(),
                    hop_id.index()
                )
                .unwrap();
            }

            // Execute second function's hops
            for hop_id in hops2 {
                writeln!(
                    w,
                    "{}call Hop_{}_{}();",
                    indent,
                    func2_id.index(),
                    hop_id.index()
                )
                .unwrap();
            }

            // Store final state
            self.write_store_final_state(w, indent, suffix, plan, cfg);
        }
    }

    fn write_interleaved_executions(
        &self,
        w: &mut String,
        indent: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        writeln!(w, "{}// Interleaved Executions", indent).unwrap();

        for (i, interleaving) in plan.interleavings.iter().enumerate() {
            writeln!(w, "{}// Interleaving {}", indent, i).unwrap();

            self.write_reset_to_initial(w, indent, plan, cfg);

            // Execute interleaved hops
            for (func_id, hop_id) in interleaving {
                writeln!(
                    w,
                    "{}call Hop_{}_{}();",
                    indent,
                    func_id.index(),
                    hop_id.index()
                )
                .unwrap();
            }

            // Assertions
            self.write_serializability_assertions(w, indent, plan, cfg);
            writeln!(w, "").unwrap();
        }
    }

    fn write_reset_to_initial(
        &self,
        w: &mut String,
        indent: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        for (var_id, (func_ctx_id, _)) in &plan.global_vars {
            let var_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            writeln!(w, "{}{} := {}_init;", indent, var_name, var_name).unwrap();
        }
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                writeln!(w, "{}{} := {}_init;", indent, map_name, map_name).unwrap();
            }
        }
    }

    fn write_store_final_state(
        &self,
        w: &mut String,
        indent: &str,
        suffix: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        for (var_id, (func_ctx_id, _)) in &plan.global_vars {
            let var_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            writeln!(w, "{}{}_{} := {};", indent, var_name, suffix, var_name).unwrap();
        }
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                writeln!(w, "{}{}_{} := {};", indent, map_name, suffix, map_name).unwrap();
            }
        }
        writeln!(w, "").unwrap();
    }

    fn write_serializability_assertions(
        &self,
        w: &mut String,
        indent: &str,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        // Variable assertions
        for (var_id, (func_ctx_id, _)) in &plan.global_vars {
            let var_name = self.boogie_global_var_name(*var_id, *func_ctx_id, cfg);
            writeln!(
                w,
                "{}assert ({} == {}_serial1) || ({} == {}_serial2);",
                indent, var_name, var_name, var_name, var_name
            )
            .unwrap();
        }

        // Table assertions
        for table_id in &plan.relevant_tables {
            let table_info = &cfg.tables[*table_id];
            for field_id in &table_info.fields {
                if cfg.fields[*field_id].is_primary {
                    continue;
                }
                let map_name = self.boogie_table_map_name(*table_id, *field_id, cfg);
                writeln!(w, "{}assert (forall pk : int :: ({}[pk] == {}_serial1[pk]) || ({}[pk] == {}_serial2[pk]));",
                    indent, map_name, map_name, map_name, map_name).unwrap();
            }
        }
    }

    // Helper methods for naming and type conversion
    fn boogie_type_name(&self, ty: &TypeName) -> &'static str {
        match ty {
            TypeName::Int => "int",
            TypeName::Bool => "bool",
            TypeName::Float => "real",
            TypeName::String => "int",
        }
    }

    fn boogie_global_var_name(
        &self,
        var_id: VarId,
        func_id: CfgFunctionId,
        cfg: &CfgProgram,
    ) -> String {
        let var_info = &cfg.functions[func_id].variables[var_id];
        format!(
            "shared_f{}_{}_{}",
            func_id.index(),
            var_info.name,
            var_id.index()
        )
    }

    fn boogie_table_map_name(
        &self,
        table_id: TableId,
        field_id: FieldId,
        cfg: &CfgProgram,
    ) -> String {
        let table_info = &cfg.tables[table_id];
        let field_info = &cfg.fields[field_id];
        format!(
            "table_{}_{}_field_{}",
            table_info.name,
            table_id.index(),
            field_info.name
        )
    }

    fn collect_modifies_for_hop(
        &self,
        hop_cfg: &crate::cfg::HopCfg,
        func_cfg: &crate::cfg::FunctionCfg,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) -> Vec<String> {
        let mut modifies_set = HashSet::new();

        for block_id in &hop_cfg.blocks {
            let block = &func_cfg.blocks[*block_id];
            for stmt in &block.statements {
                match stmt {
                    Statement::Assign { var, .. } => {
                        if let Some((func_ctx_id, _)) = plan.global_vars.get(var) {
                            modifies_set.insert(self.boogie_global_var_name(
                                *var,
                                *func_ctx_id,
                                cfg,
                            ));
                        }
                    }
                    Statement::TableAssign { table, field, .. } => {
                        modifies_set.insert(self.boogie_table_map_name(*table, *field, cfg));
                    }
                }
            }
        }

        modifies_set.into_iter().collect()
    }

    fn write_statement(
        &self,
        w: &mut String,
        indent: &str,
        stmt: &Statement,
        func_id: CfgFunctionId,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) {
        match stmt {
            Statement::Assign { var, rvalue, .. } => {
                let var_name = if let Some((ctx_func_id, _)) = plan.global_vars.get(var) {
                    self.boogie_global_var_name(*var, *ctx_func_id, cfg)
                } else {
                    format!("local_var_{}_{}", var.index(), func_id.index()) // fallback
                };
                let rhs = self.model_rvalue(rvalue, func_id, plan, cfg);
                writeln!(w, "{}{} := {};", indent, var_name, rhs).unwrap();
            }
            Statement::TableAssign {
                table,
                pk_value,
                field,
                value,
                ..
            } => {
                let map_name = self.boogie_table_map_name(*table, *field, cfg);
                let pk_str = self.model_operand(pk_value, func_id, plan, cfg);
                let val_str = self.model_operand(value, func_id, plan, cfg);
                writeln!(w, "{}{}[{}] := {};", indent, map_name, pk_str, val_str).unwrap();
            }
        }
    }

    fn model_rvalue(
        &self,
        rvalue: &Rvalue,
        func_id: CfgFunctionId,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) -> String {
        match rvalue {
            Rvalue::Use(op) => self.model_operand(op, func_id, plan, cfg),
            Rvalue::TableAccess {
                table,
                pk_value,
                field,
                ..
            } => {
                let map_name = self.boogie_table_map_name(*table, *field, cfg);
                let pk_str = self.model_operand(pk_value, func_id, plan, cfg);
                format!("{}[{}]", map_name, pk_str)
            }
            Rvalue::UnaryOp { op, operand } => {
                let e_str = self.model_operand(operand, func_id, plan, cfg);
                match op {
                    UnaryOp::Not => format!("(!{})", e_str),
                    UnaryOp::Neg => format!("(-{})", e_str),
                }
            }
            Rvalue::BinaryOp { left, op, right } => {
                let l_str = self.model_operand(left, func_id, plan, cfg);
                let r_str = self.model_operand(right, func_id, plan, cfg);
                let op_str = match op {
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
                };
                format!("({} {} {})", l_str, op_str, r_str)
            }
        }
    }

    fn model_operand(
        &self,
        operand: &Operand,
        func_id: CfgFunctionId,
        plan: &VerificationPlan,
        cfg: &CfgProgram,
    ) -> String {
        match operand {
            Operand::Var(var_id) => {
                if let Some((ctx_func_id, _)) = plan.global_vars.get(var_id) {
                    self.boogie_global_var_name(*var_id, *ctx_func_id, cfg)
                } else {
                    format!("local_var_{}_{}", var_id.index(), func_id.index()) // fallback
                }
            }
            Operand::Const(c) => match c {
                Constant::Int(i) => i.to_string(),
                Constant::Bool(b) => b.to_string(),
                Constant::Float(f) => f.to_string(),
                Constant::String(s) => {
                    let hash = s.chars().fold(0u32, |acc, c| acc.wrapping_add(c as u32));
                    hash.to_string()
                }
            },
        }
    }
}
