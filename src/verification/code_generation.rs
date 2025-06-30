use super::commutativity_check::VerificationUnit;
use crate::ast::{BinaryOp, UnaryOp};
use crate::cfg::{
    BasicBlockId, CfgProgram, Constant, FunctionId, HopId, Operand, Rvalue, Statement, TableId,
    TypeName, VarId,
};
use std::collections::HashSet;

/// Structure to manage Boogie code generation
pub struct BoogieCodeGenerator<'a> {
    unit: &'a VerificationUnit,
    cfg: &'a CfgProgram,
    code: String,
    indent_level: usize,
}

impl<'a> BoogieCodeGenerator<'a> {
    pub fn new(unit: &'a VerificationUnit, cfg: &'a CfgProgram) -> Self {
        Self {
            unit,
            cfg,
            code: String::new(),
            indent_level: 0,
        }
    }

    /// Generate complete Boogie code for the verification unit
    pub fn generate(&mut self) -> String {
        self.generate_header_comment();
        self.generate_main_procedure();
        self.code.clone()
    }

    /// Generate header comment explaining the verification
    fn generate_header_comment(&mut self) {
        let func_a = &self.cfg.functions[self.unit.function_a];
        let func_b = &self.cfg.functions[self.unit.function_b];

        self.writeln("// -------------------------------------------------------------------");
        self.writeln(&format!(
            "// Commutativity verification for functions: {} and {}",
            func_a.name, func_b.name
        ));
        self.writeln(&format!(
            "// Testing if hops {} and {} commute",
            self.unit.final_a.index(),
            self.unit.final_b.index()
        ));
        self.writeln(&format!(
            "// Number of prefix interleavings: {}",
            self.unit.merges.len()
        ));
        self.writeln("// -------------------------------------------------------------------");
        self.writeln("");
    }

    /// Generate the main verification procedure
    fn generate_main_procedure(&mut self) {
        // Extract function parameters for the procedure signature
        let func_a = &self.cfg.functions[self.unit.function_a];
        let func_b = &self.cfg.functions[self.unit.function_b];

        // Generate procedure signature
        self.write("procedure main(");

        // Add parameters from both functions
        let mut params = Vec::new();
        for &param_id in &func_a.parameters {
            let param = &func_a.variables[param_id];
            params.push(format!(
                "{}: {}",
                param.name,
                self.type_to_boogie(&param.ty)
            ));
        }
        for &param_id in &func_b.parameters {
            let param = &func_b.variables[param_id];
            // Avoid duplicate parameter names by prefixing with function name
            params.push(format!(
                "{}_{}: {}",
                func_b.name,
                param.name,
                self.type_to_boogie(&param.ty)
            ));
        }

        self.write(&params.join(", "));
        self.writeln(") {");
        self.indent();

        // Generate ALL variable declarations first (Boogie requirement)
        self.generate_all_declarations();

        // Generate initialization and verification logic
        self.generate_verification_logic();

        self.dedent();
        self.writeln("}");
    }

    /// Generate all variable declarations at the top of the procedure
    fn generate_all_declarations(&mut self) {
        self.writeln("// -------------------------------------------------------------------");
        self.writeln("// All variable declarations (Boogie requirement: variables at top)");
        self.writeln("// -------------------------------------------------------------------");

        // Collect ALL tables used in any hop in the interleaving
        let all_tables = self.collect_all_tables_used();

        // 1. Declare table maps for ALL tables used
        for &table_id in &all_tables {
            let table = &self.cfg.tables[table_id];
            self.generate_table_declaration(table_id, table);
        }

        // 2. Declare backup tables for state restoration for ALL tables
        for &table_id in &all_tables {
            let table = &self.cfg.tables[table_id];
            self.generate_table_backup_declaration(table_id, table);
        }

        // 3. Declare final state tables for comparison (only for relevant tables)
        for &table_id in &self.unit.relevant_tables {
            let table = &self.cfg.tables[table_id];
            self.generate_final_state_declarations(table_id, table);
        }

        // 4. Declare local variables used in hops (ALL variables used in any hop)
        self.generate_all_variable_declarations();

        self.writeln("");
    }

    /// Generate declaration for a table as a Boogie map
    fn generate_table_declaration(&mut self, _table_id: TableId, table: &crate::cfg::TableInfo) {
        // Generate nested map type for primary keys
        let primary_key_types: Vec<String> = table
            .primary_keys
            .iter()
            .map(|&pk_id| {
                let primary_field = &self.cfg.fields[pk_id];
                self.type_to_boogie(&primary_field.ty)
            })
            .collect();

        // Generate map for each non-primary field
        for &field_id in &table.fields {
            let field = &self.cfg.fields[field_id];
            if !field.is_primary {
                // Build nested map type: [type1][type2]...[typeN]field_type
                let mut map_type = String::new();
                for key_type in &primary_key_types {
                    map_type.push_str(&format!("[{}]", key_type));
                }
                map_type.push_str(&self.type_to_boogie(&field.ty));

                self.writeln(&format!("var {}_{}: {};", table.name, field.name, map_type));
            }
        }
    }

    /// Generate backup declaration for table state restoration
    fn generate_table_backup_declaration(
        &mut self,
        _table_id: TableId,
        table: &crate::cfg::TableInfo,
    ) {
        // Generate nested map type for primary keys
        let primary_key_types: Vec<String> = table
            .primary_keys
            .iter()
            .map(|&pk_id| {
                let primary_field = &self.cfg.fields[pk_id];
                self.type_to_boogie(&primary_field.ty)
            })
            .collect();

        for &field_id in &table.fields {
            let field = &self.cfg.fields[field_id];
            if !field.is_primary {
                // Build nested map type: [type1][type2]...[typeN]field_type
                let mut map_type = String::new();
                for key_type in &primary_key_types {
                    map_type.push_str(&format!("[{}]", key_type));
                }
                map_type.push_str(&self.type_to_boogie(&field.ty));

                self.writeln(&format!(
                    "var init_{}_{}: {};",
                    table.name, field.name, map_type
                ));
            }
        }
    }

    /// Generate final state variable declarations
    fn generate_final_state_declarations(
        &mut self,
        _table_id: TableId,
        table: &crate::cfg::TableInfo,
    ) {
        // Generate nested map type for primary keys
        let primary_key_types: Vec<String> = table
            .primary_keys
            .iter()
            .map(|&pk_id| {
                let primary_field = &self.cfg.fields[pk_id];
                self.type_to_boogie(&primary_field.ty)
            })
            .collect();

        for &field_id in &table.fields {
            let field = &self.cfg.fields[field_id];
            if !field.is_primary {
                // Build nested map type: [type1][type2]...[typeN]field_type
                let mut map_type = String::new();
                for key_type in &primary_key_types {
                    map_type.push_str(&format!("[{}]", key_type));
                }
                map_type.push_str(&self.type_to_boogie(&field.ty));

                // Declare variables for both AB and BA final states
                self.writeln(&format!(
                    "var final_AB_{}_{}: {};",
                    table.name, field.name, map_type
                ));
                self.writeln(&format!(
                    "var final_BA_{}_{}: {};",
                    table.name, field.name, map_type
                ));
            }
        }
    }

    /// Collect ALL tables used in any hop in the interleaving
    fn collect_all_tables_used(&self) -> Vec<TableId> {
        let mut all_tables = HashSet::new();

        // Add tables from relevant_tables (what we compare)
        for &table_id in &self.unit.relevant_tables {
            all_tables.insert(table_id);
        }

        // Scan through all hops in both functions to find table accesses
        let func_a = &self.cfg.functions[self.unit.function_a];
        let func_b = &self.cfg.functions[self.unit.function_b];

        // Scan function A hops
        for (_hop_id, hop) in func_a.hops.iter() {
            for &block_id in &hop.blocks {
                let block = &func_a.blocks[block_id];
                for statement in &block.statements {
                    self.collect_tables_from_statement(statement, &mut all_tables);
                }
            }
        }

        // Scan function B hops
        for (_hop_id, hop) in func_b.hops.iter() {
            for &block_id in &hop.blocks {
                let block = &func_b.blocks[block_id];
                for statement in &block.statements {
                    self.collect_tables_from_statement(statement, &mut all_tables);
                }
            }
        }

        all_tables.into_iter().collect()
    }

    /// Helper method to collect table IDs from a statement
    fn collect_tables_from_statement(&self, statement: &Statement, tables: &mut HashSet<TableId>) {
        match statement {
            Statement::Assign { rvalue, .. } => {
                self.collect_tables_from_rvalue(rvalue, tables);
            }
            Statement::TableAssign { table, .. } => {
                tables.insert(*table);
            }
        }
    }

    /// Helper method to collect table IDs from an rvalue
    fn collect_tables_from_rvalue(&self, rvalue: &Rvalue, tables: &mut HashSet<TableId>) {
        match rvalue {
            Rvalue::Use(_) => {}
            Rvalue::TableAccess { table, .. } => {
                tables.insert(*table);
            }
            Rvalue::UnaryOp { operand, .. } => {
                if let Operand::Const(_) = operand {
                    // No tables in constants
                }
                // Variables don't contain table references
            }
            Rvalue::BinaryOp { left, right, .. } => {
                // Check operands for table references (though unlikely)
                if let Operand::Const(_) = left {
                    // No tables in constants
                }
                if let Operand::Const(_) = right {
                    // No tables in constants
                }
            }
        }
    }

    /// Generate ALL variable declarations (from all hops, not just relevant_vars)
    fn generate_all_variable_declarations(&mut self) {
        let mut declared_vars = HashSet::new();

        // Collect variables from both functions
        let func_a = &self.cfg.functions[self.unit.function_a];
        let func_b = &self.cfg.functions[self.unit.function_b];

        // Declare variables from function A (excluding parameters)
        for (_var_id, var) in func_a.variables.iter() {
            if !var.is_parameter && !declared_vars.contains(&var.name) {
                self.writeln(&format!(
                    "var {}: {};",
                    var.name,
                    self.type_to_boogie(&var.ty)
                ));
                declared_vars.insert(var.name.clone());
            }
        }

        // Declare variables from function B (excluding parameters, with prefix to avoid conflicts)
        for (_var_id, var) in func_b.variables.iter() {
            if !var.is_parameter {
                let prefixed_name = format!("{}_{}", func_b.name, var.name);
                if !declared_vars.contains(&prefixed_name) {
                    self.writeln(&format!(
                        "var {}: {};",
                        prefixed_name,
                        self.type_to_boogie(&var.ty)
                    ));
                    declared_vars.insert(prefixed_name);
                }
            }
        }
    }

    /// Generate the verification logic (after all declarations)
    fn generate_verification_logic(&mut self) {
        // Collect ALL tables used in the interleaving
        let all_tables = self.collect_all_tables_used();

        self.writeln("// -------------------------------------------------------------------");
        self.writeln("// Initialize tables with havoc");
        self.writeln("// -------------------------------------------------------------------");

        // Havoc ALL tables that are used
        for &table_id in &all_tables {
            let table = &self.cfg.tables[table_id];
            for &field_id in &table.fields {
                let field = &self.cfg.fields[field_id];
                if !field.is_primary {
                    self.writeln(&format!("havoc {}_{};", table.name, field.name));
                }
            }
        }

        // Save initial state for ALL tables
        self.writeln("");
        self.writeln("// Save initial state");
        for &table_id in &all_tables {
            let table = &self.cfg.tables[table_id];
            for &field_id in &table.fields {
                let field = &self.cfg.fields[field_id];
                if !field.is_primary {
                    self.writeln(&format!(
                        "init_{}_{} := {}_{};",
                        table.name, field.name, table.name, field.name
                    ));
                }
            }
        }

        self.writeln("");

        // Generate checks for each prefix interleaving
        for (i, merge) in self.unit.merges.iter().enumerate() {
            self.generate_single_interleaving_check(i, merge, &all_tables);
        }
    }

    /// Generate verification check for a single prefix interleaving
    fn generate_single_interleaving_check(
        &mut self,
        merge_index: usize,
        merge: &[HopId],
        all_tables: &[TableId],
    ) {
        self.writeln("// -------------------------------------------------------------------");
        self.writeln(&format!("// Merge #{}: Prefix interleaving", merge_index));
        self.writeln("// -------------------------------------------------------------------");

        // First execution: prefix + [final_a, final_b]
        self.writeln("// First execution: [final_a, final_b]");
        self.generate_hop_sequence(merge);
        self.generate_hop_execution(self.unit.final_a, self.unit.function_a);
        self.generate_hop_execution(self.unit.final_b, self.unit.function_b);

        // Save final state A->B (only for relevant tables)
        self.generate_state_save("AB");

        // Restore initial state for ALL tables
        self.writeln("");
        self.writeln("// Restore initial state");
        for &table_id in all_tables {
            let table = &self.cfg.tables[table_id];
            for &field_id in &table.fields {
                let field = &self.cfg.fields[field_id];
                if !field.is_primary {
                    self.writeln(&format!(
                        "{}_{} := init_{}_{};",
                        table.name, field.name, table.name, field.name
                    ));
                }
            }
        }

        // Second execution: prefix + [final_b, final_a]
        self.writeln("");
        self.writeln("// Second execution: [final_b, final_a]");
        self.generate_hop_sequence(merge);
        self.generate_hop_execution(self.unit.final_b, self.unit.function_b);
        self.generate_hop_execution(self.unit.final_a, self.unit.function_a);

        // Save final state B->A (only for relevant tables)
        self.generate_state_save("BA");

        // Compare states (only for relevant tables)
        self.writeln("");
        self.writeln("// Compare final states");
        self.generate_state_comparison();

        self.writeln("");
    }

    /// Generate execution sequence for a list of hops
    fn generate_hop_sequence(&mut self, hops: &[HopId]) {
        for &hop_id in hops {
            // Determine which function this hop belongs to
            let function_id = self.find_function_for_hop(hop_id);
            self.generate_hop_execution(hop_id, function_id);
        }
    }

    /// Generate Boogie code for executing a single hop
    fn generate_hop_execution(&mut self, hop_id: HopId, function_id: FunctionId) {
        let function = &self.cfg.functions[function_id];
        let hop = &function.hops[hop_id];
        let node = &self.cfg.nodes[hop.node_id];

        self.writeln(&format!(
            "// Executing hop {} on {}",
            hop_id.index(),
            node.name
        ));

        // Generate unique execution context identifier for this hop execution
        let exec_id = self.get_next_execution_id();

        // Start execution at the entry block
        if let Some(entry_block) = hop.entry_block {
            self.writeln(&format!(
                "goto bb_{}_{}_{};",
                exec_id,
                function_id.index(),
                entry_block.index()
            ));
        }

        // Generate all basic block labels first
        for &block_id in &hop.blocks {
            self.generate_basic_block_with_unique_label(block_id, function_id, exec_id);
        }

        // Add end label for end of this hop
        self.writeln(&format!("hop_end_{}:", exec_id));
    }

    /// Generate a basic block with unique label and goto-based control flow
    fn generate_basic_block_with_unique_label(
        &mut self,
        block_id: BasicBlockId,
        function_id: FunctionId,
        exec_id: usize,
    ) {
        let function = &self.cfg.functions[function_id];
        let block = &function.blocks[block_id];

        // Generate unique label for this basic block execution
        self.writeln(&format!(
            "bb_{}_{}_{} :",
            exec_id,
            function_id.index(),
            block_id.index()
        ));
        self.indent();

        // Execute all statements in the block
        for statement in &block.statements {
            self.generate_statement_execution(statement, function_id);
        }

        // Handle terminator with goto using unique labels
        self.generate_terminator_with_unique_goto(&block.terminator, function_id, exec_id);

        self.dedent();
        self.writeln("");
    }

    /// Generate terminator using goto statements with unique labels
    fn generate_terminator_with_unique_goto(
        &mut self,
        terminator: &crate::cfg::Terminator,
        function_id: FunctionId,
        exec_id: usize,
    ) {
        match terminator {
            crate::cfg::Terminator::Goto(target_block) => {
                self.writeln(&format!(
                    "goto bb_{}_{}_{};",
                    exec_id,
                    function_id.index(),
                    target_block.index()
                ));
            }
            crate::cfg::Terminator::Branch {
                condition,
                then_block,
                else_block,
            } => {
                let condition_code = self.generate_operand(condition, function_id);
                self.writeln(&format!("if ({}) {{", condition_code));
                self.indent();
                self.writeln(&format!(
                    "goto bb_{}_{}_{};",
                    exec_id,
                    function_id.index(),
                    then_block.index()
                ));
                self.dedent();
                self.writeln("} else {");
                self.indent();
                self.writeln(&format!(
                    "goto bb_{}_{}_{};",
                    exec_id,
                    function_id.index(),
                    else_block.index()
                ));
                self.dedent();
                self.writeln("}");
            }
            crate::cfg::Terminator::Return(return_value) => {
                match return_value {
                    Some(operand) => {
                        let return_code = self.generate_operand(operand, function_id);
                        self.writeln(&format!("// Return value: {}", return_code));
                    }
                    None => {
                        self.writeln("// Return (void)");
                    }
                }
                // Treat return as goto to hop end
                self.writeln(&format!("goto hop_end_{};", exec_id));
            }
            crate::cfg::Terminator::Abort => {
                self.writeln("// Abort");
                // Treat abort as goto to hop end
                self.writeln(&format!("goto hop_end_{};", exec_id));
            }
            crate::cfg::Terminator::HopExit { .. } => {
                self.writeln("// Hop exit");
                // Treat hop exit as goto to hop end
                self.writeln(&format!("goto hop_end_{};", exec_id));
            }
        }
    }

    /// Get next unique execution ID for generating unique labels
    fn get_next_execution_id(&mut self) -> usize {
        // Use the current length of the code as a simple way to generate unique IDs
        // This works because each hop execution adds code, making the length unique
        self.code.lines().count()
    }

    /// Generate Boogie code for executing a statement
    fn generate_statement_execution(&mut self, statement: &Statement, function_id: FunctionId) {
        match statement {
            Statement::Assign { var, rvalue, .. } => {
                let var_name = self.get_variable_name(*var, function_id);
                let rvalue_code = self.generate_rvalue(rvalue, function_id);
                self.writeln(&format!("{} := {};", var_name, rvalue_code));
            }
            Statement::TableAssign {
                table,
                pk_fields: _,
                pk_values,
                field,
                value,
                ..
            } => {
                let table_info = &self.cfg.tables[*table];
                let field_info = &self.cfg.fields[*field];
                let value_code = self.generate_operand(value, function_id);
                let table_field_name = format!("{}_{}", table_info.name, field_info.name);

                if pk_values.len() == 1 {
                    // Single key case: table_field := table_field[k1 := value]
                    let pk_value_code = self.generate_operand(&pk_values[0], function_id);
                    self.writeln(&format!(
                        "{} := {}[{} := {}];",
                        table_field_name, table_field_name, pk_value_code, value_code
                    ));
                } else {
                    // Multi-key case: nested map update
                    // For keys [k1, k2, ..., kn], the pattern is:
                    // table[k1] := table[k1][k2 := table[k1][k2][k3 := ... table[k1][k2]...[kn-1][kn := value]...]]

                    // Build from the innermost to outermost
                    let mut inner_expr = value_code;

                    // Work backwards through the keys
                    for i in (1..pk_values.len()).rev() {
                        let key_code = self.generate_operand(&pk_values[i], function_id);

                        // Build the access path up to this level: table[k1][k2]...[ki-1]
                        let mut access_path = table_field_name.clone();
                        for j in 0..i {
                            let access_key_code = self.generate_operand(&pk_values[j], function_id);
                            access_path.push_str(&format!("[{}]", access_key_code));
                        }

                        // Update: access_path[ki := inner_expr]
                        inner_expr = format!("{}[{} := {}]", access_path, key_code, inner_expr);
                    }

                    // Finally, update the outermost level
                    let first_key_code = self.generate_operand(&pk_values[0], function_id);
                    self.writeln(&format!(
                        "{} := {}[{} := {}];",
                        table_field_name, table_field_name, first_key_code, inner_expr
                    ));
                }
            }
        }
    }

    /// Generate Boogie code for an rvalue
    fn generate_rvalue(&self, rvalue: &Rvalue, function_id: FunctionId) -> String {
        match rvalue {
            Rvalue::Use(operand) => self.generate_operand(operand, function_id),
            Rvalue::TableAccess {
                table,
                pk_fields: _,
                pk_values,
                field,
            } => {
                let table_info = &self.cfg.tables[*table];
                let field_info = &self.cfg.fields[*field];

                // Generate nested map access: table_field[key1][key2]...[keyN]
                let mut access_code = format!("{}_{}", table_info.name, field_info.name);
                for pk_value in pk_values {
                    let pk_value_code = self.generate_operand(pk_value, function_id);
                    access_code.push_str(&format!("[{}]", pk_value_code));
                }
                access_code
            }
            Rvalue::UnaryOp { op, operand } => {
                let operand_code = self.generate_operand(operand, function_id);
                let op_str = self.unary_op_to_boogie(op);
                format!("({} {})", op_str, operand_code)
            }
            Rvalue::BinaryOp { op, left, right } => {
                let left_code = self.generate_operand(left, function_id);
                let right_code = self.generate_operand(right, function_id);
                let op_str = self.binary_op_to_boogie(op);
                format!("({} {} {})", left_code, op_str, right_code)
            }
        }
    }

    /// Generate Boogie code for an operand
    fn generate_operand(&self, operand: &Operand, function_id: FunctionId) -> String {
        match operand {
            Operand::Var(var_id) => self.get_variable_name(*var_id, function_id),
            Operand::Const(constant) => self.generate_constant(constant),
        }
    }

    /// Generate Boogie code for a constant
    fn generate_constant(&self, constant: &Constant) -> String {
        match constant {
            Constant::Int(i) => i.to_string(),
            Constant::Float(f) => f.to_string(),
            Constant::Bool(b) => b.to_string(),
            Constant::String(s) => format!("\"{}\"", s),
        }
    }

    /// Get the appropriate variable name, handling function prefixes
    fn get_variable_name(&self, var_id: VarId, function_id: FunctionId) -> String {
        let function = &self.cfg.functions[function_id];
        let var = &function.variables[var_id];

        if function_id == self.unit.function_b {
            // Prefix variables from function B to avoid naming conflicts
            format!("{}_{}", function.name, var.name)
        } else {
            var.name.clone()
        }
    }

    /// Find which function contains a given hop
    fn find_function_for_hop(&self, hop_id: HopId) -> FunctionId {
        // Check if the hop belongs to function A
        if self.cfg.functions[self.unit.function_a]
            .hops
            .get(hop_id)
            .is_some()
        {
            return self.unit.function_a;
        }
        // Otherwise, it should belong to function B
        self.unit.function_b
    }

    /// Generate code to save the current state with a suffix
    fn generate_state_save(&mut self, suffix: &str) {
        self.writeln(&format!("// Save final state {}", suffix));
        for &table_id in &self.unit.relevant_tables {
            let table = &self.cfg.tables[table_id];
            for &field_id in &table.fields {
                let field = &self.cfg.fields[field_id];
                if !field.is_primary {
                    self.writeln(&format!(
                        "final_{}_{}_{} := {}_{};",
                        suffix, table.name, field.name, table.name, field.name
                    ));
                }
            }
        }
    }

    /// Generate code to compare final states
    fn generate_state_comparison(&mut self) {
        for &table_id in &self.unit.relevant_tables {
            let table = &self.cfg.tables[table_id];
            for &field_id in &table.fields {
                let field = &self.cfg.fields[field_id];
                if !field.is_primary {
                    // Generate forall assertion for nested maps
                    if table.primary_keys.len() == 1 {
                        // Single key case
                        self.writeln(&format!(
                            "assert (forall k: {} :: final_AB_{}_{} [k] == final_BA_{}_{} [k]);",
                            self.type_to_boogie(&self.cfg.fields[table.primary_keys[0]].ty),
                            table.name,
                            field.name,
                            table.name,
                            field.name
                        ));
                    } else {
                        // Multi-key case: nested forall
                        // For keys [k1, k2, ..., kn], generate:
                        // assert (forall k1: type1, k2: type2, ..., kn: typen ::
                        //         final_AB_table_field[k1][k2]...[kn] == final_BA_table_field[k1][k2]...[kn]);

                        let mut forall_vars = Vec::new();
                        let mut access_suffix = String::new();

                        for (i, &pk_id) in table.primary_keys.iter().enumerate() {
                            let pk_field = &self.cfg.fields[pk_id];
                            let var_name = format!("k{}", i + 1);
                            forall_vars.push(format!(
                                "{}: {}",
                                var_name,
                                self.type_to_boogie(&pk_field.ty)
                            ));
                            access_suffix.push_str(&format!("[{}]", var_name));
                        }

                        self.writeln(&format!(
                            "assert (forall {} :: final_AB_{}_{}{}  == final_BA_{}_{}{}  );",
                            forall_vars.join(", "),
                            table.name,
                            field.name,
                            access_suffix,
                            table.name,
                            field.name,
                            access_suffix
                        ));
                    }
                }
            }
        }
    }

    /// Convert a UnaryOp to Boogie operator string
    fn unary_op_to_boogie(&self, op: &UnaryOp) -> String {
        match op {
            UnaryOp::Not => "!".to_string(),
            UnaryOp::Neg => "-".to_string(),
        }
    }

    /// Convert a BinaryOp to Boogie operator string
    fn binary_op_to_boogie(&self, op: &BinaryOp) -> String {
        match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Lt => "<".to_string(),
            BinaryOp::Lte => "<=".to_string(),
            BinaryOp::Gt => ">".to_string(),
            BinaryOp::Gte => ">=".to_string(),
            BinaryOp::Eq => "==".to_string(),
            BinaryOp::Neq => "!=".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
        }
    }

    /// Convert a TypeName to Boogie type string
    fn type_to_boogie(&self, ty: &TypeName) -> String {
        match ty {
            TypeName::Int => "int".to_string(),
            TypeName::Float => "real".to_string(),
            TypeName::Bool => "bool".to_string(),
            TypeName::String => "string".to_string(), // Note: Boogie doesn't have native strings
        }
    }

    /// Write a line with current indentation
    fn writeln(&mut self, line: &str) {
        self.write_indent();
        self.code.push_str(line);
        self.code.push('\n');
    }

    /// Write text without newline
    fn write(&mut self, text: &str) {
        self.code.push_str(text);
    }

    /// Write current indentation
    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.code.push_str("    ");
        }
    }

    /// Increase indentation level
    fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }
}

/// Generate Boogie code with CFG access
pub fn generate_boogie_for_unit_with_cfg(unit: &VerificationUnit, cfg: &CfgProgram) -> String {
    let mut generator = BoogieCodeGenerator::new(unit, cfg);
    generator.generate()
}
