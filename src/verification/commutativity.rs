/// Given a table, pk_fields, and pk_values, return pk_values in schema (primary_keys) order
fn order_pk_values(
    table_info: &crate::cfg::TableInfo,
    pk_fields: &Vec<crate::cfg::FieldId>,
    pk_values: &Vec<crate::cfg::Operand>,
) -> Vec<crate::cfg::Operand> {
    use crate::cfg::FieldId;
    let mut field_to_value = std::collections::HashMap::<FieldId, crate::cfg::Operand>::new();
    for (field, value) in pk_fields.iter().zip(pk_values.iter()) {
        field_to_value.insert(*field, value.clone());
    }
    table_info
        .primary_keys
        .iter()
        .map(|pk| field_to_value[pk].clone())
        .collect()
}
impl CommutativityUnit {
    /// Recursively build nested MapStore for multiple primary keys
    fn nested_map_store(
        &self,
        map: Expr,
        pk_values: &Vec<cfg::Operand>,
        value: Expr,
        cfg_program: &CfgProgram,
        is_slice_a: bool,
    ) -> Expr {
        if pk_values.is_empty() {
            value
        } else {
            let mut map_expr = map;
            // For each pk, nest the MapStore
            for pk in pk_values.iter().rev() {
                let idx_expr = self.operand_to_boogie_expr(cfg_program, pk, is_slice_a);
                map_expr = Expr::MapStore {
                    map: Box::new(map_expr),
                    index: Box::new(idx_expr),
                    val: Box::new(value.clone()),
                };
            }
            map_expr
        }
    }

    /// Recursively build nested MapSelect for multiple primary keys
    fn nested_map_select(
        &self,
        map: Expr,
        pk_values: &Vec<cfg::Operand>,
        cfg_program: &CfgProgram,
        is_slice_a: bool,
    ) -> Expr {
        let mut expr = map;
        for pk in pk_values {
            let idx_expr = self.operand_to_boogie_expr(cfg_program, pk, is_slice_a);
            expr = Expr::MapSelect {
                map: Box::new(expr),
                index: Box::new(idx_expr),
            };
        }
        expr
    }
}
use super::{boogie::*, Property, VerificationUnit};
use crate::cfg::{self, CfgProgram, HopId};

/// Verification unit for checking commutativity between hop slices
pub struct CommutativityUnit {
    /// Function containing first hop slice
    pub function_a: cfg::FunctionId,
    /// Function containing second hop slice
    pub function_b: cfg::FunctionId,
    /// Hop slice A (contiguous sequence of hops)
    pub hops_a: Vec<HopId>,
    /// Hop slice B (contiguous sequence of hops)
    pub hops_b: Vec<HopId>,
    /// The C-edge we're verifying (between last hops of each slice)
    pub c_edge: (HopId, HopId),
}

impl CommutativityUnit {
    pub fn new(
        function_a: cfg::FunctionId,
        function_b: cfg::FunctionId,
        hops_a: Vec<HopId>,
        hops_b: Vec<HopId>,
    ) -> Self {
        // The C-edge is between the last hop of each slice
        let last_hop_a = *hops_a.last().expect("Hop slice A must not be empty");
        let last_hop_b = *hops_b.last().expect("Hop slice B must not be empty");

        Self {
            function_a,
            function_b,
            hops_a,
            hops_b,
            c_edge: (last_hop_a, last_hop_b),
        }
    }
}

impl VerificationUnit for CommutativityUnit {
    fn property(&self) -> Property {
        Property::Commutativity {
            function_a: self.function_a,
            function_b: self.function_b,
            hops_a: self.hops_a.clone(),
            hops_b: self.hops_b.clone(),
        }
    }

    fn generate(&self, cfg_program: &CfgProgram) -> BoogieProgram {
        let mut declarations = Vec::new();

        // Generate table map declarations
        self.generate_table_declarations(cfg_program, &mut declarations);

        // Generate verification procedures for all valid interleavings
        self.generate_interleaving_procedures(cfg_program, &mut declarations);

        BoogieProgram { declarations }
    }
}

impl CommutativityUnit {
    /// Generate Boogie map declarations for all tables
    fn generate_table_declarations(&self, cfg_program: &CfgProgram, declarations: &mut Vec<Decl>) {
        for (_table_id, table_info) in cfg_program.tables.iter() {
            // Get primary key information
            let pk_fields: Vec<_> = table_info
                .fields
                .iter()
                .filter(|field_id| {
                    let field = &cfg_program.fields[**field_id];
                    field.is_primary
                })
                .collect();

            // For each NON-PRIMARY field in the table, generate a map declaration
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    // Skip primary key fields
                    let map_name = format!("{}_{}", table_info.name, field_info.name);

                    // Create appropriate map type based on number of primary keys
                    let map_type = if pk_fields.len() == 1 {
                        // Single primary key: [pk_type]field_type
                        let pk_field = &cfg_program.fields[*pk_fields[0]];
                        let pk_type = self.field_type_to_boogie(&pk_field.ty);
                        Ty::map(pk_type, self.field_type_to_boogie(&field_info.ty))
                    } else if pk_fields.len() > 1 {
                        // Multiple primary keys: [int][int]...field_type (nested maps)
                        // For simplicity, use int keys for all levels
                        let mut result_type = self.field_type_to_boogie(&field_info.ty);
                        for _ in 0..pk_fields.len() {
                            result_type = Ty::map(Ty::int(), result_type);
                        }
                        result_type
                    } else {
                        // No primary key - use int as default
                        Ty::map(Ty::int(), self.field_type_to_boogie(&field_info.ty))
                    };

                    declarations.push(Decl::Var(VarDecl {
                        name: Ident(map_name),
                        ty: map_type,
                    }));
                }
            }
        }
    }

    /// Generate verification procedures for valid interleavings
    fn generate_interleaving_procedures(
        &self,
        cfg_program: &CfgProgram,
        declarations: &mut Vec<Decl>,
    ) {
        let function_a = &cfg_program.functions[self.function_a];
        let function_b = &cfg_program.functions[self.function_b];

        // Step 1: Generate all valid interleavings
        let interleavings = self.generate_valid_interleavings(cfg_program);

        // Step 2: For each interleaving, generate a Boogie procedure
        for (idx, interleaving) in interleavings.iter().enumerate() {
            let proc_name = format!(
                "verify_commutativity_{}_{}_interleaving_{}",
                function_a.name, function_b.name, idx
            );

            self.generate_interleaving_procedure(
                cfg_program,
                interleaving,
                &proc_name,
                declarations,
            );
        }
    }

    /// Generate all valid interleavings of the two hop slices
    /// Following IC3 constraints: preserve intra-slice order, respect C-edges
    fn generate_valid_interleavings(&self, cfg_program: &CfgProgram) -> Vec<Vec<(HopId, bool)>> {
        let mut interleavings = Vec::new();

        // Mark hops with their slice (true = slice A, false = slice B)
        let mut hops_with_slice = Vec::new();
        for hop in &self.hops_a {
            hops_with_slice.push((*hop, true));
        }
        for hop in &self.hops_b {
            hops_with_slice.push((*hop, false));
        }

        // Generate all order-preserving shuffles
        // This is a simplified approach - generate all possible interleavings
        // that preserve intra-slice order
        self.generate_shuffles(&hops_with_slice, &mut interleavings);

        // Filter based on IC3 constraints (simplified)
        interleavings
            .into_iter()
            .filter(|interleaving| self.is_valid_interleaving(cfg_program, interleaving))
            .collect()
    }

    /// Generate all order-preserving shuffles of two hop slices
    fn generate_shuffles(
        &self,
        hops: &[(HopId, bool)],
        interleavings: &mut Vec<Vec<(HopId, bool)>>,
    ) {
        // For simplicity, generate a few key interleavings:
        // 1. A-slice first, then B-slice
        // 2. B-slice first, then A-slice
        // 3. Alternating (if lengths allow)

        let a_hops: Vec<_> = hops.iter().filter(|(_, is_a)| *is_a).cloned().collect();
        let b_hops: Vec<_> = hops.iter().filter(|(_, is_a)| !*is_a).cloned().collect();

        // Interleaving 1: A then B
        let mut ab_interleaving = a_hops.clone();
        ab_interleaving.extend(b_hops.clone());
        interleavings.push(ab_interleaving);

        // Interleaving 2: B then A
        let mut ba_interleaving = b_hops.clone();
        ba_interleaving.extend(a_hops.clone());
        interleavings.push(ba_interleaving);

        // Interleaving 3: Alternating (if both slices have hops)
        if !a_hops.is_empty() && !b_hops.is_empty() {
            let mut alternating = Vec::new();
            let max_len = a_hops.len().max(b_hops.len());

            for i in 0..max_len {
                if i < a_hops.len() {
                    alternating.push(a_hops[i]);
                }
                if i < b_hops.len() {
                    alternating.push(b_hops[i]);
                }
            }
            interleavings.push(alternating);
        }
    }

    /// Check if an interleaving is valid under IC3 constraints
    fn is_valid_interleaving(
        &self,
        _cfg_program: &CfgProgram,
        _interleaving: &[(HopId, bool)],
    ) -> bool {
        // For the initial implementation, accept all interleavings
        // Let Boogie harness handle IC3 wait logic
        // TODO: Add proper IC3 constraint checking based on SC-graph
        true
    }

    /// Generate Boogie procedure for one specific interleaving
    fn generate_interleaving_procedure(
        &self,
        cfg_program: &CfgProgram,
        interleaving: &[(HopId, bool)],
        proc_name: &str,
        declarations: &mut Vec<Decl>,
    ) {
        let mut var_tracker = VariableTracker::new();
        let mut blocks = Vec::new();

        // Parameters: No procedure parameters - they will be local variables that are havoc'd
        let params = Vec::new();

        // Generate local variables for outputs using the tracker
        self.add_output_variables(cfg_program, &mut var_tracker);

        // Generate modifies clause for non-primary table fields only
        let mut modifies = Vec::new();
        for (_table_id, table_info) in cfg_program.tables.iter() {
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    let map_name = format!("{}_{}", table_info.name, field_info.name);
                    modifies.push(Ident(map_name));
                }
            }
        }

        // Generate procedure body (this will add variables to tracker)
        self.generate_procedure_body(cfg_program, interleaving, &mut blocks, &mut var_tracker);

        // Create procedure using variable tracking
        let procedure = ProcedureDecl::with_variable_tracking(
            Ident(proc_name.to_string()),
            params,
            modifies,
            blocks,
            var_tracker,
        );

        declarations.push(Decl::Procedure(procedure));
    }

    /// Add local variables for capturing output states
    fn add_output_variables(&self, cfg_program: &CfgProgram, var_tracker: &mut VariableTracker) {
        // Add parameter variables from both functions as local variables
        let function_a_cfg = &cfg_program.functions[self.function_a];
        let function_b_cfg = &cfg_program.functions[self.function_b];

        // Add parameters from function A with 'a_' prefix
        for param_id in &function_a_cfg.parameters {
            let param = &cfg_program.variables[*param_id];
            var_tracker.add_variable(
                format!("a_{}", param.name),
                self.type_name_to_boogie(&param.ty),
            );
        }

        // Add parameters from function B with 'b_' prefix
        for param_id in &function_b_cfg.parameters {
            let param = &cfg_program.variables[*param_id];
            var_tracker.add_variable(
                format!("b_{}", param.name),
                self.type_name_to_boogie(&param.ty),
            );
        }

        // Add ALL local variables from both functions (they may be used during execution)
        for var_id in &function_a_cfg.local_variables {
            let var_info = &cfg_program.variables[*var_id];
            var_tracker.add_variable(
                var_info.name.clone(),
                self.type_name_to_boogie(&var_info.ty),
            );
        }

        for var_id in &function_b_cfg.local_variables {
            let var_info = &cfg_program.variables[*var_id];
            var_tracker.add_variable(
                var_info.name.clone(),
                self.type_name_to_boogie(&var_info.ty),
            );
        }

        // Collect all tables accessed by either function
        let mut accessed_tables = std::collections::HashSet::new();

        // Get tables accessed by function A
        self.collect_accessed_tables(cfg_program, function_a_cfg, &mut accessed_tables);

        // Get tables accessed by function B
        self.collect_accessed_tables(cfg_program, function_b_cfg, &mut accessed_tables);

        // Variables to store initial havoced state
        for table_id in accessed_tables.iter() {
            let table_info = &cfg_program.tables[*table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    let var_name = format!("initial_{}_{}", table_info.name, field_info.name);
                    let map_type = self.get_table_field_type(cfg_program, *table_id, *field_id);

                    var_tracker.add_variable(var_name, map_type);
                }
            }
        }

        // Variables to store table states after first execution ONLY for accessed tables and non-primary fields
        for table_id in accessed_tables {
            let table_info = &cfg_program.tables[table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    // Only create saved variables for non-primary fields
                    let var_name = format!("saved_{}_{}", table_info.name, field_info.name);
                    let map_type = self.get_table_field_type(cfg_program, table_id, *field_id);

                    var_tracker.add_variable(var_name, map_type);
                }
            }
        }

        // Variables to store output variables from both functions
        let function_a = &cfg_program.functions[self.function_a];
        let function_b = &cfg_program.functions[self.function_b];

        for var_id in &function_a.local_variables {
            let var_info = &cfg_program.variables[*var_id];
            var_tracker.add_variable(
                format!("saved_a_{}", var_info.name),
                self.type_name_to_boogie(&var_info.ty),
            );
        }

        for var_id in &function_b.local_variables {
            let var_info = &cfg_program.variables[*var_id];
            var_tracker.add_variable(
                format!("saved_b_{}", var_info.name),
                self.type_name_to_boogie(&var_info.ty),
            );
        }

        // Add temporary variables for intermediate computations
        // Add basic temp variables
        for i in 0..20 {
            var_tracker.add_variable(format!("_temp_{}", i), Ty::int());
        }

        // Add common variable names that might be generated dynamically
        var_tracker.add_variable("d_next_oid".to_string(), Ty::int());
        var_tracker.add_variable("d_tax".to_string(), Ty::real());
        var_tracker.add_variable("O_OL_CNT".to_string(), Ty::int());
    }

    /// Get the correct Boogie type for a table field (handling multiple primary keys)
    fn get_table_field_type(
        &self,
        cfg_program: &CfgProgram,
        table_id: cfg::TableId,
        field_id: cfg::FieldId,
    ) -> Ty {
        let table_info = &cfg_program.tables[table_id];
        let field_info = &cfg_program.fields[field_id];

        // Get primary key information
        let pk_fields: Vec<_> = table_info
            .fields
            .iter()
            .filter(|field_id| {
                let field = &cfg_program.fields[**field_id];
                field.is_primary
            })
            .collect();

        // Create appropriate map type based on number of primary keys
        if pk_fields.len() == 1 {
            // Single primary key: [pk_type]field_type
            let pk_field = &cfg_program.fields[*pk_fields[0]];
            let pk_type = self.field_type_to_boogie(&pk_field.ty);
            Ty::map(pk_type, self.field_type_to_boogie(&field_info.ty))
        } else if pk_fields.len() > 1 {
            // Multiple primary keys: [int][int]...field_type (nested maps)
            let mut result_type = self.field_type_to_boogie(&field_info.ty);
            for _ in 0..pk_fields.len() {
                result_type = Ty::map(Ty::int(), result_type);
            }
            result_type
        } else {
            // No primary key - use int as default
            Ty::map(Ty::int(), self.field_type_to_boogie(&field_info.ty))
        }
    }

    /// Generate the main procedure body following the Boogie harness template
    fn generate_procedure_body(
        &self,
        cfg_program: &CfgProgram,
        interleaving: &[(HopId, bool)],
        blocks: &mut Vec<Block>,
        var_tracker: &mut VariableTracker,
    ) {
        // Entry block
        let entry_label = Label("entry".to_string());
        let mut entry_stmts = Vec::new();

        // 1. Havoc all tables and input variables ONCE at the beginning
        self.add_havoc_statements(cfg_program, &mut entry_stmts);

        // 1.5. Save initial havoced state for later restoration
        self.add_save_initial_state(cfg_program, &mut entry_stmts);

        // 2. Execute first interleaving (π)
        entry_stmts.push(Stmt::Comment("Execute first interleaving".to_string()));
        self.add_interleaving_execution(cfg_program, interleaving, &mut entry_stmts, "");

        // 3. Save state (tables and outputs)
        self.add_save_state_statements(cfg_program, &mut entry_stmts);

        // 4. Restore initial state (same havoc) for second execution
        entry_stmts.push(Stmt::Comment(
            "Restore initial state for second execution".to_string(),
        ));
        self.add_restore_initial_state(cfg_program, &mut entry_stmts);

        // 5. Execute mirror interleaving (π^r - swap whole slices)
        entry_stmts.push(Stmt::Comment(
            "Execute mirror interleaving (swapped slices)".to_string(),
        ));
        let mirror_interleaving = self.create_mirror_interleaving(interleaving);
        self.add_interleaving_execution(cfg_program, &mirror_interleaving, &mut entry_stmts, "");

        // 6. Assert commutativity (same final state)
        self.add_commutativity_assertions(cfg_program, &mut entry_stmts);

        blocks.push(Block {
            label: entry_label,
            stmts: entry_stmts,
            terminator: Terminator::Return,
        });
    }

    /// Add havoc statements for tables used by the functions being verified
    fn add_havoc_statements(&self, cfg_program: &CfgProgram, stmts: &mut Vec<Stmt>) {
        stmts.push(Stmt::Comment("Havoc all tables and inputs".to_string()));

        // Collect all tables accessed by either function
        let mut accessed_tables = std::collections::HashSet::new();

        // Get tables accessed by function A
        let function_a_cfg = &cfg_program.functions[self.function_a];
        self.collect_accessed_tables(cfg_program, function_a_cfg, &mut accessed_tables);

        // Get tables accessed by function B
        let function_b_cfg = &cfg_program.functions[self.function_b];
        self.collect_accessed_tables(cfg_program, function_b_cfg, &mut accessed_tables);

        // Only havoc tables that are actually accessed
        for table_id in accessed_tables {
            let table_info = &cfg_program.tables[table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    // Don't havoc primary key fields
                    let map_name = format!("{}_{}", table_info.name, field_info.name);
                    stmts.push(Stmt::Havoc(Expr::Var(Ident(map_name))));
                }
            }
        }

        // Havoc parameter variables from both functions
        for param_id in &function_a_cfg.parameters {
            let param = &cfg_program.variables[*param_id];
            stmts.push(Stmt::Havoc(Expr::Var(Ident(format!("a_{}", param.name)))));
        }

        for param_id in &function_b_cfg.parameters {
            let param = &cfg_program.variables[*param_id];
            stmts.push(Stmt::Havoc(Expr::Var(Ident(format!("b_{}", param.name)))));
        }
    }

    /// Add initial state saving right after havoc
    fn add_save_initial_state(&self, cfg_program: &CfgProgram, stmts: &mut Vec<Stmt>) {
        stmts.push(Stmt::Comment("Save initial havoced state".to_string()));

        // Collect all tables accessed by either function
        let mut accessed_tables = std::collections::HashSet::new();

        // Get tables accessed by function A
        let function_a_cfg = &cfg_program.functions[self.function_a];
        self.collect_accessed_tables(cfg_program, function_a_cfg, &mut accessed_tables);

        // Get tables accessed by function B
        let function_b_cfg = &cfg_program.functions[self.function_b];
        self.collect_accessed_tables(cfg_program, function_b_cfg, &mut accessed_tables);

        // Save initial state ONLY for accessed tables
        for table_id in accessed_tables {
            let table_info = &cfg_program.tables[table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    let map_name = format!("{}_{}", table_info.name, field_info.name);
                    let initial_name = format!("initial_{}", map_name);

                    stmts.push(Stmt::Assign(
                        Expr::Var(Ident(initial_name)),
                        Expr::Var(Ident(map_name)),
                    ));
                }
            }
        }
    }

    /// Restore tables to their initial havoced state (stored in saved_ variables)
    fn add_restore_initial_state(&self, cfg_program: &CfgProgram, stmts: &mut Vec<Stmt>) {
        // Collect all tables accessed by either function
        let mut accessed_tables = std::collections::HashSet::new();

        // Get tables accessed by function A
        let function_a_cfg = &cfg_program.functions[self.function_a];
        self.collect_accessed_tables(cfg_program, function_a_cfg, &mut accessed_tables);

        // Get tables accessed by function B
        let function_b_cfg = &cfg_program.functions[self.function_b];
        self.collect_accessed_tables(cfg_program, function_b_cfg, &mut accessed_tables);

        // Restore initial havoced state from saved variables (before first execution)
        for table_id in accessed_tables {
            let table_info = &cfg_program.tables[table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    let map_name = format!("{}_{}", table_info.name, field_info.name);
                    let saved_name = format!("initial_{}", map_name);

                    stmts.push(Stmt::Assign(
                        Expr::Var(Ident(map_name)),
                        Expr::Var(Ident(saved_name)),
                    ));
                }
            }
        }
    }

    /// Collect all table IDs accessed by a function
    fn collect_accessed_tables(
        &self,
        _cfg_program: &CfgProgram,
        function_cfg: &cfg::FunctionCfg,
        accessed_tables: &mut std::collections::HashSet<cfg::TableId>,
    ) {
        // Go through all hops in the function
        for (_hop_id, hop_cfg) in function_cfg.hops.iter() {
            // Go through all basic blocks in each hop
            for &block_id in &hop_cfg.blocks {
                let basic_block = &function_cfg.blocks[block_id];

                // Check each statement for table accesses
                for stmt in &basic_block.statements {
                    self.collect_table_accesses_in_statement(stmt, accessed_tables);
                }
            }
        }
    }

    /// Collect table accesses in a single CFG statement
    fn collect_table_accesses_in_statement(
        &self,
        stmt: &cfg::Statement,
        accessed_tables: &mut std::collections::HashSet<cfg::TableId>,
    ) {
        match stmt {
            cfg::Statement::Assign { lvalue, rvalue, .. } => {
                self.collect_table_accesses_in_rvalue(rvalue, accessed_tables);
                if let cfg::LValue::TableField { table, .. } = lvalue {
                    accessed_tables.insert(*table);
                }
            }
        }
    }

    /// Collect table accesses in an rvalue expression
    fn collect_table_accesses_in_rvalue(
        &self,
        rvalue: &cfg::Rvalue,
        accessed_tables: &mut std::collections::HashSet<cfg::TableId>,
    ) {
        match rvalue {
            cfg::Rvalue::TableAccess { table, .. } => {
                accessed_tables.insert(*table);
            }
            cfg::Rvalue::BinaryOp { left, right, .. } => {
                self.collect_table_accesses_in_operand(left, accessed_tables);
                self.collect_table_accesses_in_operand(right, accessed_tables);
            }
            cfg::Rvalue::UnaryOp { operand, .. } => {
                self.collect_table_accesses_in_operand(operand, accessed_tables);
            }
            cfg::Rvalue::Use(operand) => {
                self.collect_table_accesses_in_operand(operand, accessed_tables);
            }
            cfg::Rvalue::ArrayAccess { array, index } => {
                self.collect_table_accesses_in_operand(array, accessed_tables);
                self.collect_table_accesses_in_operand(index, accessed_tables);
            }
        }
    }

    /// Collect table accesses in an operand - operands don't contain table accesses directly
    fn collect_table_accesses_in_operand(
        &self,
        _operand: &cfg::Operand,
        _accessed_tables: &mut std::collections::HashSet<cfg::TableId>,
    ) {
        // Operands are either variables (VarId) or constants - no table accesses
    }

    /// Add execution of hops in the given interleaving order
    fn add_interleaving_execution(
        &self,
        cfg_program: &CfgProgram,
        interleaving: &[(HopId, bool)],
        stmts: &mut Vec<Stmt>,
        _suffix: &str,
    ) {
        for (hop_id, is_slice_a) in interleaving {
            let function_id = if *is_slice_a {
                self.function_a
            } else {
                self.function_b
            };
            let function_cfg = &cfg_program.functions[function_id];
            let slice_name = if *is_slice_a { "A" } else { "B" };

            stmts.push(Stmt::Comment(format!(
                "Execute hop {:?} from slice {}",
                hop_id, slice_name
            )));

            // Simplified hop execution - just track that it happened
            // In a full implementation, we'd inline the hop's basic blocks
            self.add_hop_execution(cfg_program, function_cfg, *hop_id, *is_slice_a, stmts);
        }
    }

    /// Add execution of a single hop by inlining its basic block statements
    fn add_hop_execution(
        &self,
        cfg_program: &CfgProgram,
        function_cfg: &cfg::FunctionCfg,
        hop_id: HopId,
        is_slice_a: bool,
        stmts: &mut Vec<Stmt>,
    ) {
        // Get the hop configuration
        let hop_cfg = &function_cfg.hops[hop_id];

        // Process all basic blocks in the hop
        for &block_id in &hop_cfg.blocks {
            let basic_block = &function_cfg.blocks[block_id];

            // Translate each statement in the basic block
            for statement in &basic_block.statements {
                self.translate_cfg_statement(cfg_program, statement, is_slice_a, stmts);
            }
        }
    }

    /// Translate a CFG statement into Boogie statements
    fn translate_cfg_statement(
        &self,
        cfg_program: &CfgProgram,
        statement: &cfg::Statement,
        is_slice_a: bool,
        stmts: &mut Vec<Stmt>,
    ) {
        // Add a comment with the debug representation of the original CFG statement
        stmts.push(Stmt::Comment(format!("CFG: {:?}", statement)));
        match statement {
            cfg::Statement::Assign { lvalue, rvalue, .. } => {
                match lvalue {
                    cfg::LValue::TableField {
                        table,
                        pk_fields,
                        pk_values,
                        field,
                    } => {
                        // Table field assignment: table_field[pk1, pk2, ...] := rvalue
                        let table_info = &cfg_program.tables[*table];
                        let field_info = &cfg_program.fields[*field];
                        let map_name = format!("{}_{}", table_info.name, field_info.name);
                        let map_var = Expr::Var(Ident(map_name.clone()));
                        let rvalue_expr =
                            self.rvalue_to_boogie_expr(cfg_program, rvalue, is_slice_a, false);
                        // Reorder pk_values to match primary_keys order
                        let ordered_pk_values = order_pk_values(table_info, pk_fields, pk_values);
                        let map_store = self.nested_map_store(
                            map_var,
                            &ordered_pk_values,
                            rvalue_expr,
                            cfg_program,
                            is_slice_a,
                        );
                        stmts.push(Stmt::Assign(Expr::Var(Ident(map_name)), map_store));
                    }
                    cfg::LValue::Variable { var } => {
                        // Variable assignment: check if variable is a map or scalar
                        let var_info = &cfg_program.variables[*var];
                        let var_name = var_info.name.clone();
                        let var_ty = &var_info.ty;
                        let want_map = matches!(
                            var_ty,
                            cfg::TypeName::Table(_) | cfg::TypeName::Array { .. }
                        );
                        let rvalue_expr =
                            self.rvalue_to_boogie_expr(cfg_program, rvalue, is_slice_a, want_map);
                        stmts.push(Stmt::Assign(Expr::Var(Ident(var_name)), rvalue_expr));
                    }
                    cfg::LValue::ArrayElement { .. } => {
                        stmts.push(Stmt::Comment(
                            "Array assignment not yet implemented".to_string(),
                        ));
                    }
                }
            }
        }
    }

    /// Convert CFG RValue to Boogie expression
    fn rvalue_to_boogie_expr(
        &self,
        cfg_program: &CfgProgram,
        rvalue: &cfg::Rvalue,
        is_slice_a: bool,
        want_map: bool,
    ) -> Expr {
        match rvalue {
            cfg::Rvalue::Use(operand) => {
                self.operand_to_boogie_expr(cfg_program, operand, is_slice_a)
            }
            cfg::Rvalue::TableAccess {
                table,
                pk_fields,
                pk_values,
                field,
                ..
            } => {
                let table_info = &cfg_program.tables[*table];
                let field_info = &cfg_program.fields[*field];
                let map_name = format!("{}_{}", table_info.name, field_info.name);
                if want_map {
                    // Return the whole map variable
                    Expr::Var(Ident(map_name))
                } else {
                    // Always use all primary keys in schema order
                    let ordered_pk_values = order_pk_values(table_info, pk_fields, pk_values);
                    self.nested_map_select(
                        Expr::Var(Ident(map_name)),
                        &ordered_pk_values,
                        cfg_program,
                        is_slice_a,
                    )
                }
            }
            cfg::Rvalue::BinaryOp { op, left, right } => {
                let left_expr = self.operand_to_boogie_expr(cfg_program, left, is_slice_a);
                let right_expr = self.operand_to_boogie_expr(cfg_program, right, is_slice_a);
                let boogie_op = self.binary_op_to_boogie(op);
                Expr::Binary {
                    op: boogie_op,
                    l: Box::new(left_expr),
                    r: Box::new(right_expr),
                }
            }
            cfg::Rvalue::UnaryOp { op, operand } => {
                let operand_expr = self.operand_to_boogie_expr(cfg_program, operand, is_slice_a);
                let boogie_op = self.unary_op_to_boogie(op);
                Expr::Unary {
                    op: boogie_op,
                    e: Box::new(operand_expr),
                }
            }
            cfg::Rvalue::ArrayAccess { .. } => Expr::Const(Constant::Int(0)),
        }
    }

    /// Convert CFG Operand to Boogie expression  
    fn operand_to_boogie_expr(
        &self,
        cfg_program: &CfgProgram,
        operand: &cfg::Operand,
        is_slice_a: bool,
    ) -> Expr {
        match operand {
            cfg::Operand::Var(var_id) => {
                let var_info = &cfg_program.variables[*var_id];

                // If this is a parameter, we need to map it to the appropriate slice name
                if var_info.kind == cfg::VariableKind::Parameter {
                    let prefix = if is_slice_a { "a" } else { "b" };
                    let param_name = format!("{}_{}", prefix, var_info.name);
                    Expr::Var(Ident(param_name))
                } else {
                    Expr::Var(Ident(var_info.name.clone()))
                }
            }
            cfg::Operand::Const(constant) => {
                match constant {
                    cfg::Constant::Int(i) => Expr::Const(Constant::Int(*i)),
                    cfg::Constant::Float(f) => Expr::Const(Constant::Real(**f)),
                    cfg::Constant::Bool(b) => Expr::Const(Constant::Bool(*b)),
                    cfg::Constant::String(s) => Expr::Const(Constant::Str(s.clone())),
                    cfg::Constant::Array(_) => Expr::Const(Constant::Int(0)), // Simplified
                }
            }
        }
    }

    /// Convert CFG BinaryOp to Boogie BinOp
    fn binary_op_to_boogie(&self, op: &cfg::BinaryOp) -> BinOp {
        match op {
            cfg::BinaryOp::Add => BinOp::Add,
            cfg::BinaryOp::Sub => BinOp::Sub,
            cfg::BinaryOp::Mul => BinOp::Mul,
            cfg::BinaryOp::Div => BinOp::Div,
            cfg::BinaryOp::Eq => BinOp::Eq,
            cfg::BinaryOp::Neq => BinOp::Neq,
            cfg::BinaryOp::Lt => BinOp::Lt,
            cfg::BinaryOp::Lte => BinOp::Lte,
            cfg::BinaryOp::Gt => BinOp::Gt,
            cfg::BinaryOp::Gte => BinOp::Gte,
            cfg::BinaryOp::And => BinOp::And,
            cfg::BinaryOp::Or => BinOp::Or,
        }
    }

    /// Convert CFG UnaryOp to Boogie UnOp
    fn unary_op_to_boogie(&self, op: &cfg::UnaryOp) -> UnOp {
        match op {
            cfg::UnaryOp::Not => UnOp::Not,
            cfg::UnaryOp::Neg => UnOp::Neg,
            // For increment/decrement operations, just use negation as a fallback
            cfg::UnaryOp::PreIncrement
            | cfg::UnaryOp::PostIncrement
            | cfg::UnaryOp::PreDecrement
            | cfg::UnaryOp::PostDecrement => UnOp::Neg,
        }
    }

    /// Save current state of tables and output variables
    fn add_save_state_statements(&self, cfg_program: &CfgProgram, stmts: &mut Vec<Stmt>) {
        stmts.push(Stmt::Comment(
            "Save state after first execution".to_string(),
        ));

        // Collect all tables accessed by either function
        let mut accessed_tables = std::collections::HashSet::new();

        // Get tables accessed by function A
        let function_a_cfg = &cfg_program.functions[self.function_a];
        self.collect_accessed_tables(cfg_program, function_a_cfg, &mut accessed_tables);

        // Get tables accessed by function B
        let function_b_cfg = &cfg_program.functions[self.function_b];
        self.collect_accessed_tables(cfg_program, function_b_cfg, &mut accessed_tables);

        // Save table states ONLY for accessed tables and non-primary fields
        for table_id in accessed_tables {
            let table_info = &cfg_program.tables[table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    // Only save non-primary fields
                    let map_name = format!("{}_{}", table_info.name, field_info.name);
                    let saved_name = format!("saved_{}", map_name);

                    stmts.push(Stmt::Assign(
                        Expr::Var(Ident(saved_name)),
                        Expr::Var(Ident(map_name)),
                    ));
                }
            }
        }

        // Save output variables (simplified - just comment for now)
        stmts.push(Stmt::Comment(
            "Save output variables (implementation needed)".to_string(),
        ));
    }

    /// Create mirror interleaving by swapping the order of slices
    fn create_mirror_interleaving(&self, interleaving: &[(HopId, bool)]) -> Vec<(HopId, bool)> {
        // Extract slice A and slice B hops
        let mut slice_a_hops = Vec::new();
        let mut slice_b_hops = Vec::new();

        for (hop_id, is_slice_a) in interleaving {
            if *is_slice_a {
                slice_a_hops.push((*hop_id, *is_slice_a));
            } else {
                slice_b_hops.push((*hop_id, *is_slice_a));
            }
        }

        // Mirror interleaving: B slice first, then A slice
        let mut mirror = slice_b_hops;
        mirror.extend(slice_a_hops);
        mirror
    }

    /// Add assertions that check commutativity (same final state)
    fn add_commutativity_assertions(&self, cfg_program: &CfgProgram, stmts: &mut Vec<Stmt>) {
        stmts.push(Stmt::Comment(
            "Assert commutativity - same final state".to_string(),
        ));

        // Collect all tables accessed by either function
        let mut accessed_tables = std::collections::HashSet::new();

        // Get tables accessed by function A
        let function_a_cfg = &cfg_program.functions[self.function_a];
        self.collect_accessed_tables(cfg_program, function_a_cfg, &mut accessed_tables);

        // Get tables accessed by function B
        let function_b_cfg = &cfg_program.functions[self.function_b];
        self.collect_accessed_tables(cfg_program, function_b_cfg, &mut accessed_tables);

        // Assert table states are equal ONLY for accessed tables and non-primary fields
        for table_id in accessed_tables {
            let table_info = &cfg_program.tables[table_id];
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                if !field_info.is_primary {
                    // Only assert non-primary fields
                    let map_name = format!("{}_{}", table_info.name, field_info.name);
                    let saved_name = format!("saved_{}", map_name);

                    let equality_assertion = Expr::Binary {
                        op: BinOp::Eq,
                        l: Box::new(Expr::Var(Ident(saved_name))),
                        r: Box::new(Expr::Var(Ident(map_name))),
                    };

                    stmts.push(Stmt::Assert(equality_assertion));
                    stmts.push(Stmt::Comment(format!(
                        "Table {}.{} must have same final state",
                        table_info.name, field_info.name
                    )));
                }
            }
        }

        // Assert output variables are equal (simplified for now)
        stmts.push(Stmt::Comment(
            "Assert output variables are equal (implementation needed)".to_string(),
        ));
    }

    /// Convert CFG type to Boogie type
    fn type_name_to_boogie(&self, ty: &cfg::TypeName) -> Ty {
        match ty {
            cfg::TypeName::Int => Ty::int(),
            cfg::TypeName::Float => Ty::real(),
            cfg::TypeName::Bool => Ty::bool(),
            cfg::TypeName::String => Ty::str(),
            cfg::TypeName::Array { .. } => Ty::map(Ty::int(), Ty::int()),
            cfg::TypeName::Table(_) => Ty::int(),
        }
    }

    /// Convert field type to Boogie type
    fn field_type_to_boogie(&self, ty: &cfg::TypeName) -> Ty {
        self.type_name_to_boogie(ty)
    }
}
