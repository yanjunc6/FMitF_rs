use super::{boogie::*, Property, VerificationUnit};
use crate::cfg::{self, CfgProgram, FunctionType};

pub struct PartitionSoundnessUnit {
    /// The function ID to verify.
    pub function_id: cfg::FunctionId,
}

impl VerificationUnit for PartitionSoundnessUnit {
    fn property(&self) -> Property {
        Property::PartitionSoundness {
            function: self.function_id,
        }
    }

    fn generate(&self, cfg_program: &CfgProgram) -> BoogieProgram {
        let function_cfg = &cfg_program.functions[self.function_id];

        // Only verify transaction functions
        if function_cfg.function_type != FunctionType::Transaction {
            return BoogieProgram {
                declarations: vec![],
            };
        }

        let mut declarations = Vec::new();

        // Generate table map declarations
        self.generate_table_declarations(cfg_program, &mut declarations);

        // Generate the main verification procedure
        self.generate_verification_procedure(cfg_program, function_cfg, &mut declarations);

        BoogieProgram { declarations }
    }
}

impl PartitionSoundnessUnit {
    /// Generate Boogie map declarations for all tables
    fn generate_table_declarations(&self, cfg_program: &CfgProgram, declarations: &mut Vec<Decl>) {
        for (_table_id, table_info) in cfg_program.tables.iter() {
            // For each field in the table, generate a map
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                let map_name = format!("{}_{}", table_info.name, field_info.name);

                // Create map type: [int]int for simplicity (should be more sophisticated)
                let map_type = Ty::map(Ty::int(), self.field_type_to_boogie(&field_info.ty));

                declarations.push(Decl::Var(VarDecl {
                    name: Ident(map_name),
                    ty: map_type,
                }));
            }
        }

        // Add accessed_keys as a global variable
        declarations.push(Decl::Var(VarDecl {
            name: Ident("accessed_keys".to_string()),
            ty: Ty::map(Ty::int(), Ty::bool()),
        }));
    }

    /// Generate the main verification procedure for partition soundness
    fn generate_verification_procedure(
        &self,
        cfg_program: &CfgProgram,
        function_cfg: &cfg::FunctionCfg,
        declarations: &mut Vec<Decl>,
    ) {
        let proc_name = format!("verify_partition_soundness_{}", function_cfg.name);

        let mut locals = Vec::new();
        let mut blocks = Vec::new();

        // No local variables needed now - accessed_keys is global

        // Generate parameter declarations
        let mut params = Vec::new();
        for param_id in &function_cfg.parameters {
            let param = &cfg_program.variables[*param_id];
            params.push(VarDecl {
                name: Ident(param.name.clone()),
                ty: self.type_name_to_boogie(&param.ty),
            });
        }

        // Generate main verification logic
        self.generate_hop_verification_blocks(cfg_program, function_cfg, &mut blocks, &mut locals);

        // Add modifies clause for table maps
        let mut modifies = Vec::new();
        for (_table_id, table_info) in cfg_program.tables.iter() {
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                let map_name = format!("{}_{}", table_info.name, field_info.name);
                modifies.push(Ident(map_name));
            }
        }
        modifies.push(Ident("accessed_keys".to_string()));

        let procedure = ProcedureDecl {
            name: Ident(proc_name),
            params,
            locals,
            modifies,
            body: blocks,
        };

        declarations.push(Decl::Procedure(procedure));
    }

    /// Generate verification blocks for each hop in the function
    fn generate_hop_verification_blocks(
        &self,
        cfg_program: &CfgProgram,
        function_cfg: &cfg::FunctionCfg,
        blocks: &mut Vec<Block>,
        _locals: &mut Vec<VarDecl>,
    ) {
        let mut _block_counter = 0;

        // Entry block - havoc all tables and parameters
        let entry_label = Label(format!("entry"));
        let mut entry_stmts = Vec::new();

        // Havoc all table maps
        for (_table_id, table_info) in cfg_program.tables.iter() {
            for field_id in &table_info.fields {
                let field_info = &cfg_program.fields[*field_id];
                let map_name = format!("{}_{}", table_info.name, field_info.name);
                entry_stmts.push(Stmt::Havoc(Expr::Var(Ident(map_name))));
            }
        }

        // Initialize accessed_keys map
        entry_stmts.push(Stmt::Havoc(Expr::Var(Ident("accessed_keys".to_string()))));

        // Add assumptions for testing partition soundness violations
        // Assume that different parameters have different values
        self.add_simple_assumptions(cfg_program, function_cfg, &mut entry_stmts);

        blocks.push(Block {
            label: entry_label,
            stmts: entry_stmts,
            terminator: if function_cfg.hop_order.is_empty() {
                Terminator::Return
            } else {
                Terminator::Goto(vec![Label(format!("hop_0"))])
            },
        });

        // Generate blocks for each hop
        for (hop_index, hop_id) in function_cfg.hop_order.iter().enumerate() {
            _block_counter += 1;

            let hop_label = Label(format!("hop_{}", hop_index));
            let mut hop_stmts = Vec::new();

            // Track table accesses in this hop
            self.generate_hop_access_tracking(cfg_program, function_cfg, *hop_id, &mut hop_stmts);

            // Add partition soundness assertion for this hop
            self.generate_partition_soundness_assertion(cfg_program, *hop_id, &mut hop_stmts);

            let next_terminator = if hop_index + 1 < function_cfg.hop_order.len() {
                Terminator::Goto(vec![Label(format!("hop_{}", hop_index + 1))])
            } else {
                Terminator::Return
            };

            blocks.push(Block {
                label: hop_label,
                stmts: hop_stmts,
                terminator: next_terminator,
            });
        }
    }

    /// Generate statements to track table accesses within a hop
    fn generate_hop_access_tracking(
        &self,
        cfg_program: &CfgProgram,
        function_cfg: &cfg::FunctionCfg,
        hop_id: cfg::HopId,
        stmts: &mut Vec<Stmt>,
    ) {
        let hop_cfg = &function_cfg.hops[hop_id];

        // Analyze all basic blocks in this hop for table accesses
        for &block_id in &hop_cfg.blocks {
            let basic_block = &function_cfg.blocks[block_id];

            for statement in &basic_block.statements {
                match statement {
                    cfg::Statement::Assign { lvalue, rvalue, .. } => {
                        // Check if lvalue is a table access
                        if let cfg::LValue::TableField {
                            table, pk_values, ..
                        } = lvalue
                        {
                            self.track_table_access(cfg_program, *table, pk_values, stmts);
                        }

                        // Check if rvalue contains table accesses
                        if let cfg::Rvalue::TableAccess {
                            table, pk_values, ..
                        } = rvalue
                        {
                            self.track_table_access(cfg_program, *table, pk_values, stmts);
                        }
                    }
                }
            }
        }
    }

    /// Generate code to track a specific table access
    fn track_table_access(
        &self,
        cfg_program: &CfgProgram,
        _table_id: cfg::TableId,
        pk_values: &[cfg::Operand],
        stmts: &mut Vec<Stmt>,
    ) {
        // For simplicity, we'll track the first primary key value
        // In a real implementation, we'd need to handle composite keys properly
        if let Some(first_pk_value) = pk_values.first() {
            let key_expr = self.operand_to_boogie_expr_with_cfg(first_pk_value, cfg_program);

            // accessed_keys := accessed_keys[key := true]
            let access_stmt = Stmt::Assign(
                Expr::Var(Ident("accessed_keys".to_string())),
                Expr::MapStore {
                    map: Box::new(Expr::Var(Ident("accessed_keys".to_string()))),
                    index: Box::new(key_expr),
                    val: Box::new(Expr::Const(Constant::Bool(true))),
                },
            );

            stmts.push(access_stmt);
        }
    }

    /// Generate the main partition soundness assertion for a hop
    fn generate_partition_soundness_assertion(
        &self,
        cfg_program: &CfgProgram,
        hop_id: cfg::HopId,
        stmts: &mut Vec<Stmt>,
    ) {
        let function_cfg = &cfg_program.functions[self.function_id];
        let hop_cfg = &function_cfg.hops[hop_id];

        // Collect all table accesses in this hop
        let mut table_accesses = Vec::new();

        for &block_id in &hop_cfg.blocks {
            let basic_block = &function_cfg.blocks[block_id];

            for statement in &basic_block.statements {
                match statement {
                    cfg::Statement::Assign { lvalue, rvalue, .. } => {
                        // Check if lvalue is a table access
                        if let cfg::LValue::TableField {
                            table, pk_values, ..
                        } = lvalue
                        {
                            table_accesses.push((*table, pk_values.clone()));
                        }

                        // Check if rvalue contains table accesses
                        if let cfg::Rvalue::TableAccess {
                            table, pk_values, ..
                        } = rvalue
                        {
                            table_accesses.push((*table, pk_values.clone()));
                        }
                    }
                }
            }
        }

        if table_accesses.is_empty() {
            stmts.push(Stmt::Comment(
                "No table accesses in this hop - partition soundness trivially holds".to_string(),
            ));
            return;
        }

        // Generate assertions comparing primary keys for all pairs of accesses
        for i in 0..table_accesses.len() {
            for j in (i + 1)..table_accesses.len() {
                let (table1, pk_values1) = &table_accesses[i];
                let (table2, pk_values2) = &table_accesses[j];

                // Get table information
                let table1_info = &cfg_program.tables[*table1];
                let table2_info = &cfg_program.tables[*table2];

                // For simplicity, just check that primary key values are equal
                // This ensures all accesses in the hop use the same key (same partition)
                if let (Some(pk1), Some(pk2)) = (pk_values1.first(), pk_values2.first()) {
                    let pk1_expr = self.operand_to_boogie_expr_with_cfg(pk1, cfg_program);
                    let pk2_expr = self.operand_to_boogie_expr_with_cfg(pk2, cfg_program);

                    let assertion = Expr::Binary {
                        op: BinOp::Eq,
                        l: Box::new(pk1_expr),
                        r: Box::new(pk2_expr),
                    };

                    stmts.push(Stmt::Assert(assertion));
                    stmts.push(Stmt::Comment(format!(
                        "Partition soundness: {}.{} and {}.{} must use same primary key",
                        table1_info.name,
                        cfg_program.fields[table1_info.primary_keys[0]].name,
                        table2_info.name,
                        cfg_program.fields[table2_info.primary_keys[0]].name
                    )));
                }
            }
        }

        // If we only have one table access, the partition soundness is trivially satisfied
        if table_accesses.len() == 1 {
            stmts.push(Stmt::Comment(
                "Only one table access - partition soundness trivially satisfied".to_string(),
            ));
        }

        stmts.push(Stmt::Comment(format!(
            "End partition soundness verification for hop {:?}",
            hop_id
        )));
    }

    /// Convert CFG operand to Boogie expression
    fn operand_to_boogie_expr_with_cfg(
        &self,
        operand: &cfg::Operand,
        cfg_program: &CfgProgram,
    ) -> Expr {
        match operand {
            cfg::Operand::Var(var_id) => {
                let var_info = &cfg_program.variables[*var_id];
                Expr::Var(Ident(var_info.name.clone()))
            }
            cfg::Operand::Const(constant) => self.constant_to_boogie_expr(constant),
        }
    }

    /// Convert CFG constant to Boogie expression
    fn constant_to_boogie_expr(&self, constant: &cfg::Constant) -> Expr {
        match constant {
            cfg::Constant::Int(i) => Expr::Const(Constant::Int(*i)),
            cfg::Constant::Float(f) => Expr::Const(Constant::Real(f.into_inner())),
            cfg::Constant::Bool(b) => Expr::Const(Constant::Bool(*b)),
            cfg::Constant::String(s) => Expr::Const(Constant::Str(s.clone())),
            cfg::Constant::Array(_) => {
                // Arrays not supported in this simple implementation
                Expr::Const(Constant::Int(0))
            }
        }
    }

    /// Convert CFG TypeName to Boogie type
    fn type_name_to_boogie(&self, ty: &cfg::TypeName) -> Ty {
        match ty {
            cfg::TypeName::Int => Ty::int(),
            cfg::TypeName::Float => Ty::real(),
            cfg::TypeName::Bool => Ty::bool(),
            cfg::TypeName::String => Ty::str(),
            cfg::TypeName::Array { .. } => {
                // Simplified - arrays as int maps
                Ty::map(Ty::int(), Ty::int())
            }
            cfg::TypeName::Table(_) => {
                // Tables not directly supported as types
                Ty::int()
            }
        }
    }

    /// Convert field type to Boogie type
    fn field_type_to_boogie(&self, ty: &cfg::TypeName) -> Ty {
        self.type_name_to_boogie(ty)
    }

    /// Add simple assumptions to help Boogie detect violations
    fn add_simple_assumptions(
        &self,
        cfg_program: &CfgProgram,
        function_cfg: &cfg::FunctionCfg,
        stmts: &mut Vec<Stmt>,
    ) {
        // For functions with multiple parameters, assume they are different
        // This helps detect when different primary keys are used in the same hop

        if function_cfg.parameters.len() >= 2 {
            // Get first two parameters (often from_id and to_id in transfer functions)
            let param1_id = function_cfg.parameters[0];
            let param2_id = function_cfg.parameters[1];
            let param1 = &cfg_program.variables[param1_id];
            let param2 = &cfg_program.variables[param2_id];

            // Assume parameters are different
            let param_diff_assumption = Expr::Binary {
                op: BinOp::Neq,
                l: Box::new(Expr::Var(Ident(param1.name.clone()))),
                r: Box::new(Expr::Var(Ident(param2.name.clone()))),
            };

            stmts.push(Stmt::Comment(
                "Assume parameters are different to test partition soundness".to_string(),
            ));
            stmts.push(Stmt::Assume(param_diff_assumption));
        }
    }
}
