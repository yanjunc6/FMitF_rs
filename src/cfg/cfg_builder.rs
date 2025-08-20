// cfg/cfg_builder.rs

use crate::ast::{self, Program as AstProgram};
use crate::cfg::*;
use std::collections::HashMap;

#[derive(Clone)]
enum SimpleStatementType {
    Abort,
    Break,
    Continue,
}

/// Context for CFG building with visitor pattern
pub struct CfgBuilder<'a> {
    ast: &'a AstProgram,
    cfg: CfgProgram,

    // NEW APPROACH: Direct mapping from logical identifiers to CFG elements
    // Key insight: Use (cfg_function_id, name) as the unique identifier for simplicity

    // Scope-aware variable mapping: (CFG FunctionId, variable_name) -> CFG VarId
    scoped_var_map: HashMap<(Option<FunctionId>, String), VarId>,

    // Other mappings remain the same but will be cleaned up
    table_map: HashMap<ast::TableId, TableId>,
    field_map: HashMap<ast::FieldId, FieldId>,
    function_map: HashMap<ast::FunctionId, FunctionId>,
    partition_map: HashMap<ast::PartitionId, FunctionId>,

    // Current context during building
    current_function: Option<FunctionId>,
    current_hop: Option<HopId>,
    current_block: Option<BasicBlockId>,

    // Loop context stack for break/continue statements
    loop_context_stack: Vec<LoopContext>,

    // For variable generation
    next_temp_id: usize,
}

#[derive(Clone)]
struct LoopContext {
    continue_target: BasicBlockId, // Where continue should jump to
    break_target: BasicBlockId,    // Where break should jump to
}

impl<'a> CfgBuilder<'a> {
    pub fn new(ast: &'a AstProgram) -> Self {
        Self {
            ast,
            cfg: CfgProgram::default(),
            scoped_var_map: HashMap::new(),
            table_map: HashMap::new(),
            field_map: HashMap::new(),
            function_map: HashMap::new(),
            partition_map: HashMap::new(),
            current_function: None,
            current_hop: None,
            current_block: None,
            loop_context_stack: Vec::new(),
            next_temp_id: 0,
        }
    }

    pub fn build(mut self) -> CfgProgram {
        // DISABLED: First pass: populate var_map by scanning all resolved variables in the AST
        // self.populate_var_map();

        // TODO: Implement new approach - use only AST resolutions map for variable references

        // Process global constants first
        self.process_global_constants();

        // Process tables
        self.process_tables();

        // Process partitions
        self.process_partitions();

        // Process functions
        self.process_functions();

        self.cfg
    }

    /// NEW APPROACH: Get or create a CFG variable using logical scope + name
    /// This ensures that the same (function_scope, variable_name) always maps to the same CFG VarId
    fn get_or_create_variable(
        &mut self,
        var_name: &str,
        var_type: &TypeName,
        var_kind: VariableKind,
        span: &Span,
    ) -> VarId {
        // Use the current CFG FunctionId as scope (simpler and more reliable)
        let scope_key = (self.current_function, var_name.to_string());

        if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
            // Variable already exists for this scope + name
            cfg_var_id
        } else {
            // Create new CFG variable
            let cfg_var = Variable {
                name: var_name.to_string(),
                ty: var_type.clone(),
                kind: var_kind,
                value: None,
                span: span.clone(),
            };

            let cfg_var_id = self.cfg.variables.alloc(cfg_var);
            self.scoped_var_map.insert(scope_key, cfg_var_id);
            cfg_var_id
        }
    }

    /// OLD APPROACH - DISABLED  
    /// This method created inconsistent variable mappings and has been replaced
    /// with the new scope-aware approach in get_or_create_variable()
    #[allow(dead_code)]
    fn populate_var_map_DISABLED(&mut self) {
        // DISABLED: This entire approach was flawed
        // Create CFG variables for all AST variables and populate var_map
        /*
        for (ast_var_id, var_type) in &self.ast.var_types {
            let ast_var = &self.ast.variables[*ast_var_id];

            let cfg_var_kind = match ast_var.kind {
                ast::VarKind::Parameter => VariableKind::Parameter,
                ast::VarKind::Local => VariableKind::Local,
                ast::VarKind::Global => VariableKind::Global,
            };

            let cfg_var = Variable {
                name: ast_var.name.clone(),
                ty: var_type.clone(),
                kind: cfg_var_kind,
                value: None,
                span: ast_var.defined_at.clone(),
            };

            let cfg_var_id = self.cfg.variables.alloc(cfg_var);
            self.var_map.insert(*ast_var_id, cfg_var_id);

            // Add to appropriate collections
            match ast_var.kind {
                ast::VarKind::Global => {
                    self.cfg.root_variables.push(cfg_var_id);
                }
                _ => {
                    // Parameters and locals will be added to function collections later
                }
            }
        }
        */
    }

    /// Static method to build CFG from AST program - used by compiler
    pub fn build_from_program(ast: &AstProgram) -> Result<CfgProgram, String> {
        // For now, assume no errors during CFG building
        // In the future, this could return errors for invalid constructs
        Ok(CfgBuilder::new(ast).build())
    }

    // ===== Top-level visitors =====

    fn process_global_constants(&mut self) {
        for &const_id in &self.ast.root_constants {
            let cfg_var_id = self.visit_constant(const_id);
            self.cfg.root_variables.push(cfg_var_id);
        }
    }

    fn process_tables(&mut self) {
        for &table_id in &self.ast.root_tables {
            let cfg_table_id = self.visit_table(table_id);
            self.cfg.root_tables.push(cfg_table_id);
        }
    }

    fn process_partitions(&mut self) {
        for &partition_id in &self.ast.root_partitions {
            let cfg_func_id = self.visit_partition(partition_id);
            self.cfg.root_functions.push(cfg_func_id);
        }
    }

    fn process_functions(&mut self) {
        for &func_id in &self.ast.root_functions {
            let cfg_func_id = self.visit_function(func_id);
            self.cfg.root_functions.push(cfg_func_id);
        }
    }

    // ===== Declaration visitors =====

    fn visit_constant(&mut self, const_id: ast::VarId) -> VarId {
        let const_decl = &self.ast.variables[const_id];

        // Evaluate constant expression if it's a global constant
        let value = if matches!(const_decl.kind, ast::VarKind::Global) {
            // For global constants, we should evaluate their initializer expression
            // This is a simplified approach - in a full compiler, we'd have a constant evaluator
            Some(self.evaluate_constant_expression_placeholder())
        } else {
            None
        };

        let var = Variable {
            name: const_decl.name.clone(),
            ty: const_decl.ty.clone(),
            kind: VariableKind::Global,
            value,
            span: const_decl.defined_at.clone(),
        };

        let cfg_var_id = self.cfg.variables.alloc(var);
        // DISABLED: Old mapping approach - constants should use global scope mapping
        // self.var_map.insert(const_id, cfg_var_id);
        cfg_var_id
    }

    fn evaluate_constant_expression_placeholder(&self) -> Constant {
        // Placeholder for constant expression evaluation
        // In a full implementation, this would recursively evaluate AST expressions
        // to produce compile-time constant values
        Constant::Int(0)
    }

    fn visit_table(&mut self, table_id: ast::TableId) -> TableId {
        let table = &self.ast.tables[table_id];

        // Process fields first
        let mut cfg_fields = Vec::new();
        let mut cfg_primary_keys = Vec::new();

        for &field_id in &table.fields {
            let cfg_field_id = self.visit_field(field_id, None);
            cfg_fields.push(cfg_field_id);

            if self.ast.fields[field_id].is_primary {
                cfg_primary_keys.push(cfg_field_id);
            }
        }

        // Create or find partition function
        let partition_func_id = if let Some(ref node_partition) = table.node_partition {
            self.resolve_or_create_partition_function(table, node_partition)
        } else {
            self.create_default_partition_function(&table.name)
        };

        // Map partition fields
        let partition_fields = self.map_partition_fields(&table.node_partition, &cfg_fields);

        let table_info = TableInfo {
            name: table.name.clone(),
            fields: cfg_fields.clone(),
            primary_keys: cfg_primary_keys,
            partition_function: partition_func_id,
            partition_fields,
        };

        let cfg_table_id = self.cfg.tables.alloc(table_info);

        // Update field table references
        for &field_id in &cfg_fields {
            self.cfg.fields[field_id].table_id = Some(cfg_table_id);
        }

        self.table_map.insert(table_id, cfg_table_id);
        cfg_table_id
    }

    fn visit_field(&mut self, field_id: ast::FieldId, table_id: Option<TableId>) -> FieldId {
        let field = &self.ast.fields[field_id];

        let field_info = FieldInfo {
            name: field.field_name.clone(),
            ty: field.field_type.clone(),
            table_id,
            is_primary: field.is_primary,
        };

        let cfg_field_id = self.cfg.fields.alloc(field_info);
        self.field_map.insert(field_id, cfg_field_id);
        cfg_field_id
    }

    fn visit_partition(&mut self, partition_id: ast::PartitionId) -> FunctionId {
        let partition = &self.ast.partitions[partition_id];

        let implementation = if partition.implementation.is_some() {
            FunctionImplementation::Concrete
        } else {
            FunctionImplementation::Abstract
        };

        // Create function with empty parameters initially
        let func_cfg = FunctionCfg {
            name: partition.name.clone(),
            function_type: FunctionType::Partition,
            implementation,
            return_type: ReturnType::Type(TypeName::Int),
            span: partition.span.clone(),
            parameters: Vec::new(), // Will be filled inside function context
            local_variables: Vec::new(),
            hops: Vec::new(),
            blocks: Vec::new(),
            entry_hop: None,
            hop_order: Vec::new(),
        };

        let cfg_func_id = self.cfg.functions.alloc(func_cfg);
        self.partition_map.insert(partition_id, cfg_func_id);

        // Process parameters and implementation inside function context
        self.with_function_context(cfg_func_id, |builder| {
            // Process parameters inside function context for consistent variable scoping
            let cfg_params = builder.visit_parameters(&partition.parameters);
            builder.cfg.functions[cfg_func_id].parameters = cfg_params;

            // Process implementation if exists
            if let Some(expr_id) = partition.implementation {
                let hop_id = builder.create_hop(cfg_func_id);
                let block_id = builder.create_basic_block(hop_id);

                builder.with_hop_context(hop_id, block_id, |builder| {
                    let (result_var, stmts) = builder.visit_expression(expr_id);
                    builder.add_statements_to_current_block(stmts);
                    builder.add_return_to_current_block(Some(Operand::Var(result_var)));
                });

                builder.finalize_function_with_single_hop(cfg_func_id, hop_id, block_id);
            }
        });

        cfg_func_id
    }

    fn visit_function(&mut self, func_id: ast::FunctionId) -> FunctionId {
        let function = &self.ast.functions[func_id];

        let func_cfg = FunctionCfg {
            name: function.name.clone(),
            function_type: FunctionType::Transaction,
            implementation: FunctionImplementation::Concrete,
            return_type: function.return_type.clone(),
            span: function.span.clone(),
            parameters: Vec::new(), // Will be populated inside function context
            local_variables: Vec::new(),
            hops: Vec::new(),
            blocks: Vec::new(),
            entry_hop: None,
            hop_order: Vec::new(),
        };

        let cfg_func_id = self.cfg.functions.alloc(func_cfg);
        self.function_map.insert(func_id, cfg_func_id);

        // Process parameters and hops inside function context for consistent scope
        self.with_function_context(cfg_func_id, |builder| {
            // Process parameters INSIDE function context so scope is consistent
            let cfg_params = builder.visit_parameters(&function.parameters);
            builder.cfg.functions[cfg_func_id].parameters = cfg_params;

            // Process hops
            if !function.hops.is_empty() {
                let mut cfg_hops = Vec::new();
                for &hop_id in &function.hops {
                    let cfg_hop_id = builder.visit_hop(hop_id, cfg_func_id);
                    cfg_hops.push(cfg_hop_id);
                }

                // Set function's hop information (visit_hop already registers hops properly)
                if !cfg_hops.is_empty() {
                    builder.cfg.functions[cfg_func_id].entry_hop = Some(cfg_hops[0]);
                    // hop_order is maintained by individual hop processing
                }
            }
        });

        cfg_func_id
    }

    fn visit_parameters(&mut self, param_ids: &[ast::ParameterId]) -> Vec<VarId> {
        let mut cfg_params = Vec::new();

        // NEW APPROACH: Use parameter names directly with current function scope
        for &param_id in param_ids {
            let param = &self.ast.parameters[param_id];

            // Create or get parameter using scope-aware mapping
            let cfg_var_id = self.get_or_create_variable(
                &param.param_name,
                &param.param_type,
                VariableKind::Parameter,
                &param.span,
            );
            cfg_params.push(cfg_var_id);
        }

        cfg_params
    }

    fn visit_hop(&mut self, hop_id: ast::HopId, func_id: FunctionId) -> HopId {
        let hop = &self.ast.hops[hop_id];

        let cfg_hop_id = self.create_hop(func_id);

        match &hop.hop_type {
            ast::HopType::Simple => {
                let block_id = self.create_basic_block(cfg_hop_id);

                self.with_hop_context(cfg_hop_id, block_id, |builder| {
                    let _blocks = builder.visit_statement_list(&hop.statements);
                    // Blocks are automatically registered via create_basic_block and visit_statement_list

                    // Add implicit void return if the hop doesn't end with an explicit return
                    // This is necessary for proper liveness analysis and control flow
                    if let Some(current_block_id) = builder.current_block {
                        let current_block = &builder.cfg.blocks[current_block_id];
                        let has_return_edge = current_block.successors.iter().any(|edge| {
                            matches!(edge.edge_type, EdgeType::Return { .. } | EdgeType::Abort)
                        });

                        if !has_return_edge {
                            // Add implicit void return for transaction functions
                            builder.add_return_to_current_block(None);
                        }
                    }
                });

                self.finalize_hop_with_entry_block(cfg_hop_id, func_id, block_id);

                // Register hop in function (avoiding duplicates)
                if !self.cfg.functions[func_id].hops.contains(&cfg_hop_id) {
                    self.cfg.functions[func_id].hops.push(cfg_hop_id);
                }
                if !self.cfg.functions[func_id].hop_order.contains(&cfg_hop_id) {
                    self.cfg.functions[func_id].hop_order.push(cfg_hop_id);
                }
            }
            ast::HopType::ForLoop {
                loop_var,
                loop_var_type,
                start: _,
                end: _,
                start_value,
                end_value,
            } => {
                match (start_value, end_value) {
                    (Some(start_val), Some(end_val)) => {
                        // Both bounds are compile-time constants - unroll the hop loop
                        let _blocks = self.process_unrolled_hop_loop(
                            cfg_hop_id,
                            func_id,
                            loop_var,
                            loop_var_type,
                            *start_val,
                            *end_val,
                            &hop.statements,
                        );
                        // process_unrolled_hop_loop handles hop registration
                    }
                    (None, _) => {
                        panic!("Hop loop start value must be a compile-time constant, but got dynamic expression");
                    }
                    (_, None) => {
                        panic!("Hop loop end value must be a compile-time constant, but got dynamic expression");
                    }
                }
            }
        }

        cfg_hop_id
    }

    // ===== Statement visitors =====

    fn visit_statement_list(&mut self, stmt_ids: &[ast::StatementId]) -> Vec<BasicBlockId> {
        let mut all_blocks = Vec::new();

        for &stmt_id in stmt_ids {
            let new_blocks = self.visit_statement(stmt_id);
            all_blocks.extend(new_blocks.iter().cloned());

            // Ensure all newly created blocks are registered in current hop and function
            // (This handles blocks created by control flow statements like if/for/while)
            if let Some(current_hop) = self.current_hop {
                for &block_id in &new_blocks {
                    self.register_block_in_hop(current_hop, block_id);
                    if let Some(function_id) = self.current_function {
                        self.register_block_in_function(function_id, block_id);
                    }
                }
            }
        }

        all_blocks
    }

    fn visit_statement(&mut self, stmt_id: ast::StatementId) -> Vec<BasicBlockId> {
        let stmt = &self.ast.statements[stmt_id];

        match &stmt.node {
            ast::StatementKind::Assignment(assign) => {
                self.visit_assignment(assign, stmt.span.clone())
            }
            ast::StatementKind::VarDecl(var_decl) => {
                self.visit_var_decl(var_decl, stmt.span.clone())
            }
            ast::StatementKind::IfStmt(if_stmt) => {
                self.visit_if_statement(if_stmt, stmt.span.clone())
            }
            ast::StatementKind::ForStmt(for_stmt) => {
                self.visit_for_statement(for_stmt, stmt.span.clone())
            }
            ast::StatementKind::WhileStmt(while_stmt) => {
                self.visit_while_statement(while_stmt, stmt.span.clone())
            }
            ast::StatementKind::Return(ret_stmt) => {
                self.visit_return_statement(ret_stmt, stmt.span.clone())
            }
            ast::StatementKind::Abort(_) => {
                self.visit_simple_statement(SimpleStatementType::Abort, stmt.span.clone())
            }
            ast::StatementKind::Break(_) => {
                self.visit_simple_statement(SimpleStatementType::Break, stmt.span.clone())
            }
            ast::StatementKind::Continue(_) => {
                self.visit_simple_statement(SimpleStatementType::Continue, stmt.span.clone())
            }
            ast::StatementKind::Expression(expr_stmt) => {
                self.visit_expression_statement(expr_stmt, stmt.span.clone())
            }
            ast::StatementKind::Empty => Vec::new(),
        }
    }

    fn visit_assignment(
        &mut self,
        assign: &ast::AssignmentStatement,
        span: Span,
    ) -> Vec<BasicBlockId> {
        // Check for table record assignment with record literal
        if let (
            ast::LValue::TableRecord {
                table_name,
                pk_fields,
                pk_exprs,
                resolved_table,
                ..
            },
            rhs_expr,
        ) = (&assign.lvalue, &self.ast.expressions[assign.rhs].node)
        {
            if let ast::ExpressionKind::RecordLiteral { fields, .. } = rhs_expr {
                // This is a table record assignment with record literal

                if fields.is_empty() {
                    // Empty record assignment - add TODO comment and don't generate anything
                    // TODO: Handle empty record assignments - should not generate any CFG statements
                    return Vec::new();
                }

                // Expand non-empty record into multiple individual field assignments

                // Process each field assignment individually
                for field_assign in fields {
                    // Create a new LValue for each field
                    let field_lvalue = ast::LValue::TableField {
                        table_name: table_name.clone(),
                        pk_fields: pk_fields.clone(),
                        pk_exprs: pk_exprs.clone(),
                        field_name: field_assign.field_name.clone(),
                        resolved_table: resolved_table.clone(),
                        resolved_pk_fields: vec![None; pk_fields.len()], // Will be resolved later
                        resolved_field: field_assign.resolved_field,
                    };

                    // Create individual assignment
                    let field_assignment = ast::AssignmentStatement {
                        lvalue: field_lvalue,
                        operator: ast::AssignmentOperator::Assign,
                        rhs: field_assign.value,
                    };

                    // Recursively process the individual field assignment
                    self.visit_assignment(&field_assignment, span.clone());
                }

                return Vec::new();
            } else if let ast::ExpressionKind::ArrayLiteral { elements, .. } = rhs_expr {
                // Handle empty array literal (which might represent empty record {})
                if elements.is_empty() {
                    // Empty record assignment - add TODO comment and don't generate anything
                    // TODO: Handle empty record assignments - should not generate any CFG statements
                    return Vec::new();
                }
            }
        }

        // Normal assignment processing
        // Process RHS expression
        let (rhs_var, rhs_stmts) = self.visit_expression(assign.rhs);
        self.add_statements_to_current_block(rhs_stmts);

        // Process LHS
        let lvalue = self.visit_lvalue(&assign.lvalue);

        // Handle compound assignment operators
        let final_rvalue = match &assign.operator {
            ast::AssignmentOperator::Assign => RValue::Use(Operand::Var(rhs_var)),
            compound_op => {
                let lhs_operand = self.lvalue_to_operand(&lvalue);
                let result_var = self.create_temp_variable(self.infer_operand_type(&lhs_operand));

                let binary_op = match compound_op {
                    ast::AssignmentOperator::AddAssign => BinaryOp::Add,
                    ast::AssignmentOperator::SubAssign => BinaryOp::Sub,
                    ast::AssignmentOperator::MulAssign => BinaryOp::Mul,
                    ast::AssignmentOperator::DivAssign => BinaryOp::Div,
                    _ => unreachable!("Already handled Assign case"),
                };

                let stmt = Statement::Assign {
                    lvalue: LValue::Variable { var: result_var },
                    rvalue: RValue::BinaryOp {
                        op: binary_op,
                        left: lhs_operand,
                        right: Operand::Var(rhs_var),
                    },
                    span: span.clone(),
                };
                self.add_statement_to_current_block(stmt);
                RValue::Use(Operand::Var(result_var))
            }
        };

        let stmt = Statement::Assign {
            lvalue,
            rvalue: final_rvalue,
            span,
        };
        self.add_statement_to_current_block(stmt);

        Vec::new() // Assignment doesn't create new blocks
    }

    fn visit_var_decl(
        &mut self,
        var_decl: &ast::VarDeclStatement,
        span: Span,
    ) -> Vec<BasicBlockId> {
        // NEW APPROACH: Create or get variable using scope + name
        let cfg_var_id = self.get_or_create_variable(
            &var_decl.var_name,
            &var_decl.var_type,
            VariableKind::Local,
            &span,
        );

        // Add to current function's local variables
        if let Some(func_id) = self.current_function {
            self.cfg.functions[func_id].local_variables.push(cfg_var_id);
        }

        // Process initialization if present
        if let Some(init_expr) = var_decl.init_value {
            // Check for array variable with array literal initialization
            if let (
                ast::TypeName::Array { .. },
                ast::ExpressionKind::ArrayLiteral { elements, .. },
            ) = (&var_decl.var_type, &self.ast.expressions[init_expr].node)
            {
                // Array initialization with array literal - expand into individual element assignments
                for (index, &element_expr) in elements.iter().enumerate() {
                    // Process the element expression
                    let (element_var, element_stmts) = self.visit_expression(element_expr);
                    self.add_statements_to_current_block(element_stmts);

                    // Create individual element assignment: array[index] = element
                    let index_const = self.create_temp_variable(TypeName::Int);
                    let index_stmt = Statement::Assign {
                        lvalue: LValue::Variable { var: index_const },
                        rvalue: RValue::Use(Operand::Const(Constant::Int(index as i64))),
                        span: span.clone(),
                    };
                    self.add_statement_to_current_block(index_stmt);

                    let element_assign = Statement::Assign {
                        lvalue: LValue::ArrayElement {
                            array: cfg_var_id,
                            index: Operand::Var(index_const),
                        },
                        rvalue: RValue::Use(Operand::Var(element_var)),
                        span: span.clone(),
                    };
                    self.add_statement_to_current_block(element_assign);
                }
            } else {
                // Normal initialization (not array literal)
                let (init_var, init_stmts) = self.visit_expression(init_expr);
                self.add_statements_to_current_block(init_stmts);

                let stmt = Statement::Assign {
                    lvalue: LValue::Variable { var: cfg_var_id },
                    rvalue: RValue::Use(Operand::Var(init_var)),
                    span,
                };
                self.add_statement_to_current_block(stmt);
            }
        }

        Vec::new()
    }

    fn visit_if_statement(&mut self, if_stmt: &ast::IfStatement, _span: Span) -> Vec<BasicBlockId> {
        // Process condition
        let (cond_var, cond_stmts) = self.visit_expression(if_stmt.condition);
        self.add_statements_to_current_block(cond_stmts);

        // Create blocks
        let then_block = self.create_basic_block(self.current_hop.unwrap());
        let else_block = if if_stmt.else_branch.is_some() {
            Some(self.create_basic_block(self.current_hop.unwrap()))
        } else {
            None
        };
        let merge_block = self.create_basic_block(self.current_hop.unwrap());

        // Add conditional jump from current block
        self.add_conditional_jump_from_current_block(
            cond_var,
            then_block,
            else_block.unwrap_or(merge_block),
        );

        // Process then branch
        self.current_block = Some(then_block);
        self.visit_statement_list(&if_stmt.then_branch);
        self.add_jump_from_current_block_to(merge_block);

        // Process else branch if present
        if let Some(ref else_stmts) = if_stmt.else_branch {
            self.current_block = Some(else_block.unwrap());
            self.visit_statement_list(else_stmts);
            self.add_jump_from_current_block_to(merge_block);
        }

        // Set merge block as current
        self.current_block = Some(merge_block);

        let mut blocks = vec![then_block];
        if let Some(eb) = else_block {
            blocks.push(eb);
        }
        blocks.push(merge_block);
        blocks
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: &ast::ForStatement,
        span: Span,
    ) -> Vec<BasicBlockId> {
        // First, try to find the for loop variable via AST resolution
        // For statements create their own scoped variables in name resolution
        let loop_var_id = self.get_or_create_variable(
            &for_stmt.loop_var,
            &for_stmt.loop_var_type,
            VariableKind::Local,
            &span,
        );

        // Add to current function's local variables if not already present
        if let Some(func_id) = self.current_function {
            if !self.cfg.functions[func_id]
                .local_variables
                .contains(&loop_var_id)
            {
                self.cfg.functions[func_id]
                    .local_variables
                    .push(loop_var_id);
            }
        }

        // Initialize loop variable with init expression
        let (init_var, init_stmts) = self.visit_expression(for_stmt.init);
        self.add_statements_to_current_block(init_stmts);

        let init_stmt = Statement::Assign {
            lvalue: LValue::Variable { var: loop_var_id },
            rvalue: RValue::Use(Operand::Var(init_var)),
            span: Span::default(),
        };
        self.add_statement_to_current_block(init_stmt);

        // Create loop blocks
        let condition_block = self.create_basic_block(self.current_hop.unwrap());
        let body_block = self.create_basic_block(self.current_hop.unwrap());
        let increment_block = self.create_basic_block(self.current_hop.unwrap());
        let exit_block = self.create_basic_block(self.current_hop.unwrap());

        // Push loop context for break/continue statements
        self.loop_context_stack.push(LoopContext {
            continue_target: increment_block,
            break_target: exit_block,
        });

        // Jump to condition block
        self.add_jump_from_current_block_to(condition_block);

        // Process condition block
        self.current_block = Some(condition_block);
        let (cond_var, cond_stmts) = self.visit_expression(for_stmt.condition);
        self.add_statements_to_current_block(cond_stmts);

        // Add conditional jump: if condition true -> body, else -> exit
        self.add_conditional_jump_from_current_block(cond_var, body_block, exit_block);

        // Process body block
        self.current_block = Some(body_block);
        self.visit_statement_list(&for_stmt.body);
        self.add_jump_from_current_block_to(increment_block);

        // Process increment block
        self.current_block = Some(increment_block);
        let (incr_var, incr_stmts) = self.visit_expression(for_stmt.increment);
        self.add_statements_to_current_block(incr_stmts);

        // Update loop variable with increment
        let update_stmt = Statement::Assign {
            lvalue: LValue::Variable { var: loop_var_id },
            rvalue: RValue::Use(Operand::Var(incr_var)),
            span: Span::default(),
        };
        self.add_statement_to_current_block(update_stmt);

        // Jump back to condition
        self.add_jump_from_current_block_to(condition_block);

        // Pop loop context
        self.loop_context_stack.pop();

        // Set exit block as current
        self.current_block = Some(exit_block);

        vec![condition_block, body_block, increment_block, exit_block]
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: &ast::WhileStatement,
        _span: Span,
    ) -> Vec<BasicBlockId> {
        // Create loop blocks
        let condition_block = self.create_basic_block(self.current_hop.unwrap());
        let body_block = self.create_basic_block(self.current_hop.unwrap());
        let exit_block = self.create_basic_block(self.current_hop.unwrap());

        // Push loop context for break/continue statements
        self.loop_context_stack.push(LoopContext {
            continue_target: condition_block, // Continue goes back to condition
            break_target: exit_block,
        });

        // Jump to condition block
        self.add_jump_from_current_block_to(condition_block);

        // Process condition block
        self.current_block = Some(condition_block);
        let (cond_var, cond_stmts) = self.visit_expression(while_stmt.condition);
        self.add_statements_to_current_block(cond_stmts);

        // Add conditional jump: if condition true -> body, else -> exit
        self.add_conditional_jump_from_current_block(cond_var, body_block, exit_block);

        // Process body block
        self.current_block = Some(body_block);
        self.visit_statement_list(&while_stmt.body);

        // Jump back to condition after body execution
        self.add_jump_from_current_block_to(condition_block);

        // Pop loop context
        self.loop_context_stack.pop();

        // Set exit block as current
        self.current_block = Some(exit_block);

        vec![condition_block, body_block, exit_block]
    }

    fn visit_return_statement(
        &mut self,
        ret_stmt: &ast::ReturnStatement,
        _span: Span,
    ) -> Vec<BasicBlockId> {
        let return_value = if let Some(expr_id) = ret_stmt.value {
            let (result_var, expr_stmts) = self.visit_expression(expr_id);
            self.add_statements_to_current_block(expr_stmts);
            Some(Operand::Var(result_var))
        } else {
            None
        };

        self.add_return_to_current_block(return_value);
        Vec::new()
    }

    fn visit_simple_statement(
        &mut self,
        stmt_type: SimpleStatementType,
        _span: Span,
    ) -> Vec<BasicBlockId> {
        match stmt_type {
            SimpleStatementType::Abort => self.add_abort_to_current_block(),
            SimpleStatementType::Break => {
                if let Some(loop_context) = self.loop_context_stack.last() {
                    self.add_jump_from_current_block_to(loop_context.break_target);
                } else {
                    panic!("Break statement outside of loop context");
                }
            }
            SimpleStatementType::Continue => {
                if let Some(loop_context) = self.loop_context_stack.last() {
                    self.add_jump_from_current_block_to(loop_context.continue_target);
                } else {
                    panic!("Continue statement outside of loop context");
                }
            }
        }
        Vec::new()
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &ast::ExpressionStatement,
        _span: Span,
    ) -> Vec<BasicBlockId> {
        let (_, expr_stmts) = self.visit_expression(expr_stmt.expression);
        self.add_statements_to_current_block(expr_stmts);
        Vec::new()
    }

    // ===== Expression visitors =====

    fn visit_expression(&mut self, expr_id: ast::ExpressionId) -> (VarId, Vec<Statement>) {
        let expr = &self.ast.expressions[expr_id];

        match &expr.node {
            ast::ExpressionKind::IntLit(val) => {
                self.visit_literal(Constant::Int(*val), TypeName::Int, expr.span.clone())
            }
            ast::ExpressionKind::FloatLit(val) => self.visit_literal(
                Constant::Float(ordered_float::OrderedFloat(*val)),
                TypeName::Float,
                expr.span.clone(),
            ),
            ast::ExpressionKind::StringLit(val) => self.visit_literal(
                Constant::String(val.clone()),
                TypeName::String,
                expr.span.clone(),
            ),
            ast::ExpressionKind::BoolLit(val) => {
                self.visit_literal(Constant::Bool(*val), TypeName::Bool, expr.span.clone())
            }
            ast::ExpressionKind::Ident(name) => {
                // NEW APPROACH: Use variable name + current scope to get consistent CFG variable
                let scope_key = (self.current_function, name.clone());

                if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
                    let temp = self.create_temp_variable(self.cfg.variables[cfg_var_id].ty.clone());
                    let stmt = Statement::Assign {
                        lvalue: LValue::Variable { var: temp },
                        rvalue: RValue::Use(Operand::Var(cfg_var_id)),
                        span: expr.span.clone(),
                    };
                    (temp, vec![stmt])
                } else {
                    // Fallback: Try to get type information from AST resolution to create the variable
                    if let Some(&ast_var_id) = self.ast.resolutions.get(&expr_id) {
                        if let Some(var_type) = self.ast.var_types.get(&ast_var_id) {
                            // Create the variable using scope-aware mapping
                            let cfg_var_id = self.get_or_create_variable(
                                name,
                                var_type,
                                VariableKind::Local, // Default to local - this could be refined
                                &expr.span,
                            );

                            let temp = self.create_temp_variable(var_type.clone());
                            let stmt = Statement::Assign {
                                lvalue: LValue::Variable { var: temp },
                                rvalue: RValue::Use(Operand::Var(cfg_var_id)),
                                span: expr.span.clone(),
                            };
                            (temp, vec![stmt])
                        } else {
                            panic!(
                                "Variable type not found for: {} (AST VarId: {:?})",
                                name, ast_var_id
                            )
                        }
                    } else {
                        panic!("Variable not found in scope and AST resolution failed: {} (scope: {:?})", name, scope_key)
                    }
                }
            }
            ast::ExpressionKind::BinaryOp {
                left, op, right, ..
            } => self.visit_binary_op(*left, op, *right, expr.span.clone()),
            ast::ExpressionKind::UnaryOp {
                op, expr: operand, ..
            } => self.visit_unary_op(op, *operand, expr.span.clone()),
            ast::ExpressionKind::TableFieldAccess {
                table_name,
                pk_exprs,
                field_name,
                ..
            } => self.visit_table_field_access(table_name, pk_exprs, field_name, expr.span.clone()),
            ast::ExpressionKind::TableAccess {
                table_name,
                pk_exprs,
                pk_fields,
                ..
            } => self.visit_table_access(table_name, pk_exprs, pk_fields, expr.span.clone()),
            ast::ExpressionKind::ArrayAccess {
                array_name, index, ..
            } => {
                // NEW APPROACH: Use array name + current scope for consistent lookup
                let scope_key = (self.current_function, array_name.clone());

                if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
                    self.visit_array_access_resolved(cfg_var_id, *index, expr.span.clone())
                } else {
                    panic!(
                        "Array not found in scope: {} (scope: {:?})",
                        array_name, scope_key
                    )
                }
            }
            ast::ExpressionKind::ArrayLiteral { elements, .. } => {
                self.visit_array_literal(elements, expr.span.clone())
            }
            ast::ExpressionKind::RecordLiteral { fields, .. } => {
                self.visit_record_literal(fields, expr.span.clone())
            }
            ast::ExpressionKind::FieldAccess {
                object_name,
                field_name,
                ..
            } => {
                // NEW APPROACH: Use object name + current scope for consistent lookup
                let scope_key = (self.current_function, object_name.clone());

                if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
                    self.visit_field_access_resolved(cfg_var_id, field_name, expr.span.clone())
                } else {
                    panic!(
                        "Object not found in scope: {} (scope: {:?})",
                        object_name, scope_key
                    )
                }
            }
        }
    }

    fn visit_literal(
        &mut self,
        constant: Constant,
        ty: TypeName,
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let temp = self.create_temp_variable(ty);
        let stmt = Statement::Assign {
            lvalue: LValue::Variable { var: temp },
            rvalue: RValue::Use(Operand::Const(constant)),
            span,
        };
        (temp, vec![stmt])
    }

    fn visit_binary_op(
        &mut self,
        left: ast::ExpressionId,
        op: &ast::BinaryOp,
        right: ast::ExpressionId,
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();

        let (left_var, left_stmts) = self.visit_expression(left);
        stmts.extend(left_stmts);

        let (right_var, right_stmts) = self.visit_expression(right);
        stmts.extend(right_stmts);

        let cfg_op = self.map_binary_op(op);
        let result_type = self.infer_binary_op_type(
            &self.cfg.variables[left_var].ty,
            &self.cfg.variables[right_var].ty,
            &cfg_op,
        );

        let result = self.create_temp_variable(result_type);
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::BinaryOp {
                op: cfg_op,
                left: Operand::Var(left_var),
                right: Operand::Var(right_var),
            },
            span,
        });

        (result, stmts)
    }

    fn visit_unary_op(
        &mut self,
        op: &ast::UnaryOp,
        operand: ast::ExpressionId,
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        match op {
            ast::UnaryOp::Not | ast::UnaryOp::Neg => {
                // Standard unary operations
                let (operand_var, mut stmts) = self.visit_expression(operand);

                let cfg_op = self.map_unary_op(op);
                let result_type = self.cfg.variables[operand_var].ty.clone();
                let result = self.create_temp_variable(result_type);

                stmts.push(Statement::Assign {
                    lvalue: LValue::Variable { var: result },
                    rvalue: RValue::UnaryOp {
                        op: cfg_op,
                        operand: Operand::Var(operand_var),
                    },
                    span,
                });

                (result, stmts)
            }
            ast::UnaryOp::PreIncrement => {
                self.handle_increment_decrement(operand, span, true, true)
            }
            ast::UnaryOp::PostIncrement => {
                self.handle_increment_decrement(operand, span, false, true)
            }
            ast::UnaryOp::PreDecrement => {
                self.handle_increment_decrement(operand, span, true, false)
            }
            ast::UnaryOp::PostDecrement => {
                self.handle_increment_decrement(operand, span, false, false)
            }
        }
    }

    fn visit_table_field_access(
        &mut self,
        table_name: &str,
        pk_exprs: &[ast::ExpressionId],
        field_name: &str,
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();

        // Resolve table
        let table_id = self
            .ast
            .table_map
            .get(table_name)
            .and_then(|&ast_table_id| self.table_map.get(&ast_table_id))
            .copied()
            .unwrap_or_else(|| panic!("Undefined table: {}", table_name));

        // Get table info and find field (extract the values we need before borrowing self mutably)
        let (primary_keys, field_id, field_ty) = {
            let table_info = &self.cfg.tables[table_id];

            // Find field
            let field_id = table_info
                .fields
                .iter()
                .find(|&&fid| self.cfg.fields[fid].name == *field_name)
                .copied()
                .unwrap_or_else(|| {
                    panic!("Field {} not found in table {}", field_name, table_name)
                });

            let field_info = &self.cfg.fields[field_id];

            (
                table_info.primary_keys.clone(),
                field_id,
                field_info.ty.clone(),
            )
        };

        // Process primary key expressions
        let mut pk_values = Vec::new();
        for &pk_expr in pk_exprs {
            let (pk_var, pk_stmts) = self.visit_expression(pk_expr);
            stmts.extend(pk_stmts);
            pk_values.push(Operand::Var(pk_var));
        }

        let result = self.create_temp_variable(field_ty);
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::TableAccess {
                table: table_id,
                pk_fields: primary_keys,
                pk_values,
                field: field_id,
            },
            span,
        });

        (result, stmts)
    }

    fn visit_array_access_resolved(
        &mut self,
        array_var: VarId,
        index: ast::ExpressionId,
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();

        let (index_var, index_stmts) = self.visit_expression(index);
        stmts.extend(index_stmts);

        // Get element type
        let array_type = &self.cfg.variables[array_var].ty;
        let elem_type = self.get_array_element_type(array_type);

        let result = self.create_temp_variable(elem_type);
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::ArrayAccess {
                array: Operand::Var(array_var),
                index: Operand::Var(index_var),
            },
            span,
        });

        (result, stmts)
    }

    fn visit_array_literal(
        &mut self,
        elements: &[ast::ExpressionId],
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();
        let mut element_constants = Vec::new();

        // Process each element and determine the array element type
        let element_type = if elements.is_empty() {
            TypeName::Int // Default for empty arrays
        } else {
            // Process first element to determine type
            let (first_elem_var, first_elem_stmts) = self.visit_expression(elements[0]);
            stmts.extend(first_elem_stmts);

            let elem_type = self.cfg.variables[first_elem_var].ty.clone();

            // Convert first element to constant if possible
            element_constants.push(self.extract_constant_from_var(first_elem_var));

            // Process remaining elements
            for &elem_id in &elements[1..] {
                let (elem_var, elem_stmts) = self.visit_expression(elem_id);
                stmts.extend(elem_stmts);
                element_constants.push(self.extract_constant_from_var(elem_var));
            }

            elem_type
        };

        let result = self.create_temp_variable(TypeName::Array {
            element_type: Box::new(element_type),
            size: Some(elements.len()),
            size_expr: None,
        });

        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::Use(Operand::Const(Constant::Array(element_constants))),
            span,
        });

        (result, stmts)
    }

    fn visit_table_access(
        &mut self,
        table_name: &str,
        pk_exprs: &[ast::ExpressionId],
        pk_fields: &[String],
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();

        // Resolve table
        let table_id = self
            .ast
            .table_map
            .get(table_name)
            .and_then(|&ast_table_id| self.table_map.get(&ast_table_id))
            .copied()
            .unwrap_or_else(|| panic!("Undefined table: {}", table_name));

        // Get table info (extract the values we need before borrowing self mutably)
        let table_type = {
            let table_info = &self.cfg.tables[table_id];

            // Verify pk_fields match table primary keys - for validation only
            for pk_field_name in pk_fields {
                table_info
                    .fields
                    .iter()
                    .find(|&&fid| self.cfg.fields[fid].name == *pk_field_name)
                    .copied()
                    .unwrap_or_else(|| {
                        panic!(
                            "Primary key field {} not found in table {}",
                            pk_field_name, table_name
                        )
                    });
            }

            // Use Table type for table record access
            TypeName::Table(table_name.to_string())
        };

        // Process primary key expressions
        let mut pk_values = Vec::new();
        for &pk_expr in pk_exprs {
            let (pk_var, pk_stmts) = self.visit_expression(pk_expr);
            stmts.extend(pk_stmts);
            pk_values.push(Operand::Var(pk_var));
        }

        // For now, create a temporary variable to represent the table record
        // In a full implementation, this would need proper table record types
        let result = self.create_temp_variable(table_type);

        // Create a placeholder assignment - this would need to be extended with proper table record access
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::Use(Operand::Var(result)), // Placeholder - needs proper table record access
            span,
        });

        (result, stmts)
    }

    fn visit_record_literal(
        &mut self,
        fields: &[ast::FieldAssignment],
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();

        // For now, use a simplified approach since we don't have Record types
        // Process each field assignment to ensure expressions are evaluated
        for field_assign in fields {
            let (_, field_stmts) = self.visit_expression(field_assign.value);
            stmts.extend(field_stmts);
        }

        // Create a placeholder result - in a full implementation this would need proper record types
        let result = self.create_temp_variable(TypeName::String); // Placeholder type
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::Use(Operand::Const(Constant::String(
                "record_literal".to_string(),
            ))),
            span,
        });

        (result, stmts)
    }

    fn visit_field_access_resolved(
        &mut self,
        object_var: VarId,
        field_name: &str,
        span: Span,
    ) -> (VarId, Vec<Statement>) {
        let mut stmts = Vec::new();

        // Get object type - for now assume it's a table type
        let result_type = match &self.cfg.variables[object_var].ty {
            TypeName::Table(table_name) => {
                // Find the field in the table and return its type
                let table_id = self
                    .ast
                    .table_map
                    .get(table_name)
                    .and_then(|&ast_table_id| self.table_map.get(&ast_table_id))
                    .copied()
                    .unwrap_or_else(|| panic!("Table {} not found", table_name));

                let table_info = &self.cfg.tables[table_id];
                let field_id = table_info
                    .fields
                    .iter()
                    .find(|&&fid| self.cfg.fields[fid].name == *field_name)
                    .copied()
                    .unwrap_or_else(|| {
                        panic!("Field {} not found in table {}", field_name, table_name)
                    });

                self.cfg.fields[field_id].ty.clone()
            }
            _ => {
                // For non-table types, default to string - this needs proper type system extension
                TypeName::String
            }
        };

        let result = self.create_temp_variable(result_type);

        // Create a placeholder assignment - this would need proper field access implementation
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: result },
            rvalue: RValue::Use(Operand::Var(object_var)), // Placeholder - needs proper field access
            span,
        });

        (result, stmts)
    }

    fn visit_lvalue(&mut self, lvalue: &ast::LValue) -> LValue {
        match lvalue {
            ast::LValue::Var { name, .. } => {
                // NEW APPROACH: Use variable name + current scope for consistent lookup
                let scope_key = (self.current_function, name.clone());

                if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
                    LValue::Variable { var: cfg_var_id }
                } else {
                    panic!(
                        "Variable not found in scope: {} (scope: {:?})",
                        name, scope_key
                    )
                }
            }
            ast::LValue::TableField {
                table_name,
                pk_exprs,
                field_name,
                ..
            } => {
                // Resolve table
                let table_id = self
                    .ast
                    .table_map
                    .get(table_name)
                    .and_then(|&ast_table_id| self.table_map.get(&ast_table_id))
                    .copied()
                    .unwrap_or_else(|| panic!("Undefined table: {}", table_name));

                // Get table info and find field (extract before borrowing mutably)
                let (primary_keys, field_id) = {
                    let table_info = &self.cfg.tables[table_id];

                    // Find field
                    let field_id = table_info
                        .fields
                        .iter()
                        .find(|&&fid| self.cfg.fields[fid].name == *field_name)
                        .copied()
                        .unwrap_or_else(|| {
                            panic!("Field {} not found in table {}", field_name, table_name)
                        });

                    (table_info.primary_keys.clone(), field_id)
                };

                // Process primary key expressions
                let mut pk_values = Vec::new();
                for &pk_expr in pk_exprs {
                    let (pk_var, pk_stmts) = self.visit_expression(pk_expr);
                    self.add_statements_to_current_block(pk_stmts);
                    pk_values.push(Operand::Var(pk_var));
                }

                LValue::TableField {
                    table: table_id,
                    pk_fields: primary_keys,
                    pk_values,
                    field: field_id,
                }
            }
            ast::LValue::ArrayElement {
                array_name, index, ..
            } => {
                // NEW APPROACH: Use array name + current scope for consistent lookup
                let scope_key = (self.current_function, array_name.clone());

                if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
                    let (index_var, index_stmts) = self.visit_expression(*index);
                    self.add_statements_to_current_block(index_stmts);

                    LValue::ArrayElement {
                        array: cfg_var_id,
                        index: Operand::Var(index_var),
                    }
                } else {
                    panic!(
                        "Array not found in scope: {} (scope: {:?})",
                        array_name, scope_key
                    )
                }
            }
            ast::LValue::TableRecord {
                table_name,
                pk_exprs,
                ..
            } => {
                // Resolve table
                let table_id = self
                    .ast
                    .table_map
                    .get(table_name)
                    .and_then(|&ast_table_id| self.table_map.get(&ast_table_id))
                    .copied()
                    .unwrap_or_else(|| panic!("Undefined table: {}", table_name));

                // Get table info (extract the values we need before borrowing self mutably)
                let primary_keys = {
                    let table_info = &self.cfg.tables[table_id];
                    table_info.primary_keys.clone()
                };

                // Process primary key expressions
                let mut pk_values = Vec::new();
                for &pk_expr in pk_exprs {
                    let (pk_var, pk_stmts) = self.visit_expression(pk_expr);
                    self.add_statements_to_current_block(pk_stmts);
                    pk_values.push(Operand::Var(pk_var));
                }

                // For TableRecord LValue, we need to create a synthetic field access
                // since CFG doesn't have direct table record LValues
                // This is a design choice - we could extend CFG to support table record LValues
                // For now, treat it as accessing the entire table record (use first field as placeholder)
                let field_id = self.cfg.tables[table_id]
                    .fields
                    .first()
                    .copied()
                    .unwrap_or_else(|| {
                        panic!("Table {} has no fields for record access", table_name)
                    });

                LValue::TableField {
                    table: table_id,
                    pk_fields: primary_keys,
                    pk_values,
                    field: field_id, // This is a placeholder - TableRecord should ideally be its own LValue type
                }
            }
            ast::LValue::FieldAccess {
                object_name,
                field_name,
                ..
            } => {
                // NEW APPROACH: Use object name + current scope for consistent lookup
                let scope_key = (self.current_function, object_name.clone());

                let object_var = if let Some(&cfg_var_id) = self.scoped_var_map.get(&scope_key) {
                    cfg_var_id
                } else {
                    panic!(
                        "Object not found in scope: {} (scope: {:?})",
                        object_name, scope_key
                    )
                };

                // For field access LValue, we need to create a synthetic table field access
                // This is a simplified approach since we don't have proper object field LValues in CFG
                // In a full implementation, this would need proper object type system

                // Try to find if this is a table variable and resolve the field
                let object_type = &self.cfg.variables[object_var].ty;
                match object_type {
                    TypeName::Table(table_name) => {
                        // Find the table and field
                        let table_id = self
                            .ast
                            .table_map
                            .get(table_name)
                            .and_then(|&ast_table_id| self.table_map.get(&ast_table_id))
                            .copied()
                            .unwrap_or_else(|| {
                                panic!("Table {} not found for field access", table_name)
                            });

                        let table_info = &self.cfg.tables[table_id];
                        let field_id = table_info
                            .fields
                            .iter()
                            .find(|&&fid| self.cfg.fields[fid].name == *field_name)
                            .copied()
                            .unwrap_or_else(|| {
                                panic!("Field {} not found in table {}", field_name, table_name)
                            });

                        // Create primary key values from the object variable
                        // This is a simplified approach - we'd need the actual PK values
                        let pk_values = vec![Operand::Var(object_var)];

                        LValue::TableField {
                            table: table_id,
                            pk_fields: table_info.primary_keys.clone(),
                            pk_values,
                            field: field_id,
                        }
                    }
                    _ => {
                        // For non-table types, this is not supported in current CFG design
                        // Create a synthetic variable access as fallback
                        LValue::Variable { var: object_var }
                    }
                }
            }
        }
    }

    // ===== Helper methods =====

    fn with_function_context<F>(&mut self, func_id: FunctionId, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let old_function = self.current_function;
        self.current_function = Some(func_id);
        f(self);
        self.current_function = old_function;
    }

    fn with_hop_context<F>(&mut self, hop_id: HopId, block_id: BasicBlockId, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let old_hop = self.current_hop;
        let old_block = self.current_block;
        self.current_hop = Some(hop_id);
        self.current_block = Some(block_id);
        f(self);
        self.current_hop = old_hop;
        self.current_block = old_block;
    }

    fn create_temp_variable(&mut self, ty: TypeName) -> VarId {
        let var = Variable {
            name: format!("_t{}", self.next_temp_id),
            ty,
            kind: VariableKind::Temporary,
            value: None,
            span: Span::default(),
        };
        self.next_temp_id += 1;
        self.cfg.variables.alloc(var)
    }

    fn create_hop(&mut self, func_id: FunctionId) -> HopId {
        let hop_cfg = HopCfg {
            function_id: func_id,
            entry_block: None,
            blocks: Vec::new(),
            span: Span::default(),
        };
        self.cfg.hops.alloc(hop_cfg)
    }

    fn create_basic_block(&mut self, hop_id: HopId) -> BasicBlockId {
        let block = BasicBlock {
            hop_id,
            statements: Vec::new(),
            span: Span::default(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        };
        let block_id = self.cfg.blocks.alloc(block);

        // Register block immediately in hop and function
        self.register_block_in_hop(hop_id, block_id);
        if let Some(function_id) = self.current_function {
            self.register_block_in_function(function_id, block_id);
        }

        block_id
    }

    /// Register a block in a hop (avoiding duplicates)
    fn register_block_in_hop(&mut self, hop_id: HopId, block_id: BasicBlockId) {
        if !self.cfg.hops[hop_id].blocks.contains(&block_id) {
            self.cfg.hops[hop_id].blocks.push(block_id);
        }
    }

    /// Register a block in a function (avoiding duplicates)  
    fn register_block_in_function(&mut self, func_id: FunctionId, block_id: BasicBlockId) {
        if !self.cfg.functions[func_id].blocks.contains(&block_id) {
            self.cfg.functions[func_id].blocks.push(block_id);
        }
    }

    fn add_statements_to_current_block(&mut self, stmts: Vec<Statement>) {
        if let Some(block_id) = self.current_block {
            self.cfg.blocks[block_id].statements.extend(stmts);
        }
    }

    fn add_statement_to_current_block(&mut self, stmt: Statement) {
        if let Some(block_id) = self.current_block {
            self.cfg.blocks[block_id].statements.push(stmt);
        }
    }

    fn add_conditional_jump_from_current_block(
        &mut self,
        cond: VarId,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
    ) {
        if let Some(current_block) = self.current_block {
            self.add_control_flow_edge(
                current_block,
                then_block,
                EdgeType::ConditionalTrue {
                    condition: Operand::Var(cond),
                },
            );
            self.add_control_flow_edge(
                current_block,
                else_block,
                EdgeType::ConditionalFalse {
                    condition: Operand::Var(cond),
                },
            );
        }
    }

    fn add_jump_from_current_block_to(&mut self, target: BasicBlockId) {
        if let Some(current_block) = self.current_block {
            self.add_control_flow_edge(current_block, target, EdgeType::Unconditional);
        }
    }

    fn add_return_to_current_block(&mut self, value: Option<Operand>) {
        if let Some(current_block) = self.current_block {
            // Return edges don't have targets, they end the control flow
            let edge = ControlFlowEdge {
                from: current_block,
                to: current_block, // Placeholder - return edges don't have meaningful targets
                edge_type: EdgeType::Return { value },
            };
            self.cfg.blocks[current_block].successors.push(edge);
        }
    }

    fn add_abort_to_current_block(&mut self) {
        if let Some(current_block) = self.current_block {
            let edge = ControlFlowEdge {
                from: current_block,
                to: current_block, // Placeholder - abort edges don't have meaningful targets
                edge_type: EdgeType::Abort,
            };
            self.cfg.blocks[current_block].successors.push(edge);
        }
    }

    fn add_control_flow_edge(&mut self, from: BasicBlockId, to: BasicBlockId, edge_type: EdgeType) {
        let edge = ControlFlowEdge {
            from,
            to,
            edge_type,
        };
        self.cfg.blocks[from].successors.push(edge.clone());
        self.cfg.blocks[to].predecessors.push(edge);
    }

    fn finalize_function_with_single_hop(
        &mut self,
        func_id: FunctionId,
        hop_id: HopId,
        block_id: BasicBlockId,
    ) {
        // Register hop in function
        if !self.cfg.functions[func_id].hops.contains(&hop_id) {
            self.cfg.functions[func_id].hops.push(hop_id);
        }
        self.cfg.functions[func_id].entry_hop = Some(hop_id);

        // Register hop in function's hop order
        if !self.cfg.functions[func_id].hop_order.contains(&hop_id) {
            self.cfg.functions[func_id].hop_order.push(hop_id);
        }

        // Set hop's entry block (blocks already registered via create_basic_block)
        self.cfg.hops[hop_id].entry_block = Some(block_id);
    }

    fn finalize_hop_with_entry_block(
        &mut self,
        hop_id: HopId,
        _func_id: FunctionId,
        entry_block: BasicBlockId,
    ) {
        // Set hop's entry block (block already registered via create_basic_block)
        self.cfg.hops[hop_id].entry_block = Some(entry_block);
    }

    fn resolve_or_create_partition_function(
        &mut self,
        _table: &ast::TableDeclaration,
        node_partition: &ast::NodePartition,
    ) -> FunctionId {
        // First try to find if this partition was already resolved in the AST
        if let Some(partition_id) = node_partition.resolved_partition {
            if let Some(&cfg_func_id) = self.partition_map.get(&partition_id) {
                return cfg_func_id;
            }
        }

        // Try looking up by name in the AST partition map
        if let Some(&partition_id) = self.ast.partition_map.get(&node_partition.partition_name) {
            if let Some(&cfg_func_id) = self.partition_map.get(&partition_id) {
                return cfg_func_id;
            }
            // If not in our partition_map yet, we need to process it first
            return self.visit_partition(partition_id);
        }

        panic!(
            "Partition function {} not found",
            node_partition.partition_name
        );
    }

    fn create_default_partition_function(&mut self, table_name: &str) -> FunctionId {
        let func_name = format!("default_partition_{}", table_name);

        let func_cfg = FunctionCfg {
            name: func_name,
            function_type: FunctionType::Partition,
            implementation: FunctionImplementation::Concrete,
            return_type: ReturnType::Type(TypeName::Int),
            span: Span::default(),
            parameters: Vec::new(),
            local_variables: Vec::new(),
            hops: Vec::new(),
            blocks: Vec::new(),
            entry_hop: None,
            hop_order: Vec::new(),
        };

        self.cfg.functions.alloc(func_cfg)
    }

    fn map_partition_fields(
        &self,
        node_partition: &Option<ast::NodePartition>,
        cfg_fields: &[FieldId],
    ) -> Vec<FieldId> {
        if let Some(partition) = node_partition {
            // Map field names to field IDs
            let mut result = Vec::new();
            for field_name in &partition.arguments {
                if let Some(&field_id) = cfg_fields
                    .iter()
                    .find(|&&fid| self.cfg.fields[fid].name == *field_name)
                {
                    result.push(field_id);
                } else {
                    panic!("Partition field {} not found in table", field_name);
                }
            }
            result
        } else {
            Vec::new()
        }
    }

    fn process_unrolled_hop_loop(
        &mut self,
        hop_id: HopId,
        func_id: FunctionId,
        loop_var: &str,
        loop_var_type: &TypeName,
        start_val: i64,
        end_val: i64,
        statements: &[ast::StatementId],
    ) -> Vec<BasicBlockId> {
        let mut all_blocks = Vec::new();

        // Create a separate hop for each loop iteration
        for iteration in start_val..=end_val {
            // Create a new hop for this iteration
            let iteration_hop_id = self.create_hop(func_id);

            // Create the entry block for this hop iteration
            let block_id = self.create_basic_block(iteration_hop_id);

            self.with_hop_context(iteration_hop_id, block_id, |builder| {
                // Create the loop variable for this iteration with the constant value
                let loop_var_id = builder.cfg.variables.alloc(Variable {
                    name: loop_var.to_string(),
                    ty: loop_var_type.clone(),
                    kind: VariableKind::Local,
                    value: Some(Constant::Int(iteration)),
                    span: Span::default(),
                });

                // Add the loop variable to the function's local variables
                builder.cfg.functions[func_id]
                    .local_variables
                    .push(loop_var_id);

                // NEW APPROACH: Use scope + name for loop variable
                // The loop variable should be consistently mapped using the new system
                let scope_key = (builder.current_function, loop_var.to_string());
                builder.scoped_var_map.insert(scope_key, loop_var_id);

                // Create assignment statement to initialize the loop variable
                let init_stmt = Statement::Assign {
                    lvalue: LValue::Variable { var: loop_var_id },
                    rvalue: RValue::Use(Operand::Const(Constant::Int(iteration))),
                    span: Span::default(),
                };
                builder.add_statement_to_current_block(init_stmt);

                // Process all statements in this hop iteration
                let stmt_blocks = builder.visit_statement_list(statements);

                // The first block is the one we created, subsequent blocks come from complex statements
                all_blocks.extend(stmt_blocks);
            });

            // Finalize this hop iteration
            self.finalize_hop_with_entry_block(iteration_hop_id, func_id, block_id);

            // Add this hop to the function's hop list and hop order (avoiding duplicates)
            if !self.cfg.functions[func_id].hops.contains(&iteration_hop_id) {
                self.cfg.functions[func_id].hops.push(iteration_hop_id);
            }
            if !self.cfg.functions[func_id]
                .hop_order
                .contains(&iteration_hop_id)
            {
                self.cfg.functions[func_id].hop_order.push(iteration_hop_id);
            }

            all_blocks.push(block_id);
        }

        // Update the original hop to be the container/parent hop
        self.cfg.hops[hop_id].entry_block = all_blocks.first().copied();
        self.cfg.hops[hop_id].blocks = all_blocks.clone();

        // Register the main container hop in function (avoiding duplicates)
        if !self.cfg.functions[func_id].hops.contains(&hop_id) {
            self.cfg.functions[func_id].hops.push(hop_id);
        }
        if !self.cfg.functions[func_id].hop_order.contains(&hop_id) {
            self.cfg.functions[func_id].hop_order.push(hop_id);
        }

        all_blocks
    }

    fn lvalue_to_operand(&mut self, lvalue: &LValue) -> Operand {
        match lvalue {
            LValue::Variable { var } => Operand::Var(*var),
            LValue::ArrayElement { array, index } => {
                // For array access, we need to create a temporary variable that holds the array element value
                let temp_var =
                    self.create_temp_variable(self.infer_operand_type(&Operand::Var(*array)));
                let stmt = Statement::Assign {
                    lvalue: LValue::Variable { var: temp_var },
                    rvalue: RValue::ArrayAccess {
                        array: Operand::Var(*array),
                        index: index.clone(),
                    },
                    span: Span::default(),
                };
                self.add_statement_to_current_block(stmt);
                Operand::Var(temp_var)
            }
            LValue::TableField {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                // For table field access, we need to create a temporary variable that holds the field value
                let field_type = self.cfg.fields[*field].ty.clone();
                let temp_var = self.create_temp_variable(field_type);
                let stmt = Statement::Assign {
                    lvalue: LValue::Variable { var: temp_var },
                    rvalue: RValue::TableAccess {
                        table: *table,
                        pk_fields: pk_fields.clone(),
                        pk_values: pk_values.clone(),
                        field: *field,
                    },
                    span: Span::default(),
                };
                self.add_statement_to_current_block(stmt);
                Operand::Var(temp_var)
            }
        }
    }

    fn infer_operand_type(&self, operand: &Operand) -> TypeName {
        match operand {
            Operand::Var(var_id) => self.cfg.variables[*var_id].ty.clone(),
            Operand::Const(constant) => self.constant_type(constant),
        }
    }

    fn infer_binary_op_type(
        &self,
        left_ty: &TypeName,
        right_ty: &TypeName,
        op: &BinaryOp,
    ) -> TypeName {
        match op {
            // Arithmetic operations
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                match (left_ty, right_ty) {
                    (TypeName::Int, TypeName::Int) => TypeName::Int,
                    (TypeName::Float, _) | (_, TypeName::Float) => TypeName::Float,
                    (TypeName::String, TypeName::String) if matches!(op, BinaryOp::Add) => {
                        TypeName::String
                    } // String concatenation
                    _ => left_ty.clone(), // Default to left type
                }
            }
            // Comparison operations - always return bool
            BinaryOp::Lt
            | BinaryOp::Lte
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::Eq
            | BinaryOp::Neq => TypeName::Bool,
            // Logical operations - work on bool, return bool
            BinaryOp::And | BinaryOp::Or => {
                match (left_ty, right_ty) {
                    (TypeName::Bool, TypeName::Bool) => TypeName::Bool,
                    _ => TypeName::Bool, // Coerce to bool for logical ops
                }
            }
        }
    }

    fn get_array_element_type(&self, array_type: &TypeName) -> TypeName {
        match array_type {
            TypeName::Array { element_type, .. } => (**element_type).clone(),
            _ => panic!("Expected array type, got {:?}", array_type),
        }
    }

    fn constant_type(&self, constant: &Constant) -> TypeName {
        match constant {
            Constant::Int(_) => TypeName::Int,
            Constant::Float(_) => TypeName::Float,
            Constant::Bool(_) => TypeName::Bool,
            Constant::String(_) => TypeName::String,
            Constant::Array(elements) => {
                // Determine element type from first element
                let element_type = if elements.is_empty() {
                    TypeName::Int // Default for empty arrays
                } else {
                    self.constant_type(&elements[0])
                };
                TypeName::Array {
                    element_type: Box::new(element_type),
                    size: Some(elements.len()),
                    size_expr: None,
                }
            }
        }
    }

    fn map_binary_op(&self, op: &ast::BinaryOp) -> BinaryOp {
        match op {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::Lt => BinaryOp::Lt,
            ast::BinaryOp::Lte => BinaryOp::Lte,
            ast::BinaryOp::Gt => BinaryOp::Gt,
            ast::BinaryOp::Gte => BinaryOp::Gte,
            ast::BinaryOp::Eq => BinaryOp::Eq,
            ast::BinaryOp::Neq => BinaryOp::Neq,
            ast::BinaryOp::And => BinaryOp::And,
            ast::BinaryOp::Or => BinaryOp::Or,
        }
    }

    fn map_unary_op(&self, op: &ast::UnaryOp) -> UnaryOp {
        match op {
            ast::UnaryOp::Not => UnaryOp::Not,
            ast::UnaryOp::Neg => UnaryOp::Neg,
            ast::UnaryOp::PreIncrement
            | ast::UnaryOp::PostIncrement
            | ast::UnaryOp::PreDecrement
            | ast::UnaryOp::PostDecrement => {
                panic!(
                    "Increment/decrement operators should be handled separately in visit_unary_op"
                )
            }
        }
    }

    fn get_increment_constant(&self, ty: &TypeName) -> Constant {
        match ty {
            TypeName::Int => Constant::Int(1),
            TypeName::Float => Constant::Float(ordered_float::OrderedFloat(1.0)),
            _ => panic!("Increment/decrement not supported for type: {:?}", ty),
        }
    }

    fn handle_increment_decrement(
        &mut self,
        operand: ast::ExpressionId,
        span: Span,
        is_pre: bool,
        is_increment: bool,
    ) -> (VarId, Vec<Statement>) {
        let (operand_var, mut stmts) = self.visit_expression(operand);
        let operand_type = self.cfg.variables[operand_var].ty.clone();

        let binary_op = if is_increment {
            BinaryOp::Add
        } else {
            BinaryOp::Sub
        };
        let modified_var = self.create_temp_variable(operand_type.clone());

        // Create the modified value
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: modified_var },
            rvalue: RValue::BinaryOp {
                op: binary_op,
                left: Operand::Var(operand_var),
                right: Operand::Const(self.get_increment_constant(&operand_type)),
            },
            span: span.clone(),
        });

        // Store the modified value back to the original variable
        stmts.push(Statement::Assign {
            lvalue: LValue::Variable { var: operand_var },
            rvalue: RValue::Use(Operand::Var(modified_var)),
            span: span.clone(),
        });

        if is_pre {
            // Pre-increment/decrement: return the modified value
            (modified_var, stmts)
        } else {
            // Post-increment/decrement: return the original value
            let original_var = self.create_temp_variable(operand_type);
            // Insert assignment to preserve original value at the beginning
            stmts.insert(
                stmts.len() - 2,
                Statement::Assign {
                    lvalue: LValue::Variable { var: original_var },
                    rvalue: RValue::Use(Operand::Var(operand_var)),
                    span: span.clone(),
                },
            );
            (original_var, stmts)
        }
    }

    fn extract_constant_from_var(&self, var_id: VarId) -> Constant {
        let var = &self.cfg.variables[var_id];
        match &var.value {
            Some(constant) => constant.clone(),
            None => {
                // For variables without constant values, create a default constant based on type
                match &var.ty {
                    TypeName::Int => Constant::Int(0),
                    TypeName::Float => Constant::Float(ordered_float::OrderedFloat(0.0)),
                    TypeName::Bool => Constant::Bool(false),
                    TypeName::String => Constant::String(String::new()),
                    _ => Constant::Int(0), // Default fallback
                }
            }
        }
    }
}
