use std::collections::HashMap;

use crate::ast;
use crate::cfg::*;

/// Builds CFG from an AST Program.
pub struct CfgBuilder;

/// Context for building CFG - holds the program being constructed
pub struct CfgCtx {
    pub program: CfgProgram,
    pub table_map: HashMap<String, TableId>,
    pub field_map: HashMap<String, FieldId>,
    pub function_map: HashMap<String, FunctionId>, // Unified map for both partitions and transactions
    pub variable_map: HashMap<String, VarId>, // Unified map for all variables (global constants, parameters, locals)
}

/// Unified helper struct to manage building CFG for both functions and partitions.
struct FunctionContextBuilder<'a> {
    ctx: &'a mut CfgCtx,
    function: FunctionCfg,
    var_map: HashMap<String, VarId>,
    current_hop_id: Option<HopId>,
    current_block_id: Option<BasicBlockId>,
    temp_counter: usize,
}

impl CfgBuilder {
    pub fn new() -> Self {
        Self
    }

    /// Build CFG from AST Program
    pub fn build_from_program(program: &ast::Program) -> Result<CfgCtx, String> {
        let mut ctx = CfgCtx {
            program: CfgProgram {
                tables: id_arena::Arena::new(),
                fields: id_arena::Arena::new(),
                functions: id_arena::Arena::new(),
                variables: id_arena::Arena::new(),
                root_tables: Vec::new(),
                root_functions: Vec::new(),
                root_variables: Vec::new(),
            },
            table_map: HashMap::new(),
            field_map: HashMap::new(),
            function_map: HashMap::new(),
            variable_map: HashMap::new(),
        };

        Self::build_constants(program, &mut ctx)?; // Build global constants first
        Self::build_partitions(program, &mut ctx)?; // Convert partitions to functions first
        Self::build_tables(program, &mut ctx)?; // Then build tables (which reference partitions)
        Self::build_functions(program, &mut ctx)?; // Convert transactions to functions

        // Build predecessor relationships for dataflow analysis
        Self::build_predecessors(&mut ctx);

        Ok(ctx)
    }

    /// Build predecessor relationships for all basic blocks in all functions
    /// This is called automatically after all functions are built
    fn build_predecessors(ctx: &mut CfgCtx) {
        for (_, function) in ctx.program.functions.iter_mut() {
            // Clear existing predecessors
            for (_, block) in function.blocks.iter_mut() {
                block.predecessors.clear();
            }

            // Collect all successor relationships first
            let mut predecessor_edges: Vec<(BasicBlockId, ControlFlowEdge)> = Vec::new();
            for (from_block_id, from_block) in function.blocks.iter() {
                for successor in &from_block.successors {
                    predecessor_edges.push((
                        successor.to,
                        ControlFlowEdge {
                            from: from_block_id,
                            to: successor.to,
                            edge_type: successor.edge_type.clone(),
                        },
                    ));
                }
            }

            // Add predecessors to target blocks
            for (target_block_id, pred_edge) in predecessor_edges {
                if let Some(target_block) = function.blocks.get_mut(target_block_id) {
                    target_block.predecessors.push(pred_edge);
                }
            }
        }
    }

    fn build_constants(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &const_var_id in &program.root_constants {
            let var_decl = &program.variables[const_var_id];

            // Find the corresponding ConstDeclaration to get the expression
            let const_decl = program
                .constants
                .iter()
                .find(|(_, decl)| decl.name == var_decl.name)
                .map(|(_, decl)| decl)
                .ok_or_else(|| {
                    format!(
                        "ConstDeclaration not found for constant '{}'",
                        var_decl.name
                    )
                })?;

            // Try to evaluate the constant expression, but keep the expression if evaluation fails
            let const_value = match Self::evaluate_constant_expression(program, const_decl.value) {
                Ok(value) => Some(value),
                Err(_) => {
                    // TODO: For complex expressions, we'll need to convert them to temp variables
                    // during CFG construction of the specific function that uses them.
                    // For now, we'll store None and handle this during function processing.
                    None
                }
            };

            let variable = Variable {
                name: var_decl.name.clone(),
                ty: var_decl.ty.clone(),
                kind: VariableKind::Global,
                value: const_value,
                span: var_decl.defined_at.clone(),
            };

            let var_id = ctx.program.variables.alloc(variable);
            ctx.variable_map.insert(var_decl.name.clone(), var_id);
            ctx.program.root_variables.push(var_id);
        }
        Ok(())
    }

    /// Simple constant expression evaluator (for now, just handle literals)
    fn evaluate_constant_expression(
        program: &ast::Program,
        expr_id: ast::ExpressionId,
    ) -> Result<Constant, String> {
        let expr = &program.expressions[expr_id];
        match &expr.node {
            ast::ExpressionKind::IntLit(value) => Ok(Constant::Int(*value)),
            ast::ExpressionKind::FloatLit(value) => {
                Ok(Constant::Float(ordered_float::OrderedFloat(*value)))
            }
            ast::ExpressionKind::BoolLit(value) => Ok(Constant::Bool(*value)),
            ast::ExpressionKind::StringLit(value) => Ok(Constant::String(value.clone())),
            _ => Err(format!("Unsupported constant expression: {:?}", expr.node)),
        }
    }

    fn build_partitions(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &partition_id in &program.root_partitions {
            let partition_ast = &program.partitions[partition_id];

            // Use the unified function builder to build partitions
            let builder = FunctionContextBuilder::new_for_partition(ctx, partition_ast)?;
            let function = builder.build_partition(program, partition_ast)?;
            let cfg_function_id = ctx.program.functions.alloc(function);
            ctx.function_map
                .insert(partition_ast.name.clone(), cfg_function_id);
            ctx.program.root_functions.push(cfg_function_id);
        }
        Ok(())
    }

    fn build_tables(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &table_id in &program.root_tables {
            let table_ast = &program.tables[table_id];

            let mut field_ids = Vec::new();
            let mut primary_key_ids = Vec::new();

            for &field_ast_id in &table_ast.fields {
                let field_ast = &program.fields[field_ast_id];

                let cfg_field = FieldInfo {
                    name: field_ast.field_name.clone(),
                    ty: field_ast.field_type.clone(),
                    table_id: None,
                    is_primary: field_ast.is_primary,
                };

                let cfg_field_id = ctx.program.fields.alloc(cfg_field);
                field_ids.push(cfg_field_id);
                ctx.field_map
                    .insert(field_ast.field_name.clone(), cfg_field_id);

                if field_ast.is_primary {
                    primary_key_ids.push(cfg_field_id);
                }
            }

            // Extract partition function and fields
            // Note: Semantic analysis should have ensured that every table has a partition
            let node_partition = table_ast.node_partition.as_ref().expect(
                "Table should have a partition function - this should have been caught by semantic analysis"
            );

            // Get the partition function ID
            let partition_func_id = ctx
                .function_map
                .get(&node_partition.partition_name)
                .ok_or_else(|| {
                    format!(
                        "Partition function '{}' not found for table '{}'",
                        node_partition.partition_name, table_ast.name
                    )
                })?;

            // Map partition argument field names to field IDs
            let mut partition_field_ids = Vec::new();
            for arg_name in &node_partition.arguments {
                let field_id = ctx.field_map.get(arg_name).ok_or_else(|| {
                    format!(
                        "Partition field '{}' not found in table '{}'",
                        arg_name, table_ast.name
                    )
                })?;
                partition_field_ids.push(*field_id);
            }

            let (partition_function, partition_fields) = (*partition_func_id, partition_field_ids);

            let cfg_table = TableInfo {
                name: table_ast.name.clone(),
                fields: field_ids.clone(),
                primary_keys: primary_key_ids,
                partition_function,
                partition_fields,
            };

            let cfg_table_id = ctx.program.tables.alloc(cfg_table);
            ctx.table_map.insert(table_ast.name.clone(), cfg_table_id);
            ctx.program.root_tables.push(cfg_table_id);

            for field_id in field_ids {
                ctx.program.fields[field_id].table_id = Some(cfg_table_id);
            }
        }
        Ok(())
    }

    fn build_functions(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &func_id in &program.root_functions {
            let func_ast = &program.functions[func_id];
            let builder = FunctionContextBuilder::new_for_function(ctx, func_ast)?;
            let function = builder.build_function(program, func_ast)?;
            let cfg_func_id = ctx.program.functions.alloc(function);
            ctx.function_map.insert(func_ast.name.clone(), cfg_func_id);
            ctx.program.root_functions.push(cfg_func_id);
        }
        Ok(())
    }
}

impl<'a> FunctionContextBuilder<'a> {
    fn new_for_function(
        ctx: &'a mut CfgCtx,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<Self, String> {
        let function = FunctionCfg {
            name: func_ast.name.clone(),
            function_type: FunctionType::Transaction, // Regular functions are transactions
            implementation: FunctionImplementation::Concrete, // AST functions are always concrete
            return_type: func_ast.return_type.clone(),
            span: func_ast.span.clone(),
            parameters: Vec::new(),
            local_variables: Vec::new(),
            hops: id_arena::Arena::new(),
            blocks: id_arena::Arena::new(),
            entry_hop: None,
            hop_order: Vec::new(),
        };

        Ok(Self {
            ctx,
            function,
            var_map: HashMap::new(),
            current_hop_id: None,
            current_block_id: None,
            temp_counter: 0,
        })
    }

    fn new_for_partition(
        ctx: &'a mut CfgCtx,
        partition_ast: &ast::PartitionDeclaration,
    ) -> Result<Self, String> {
        // Determine if this partition is abstract or concrete
        let implementation = if partition_ast.implementation.is_some() {
            FunctionImplementation::Concrete
        } else {
            FunctionImplementation::Abstract
        };

        let function = FunctionCfg {
            name: partition_ast.name.clone(),
            function_type: FunctionType::Partition,
            implementation,
            return_type: ast::ReturnType::Type(ast::TypeName::Int), // Partitions always return int
            span: partition_ast.span.clone(),
            parameters: Vec::new(),
            local_variables: Vec::new(),
            hops: id_arena::Arena::new(),
            blocks: id_arena::Arena::new(),
            entry_hop: None,
            hop_order: Vec::new(),
        };

        Ok(Self {
            ctx,
            function,
            var_map: HashMap::new(),
            current_hop_id: None,
            current_block_id: None,
            temp_counter: 0,
        })
    }

    fn build_function(
        mut self,
        program: &ast::Program,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<FunctionCfg, String> {
        self.build_parameters_from_function(program, func_ast)?;
        self.build_hops(program, func_ast)?;
        Ok(self.function)
    }

    fn build_partition(
        mut self,
        program: &ast::Program,
        partition_ast: &ast::PartitionDeclaration,
    ) -> Result<FunctionCfg, String> {
        self.build_parameters_from_partition(program, partition_ast)?;

        // If concrete, build the implementation as a single expression return
        if let Some(impl_expr_id) = partition_ast.implementation {
            self.build_expression_as_return(program, impl_expr_id)?;
        }

        Ok(self.function)
    }

    fn build_parameters_from_function(
        &mut self,
        program: &ast::Program,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<(), String> {
        for &param_id in &func_ast.parameters {
            let param_ast = &program.parameters[param_id];

            let var = Variable {
                name: param_ast.param_name.clone(),
                ty: param_ast.param_type.clone(),
                kind: VariableKind::Parameter,
                value: None,
                span: ast::Span::default(),
            };

            let var_id = self.ctx.program.variables.alloc(var);
            self.var_map.insert(param_ast.param_name.clone(), var_id);
            self.function.parameters.push(var_id);
        }
        Ok(())
    }

    fn build_parameters_from_partition(
        &mut self,
        program: &ast::Program,
        partition_ast: &ast::PartitionDeclaration,
    ) -> Result<(), String> {
        for &param_id in &partition_ast.parameters {
            let param_ast = &program.parameters[param_id];

            let var = Variable {
                name: param_ast.param_name.clone(),
                ty: param_ast.param_type.clone(),
                kind: VariableKind::Parameter,
                value: None,
                span: ast::Span::default(),
            };

            let var_id = self.ctx.program.variables.alloc(var);
            self.var_map.insert(param_ast.param_name.clone(), var_id);
            self.function.parameters.push(var_id);
        }
        Ok(())
    }

    /// Build partition implementation as a single hop with return statement
    fn build_expression_as_return(
        &mut self,
        program: &ast::Program,
        expr_id: ast::ExpressionId,
    ) -> Result<(), String> {
        // Create a single hop for the partition implementation
        let hop = HopCfg {
            entry_block: None,
            blocks: Vec::new(),
            span: program.expressions[expr_id].span.clone(),
        };

        let hop_id = self.function.hops.alloc(hop);
        self.function.hop_order.push(hop_id);
        self.function.entry_hop = Some(hop_id);

        // Create a basic block for the expression evaluation
        let block = BasicBlock {
            hop_id,
            statements: Vec::new(),
            span: program.expressions[expr_id].span.clone(),
            predecessors: Vec::new(), // Will be populated later by build_predecessors()
            successors: Vec::new(),   // Will be set when we add the return edge
        };

        let block_id = self.function.blocks.alloc(block);
        self.function.hops[hop_id].blocks.push(block_id);
        self.function.hops[hop_id].entry_block = Some(block_id);
        self.current_block_id = Some(block_id);

        // Build the expression and convert to return statement
        let result_operand = self.build_expression(program, expr_id)?;

        // Add return edge as successor
        self.function.blocks[block_id]
            .successors
            .push(ControlFlowEdge {
                from: block_id,
                to: block_id, // Self-reference for return (will be handled by dataflow analysis)
                edge_type: EdgeType::Return {
                    value: Some(result_operand),
                },
            });

        Ok(())
    }

    fn build_hops(
        &mut self,
        program: &ast::Program,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<(), String> {
        for &hop_ast_id in &func_ast.hops {
            let hop_ast = &program.hops[hop_ast_id];

            match &hop_ast.hop_type {
                ast::HopType::Simple => {
                    self.create_simple_hop(program, hop_ast)?;
                }
                ast::HopType::ForLoop {
                    loop_var,
                    loop_var_type,
                    start_value,
                    end_value,
                    ..
                } => {
                    // Flatten "hops for" into deterministic number of hops using pre-computed constants
                    self.create_flattened_hops_for(
                        program,
                        hop_ast,
                        loop_var,
                        loop_var_type,
                        *start_value,
                        *end_value,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn create_simple_hop(
        &mut self,
        program: &ast::Program,
        hop_ast: &ast::HopBlock,
    ) -> Result<(), String> {
        let hop = HopCfg {
            entry_block: None,
            blocks: Vec::new(),
            span: hop_ast.span.clone(),
        };

        let hop_id = self.function.hops.alloc(hop);
        self.function.hop_order.push(hop_id);

        if self.function.entry_hop.is_none() {
            self.function.entry_hop = Some(hop_id);
        }

        self.build_hop_statements(program, hop_id, &hop_ast.statements)?;
        Ok(())
    }

    fn create_flattened_hops_for(
        &mut self,
        program: &ast::Program,
        hop_ast: &ast::HopBlock,
        loop_var: &str,
        loop_var_type: &ast::TypeName,
        start_value: Option<i64>,
        end_value: Option<i64>,
    ) -> Result<(), String> {
        // Use pre-computed constant values if available
        let start_val = start_value.ok_or_else(|| {
            "Cannot expand hops for loop: start value is not a compile-time constant".to_string()
        })?;

        let end_val = end_value.ok_or_else(|| {
            "Cannot expand hops for loop: end value is not a compile-time constant".to_string()
        })?;

        // Calculate number of iterations from start to end
        let num_iterations = if end_val > start_val {
            (end_val - start_val) as usize
        } else {
            0
        };

        let loop_var_obj = Variable {
            name: loop_var.to_string(),
            ty: loop_var_type.clone(),
            kind: VariableKind::Local,
            value: None,
            span: ast::Span::default(),
        };
        let loop_var_id = self.ctx.program.variables.alloc(loop_var_obj);
        self.var_map.insert(loop_var.to_string(), loop_var_id);
        self.function.local_variables.push(loop_var_id);

        for i in 0..num_iterations {
            let hop = HopCfg {
                entry_block: None,
                blocks: Vec::new(),
                span: hop_ast.span.clone(),
            };

            let hop_id = self.function.hops.alloc(hop);
            self.function.hop_order.push(hop_id);

            if self.function.entry_hop.is_none() {
                self.function.entry_hop = Some(hop_id);
            }

            let iteration_value = start_val + i as i64;
            self.build_hop_with_loop_iteration(
                program,
                hop_id,
                &hop_ast.statements,
                loop_var_id,
                iteration_value,
            )?;
        }
        Ok(())
    }

    fn build_hop_statements(
        &mut self,
        program: &ast::Program,
        hop_id: HopId,
        statements: &[ast::StatementId],
    ) -> Result<(), String> {
        self.current_hop_id = Some(hop_id);

        let entry_block = self.new_basic_block(hop_id)?;
        self.function.hops[hop_id].entry_block = Some(entry_block);
        self.current_block_id = Some(entry_block);

        for &stmt_id in statements {
            if self.current_block_id.is_none() {
                break;
            }
            self.build_statement(program, &program.statements[stmt_id])?;
        }
        Ok(())
    }

    fn build_hop_with_loop_iteration(
        &mut self,
        program: &ast::Program,
        hop_id: HopId,
        statements: &[ast::StatementId],
        loop_var_id: VarId,
        iteration_value: i64,
    ) -> Result<(), String> {
        self.current_hop_id = Some(hop_id);

        let entry_block = self.new_basic_block(hop_id)?;
        self.function.hops[hop_id].entry_block = Some(entry_block);
        self.current_block_id = Some(entry_block);

        self.add_statement(
            entry_block,
            Statement::Assign {
                lvalue: LValue::Variable { var: loop_var_id },
                rvalue: Rvalue::Use(Operand::Const(Constant::Int(iteration_value))),
                span: ast::Span::default(),
            },
        );

        for &stmt_id in statements {
            if self.current_block_id.is_none() {
                break;
            }
            self.build_statement(program, &program.statements[stmt_id])?;
        }
        Ok(())
    }

    fn build_statement(
        &mut self,
        program: &ast::Program,
        stmt: &ast::Statement,
    ) -> Result<(), String> {
        let current_block = self
            .current_block_id
            .ok_or("No active block for statement")?;

        match &stmt.node {
            ast::StatementKind::VarDecl(var_decl) => {
                let var = Variable {
                    name: var_decl.var_name.clone(),
                    ty: var_decl.var_type.clone(),
                    kind: VariableKind::Local,
                    value: None,
                    span: stmt.span.clone(),
                };

                let var_id = self.ctx.program.variables.alloc(var);
                self.var_map.insert(var_decl.var_name.clone(), var_id);
                self.function.local_variables.push(var_id);

                // Only generate initialization if there's an init value
                if let Some(init_expr_id) = var_decl.init_value {
                    // Check if this is an array initialization with an array literal
                    let init_expr = &program.expressions[init_expr_id];
                    if let ast::ExpressionKind::ArrayLiteral { elements, .. } = &init_expr.node {
                        // For array literals, generate multiple array element assignments
                        for (index, &element_expr_id) in elements.iter().enumerate() {
                            let element_operand =
                                self.build_expression(program, element_expr_id)?;

                            // Generate: array[index] = element
                            self.add_statement(
                                current_block,
                                Statement::Assign {
                                    lvalue: LValue::ArrayElement {
                                        array: var_id,
                                        index: Operand::Const(Constant::Int(index as i64)),
                                    },
                                    rvalue: Rvalue::Use(element_operand),
                                    span: stmt.span.clone(),
                                },
                            );
                        }
                    } else {
                        // Regular initialization
                        let init_operand = self.build_expression(program, init_expr_id)?;

                        self.add_statement(
                            current_block,
                            Statement::Assign {
                                lvalue: LValue::Variable { var: var_id },
                                rvalue: Rvalue::Use(init_operand),
                                span: stmt.span.clone(),
                            },
                        );
                    }
                }
            }
            ast::StatementKind::Assignment(assign) => {
                self.build_assignment(program, assign, stmt.span.clone())?;
            }
            ast::StatementKind::Return(ret) => {
                let return_value = if let Some(expr_id) = ret.value {
                    Some(self.build_expression(program, expr_id)?)
                } else {
                    None
                };

                // Add return edge
                self.function.blocks[current_block]
                    .successors
                    .push(ControlFlowEdge {
                        from: current_block,
                        to: current_block, // Self-reference for return (handled by dataflow analysis)
                        edge_type: EdgeType::Return {
                            value: return_value,
                        },
                    });

                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Abort(_) => {
                // Add abort edge (no successor)
                self.function.blocks[current_block]
                    .successors
                    .push(ControlFlowEdge {
                        from: current_block,
                        to: current_block, // Self-reference for abort (handled by dataflow analysis)
                        edge_type: EdgeType::Abort,
                    });

                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Break(_) => {
                // Break statements should not appear in hops (caught in semantics)
                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Continue(_) => {
                // Continue statements should not appear in hops (caught in semantics)
                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Empty => {
                // No-op
            }
            ast::StatementKind::Expression(expr_stmt) => {
                // For expression statements, just evaluate the expression
                // This is useful for statements like ++x; or function calls
                self.build_expression(program, expr_stmt.expression)?;
            }
            ast::StatementKind::IfStmt(_)
            | ast::StatementKind::ForStmt(_)
            | ast::StatementKind::WhileStmt(_) => {
                // Control flow statements not supported in hops - should have been caught in semantics
                return Ok(());
            }
        }
        Ok(())
    }

    fn build_assignment(
        &mut self,
        program: &ast::Program,
        assign: &ast::AssignmentStatement,
        span: ast::Span,
    ) -> Result<(), String> {
        let current_block = self.current_block_id.ok_or("No active block")?;

        match &assign.lvalue {
            ast::LValue::Var { resolved_var, .. } => {
                let var_id = if let Some(resolved_var) = resolved_var {
                    let var_ast = &program.variables[*resolved_var];
                    *self
                        .var_map
                        .get(&var_ast.name)
                        .ok_or_else(|| format!("Variable {} not found in CFG", var_ast.name))?
                } else {
                    return Ok(()); // Unresolved - skip
                };

                let rhs_operand = self.build_expression(program, assign.rhs)?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        lvalue: LValue::Variable { var: var_id },
                        rvalue: Rvalue::Use(rhs_operand),
                        span,
                    },
                );
            }
            ast::LValue::TableField {
                resolved_table,
                resolved_pk_fields,
                resolved_field,
                pk_exprs,
                ..
            } => {
                if let (Some(table_ast_id), Some(field_ast_id)) = (resolved_table, resolved_field) {
                    let table_ast = &program.tables[*table_ast_id];
                    let field_ast = &program.fields[*field_ast_id];

                    let table_id = *self
                        .ctx
                        .table_map
                        .get(&table_ast.name)
                        .ok_or("Table not found")?;
                    let field_id = *self
                        .ctx
                        .field_map
                        .get(&field_ast.field_name)
                        .ok_or("Field not found")?;

                    let mut pk_field_ids = Vec::new();
                    let mut pk_operands = Vec::new();

                    for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                        if let Some(Some(pk_field)) = resolved_pk_fields.get(i) {
                            let field_ast = &program.fields[*pk_field];
                            let pk_field_id = *self
                                .ctx
                                .field_map
                                .get(&field_ast.field_name)
                                .ok_or("PK field not found")?;
                            let pk_operand = self.build_expression(program, pk_expr)?;

                            pk_field_ids.push(pk_field_id);
                            pk_operands.push(pk_operand);
                        }
                    }

                    let value_operand = self.build_expression(program, assign.rhs)?;

                    self.add_statement(
                        current_block,
                        Statement::Assign {
                            lvalue: LValue::TableField {
                                table: table_id,
                                pk_fields: pk_field_ids,
                                pk_values: pk_operands,
                                field: field_id,
                            },
                            rvalue: Rvalue::Use(value_operand),
                            span,
                        },
                    );
                }
            }
            ast::LValue::TableRecord {
                resolved_table,
                resolved_pk_fields,
                pk_exprs,
                ..
            } => {
                if let Some(table_ast_id) = resolved_table {
                    let table_ast = &program.tables[*table_ast_id];
                    let table_id = *self
                        .ctx
                        .table_map
                        .get(&table_ast.name)
                        .ok_or("Table not found")?;

                    let mut pk_field_ids = Vec::new();
                    let mut pk_operands = Vec::new();

                    for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                        if let Some(Some(pk_field)) = resolved_pk_fields.get(i) {
                            let field_ast = &program.fields[*pk_field];
                            let pk_field_id = *self
                                .ctx
                                .field_map
                                .get(&field_ast.field_name)
                                .ok_or("PK field not found")?;
                            let pk_operand = self.build_expression(program, pk_expr)?;

                            pk_field_ids.push(pk_field_id);
                            pk_operands.push(pk_operand);
                        }
                    }

                    // Expand record literal into multiple TableAssign statements
                    if let ast::ExpressionKind::RecordLiteral { fields, .. } =
                        &program.expressions[assign.rhs].node
                    {
                        for field_assignment in fields {
                            if let Some(&field_id) =
                                self.ctx.field_map.get(&field_assignment.field_name)
                            {
                                let value_operand =
                                    self.build_expression(program, field_assignment.value)?;

                                self.add_statement(
                                    current_block,
                                    Statement::Assign {
                                        lvalue: LValue::TableField {
                                            table: table_id,
                                            pk_fields: pk_field_ids.clone(),
                                            pk_values: pk_operands.clone(),
                                            field: field_id,
                                        },
                                        rvalue: Rvalue::Use(value_operand),
                                        span: span.clone(),
                                    },
                                );
                            }
                        }
                    }
                }
            }
            ast::LValue::ArrayElement {
                index,
                resolved_var,
                ..
            } => {
                if let Some(var_ast_id) = resolved_var {
                    let var_ast = &program.variables[*var_ast_id];
                    let var_id = *self
                        .var_map
                        .get(&var_ast.name)
                        .ok_or_else(|| format!("Variable {} not found in CFG", var_ast.name))?;

                    let index_operand = self.build_expression(program, *index)?;
                    let value_operand = self.build_expression(program, assign.rhs)?;

                    self.add_statement(
                        current_block,
                        Statement::Assign {
                            lvalue: LValue::ArrayElement {
                                array: var_id,
                                index: index_operand,
                            },
                            rvalue: Rvalue::Use(value_operand),
                            span,
                        },
                    );
                }
            }
            ast::LValue::FieldAccess { resolved_var, .. } => {
                // TODO: Implement field access assignment in CFG
                // For now, skip unresolved field accesses
                if resolved_var.is_some() {
                    // Placeholder implementation - field access not yet fully supported in CFG
                    // This would need to be expanded based on the target field type and semantics
                }
            }
        }
        Ok(())
    }

    fn build_expression(
        &mut self,
        program: &ast::Program,
        expr_id: ast::ExpressionId,
    ) -> Result<Operand, String> {
        let expr = &program.expressions[expr_id];

        match &expr.node {
            ast::ExpressionKind::Ident(name) => {
                // First check local variables (parameters and locals)
                if let Some(&var_id) = self.var_map.get(name) {
                    Ok(Operand::Var(var_id))
                // Then check global constants
                } else if let Some(&var_id) = self.ctx.variable_map.get(name) {
                    Ok(Operand::Var(var_id))
                } else {
                    Ok(Operand::Const(Constant::Int(0))) // Default for unresolved
                }
            }
            ast::ExpressionKind::IntLit(val) => Ok(Operand::Const(Constant::Int(*val))),
            ast::ExpressionKind::FloatLit(val) => Ok(Operand::Const(Constant::Float(
                ordered_float::OrderedFloat::from(*val),
            ))),
            ast::ExpressionKind::StringLit(val) => {
                Ok(Operand::Const(Constant::String(val.clone())))
            }
            ast::ExpressionKind::BoolLit(val) => Ok(Operand::Const(Constant::Bool(*val))),
            ast::ExpressionKind::UnaryOp { op, expr, .. } => {
                match op {
                    ast::UnaryOp::Not => {
                        let operand = self.build_expression(program, *expr)?;
                        let temp_var = self.create_temp_var();
                        let rvalue = Rvalue::UnaryOp {
                            op: UnaryOp::Not,
                            operand,
                        };

                        if let Some(block_id) = self.current_block_id {
                            self.add_statement(
                                block_id,
                                Statement::Assign {
                                    lvalue: LValue::Variable { var: temp_var },
                                    rvalue,
                                    span: program.expressions[*expr].span.clone(),
                                },
                            );
                        }
                        Ok(Operand::Var(temp_var))
                    }
                    ast::UnaryOp::Neg => {
                        let operand = self.build_expression(program, *expr)?;
                        let temp_var = self.create_temp_var();
                        let rvalue = Rvalue::UnaryOp {
                            op: UnaryOp::Neg,
                            operand,
                        };

                        if let Some(block_id) = self.current_block_id {
                            self.add_statement(
                                block_id,
                                Statement::Assign {
                                    lvalue: LValue::Variable { var: temp_var },
                                    rvalue,
                                    span: program.expressions[*expr].span.clone(),
                                },
                            );
                        }
                        Ok(Operand::Var(temp_var))
                    }
                    ast::UnaryOp::PreIncrement
                    | ast::UnaryOp::PostIncrement
                    | ast::UnaryOp::PreDecrement
                    | ast::UnaryOp::PostDecrement => {
                        // Convert increment/decrement to simple assignment: a = a + 1 or a = a - 1
                        let operand = self.build_expression(program, *expr)?;

                        // For increment/decrement, we need to know what variable we're operating on
                        // This should be handled during lvalue processing, but for expressions we need to create temps
                        let temp_var = self.create_temp_var();
                        let one_operand = Operand::Const(Constant::Int(1));

                        let binary_op = match op {
                            ast::UnaryOp::PreIncrement | ast::UnaryOp::PostIncrement => {
                                BinaryOp::Add
                            }
                            ast::UnaryOp::PreDecrement | ast::UnaryOp::PostDecrement => {
                                BinaryOp::Sub
                            }
                            _ => unreachable!(),
                        };

                        let rvalue = Rvalue::BinaryOp {
                            left: operand.clone(),
                            op: binary_op,
                            right: one_operand,
                        };

                        if let Some(block_id) = self.current_block_id {
                            self.add_statement(
                                block_id,
                                Statement::Assign {
                                    lvalue: LValue::Variable { var: temp_var },
                                    rvalue,
                                    span: program.expressions[*expr].span.clone(),
                                },
                            );
                        }

                        // For post-increment/decrement, return the original value
                        // For pre-increment/decrement, return the new value
                        match op {
                            ast::UnaryOp::PreIncrement | ast::UnaryOp::PreDecrement => {
                                Ok(Operand::Var(temp_var))
                            }
                            ast::UnaryOp::PostIncrement | ast::UnaryOp::PostDecrement => {
                                // Return the original operand value
                                Ok(operand)
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            ast::ExpressionKind::BinaryOp {
                left, op, right, ..
            } => {
                let left_operand = self.build_expression(program, *left)?;
                let right_operand = self.build_expression(program, *right)?;
                let temp_var = self.create_temp_var();
                let rvalue = match op {
                    ast::BinaryOp::Add => Rvalue::BinaryOp {
                        op: BinaryOp::Add,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Sub => Rvalue::BinaryOp {
                        op: BinaryOp::Sub,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Mul => Rvalue::BinaryOp {
                        op: BinaryOp::Mul,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Div => Rvalue::BinaryOp {
                        op: BinaryOp::Div,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Lt => Rvalue::BinaryOp {
                        op: BinaryOp::Lt,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Lte => Rvalue::BinaryOp {
                        op: BinaryOp::Lte,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Gt => Rvalue::BinaryOp {
                        op: BinaryOp::Gt,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Gte => Rvalue::BinaryOp {
                        op: BinaryOp::Gte,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Eq => Rvalue::BinaryOp {
                        op: BinaryOp::Eq,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Neq => Rvalue::BinaryOp {
                        op: BinaryOp::Neq,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::And => Rvalue::BinaryOp {
                        op: BinaryOp::And,
                        left: left_operand,
                        right: right_operand,
                    },
                    ast::BinaryOp::Or => Rvalue::BinaryOp {
                        op: BinaryOp::Or,
                        left: left_operand,
                        right: right_operand,
                    },
                };

                if let Some(block_id) = self.current_block_id {
                    self.add_statement(
                        block_id,
                        Statement::Assign {
                            lvalue: LValue::Variable { var: temp_var },
                            rvalue,
                            span: program.expressions[*left].span.clone(),
                        },
                    );
                }
                Ok(Operand::Var(temp_var))
            }
            ast::ExpressionKind::TableFieldAccess {
                table_name: _,
                pk_fields: _,
                pk_exprs,
                field_name: _,
                resolved_table,
                resolved_pk_fields: _,
                resolved_field,
                resolved_type: _,
            } => {
                // Create a temporary variable to hold the table access result
                let temp_var = self.create_temp_var();

                if let (Some(table_ast_id), Some(field_ast_id)) = (resolved_table, resolved_field) {
                    let table_ast = &program.tables[*table_ast_id];
                    let field_ast = &program.fields[*field_ast_id];

                    let table_id = *self
                        .ctx
                        .table_map
                        .get(&table_ast.name)
                        .ok_or("Table not found in CFG")?;
                    let field_id = *self
                        .ctx
                        .field_map
                        .get(&field_ast.field_name)
                        .ok_or("Field not found in CFG")?;

                    // Build primary key expressions
                    let mut pk_values = Vec::new();
                    for pk_expr in pk_exprs {
                        pk_values.push(self.build_expression(program, *pk_expr)?);
                    }

                    // Create table access rvalue
                    let rvalue = Rvalue::TableAccess {
                        table: table_id,
                        pk_fields: vec![], // We don't need the field IDs for the rvalue
                        pk_values,
                        field: field_id,
                    };

                    // Generate assignment statement
                    if let Some(block_id) = self.current_block_id {
                        let block = &mut self.function.blocks[block_id];
                        block.statements.push(Statement::Assign {
                            lvalue: LValue::Variable { var: temp_var },
                            rvalue,
                            span: program.expressions[expr_id].span.clone(),
                        });
                    }
                }
                Ok(Operand::Var(temp_var))
            }
            _ => {
                // For other expression types, return a default value for now
                // This can be expanded later to handle more complex expressions
                Ok(Operand::Const(Constant::Int(0)))
            }
        }
    }

    fn create_temp_var(&mut self) -> VarId {
        let temp_name = format!("_temp_{}", self.temp_counter);
        self.temp_counter += 1;

        let var = Variable {
            name: temp_name.clone(),
            ty: ast::TypeName::Int,
            kind: VariableKind::Temporary,
            value: None,
            span: ast::Span::default(),
        };

        let var_id = self.ctx.program.variables.alloc(var);
        self.var_map.insert(temp_name, var_id);
        self.function.local_variables.push(var_id);
        var_id
    }

    fn new_basic_block(&mut self, hop_id: HopId) -> Result<BasicBlockId, String> {
        let block = BasicBlock {
            hop_id,
            statements: Vec::new(),
            span: ast::Span::default(),
            predecessors: Vec::new(), // Will be populated later by build_predecessors()
            successors: Vec::new(),   // Will be set when adding control flow edges
        };

        let block_id = self.function.blocks.alloc(block);
        self.function.hops[hop_id].blocks.push(block_id);
        Ok(block_id)
    }

    fn add_statement(&mut self, block_id: BasicBlockId, stmt: Statement) {
        self.function.blocks[block_id].statements.push(stmt);
    }
}
