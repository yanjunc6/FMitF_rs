use std::collections::HashMap;

use crate::ast;
use crate::cfg::*;

/// Builds CFG from an AST Program.
pub struct CfgBuilder;

/// Context for building CFG - holds the program being constructed
pub struct CfgCtx {
    pub program: CfgProgram,

    // Lookup maps for resolved AST items
    pub node_map: HashMap<String, NodeId>,
    pub table_map: HashMap<String, TableId>,
    pub field_map: HashMap<String, FieldId>,
}

/// Helper struct to manage building a single function's CFG.
struct FunctionContextBuilder<'a> {
    // Context references
    ctx: &'a mut CfgCtx,

    // Function being built
    function: FunctionCfg,

    // Local mappings for this function
    var_map: HashMap<String, VarId>,

    // Loop context stack for break/continue
    loop_stack: Vec<LoopContext>,

    // Current building state
    current_hop_id: Option<HopId>,
    current_block_id: Option<BasicBlockId>,
}

#[derive(Debug, Clone)]
struct LoopContext {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
}

impl CfgBuilder {
    pub fn new() -> Self {
        Self
    }

    /// Build CFG from AST Program
    pub fn build_from_program(program: &ast::Program) -> Result<CfgCtx, String> {
        let mut ctx = CfgCtx {
            program: CfgProgram {
                nodes: id_arena::Arena::new(),
                tables: id_arena::Arena::new(),
                fields: id_arena::Arena::new(),
                functions: id_arena::Arena::new(),
                root_nodes: Vec::new(),
                root_tables: Vec::new(),
                root_functions: Vec::new(),
            },
            node_map: HashMap::new(),
            table_map: HashMap::new(),
            field_map: HashMap::new(),
        };

        // Build global items first
        Self::build_nodes(program, &mut ctx)?;
        Self::build_tables(program, &mut ctx)?;
        Self::build_functions(program, &mut ctx)?;

        Ok(ctx)
    }

    /// Build all nodes
    fn build_nodes(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &node_id in &program.root_nodes {
            let node_ast = &program.nodes[node_id];

            let cfg_node = NodeInfo {
                name: node_ast.name.clone(),
                tables: Vec::new(), // Will be populated when building tables
            };

            let cfg_node_id = ctx.program.nodes.alloc(cfg_node);
            ctx.node_map.insert(node_ast.name.clone(), cfg_node_id);
            ctx.program.root_nodes.push(cfg_node_id);
        }
        Ok(())
    }

    /// Build all tables and fields
    fn build_tables(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &table_id in &program.root_tables {
            let table_ast = &program.tables[table_id];

            // Look up the node
            let node_id = ctx
                .node_map
                .get(&program.nodes[table_ast.node].name)
                .ok_or_else(|| format!("Node {} not found", program.nodes[table_ast.node].name))?;

            // Build fields first
            let mut field_ids = Vec::new();
            let mut primary_key_ids = Vec::new(); // Changed to collect multiple primary keys

            for &field_ast_id in &table_ast.fields {
                let field_ast = &program.fields[field_ast_id];

                let cfg_field = FieldInfo {
                    name: field_ast.field_name.clone(),
                    ty: field_ast.field_type.clone(),
                    table_id: None, // Will be updated after table_id is allocated
                    is_primary: field_ast.is_primary,
                };

                let cfg_field_id = ctx.program.fields.alloc(cfg_field);
                field_ids.push(cfg_field_id);
                ctx.field_map
                    .insert(field_ast.field_name.clone(), cfg_field_id);

                if field_ast.is_primary {
                    primary_key_ids.push(cfg_field_id); // Add to list instead of overwriting
                }
            }

            // Verify that we have at least one primary key
            if primary_key_ids.is_empty() {
                return Err(format!("Table {} has no primary key", table_ast.name));
            }

            // Build table with composite primary keys
            let cfg_table = TableInfo {
                name: table_ast.name.clone(),
                node_id: *node_id,
                fields: field_ids.clone(),
                primary_keys: primary_key_ids, // Use the list of primary keys
            };

            let cfg_table_id = ctx.program.tables.alloc(cfg_table);
            ctx.table_map.insert(table_ast.name.clone(), cfg_table_id);
            ctx.program.root_tables.push(cfg_table_id);

            // Update field table references
            for field_id in field_ids {
                ctx.program.fields[field_id].table_id = Some(cfg_table_id);
            }

            // Update node's table list
            ctx.program.nodes[*node_id].tables.push(cfg_table_id);
        }
        Ok(())
    }

    /// Build all functions
    fn build_functions(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &func_id in &program.root_functions {
            let func_ast = &program.functions[func_id];

            let builder = FunctionContextBuilder::new(ctx, func_ast)?; // Removed mut
            let function = builder.build(program, func_ast)?;

            let cfg_func_id = ctx.program.functions.alloc(function);
            ctx.program.root_functions.push(cfg_func_id);
        }
        Ok(())
    }
}

impl<'a> FunctionContextBuilder<'a> {
    fn new(ctx: &'a mut CfgCtx, func_ast: &ast::FunctionDeclaration) -> Result<Self, String> {
        let function = FunctionCfg {
            name: func_ast.name.clone(),
            return_type: func_ast.return_type.clone(),
            span: func_ast.span.clone(),
            variables: id_arena::Arena::new(),
            parameters: Vec::new(),
            hops: id_arena::Arena::new(),
            blocks: id_arena::Arena::new(),
            entry_hop: None, // Will be set
            hop_order: Vec::new(),
        };

        Ok(Self {
            ctx,
            function,
            var_map: HashMap::new(),
            loop_stack: Vec::new(),
            current_hop_id: None,
            current_block_id: None,
        })
    }

    fn build(
        mut self,
        program: &ast::Program,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<FunctionCfg, String> {
        // Build parameters
        self.build_parameters(program, func_ast)?;

        // Build hops
        self.build_hops(program, func_ast)?;

        Ok(self.function)
    }

    fn build_parameters(
        &mut self,
        program: &ast::Program,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<(), String> {
        for &param_id in &func_ast.parameters {
            let param_ast = &program.parameters[param_id];

            let var = Variable {
                name: param_ast.param_name.clone(),
                ty: param_ast.param_type.clone(),
                is_parameter: true,
            };

            let var_id = self.function.variables.alloc(var);
            self.var_map.insert(param_ast.param_name.clone(), var_id);
            self.function.parameters.push(var_id);
        }
        Ok(())
    }

    fn build_hops(
        &mut self,
        program: &ast::Program,
        func_ast: &ast::FunctionDeclaration,
    ) -> Result<(), String> {
        if func_ast.hops.is_empty() {
            return Err("Function has no hops".to_string());
        }

        // Build all hops first and populate hop_order
        for &hop_ast_id in &func_ast.hops {
            let hop_ast = &program.hops[hop_ast_id];

            // Resolve node
            let node_id = if let Some(resolved_node) = hop_ast.resolved_node {
                resolved_node
            } else {
                return Err(format!("Hop node {} not resolved", hop_ast.node_name));
            };

            // Find the CFG node ID
            let cfg_node_id = self
                .ctx
                .node_map
                .get(&program.nodes[node_id].name)
                .ok_or_else(|| format!("CFG node not found for {}", program.nodes[node_id].name))?;

            let hop = HopCfg {
                node_id: *cfg_node_id,
                entry_block: None, // Will be set
                blocks: Vec::new(),
                span: hop_ast.span.clone(),
            };

            let hop_id = self.function.hops.alloc(hop);
            self.function.hop_order.push(hop_id);

            if self.function.entry_hop.is_none() {
                // First hop allocated becomes entry hop
                self.function.entry_hop = Some(hop_id);
            }
        }

        // Build statements for each hop using the IDs from hop_order
        for (hop_index, &hop_ast_id) in func_ast.hops.iter().enumerate() {
            let hop_ast = &program.hops[hop_ast_id];
            let hop_id = self.function.hop_order[hop_index]; // Use actual HopId from hop_order

            self.current_hop_id = Some(hop_id);

            // Create entry block for this hop
            let entry_block = self.new_basic_block(hop_id)?;
            self.function.hops[hop_id].entry_block = Some(entry_block); // Assign Some(id)
            self.current_block_id = Some(entry_block);

            // Build statements
            for &stmt_id in &hop_ast.statements {
                if self.current_block_id.is_none() {
                    break; // Block was terminated
                }
                self.build_statement(program, &program.statements[stmt_id])?;
            }

            // Handle hop transition
            if let Some(active_block) = self.current_block_id.take() {
                if hop_index < func_ast.hops.len() - 1 {
                    // Transition to next hop
                    let next_hop_id = self.function.hop_order[hop_index + 1]; // Use actual HopId
                    self.set_terminator(
                        active_block,
                        Terminator::HopExit {
                            next_hop: Some(next_hop_id),
                        },
                    );
                } else {
                    // Last hop - function exit
                    match func_ast.return_type {
                        ast::ReturnType::Void => {
                            self.set_terminator(active_block, Terminator::Return(None));
                        }
                        _ => {
                            self.set_terminator(active_block, Terminator::Abort);
                        }
                    }
                }
            }
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
                // Create variable
                let var = Variable {
                    name: var_decl.var_name.clone(),
                    ty: var_decl.var_type.clone(),
                    is_parameter: false,
                };

                let var_id = self.function.variables.alloc(var);
                self.var_map.insert(var_decl.var_name.clone(), var_id);

                // Build initializer
                let init_operand = self.build_expression(program, var_decl.init_value)?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        var: var_id,
                        rvalue: Rvalue::Use(init_operand),
                        span: stmt.span.clone(),
                    },
                );
            }
            ast::StatementKind::VarAssignment(var_assign) => {
                let var_id = if let Some(resolved_var) = var_assign.resolved_var {
                    let var_ast = &program.variables[resolved_var];
                    *self
                        .var_map
                        .get(&var_ast.name)
                        .ok_or_else(|| format!("Variable {} not found in CFG", var_ast.name))?
                } else {
                    return Err(format!("Variable {} not resolved", var_assign.var_name));
                };

                let rhs_operand = self.build_expression(program, var_assign.rhs)?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        var: var_id,
                        rvalue: Rvalue::Use(rhs_operand),
                        span: stmt.span.clone(),
                    },
                );
            }
            ast::StatementKind::Assignment(assign) => {
                let table_id = if let Some(resolved_table) = assign.resolved_table {
                    let table_ast = &program.tables[resolved_table];
                    *self
                        .ctx
                        .table_map
                        .get(&table_ast.name)
                        .ok_or_else(|| format!("Table {} not found in CFG", table_ast.name))?
                } else {
                    return Err(format!("Table {} not resolved", assign.table_name));
                };

                // Build all primary key fields and values
                let mut pk_field_ids = Vec::new();
                let mut pk_operands = Vec::new();

                for (i, &pk_expr) in assign.pk_exprs.iter().enumerate() {
                    if let Some(pk_field) = assign.resolved_pk_fields.get(i).and_then(|&f| f) {
                        let field_ast = &program.fields[pk_field];
                        let pk_field_id = *self
                            .ctx
                            .field_map
                            .get(&field_ast.field_name)
                            .ok_or_else(|| {
                                format!(
                                    "Primary key field {} not found in CFG",
                                    field_ast.field_name
                                )
                            })?;

                        let pk_operand = self.build_expression(program, pk_expr)?;

                        pk_field_ids.push(pk_field_id);
                        pk_operands.push(pk_operand);
                    } else {
                        return Err(format!("Primary key field {} not resolved", i));
                    }
                }

                if pk_field_ids.is_empty() {
                    return Err(format!("No primary key fields provided"));
                }

                let field_id = if let Some(resolved_field) = assign.resolved_field {
                    let field_ast = &program.fields[resolved_field];
                    *self
                        .ctx
                        .field_map
                        .get(&field_ast.field_name)
                        .ok_or_else(|| format!("Field {} not found in CFG", field_ast.field_name))?
                } else {
                    return Err(format!("Field {} not resolved", assign.field_name));
                };

                let value_operand = self.build_expression(program, assign.rhs)?;

                self.add_statement(
                    current_block,
                    Statement::TableAssign {
                        table: table_id,
                        pk_fields: pk_field_ids,
                        pk_values: pk_operands,
                        field: field_id,
                        value: value_operand,
                        span: stmt.span.clone(),
                    },
                );
            }
            ast::StatementKind::MultiAssignment(multi_assign) => {
                // Expand multi-assignment into multiple single assignments
                // This keeps the CFG unchanged while providing the grammar sugar

                let table_id = if let Some(resolved_table) = multi_assign.resolved_table {
                    let table_ast = &program.tables[resolved_table];
                    *self
                        .ctx
                        .table_map
                        .get(&table_ast.name)
                        .ok_or_else(|| format!("Table {} not found in CFG", table_ast.name))?
                } else {
                    return Err(format!("Table {} not resolved", multi_assign.table_name));
                };

                // Build all primary key fields and values once (shared by all assignments)
                let mut pk_field_ids = Vec::new();
                let mut pk_operands = Vec::new();

                for (i, &pk_expr) in multi_assign.pk_exprs.iter().enumerate() {
                    if let Some(pk_field) = multi_assign.resolved_pk_fields.get(i).and_then(|&f| f) {
                        let field_ast = &program.fields[pk_field];
                        let pk_field_id = *self
                            .ctx
                            .field_map
                            .get(&field_ast.field_name)
                            .ok_or_else(|| {
                                format!(
                                    "Primary key field {} not found in CFG",
                                    field_ast.field_name
                                )
                            })?;

                        let pk_operand = self.build_expression(program, pk_expr)?;

                        pk_field_ids.push(pk_field_id);
                        pk_operands.push(pk_operand);
                    } else {
                        return Err(format!("Primary key field {} not resolved", i));
                    }
                }

                if pk_field_ids.is_empty() {
                    return Err(format!("No primary key fields provided"));
                }

                // Generate one TableAssign statement for each field assignment
                for assignment in &multi_assign.assignments {
                    let field_id = if let Some(resolved_field) = assignment.resolved_field {
                        let field_ast = &program.fields[resolved_field];
                        *self
                            .ctx
                            .field_map
                            .get(&field_ast.field_name)
                            .ok_or_else(|| format!("Field {} not found in CFG", field_ast.field_name))?
                    } else {
                        return Err(format!("Field {} not resolved", assignment.field_name));
                    };

                    let value_operand = self.build_expression(program, assignment.rhs)?;

                    self.add_statement(
                        current_block,
                        Statement::TableAssign {
                            table: table_id,
                            pk_fields: pk_field_ids.clone(),
                            pk_values: pk_operands.clone(),
                            field: field_id,
                            value: value_operand,
                            span: stmt.span.clone(),
                        },
                    );
                }
            }
            ast::StatementKind::IfStmt(if_stmt) => {
                self.build_if_statement(program, if_stmt)?;
            }
            ast::StatementKind::WhileStmt(while_stmt) => {
                self.build_while_statement(program, while_stmt)?;
            }
            ast::StatementKind::Return(ret_stmt) => {
                let ret_operand = if let Some(expr_id) = ret_stmt.value {
                    Some(self.build_expression(program, expr_id)?)
                } else {
                    None
                };

                self.set_terminator(current_block, Terminator::Return(ret_operand));
                self.current_block_id = None;
            }
            ast::StatementKind::Abort(_) => {
                self.set_terminator(current_block, Terminator::Abort);
                self.current_block_id = None;
            }
            ast::StatementKind::Break(_) => {
                let loop_ctx = self.loop_stack.last().ok_or("Break outside loop")?;
                self.set_terminator(current_block, Terminator::Goto(loop_ctx.break_target));
                self.current_block_id = None;
            }
            ast::StatementKind::Continue(_) => {
                let loop_ctx = self.loop_stack.last().ok_or("Continue outside loop")?;
                self.set_terminator(current_block, Terminator::Goto(loop_ctx.continue_target));
                self.current_block_id = None;
            }
            ast::StatementKind::Empty => {
                // No-op
            }
        }

        Ok(())
    }

    fn build_if_statement(
        &mut self,
        program: &ast::Program,
        if_stmt: &ast::IfStatement,
    ) -> Result<(), String> {
        let current_block = self
            .current_block_id
            .ok_or("No active block for if statement")?;
        let current_hop = self
            .current_hop_id
            .ok_or("No active hop for if statement")?;

        let condition = self.build_expression(program, if_stmt.condition)?;

        let then_block = self.new_basic_block(current_hop)?;
        let merge_block = self.new_basic_block(current_hop)?;

        let else_block = if if_stmt.else_branch.is_some() {
            self.new_basic_block(current_hop)?
        } else {
            merge_block
        };

        self.set_terminator(
            current_block,
            Terminator::Branch {
                condition,
                then_block,
                else_block,
            },
        );

        // Build then branch
        self.current_block_id = Some(then_block);
        for &stmt_id in &if_stmt.then_branch {
            if self.current_block_id.is_none() {
                break;
            }
            self.build_statement(program, &program.statements[stmt_id])?;
        }
        if let Some(active_block) = self.current_block_id.take() {
            self.set_terminator(active_block, Terminator::Goto(merge_block));
        }

        // Build else branch if present
        if let Some(else_stmts) = &if_stmt.else_branch {
            self.current_block_id = Some(else_block);
            for &stmt_id in else_stmts {
                if self.current_block_id.is_none() {
                    break;
                }
                self.build_statement(program, &program.statements[stmt_id])?;
            }
            if let Some(active_block) = self.current_block_id.take() {
                self.set_terminator(active_block, Terminator::Goto(merge_block));
            }
        }

        self.current_block_id = Some(merge_block);
        Ok(())
    }

    fn build_while_statement(
        &mut self,
        program: &ast::Program,
        while_stmt: &ast::WhileStatement,
    ) -> Result<(), String> {
        let current_block = self
            .current_block_id
            .ok_or("No active block for while statement")?;
        let current_hop = self
            .current_hop_id
            .ok_or("No active hop for while statement")?;

        let header_block = self.new_basic_block(current_hop)?;
        let body_block = self.new_basic_block(current_hop)?;
        let exit_block = self.new_basic_block(current_hop)?;

        self.set_terminator(current_block, Terminator::Goto(header_block));

        // Header block
        self.current_block_id = Some(header_block);
        let condition = self.build_expression(program, while_stmt.condition)?;
        self.set_terminator(
            header_block,
            Terminator::Branch {
                condition,
                then_block: body_block,
                else_block: exit_block,
            },
        );

        // Body block
        self.current_block_id = Some(body_block);
        self.loop_stack.push(LoopContext {
            continue_target: header_block,
            break_target: exit_block,
        });

        for &stmt_id in &while_stmt.body {
            if self.current_block_id.is_none() {
                break;
            }
            self.build_statement(program, &program.statements[stmt_id])?;
        }

        self.loop_stack.pop();

        if let Some(active_block) = self.current_block_id.take() {
            self.set_terminator(active_block, Terminator::Goto(header_block));
        }

        self.current_block_id = Some(exit_block);
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
                let var_id = self
                    .var_map
                    .get(name)
                    .ok_or_else(|| format!("Variable {} not found", name))?;
                Ok(Operand::Var(*var_id))
            }
            ast::ExpressionKind::IntLit(val) => Ok(Operand::Const(Constant::Int(*val))),
            ast::ExpressionKind::FloatLit(val) => Ok(Operand::Const(Constant::Float(
                ordered_float::OrderedFloat::from(*val),
            ))),
            ast::ExpressionKind::StringLit(val) => {
                Ok(Operand::Const(Constant::String(val.clone())))
            }
            ast::ExpressionKind::BoolLit(val) => Ok(Operand::Const(Constant::Bool(*val))),
            ast::ExpressionKind::TableFieldAccess {
                resolved_table,
                resolved_pk_fields,
                pk_exprs,
                resolved_field,
                resolved_type,
                table_name,
                field_name,
                ..
            } => {
                // For complex expressions, we need to create a temporary variable
                let table_id = if let Some(resolved_table) = resolved_table {
                    let table_ast = &program.tables[*resolved_table];
                    *self
                        .ctx
                        .table_map
                        .get(&table_ast.name)
                        .ok_or_else(|| format!("Table {} not found in CFG", table_ast.name))?
                } else {
                    return Err(format!("Table {} not resolved", table_name));
                };

                // Build all primary key fields and values
                let mut pk_field_ids = Vec::new();
                let mut pk_operands = Vec::new();

                for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                    if let Some(pk_field) = resolved_pk_fields.get(i).and_then(|&f| f) {
                        let field_ast = &program.fields[pk_field];
                        let pk_field_id = *self
                            .ctx
                            .field_map
                            .get(&field_ast.field_name)
                            .ok_or_else(|| {
                                format!(
                                    "Primary key field {} not found in CFG",
                                    field_ast.field_name
                                )
                            })?;

                        let pk_operand = self.build_expression(program, pk_expr)?;

                        pk_field_ids.push(pk_field_id);
                        pk_operands.push(pk_operand);
                    } else {
                        return Err(format!("Primary key field {} not resolved", i));
                    }
                }

                if pk_field_ids.is_empty() {
                    return Err(format!("No primary key fields provided"));
                }

                let field_id = if let Some(resolved_field) = resolved_field {
                    let field_ast = &program.fields[*resolved_field];
                    *self
                        .ctx
                        .field_map
                        .get(&field_ast.field_name)
                        .ok_or_else(|| format!("Field {} not found in CFG", field_ast.field_name))?
                } else {
                    return Err(format!("Field {} not resolved", field_name));
                };

                // Get field type for temp variable - use resolved type if available, otherwise field type
                let field_type = resolved_type.clone().unwrap_or_else(|| self.ctx.program.fields[field_id].ty.clone());

                // Create temporary variable
                let temp_var = Variable {
                    name: format!("_temp_{}", self.function.variables.len()),
                    ty: field_type,
                    is_parameter: false,
                };

                let temp_var_id = self.function.variables.alloc(temp_var);

                // Create assignment
                let current_block = self
                    .current_block_id
                    .ok_or("No active block for table access")?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        var: temp_var_id,
                        rvalue: Rvalue::TableAccess {
                            table: table_id,
                            pk_fields: pk_field_ids,
                            pk_values: pk_operands,
                            field: field_id,
                        },
                        span: expr.span.clone(),
                    },
                );

                Ok(Operand::Var(temp_var_id))
            }
            ast::ExpressionKind::UnaryOp {
                op,
                expr: inner_expr,
                resolved_type,
            } => {
                let operand = self.build_expression(program, *inner_expr)?;

                // Use resolved type from semantic analysis - required for CFG building
                let result_type = resolved_type.clone().ok_or_else(|| {
                    "Unary operation type not resolved by semantic analysis".to_string()
                })?;
                
                let temp_var = Variable {
                    name: format!("_temp_{}", self.function.variables.len()),
                    ty: result_type,
                    is_parameter: false,
                };

                let temp_var_id = self.function.variables.alloc(temp_var);

                let current_block = self
                    .current_block_id
                    .ok_or("No active block for unary operation")?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        var: temp_var_id,
                        rvalue: Rvalue::UnaryOp {
                            op: op.clone(), // Clone the op
                            operand,
                        },
                        span: expr.span.clone(),
                    },
                );

                Ok(Operand::Var(temp_var_id))
            }
            ast::ExpressionKind::BinaryOp { left, op, right, resolved_type } => {
                let left_operand = self.build_expression(program, *left)?;
                let right_operand = self.build_expression(program, *right)?;

                // Use resolved type from semantic analysis - required for CFG building
                let result_type = resolved_type.clone().ok_or_else(|| {
                    "Binary operation type not resolved by semantic analysis".to_string()
                })?;
                
                let temp_var = Variable {
                    name: format!("_temp_{}", self.function.variables.len()),
                    ty: result_type,
                    is_parameter: false,
                };

                let temp_var_id = self.function.variables.alloc(temp_var);

                let current_block = self
                    .current_block_id
                    .ok_or("No active block for binary operation")?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        var: temp_var_id,
                        rvalue: Rvalue::BinaryOp {
                            op: op.clone(), // Clone the op
                            left: left_operand,
                            right: right_operand,
                        },
                        span: expr.span.clone(),
                    },
                );

                Ok(Operand::Var(temp_var_id))
            }
        }
    }

    fn new_basic_block(&mut self, hop_id: HopId) -> Result<BasicBlockId, String> {
        let block = BasicBlock {
            hop_id,
            statements: Vec::new(),
            terminator: Terminator::Abort, // Placeholder
            span: ast::Span::default(),
        };

        let block_id = self.function.blocks.alloc(block);
        self.function.hops[hop_id].blocks.push(block_id);

        Ok(block_id)
    }

    fn add_statement(&mut self, block_id: BasicBlockId, stmt: Statement) {
        self.function.blocks[block_id].statements.push(stmt);
    }

    fn set_terminator(&mut self, block_id: BasicBlockId, terminator: Terminator) {
        self.function.blocks[block_id].terminator = terminator;
    }
}
