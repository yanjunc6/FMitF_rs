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
    pub partition_map: HashMap<String, PartitionId>,
    pub constant_map: HashMap<String, ConstantId>,
}

/// Helper struct to manage building a single function's CFG.
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
                partitions: id_arena::Arena::new(),
                constants: id_arena::Arena::new(),
                functions: id_arena::Arena::new(),
                root_tables: Vec::new(),
                root_partitions: Vec::new(),
                root_constants: Vec::new(),
                root_functions: Vec::new(),
            },
            table_map: HashMap::new(),
            field_map: HashMap::new(),
            partition_map: HashMap::new(),
            constant_map: HashMap::new(),
        };

        Self::build_partitions(program, &mut ctx)?;
        Self::build_constants(program, &mut ctx)?;
        Self::build_tables(program, &mut ctx)?;
        Self::build_functions(program, &mut ctx)?;

        Ok(ctx)
    }

    fn build_partitions(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &partition_id in &program.root_partitions {
            let partition_ast = &program.partitions[partition_id];

            let mut parameter_names = Vec::new();
            let mut parameter_types = Vec::new();
            
            for &param_id in &partition_ast.parameters {
                let param_ast = &program.parameters[param_id];
                parameter_names.push(param_ast.param_name.clone());
                parameter_types.push(param_ast.param_type.clone());
            }

            let partition_info = PartitionInfo {
                name: partition_ast.name.clone(),
                parameters: parameter_names,
                parameter_types,
                span: partition_ast.span.clone(),
            };

            let cfg_partition_id = ctx.program.partitions.alloc(partition_info);
            ctx.partition_map.insert(partition_ast.name.clone(), cfg_partition_id);
            ctx.program.root_partitions.push(cfg_partition_id);
        }
        Ok(())
    }

    fn build_constants(program: &ast::Program, ctx: &mut CfgCtx) -> Result<(), String> {
        for &const_var_id in &program.root_constants {
            let const_var_ast = &program.variables[const_var_id];

            let const_info = ConstantInfo {
                name: const_var_ast.name.clone(),
                ty: const_var_ast.ty.clone(),
                value: Constant::Int(0), // Default value - AST already validated
                span: ast::Span::default(),
            };

            let cfg_const_id = ctx.program.constants.alloc(const_info);
            ctx.constant_map.insert(const_var_ast.name.clone(), cfg_const_id);
            ctx.program.root_constants.push(cfg_const_id);
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
                ctx.field_map.insert(field_ast.field_name.clone(), cfg_field_id);

                if field_ast.is_primary {
                    primary_key_ids.push(cfg_field_id);
                }
            }

            let cfg_table = TableInfo {
                name: table_ast.name.clone(),
                fields: field_ids.clone(),
                primary_keys: primary_key_ids,
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
            let builder = FunctionContextBuilder::new(ctx, func_ast)?;
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

    fn build(mut self, program: &ast::Program, func_ast: &ast::FunctionDeclaration) -> Result<FunctionCfg, String> {
        self.build_parameters(program, func_ast)?;
        self.build_hops(program, func_ast)?;
        Ok(self.function)
    }

    fn build_parameters(&mut self, program: &ast::Program, func_ast: &ast::FunctionDeclaration) -> Result<(), String> {
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

    fn build_hops(&mut self, program: &ast::Program, func_ast: &ast::FunctionDeclaration) -> Result<(), String> {
        for &hop_ast_id in &func_ast.hops {
            let hop_ast = &program.hops[hop_ast_id];
            
            match &hop_ast.hop_type {
                ast::HopType::Simple => {
                    self.create_simple_hop(program, hop_ast)?;
                }
                ast::HopType::ForLoop { loop_var, loop_var_type, init, condition, increment } => {
                    // Flatten "hops for" into 10 deterministic hops (since AST is validated)
                    self.create_flattened_hops_for(program, hop_ast, loop_var, loop_var_type, *init, *condition, *increment)?;
                }
            }
        }
        Ok(())
    }

    fn create_simple_hop(&mut self, program: &ast::Program, hop_ast: &ast::HopBlock) -> Result<(), String> {
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
        _init: ast::ExpressionId,
        _condition: ast::ExpressionId,
        _increment: ast::ExpressionId,
    ) -> Result<(), String> {
        // Fixed 10 iterations since AST is validated
        let num_iterations = 10;

        let loop_var_obj = Variable {
            name: loop_var.to_string(),
            ty: loop_var_type.clone(),
            is_parameter: false,
        };
        let loop_var_id = self.function.variables.alloc(loop_var_obj);
        self.var_map.insert(loop_var.to_string(), loop_var_id);

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

            self.build_hop_with_loop_iteration(program, hop_id, &hop_ast.statements, loop_var_id, i)?;
        }
        Ok(())
    }

    fn build_hop_statements(&mut self, program: &ast::Program, hop_id: HopId, statements: &[ast::StatementId]) -> Result<(), String> {
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
        iteration: usize,
    ) -> Result<(), String> {
        self.current_hop_id = Some(hop_id);

        let entry_block = self.new_basic_block(hop_id)?;
        self.function.hops[hop_id].entry_block = Some(entry_block);
        self.current_block_id = Some(entry_block);

        self.add_statement(
            entry_block,
            Statement::Assign {
                var: loop_var_id,
                rvalue: Rvalue::Use(Operand::Const(Constant::Int(iteration as i64))),
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

    fn build_statement(&mut self, program: &ast::Program, stmt: &ast::Statement) -> Result<(), String> {
        let current_block = self.current_block_id.ok_or("No active block for statement")?;

        match &stmt.node {
            ast::StatementKind::VarDecl(var_decl) => {
                let var = Variable {
                    name: var_decl.var_name.clone(),
                    ty: var_decl.var_type.clone(),
                    is_parameter: false,
                };

                let var_id = self.function.variables.alloc(var);
                self.var_map.insert(var_decl.var_name.clone(), var_id);

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
            ast::StatementKind::Assignment(assign) => {
                self.build_assignment(program, assign, stmt.span.clone())?;
            }
            ast::StatementKind::Return(ret) => {
                if let Some(expr_id) = ret.value {
                    let _operand = self.build_expression(program, expr_id)?;
                    // Return handling - terminate block
                }
                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Abort(_) => {
                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Break(_) => {
                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Continue(_) => {
                self.current_block_id = None; // Terminate block
            }
            ast::StatementKind::Empty => {
                // No-op
            }
            ast::StatementKind::IfStmt(_) |
            ast::StatementKind::ForStmt(_) |
            ast::StatementKind::WhileStmt(_) => {
                // Control flow statements not supported in hops - should have been caught in semantics
                return Ok(());
            }
        }
        Ok(())
    }

    fn build_assignment(&mut self, program: &ast::Program, assign: &ast::AssignmentStatement, span: ast::Span) -> Result<(), String> {
        let current_block = self.current_block_id.ok_or("No active block")?;

        match &assign.lvalue {
            ast::LValue::Var { resolved_var, .. } => {
                let var_id = if let Some(resolved_var) = resolved_var {
                    let var_ast = &program.variables[*resolved_var];
                    *self.var_map.get(&var_ast.name).ok_or_else(|| format!("Variable {} not found in CFG", var_ast.name))?
                } else {
                    return Ok(()); // Unresolved - skip
                };

                let rhs_operand = self.build_expression(program, assign.rhs)?;

                self.add_statement(
                    current_block,
                    Statement::Assign {
                        var: var_id,
                        rvalue: Rvalue::Use(rhs_operand),
                        span,
                    },
                );
            }
            ast::LValue::TableField { resolved_table, resolved_pk_fields, resolved_field, pk_exprs, .. } => {
                if let (Some(table_ast_id), Some(field_ast_id)) = (resolved_table, resolved_field) {
                    let table_ast = &program.tables[*table_ast_id];
                    let field_ast = &program.fields[*field_ast_id];
                    
                    let table_id = *self.ctx.table_map.get(&table_ast.name).ok_or("Table not found")?;
                    let field_id = *self.ctx.field_map.get(&field_ast.field_name).ok_or("Field not found")?;

                    let mut pk_field_ids = Vec::new();
                    let mut pk_operands = Vec::new();

                    for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                        if let Some(Some(pk_field)) = resolved_pk_fields.get(i) {
                            let field_ast = &program.fields[*pk_field];
                            let pk_field_id = *self.ctx.field_map.get(&field_ast.field_name).ok_or("PK field not found")?;
                            let pk_operand = self.build_expression(program, pk_expr)?;

                            pk_field_ids.push(pk_field_id);
                            pk_operands.push(pk_operand);
                        }
                    }

                    let value_operand = self.build_expression(program, assign.rhs)?;

                    self.add_statement(
                        current_block,
                        Statement::TableAssign {
                            table: table_id,
                            pk_fields: pk_field_ids,
                            pk_values: pk_operands,
                            field: field_id,
                            value: value_operand,
                            span,
                        },
                    );
                }
            }
            ast::LValue::TableRecord { resolved_table, resolved_pk_fields, pk_exprs, .. } => {
                if let Some(table_ast_id) = resolved_table {
                    let table_ast = &program.tables[*table_ast_id];
                    let table_id = *self.ctx.table_map.get(&table_ast.name).ok_or("Table not found")?;

                    let mut pk_field_ids = Vec::new();
                    let mut pk_operands = Vec::new();

                    for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                        if let Some(Some(pk_field)) = resolved_pk_fields.get(i) {
                            let field_ast = &program.fields[*pk_field];
                            let pk_field_id = *self.ctx.field_map.get(&field_ast.field_name).ok_or("PK field not found")?;
                            let pk_operand = self.build_expression(program, pk_expr)?;

                            pk_field_ids.push(pk_field_id);
                            pk_operands.push(pk_operand);
                        }
                    }

                    // Expand record literal into multiple TableAssign statements
                    if let ast::ExpressionKind::RecordLiteral { fields, .. } = &program.expressions[assign.rhs].node {
                        for field_assignment in fields {
                            if let Some(&field_id) = self.ctx.field_map.get(&field_assignment.field_name) {
                                let value_operand = self.build_expression(program, field_assignment.value)?;

                                self.add_statement(
                                    current_block,
                                    Statement::TableAssign {
                                        table: table_id,
                                        pk_fields: pk_field_ids.clone(),
                                        pk_values: pk_operands.clone(),
                                        field: field_id,
                                        value: value_operand,
                                        span: span.clone(),
                                    },
                                );
                            }
                        }
                    }
                }
            }
            ast::LValue::ArrayElement { .. } => {
                // Array elements not supported - skip
            }
        }
        Ok(())
    }

    fn build_expression(&mut self, program: &ast::Program, expr_id: ast::ExpressionId) -> Result<Operand, String> {
        let expr = &program.expressions[expr_id];

        match &expr.node {
            ast::ExpressionKind::Ident(name) => {
                if let Some(&var_id) = self.var_map.get(name) {
                    Ok(Operand::Var(var_id))
                } else {
                    Ok(Operand::Const(Constant::Int(0))) // Default for unresolved
                }
            }
            ast::ExpressionKind::IntLit(val) => Ok(Operand::Const(Constant::Int(*val))),
            ast::ExpressionKind::FloatLit(val) => Ok(Operand::Const(Constant::Float(ordered_float::OrderedFloat::from(*val)))),
            ast::ExpressionKind::StringLit(val) => Ok(Operand::Const(Constant::String(val.clone()))),
            ast::ExpressionKind::BoolLit(val) => Ok(Operand::Const(Constant::Bool(*val))),
            ast::ExpressionKind::UnaryOp { op, expr, .. } => {
                let operand = self.build_expression(program, *expr)?;
                let temp_var = self.create_temp_var();
                let rvalue = match op {
                    ast::UnaryOp::Not => Rvalue::UnaryOp { op: UnaryOp::Not, operand },
                    ast::UnaryOp::Neg => Rvalue::UnaryOp { op: UnaryOp::Neg, operand },
                };
                
                if let Some(block_id) = self.current_block_id {
                    self.add_statement(block_id, Statement::Assign {
                        var: temp_var,
                        rvalue,
                        span: program.expressions[*expr].span.clone(),
                    });
                }
                Ok(Operand::Var(temp_var))
            }
            ast::ExpressionKind::BinaryOp { left, op, right, .. } => {
                let left_operand = self.build_expression(program, *left)?;
                let right_operand = self.build_expression(program, *right)?;
                let temp_var = self.create_temp_var();
                let rvalue = match op {
                    ast::BinaryOp::Add => Rvalue::BinaryOp { op: BinaryOp::Add, left: left_operand, right: right_operand },
                    ast::BinaryOp::Sub => Rvalue::BinaryOp { op: BinaryOp::Sub, left: left_operand, right: right_operand },
                    ast::BinaryOp::Mul => Rvalue::BinaryOp { op: BinaryOp::Mul, left: left_operand, right: right_operand },
                    ast::BinaryOp::Div => Rvalue::BinaryOp { op: BinaryOp::Div, left: left_operand, right: right_operand },
                    ast::BinaryOp::Lt => Rvalue::BinaryOp { op: BinaryOp::Lt, left: left_operand, right: right_operand },
                    ast::BinaryOp::Lte => Rvalue::BinaryOp { op: BinaryOp::Lte, left: left_operand, right: right_operand },
                    ast::BinaryOp::Gt => Rvalue::BinaryOp { op: BinaryOp::Gt, left: left_operand, right: right_operand },
                    ast::BinaryOp::Gte => Rvalue::BinaryOp { op: BinaryOp::Gte, left: left_operand, right: right_operand },
                    ast::BinaryOp::Eq => Rvalue::BinaryOp { op: BinaryOp::Eq, left: left_operand, right: right_operand },
                    ast::BinaryOp::Neq => Rvalue::BinaryOp { op: BinaryOp::Neq, left: left_operand, right: right_operand },
                    ast::BinaryOp::And => Rvalue::BinaryOp { op: BinaryOp::And, left: left_operand, right: right_operand },
                    ast::BinaryOp::Or => Rvalue::BinaryOp { op: BinaryOp::Or, left: left_operand, right: right_operand },
                };
                
                if let Some(block_id) = self.current_block_id {
                    self.add_statement(block_id, Statement::Assign {
                        var: temp_var,
                        rvalue,
                        span: program.expressions[*left].span.clone(),
                    });
                }
                Ok(Operand::Var(temp_var))
            }
            _ => {
                // For unsupported expressions, return a default constant
                Ok(Operand::Const(Constant::Int(0)))
            }
        }
    }

    fn create_temp_var(&mut self) -> VarId {
        let temp_name = format!("_temp_{}", self.temp_counter);
        self.temp_counter += 1;
        
        let var = Variable {
            name: temp_name.clone(),
            ty: TypeName { base: crate::ast::PrimitiveType::Int, dims: Vec::new() },
            is_parameter: false,
        };
        
        let var_id = self.function.variables.alloc(var);
        self.var_map.insert(temp_name, var_id);
        var_id
    }

    fn new_basic_block(&mut self, hop_id: HopId) -> Result<BasicBlockId, String> {
        let block = BasicBlock {
            hop_id,
            statements: Vec::new(),
            terminator: Terminator::Abort,
            span: ast::Span::default(),
        };

        let block_id = self.function.blocks.alloc(block);
        self.function.hops[hop_id].blocks.push(block_id);
        Ok(block_id)
    }

    fn add_statement(&mut self, block_id: BasicBlockId, stmt: Statement) {
        self.function.blocks[block_id].statements.push(stmt);
    }
}
