use std::collections::HashMap;
use std::rc::Rc;

use crate::ast;
use crate::cfg::{
    self, BasicBlock, BasicBlockData, CfgCtx, Constant, FieldId, FunctionBody, FunctionId, HopId,
    HopInfo, IndexVec, Local, LocalDecl, NodeId, Operand, Rvalue, Statement, TableId, Terminator,
    LoopContext, ReturnType, TypeName, UnaryOp, BinaryOp,
};

/// Builds CFG from an AST Program.
pub struct CfgBuilder<'ast> {
    program_ast: &'ast ast::Program,
    // These maps are built once and then read by FunctionContextBuilder
    node_map: HashMap<Rc<ast::NodeDef>, NodeId>,
    table_map: HashMap<Rc<ast::TableDeclaration>, TableId>,
    field_map: HashMap<Rc<ast::FieldDeclaration>, FieldId>,
}

/// Helper struct to manage building a single function's CFG.
struct FunctionContextBuilder<'a, 'ast> {
    // Global context and AST references
    ctx: &'a mut CfgCtx, // Mutable reference to add global items like fields if necessary (though mostly populated before)
    #[allow(dead_code)] // May be used for global lookups not covered by maps
    program_ast: &'ast ast::Program,
    node_map: &'a HashMap<Rc<ast::NodeDef>, NodeId>,
    table_map: &'a HashMap<Rc<ast::TableDeclaration>, TableId>,
    field_map: &'a HashMap<Rc<ast::FieldDeclaration>, FieldId>,

    // Function-specific data being built
    fn_ast: &'ast ast::FunctionDeclaration,
    body: FunctionBody, // The CFG body we are constructing for this function

    local_map: HashMap<String, Local>, // AST name to CFG Local for the current function
    next_local_idx: u32,               // For allocating new Locals

    loop_stack: Vec<LoopContext>, // For break/continue resolution

    // Current building state within the function
    current_hop_id: Option<HopId>,    // CFG HopId of the hop being processed
    current_block_id: Option<BasicBlock>, // Active basic block being appended to
}

// Enum to help manage expression building logic
enum RvalueOrOperand {
    Rvalue(cfg::Rvalue),
    Operand(cfg::Operand),
}

impl<'ast> CfgBuilder<'ast> {
    pub fn new(program_ast: &'ast ast::Program) -> Self {
        CfgBuilder {
            program_ast,
            node_map: HashMap::new(),
            table_map: HashMap::new(),
            field_map: HashMap::new(),
        }
    }

    /// Consumes the builder and returns the populated CfgCtx and a list of FunctionBody CFGs.
    pub fn build(mut self) -> (CfgCtx, IndexVec<FunctionId, FunctionBody>) {
        let mut ctx = CfgCtx {
            nodes: IndexVec::new(),
            tables: IndexVec::new(),
            fields: IndexVec::new(),
            table_fields: HashMap::new(),
            table_primary_keys: HashMap::new(),
        };

        self.populate_global_maps(&mut ctx);

        let mut function_bodies = IndexVec::new();
        for (idx, func_ast) in self.program_ast.functions.iter().enumerate() {
            let func_id = FunctionId::new(idx as u32);

            let mut fn_ctx_builder = FunctionContextBuilder {
                ctx: &mut ctx,
                program_ast: self.program_ast,
                node_map: &self.node_map,
                table_map: &self.table_map,
                field_map: &self.field_map,
                fn_ast,
                body: FunctionBody {
                    id: func_id,
                    name: Rc::from(func_ast.name.as_str()),
                    params: Vec::new(), // Populated by build_function
                    return_type: Self::convert_ast_return_type(&func_ast.return_type),
                    hops: IndexVec::new(),
                    blocks: IndexVec::new(),
                    locals: IndexVec::new(),
                    entry_block: BasicBlock::new(0), // Placeholder, set by build_function
                },
                local_map: HashMap::new(),
                next_local_idx: 0,
                loop_stack: Vec::new(),
                current_hop_id: None,
                current_block_id: None,
            };

            let built_body = fn_ctx_builder.build_function();
            function_bodies.push(built_body);
        }

        (ctx, function_bodies)
    }

    /// Populates CfgCtx with global definitions (nodes, tables, fields) and builder's internal maps.
    fn populate_global_maps(&mut self, ctx: &mut CfgCtx) {
        // Fix arena iteration - use iter() which returns (Id, &Value) pairs
        for (node_id, node_def) in self.program_ast.nodes.iter() {
            ctx.node_map.insert(node_def.name.clone(), node_id);
        }

        for (table_id, table_decl) in self.program_ast.tables.iter() {
            ctx.table_map.insert(table_decl.name.clone(), table_id);
            
            // Build field mappings
            let mut field_ids = Vec::new();
            for &field_id in &table_decl.fields {
                let field = &self.program_ast.fields[field_id];
                field_ids.push(field_id);
                if field.is_primary {
                    ctx.table_primary_keys.insert(table_id, field_id);
                }
            }
            ctx.table_fields.insert(table_id, field_ids);
        }
    }
    
    // Static conversion helpers
    fn convert_ast_type(ty: &ast::TypeName) -> cfg::TypeName {
        match ty {
            ast::TypeName::Int => cfg::TypeName::Int,
            ast::TypeName::Float => cfg::TypeName::Float,
            ast::TypeName::String => cfg::TypeName::String,
            ast::TypeName::Bool => cfg::TypeName::Bool,
        }
    }

    fn convert_ast_return_type(ty: &ast::ReturnType) -> cfg::ReturnType {
        match ty {
            ast::ReturnType::Void => cfg::ReturnType::Void,
            ast::ReturnType::Type(t) => cfg::ReturnType::Type(Self::convert_ast_type(t)),
        }
    }

    fn convert_ast_binop(op: ast::BinaryOp) -> cfg::BinaryOp {
        match op {
            ast::BinaryOp::Add => cfg::BinaryOp::Add,
            ast::BinaryOp::Sub => cfg::BinaryOp::Sub,
            ast::BinaryOp::Mul => cfg::BinaryOp::Mul,
            ast::BinaryOp::Div => cfg::BinaryOp::Div,
            ast::BinaryOp::Lt => cfg::BinaryOp::Lt,
            ast::BinaryOp::Lte => cfg::BinaryOp::Lte,
            ast::BinaryOp::Gt => cfg::BinaryOp::Gt,
            ast::BinaryOp::Gte => cfg::BinaryOp::Gte,
            ast::BinaryOp::Eq => cfg::BinaryOp::Eq,
            ast::BinaryOp::Neq => cfg::BinaryOp::Neq,
            ast::BinaryOp::And => cfg::BinaryOp::And,
            ast::BinaryOp::Or => cfg::BinaryOp::Or,
        }
    }

    fn convert_ast_unop(op: ast::UnaryOp) -> cfg::UnaryOp {
        match op {
            ast::UnaryOp::Not => cfg::UnaryOp::Not,
            ast::UnaryOp::Neg => cfg::UnaryOp::Neg,
        }
    }
}

impl<'a, 'ast> FunctionContextBuilder<'a, 'ast> {
    /// Builds the CFG for the function associated with this builder.
    fn build_function(mut self) -> FunctionBody {
        // 1. Process parameters: create LocalDecls and map names
        for param_ast in &self.fn_ast.parameters {
            let local_id = self.new_local_decl(
                Some(param_ast.param_name.clone()),
                param_ast.param_type.clone(),
                true,  // is_param
                false, // is_global
                param_ast.span.clone(),
            );
            self.body.params.push(local_id);
        }

        // 2. Handle functions with no explicit hops (create a dummy hop)
        if self.fn_ast.hops.is_empty() {
            let (dummy_node_id, dummy_span) = self.program_ast.nodes.first().map_or_else(
                || {
                    // Fallback if no nodes defined; ideally a semantic error or default node.
                    (NodeId::new(0), self.fn_ast.span.clone())
                },
                |node_def| (*self.node_map.get(node_def).unwrap(), node_def.span.clone()),
            );

            let dummy_hop_id = self.body.hops.push(HopInfo {
                node: dummy_node_id,
                entry: BasicBlock::new(0), // Placeholder
                exits: Vec::new(),
                span: dummy_span,
            });
            self.current_hop_id = Some(dummy_hop_id);

            let entry_block_id = self.new_basic_block(dummy_hop_id);
            self.body.hops[dummy_hop_id].entry = entry_block_id;
            self.body.entry_block = entry_block_id; // Function entry

            self.terminate_block_for_function_end(entry_block_id, dummy_hop_id);
            self.populate_block_successors();
            return self.body;
        }

        // 3. Create all HopInfo and their entry BasicBlocks first
        let mut hop_entry_blocks = Vec::new();
        for (idx, hop_ast) in self.fn_ast.hops.iter().enumerate() {
            let node_rc = hop_ast.node.clone();
            let cfg_node_id = *self.node_map.get(&node_rc)
                .expect("AST NodeDef not found in map (should be caught by semantic analysis)");

            let hop_id = self.body.hops.push(HopInfo {
                node: cfg_node_id,
                entry: BasicBlock::new(0), // Placeholder
                exits: Vec::new(),
                span: hop_ast.span.clone(),
            });
            let entry_bb = self.new_basic_block(hop_id);
            self.body.hops[hop_id].entry = entry_bb;
            hop_entry_blocks.push(entry_bb);

            if idx == 0 {
                self.body.entry_block = entry_bb; // Function entry
            }
        }

        // 4. Build statements for each hop
        for (hop_idx, hop_ast) in self.fn_ast.hops.iter().enumerate() {
            let current_cfg_hop_id = HopId::new(hop_idx as u32);
            self.current_hop_id = Some(current_cfg_hop_id);
            self.current_block_id = Some(hop_entry_blocks[hop_idx]); // Start with the hop's entry block

            for stmt_ast in &hop_ast.statements {
                if self.current_block_id.is_none() {
                    // Block was terminated by a previous statement (return, abort, break, continue)
                    break;
                }
                self.build_statement(stmt_ast);
            }

            // If the current block is still active after all statements in this hop
            if let Some(active_block_id) = self.current_block_id.take() {
                if hop_idx < self.fn_ast.hops.len() - 1 {
                    // Not the last hop: transition to the next hop's entry block
                    let next_hop_entry_block_id = hop_entry_blocks[hop_idx + 1];
                    let next_cfg_hop_id = HopId::new((hop_idx + 1) as u32);
                    self.set_terminator(active_block_id, Terminator::HopTransition {
                        next_hop: next_cfg_hop_id,
                        target_block: next_hop_entry_block_id,
                    });
                    self.body.hops[current_cfg_hop_id].exits.push(active_block_id);
                } else {
                    // Last hop: terminate the function
                    self.terminate_block_for_function_end(active_block_id, current_cfg_hop_id);
                }
            }
        }

        self.populate_block_successors();
        self.body
    }

    /// Helper to terminate a block at the end of a function or a dummy hop.
    fn terminate_block_for_function_end(&mut self, block_id: BasicBlock, hop_id: HopId) {
         match self.body.return_type {
            ReturnType::Void => {
                self.set_terminator(block_id, Terminator::Return(None));
            }
            _ => {
                // Non-void functions must have an explicit return. If we reach here, it's an implicit abort.
                // This should ideally be a semantic error.
                self.set_terminator(block_id, Terminator::Abort);
            }
        }
        self.body.hops[hop_id].exits.push(block_id);
    }


    /// Creates a LocalDecl, adds it to the function body, and maps its name.
    fn new_local_decl(&mut self, name_opt: Option<String>, ty: ast::TypeName, is_param: bool, is_global: bool, span: ast::Span) -> Local {
        let local_id = Local::new(self.next_local_idx);
        self.next_local_idx += 1;

        let name_rc = Rc::from(name_opt.clone().unwrap_or_else(|| format!("_temp{}", local_id.as_usize())));

        self.body.locals.push(LocalDecl {
            name: name_rc,
            ty: CfgBuilder::convert_ast_type(&ty),
            is_param,
            is_global, // Note: CFG `LocalDecl` has `is_global`. AST `VarDeclStatement` also has it.
            span,
        });

        if let Some(name) = name_opt {
            self.local_map.insert(name, local_id);
        }
        local_id
    }

    /// Creates a new BasicBlock associated with the given HopId.
    fn new_basic_block(&mut self, hop_id: HopId) -> BasicBlock {
        self.body.blocks.push(BasicBlockData {
            statements: Vec::new(),
            terminator: Terminator::Abort, // Placeholder, must be overwritten
            hop: hop_id,
            successors: Vec::new(), // Populated at the end
        })
    }

    fn add_statement(&mut self, block_id: BasicBlock, stmt: cfg::Statement) {
        self.body.blocks[block_id].statements.push(stmt);
    }

    fn set_terminator(&mut self, block_id: BasicBlock, terminator: cfg::Terminator) {
        self.body.blocks[block_id].terminator = terminator;
    }

    /// Builds a single AST statement and appends its CFG equivalent.
    /// Updates `self.current_block_id` if the statement terminates the current block.
    fn build_statement(&mut self, stmt_ast: &'ast ast::Statement) {
        let current_bb_id = self.current_block_id.expect("No active basic block for statement");
        let span = stmt_ast.span.clone(); // Capture span for all generated CFG elements

        match &stmt_ast.node {
            ast::StatementKind::VarDecl(var_decl_ast) => {
                let init_operand = self.build_expr_to_operand(&var_decl_ast.init_value, current_bb_id);
                let local_id = self.new_local_decl(
                    Some(var_decl_ast.var_name.clone()),
                    var_decl_ast.var_type.clone(),
                    false, // not a param
                    var_decl_ast.is_global,
                    span.clone(),
                );
                self.add_statement(current_bb_id, cfg::Statement::Assign {
                    local: local_id,
                    rvalue: cfg::Rvalue::Use(init_operand), // Assign the already computed operand
                    span,
                });
            }
            ast::StatementKind::VarAssignment(assign_ast) => {
                let local_id = *self.local_map.get(&assign_ast.var_name)
                    .expect("Undeclared variable (semantic error)");
                let rhs_operand = self.build_expr_to_operand(&assign_ast.rhs, current_bb_id);
                self.add_statement(current_bb_id, cfg::Statement::Assign {
                    local: local_id,
                    rvalue: cfg::Rvalue::Use(rhs_operand),
                    span,
                });
            }
            ast::StatementKind::Assignment(assign_ast) => {
                let table_id = *self.table_map.get(&assign_ast.table).expect("Table AST not found");
                let pk_operand = self.build_expr_to_operand(&assign_ast.pk_expr, current_bb_id);
                let field_id = *self.field_map.get(&assign_ast.field).expect("Field AST not found");
                let rhs_operand = self.build_expr_to_operand(&assign_ast.rhs, current_bb_id);
                self.add_statement(current_bb_id, cfg::Statement::TableAssign {
                    table: table_id,
                    primary_key: pk_operand,
                    field: field_id,
                    value: rhs_operand,
                    span,
                });
            }
            ast::StatementKind::IfStmt(if_ast) => {
                let cond_operand = self.build_expr_to_operand(&if_ast.condition, current_bb_id);
                
                let then_block_id = self.new_basic_block(self.current_hop_id.unwrap());
                let merge_block_id = self.new_basic_block(self.current_hop_id.unwrap());

                let else_target_block_id = if if_ast.else_branch.is_some() {
                    self.new_basic_block(self.current_hop_id.unwrap())
                } else {
                    merge_block_id // No else, condition false goes directly to merge
                };

                self.set_terminator(current_bb_id, Terminator::Branch {
                    condition: cond_operand,
                    then_block: then_block_id,
                    else_block: else_target_block_id,
                });
                // current_bb_id is now terminated. Subsequent statements go into new blocks.
                
                // Build then branch
                self.current_block_id = Some(then_block_id);
                for stmt in &if_ast.then_branch {
                    if self.current_block_id.is_none() { break; } // Terminated within then-branch
                    self.build_statement(stmt);
                }
                if let Some(active_then_block) = self.current_block_id.take() {
                    self.set_terminator(active_then_block, Terminator::Goto(merge_block_id));
                }

                // Build else branch if it exists
                if let Some(else_stmts) = &if_ast.else_branch {
                    self.current_block_id = Some(else_target_block_id); // else_target_block_id is the start of else
                    for stmt in else_stmts {
                        if self.current_block_id.is_none() { break; } // Terminated within else-branch
                        self.build_statement(stmt);
                    }
                    if let Some(active_else_block) = self.current_block_id.take() {
                        self.set_terminator(active_else_block, Terminator::Goto(merge_block_id));
                    }
                }
                
                self.current_block_id = Some(merge_block_id); // Continue building from merge block
            }
            ast::StatementKind::WhileStmt(while_ast) => {
                let header_block_id = self.new_basic_block(self.current_hop_id.unwrap());
                let body_block_id = self.new_basic_block(self.current_hop_id.unwrap());
                let exit_block_id = self.new_basic_block(self.current_hop_id.unwrap());

                self.set_terminator(current_bb_id, Terminator::Goto(header_block_id));

                // Header block: evaluate condition
                self.current_block_id = Some(header_block_id);
                let cond_operand = self.build_expr_to_operand(&while_ast.condition, header_block_id);
                self.set_terminator(header_block_id, Terminator::Branch {
                    condition: cond_operand,
                    then_block: body_block_id,
                    else_block: exit_block_id,
                });
                
                // Body block: push loop context, build statements, jump to header
                self.current_block_id = Some(body_block_id);
                self.loop_stack.push(LoopContext {
                    continue_target: header_block_id,
                    break_target: exit_block_id,
                    hop: self.current_hop_id.unwrap(),
                });
                for stmt in &while_ast.body {
                    if self.current_block_id.is_none() { break; } // Terminated by break/continue/return
                    self.build_statement(stmt);
                }
                self.loop_stack.pop();
                if let Some(active_body_block) = self.current_block_id.take() {
                    self.set_terminator(active_body_block, Terminator::Goto(header_block_id));
                }
                
                self.current_block_id = Some(exit_block_id); // Continue from exit block
            }
            ast::StatementKind::Return(ret_ast) => {
                let ret_val_opt = ret_ast.value.as_ref().map(|expr| self.build_expr_to_operand(expr, current_bb_id));
                self.set_terminator(current_bb_id, Terminator::Return(ret_val_opt));
                self.body.hops[self.current_hop_id.unwrap()].exits.push(current_bb_id);
                self.current_block_id = None; // Block terminated
            }
            ast::StatementKind::Abort(_) => {
                self.set_terminator(current_bb_id, Terminator::Abort);
                self.body.hops[self.current_hop_id.unwrap()].exits.push(current_bb_id);
                self.current_block_id = None; // Block terminated
            }
            ast::StatementKind::Break(_) => {
                let loop_ctx = self.loop_stack.last().expect("Break outside loop (semantic error)");
                // TODO: Add check if loop_ctx.hop matches current_hop_id (breaks cannot cross hops)
                self.set_terminator(current_bb_id, Terminator::Goto(loop_ctx.break_target));
                self.current_block_id = None; // Block terminated
            }
            ast::StatementKind::Continue(_) => {
                let loop_ctx = self.loop_stack.last().expect("Continue outside loop (semantic error)");
                // TODO: Add check if loop_ctx.hop matches current_hop_id
                self.set_terminator(current_bb_id, Terminator::Goto(loop_ctx.continue_target));
                self.current_block_id = None; // Block terminated
            }
            ast::StatementKind::Empty => { /* No-op, current_block_id remains active */ }
        }
    }

    /// Builds an AST expression, ensuring the result is a `cfg::Operand`.
    /// If the AST expression is complex (e.g., binary op, table access), it creates a temporary
    /// local, emits an assignment statement for the complex part, and returns the temporary local.
    fn build_expr_to_operand(&mut self, expr_ast: &'ast ast::Expression, current_bb_id: BasicBlock) -> cfg::Operand {
        let span = expr_ast.span.clone();
        match self.build_expr_to_rvalue_or_operand(expr_ast, current_bb_id) {
            RvalueOrOperand::Operand(op) => op,
            RvalueOrOperand::Rvalue(rval) => {
                // Determine type for the temporary local. This is simplified.
                // A proper implementation would use type information from a semantic analysis pass.
                let temp_var_type_ast = match &rval {
                    cfg::Rvalue::TableAccess { table: _, field, .. } => {
                        // Get type from FieldDeclaration in CfgCtx
                        self.convert_cfg_type_to_ast(self.ctx.fields[*field].field_type.clone())
                    }
                    // For UnaryOp and BinaryOp, the type depends on the operation and operand types.
                    // This requires more sophisticated type inference rules.
                    // For now, let's assume Int for simplicity if not TableAccess. This is a placeholder.
                    _ => ast::TypeName::Int, // Placeholder: Needs proper type inference
                };

                let temp_local = self.new_local_decl(None, temp_var_type_ast, false, false, span.clone());
                self.add_statement(current_bb_id, cfg::Statement::Assign {
                    local: temp_local,
                    rvalue: rval,
                    span,
                });
                cfg::Operand::Local(temp_local)
            }
        }
    }
    
    /// Converts cfg::TypeName to ast::TypeName (helper for temp var type).
    fn convert_cfg_type_to_ast(&self, ty: cfg::TypeName) -> ast::TypeName {
        match ty {
            cfg::TypeName::Int => ast::TypeName::Int,
            cfg::TypeName::Float => ast::TypeName::Float,
            cfg::TypeName::String => ast::TypeName::String,
            cfg::TypeName::Bool => ast::TypeName::Bool,
        }
    }

    /// Builds an AST expression into either a direct `cfg::Operand` or a `cfg::Rvalue`.
    fn build_expr_to_rvalue_or_operand(&mut self, expr_ast: &'ast ast::Expression, current_bb_id: BasicBlock) -> RvalueOrOperand {
        match &expr_ast.node {
            ast::ExpressionKind::Ident(name) => {
                let local_id = *self.local_map.get(name).expect("Undeclared identifier (semantic error)");
                RvalueOrOperand::Operand(cfg::Operand::Local(local_id))
            }
            ast::ExpressionKind::IntLit(val) => RvalueOrOperand::Operand(cfg::Operand::Constant(Constant::Int(*val))),
            ast::ExpressionKind::FloatLit(val) => RvalueOrOperand::Operand(cfg::Operand::Constant(Constant::Float(*val))),
            ast::ExpressionKind::StringLit(val) => RvalueOrOperand::Operand(cfg::Operand::Constant(Constant::String(Rc::from(val.as_str())))),
            ast::ExpressionKind::BoolLit(val) => RvalueOrOperand::Operand(cfg::Operand::Constant(Constant::Bool(*val))),
            ast::ExpressionKind::TableFieldAccess { table, pk_expr, field, .. } => {
                let table_id = *self.table_map.get(table).expect("Table AST not found");
                let pk_op = self.build_expr_to_operand(pk_expr, current_bb_id); // pk_expr must yield an Operand
                let field_id = *self.field_map.get(field).expect("Field AST not found");
                RvalueOrOperand::Rvalue(cfg::Rvalue::TableAccess {
                    table: table_id,
                    primary_key: Box::new(pk_op),
                    field: field_id,
                })
            }
            ast::ExpressionKind::UnaryOp { op, expr } => {
                let operand = self.build_expr_to_operand(expr, current_bb_id);
                RvalueOrOperand::Rvalue(cfg::Rvalue::UnaryOp {
                    op: CfgBuilder::convert_ast_unop(*op),
                    operand: Box::new(operand),
                })
            }
            ast::ExpressionKind::BinaryOp { left, op, right } => {
                let left_op = self.build_expr_to_operand(left, current_bb_id);
                let right_op = self.build_expr_to_operand(right, current_bb_id);
                RvalueOrOperand::Rvalue(cfg::Rvalue::BinaryOp {
                    left: Box::new(left_op),
                    op: CfgBuilder::convert_ast_binop(*op),
                    right: Box::new(right_op),
                })
            }
        }
    }

    /// Populates the `successors` field for each basic block in the current function body.
    fn populate_block_successors(&mut self) {
        for i in 0..self.body.blocks.len() {
            let block_id = BasicBlock::new(i as u32);
            // Clone terminator to avoid borrow issues if it contains block IDs we might modify (not an issue here)
            let terminator = self.body.blocks[block_id].terminator.clone();
            let mut successors = Vec::new();
            match terminator {
                Terminator::Goto(target) => {
                    successors.push(target);
                }
                Terminator::Branch { then_block, else_block, .. } => {
                    successors.push(then_block);
                    successors.push(else_block);
                }
                Terminator::HopTransition { target_block, .. } => {
                    successors.push(target_block);
                }
                Terminator::Return(_) | Terminator::Abort => {
                    // No successors
                }
            }
            self.body.blocks[block_id].successors = successors;
        }
    }
}