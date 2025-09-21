//! cfg/cfg_builder.rs
//!
//! This module is responsible for converting the semantic-analyzed Abstract Syntax Tree (AST)
//! into a Control Flow Graph (CFG) Intermediate Representation.
//! It handles the translation of all language constructs, including the special scoping
//! and unrolling rules for transactional hops.

use crate::ast::{self, VisitorMut};
use crate::cfg;
use id_arena::{Arena, Id};
use std::collections::HashMap;
use ordered_float::OrderedFloat;

// Helper macro for unimplemented features
macro_rules! cfg_unimplemented {
    ($($arg:tt)*) => {
        unimplemented!("CFG build: {}", format!($($arg)*))
    };
}

// ============================================================================
// --- Main Builder Entry Point
// ============================================================================

/// The main entry point for CFG construction. Consumes the AST and produces a CFG.
pub fn build(ast: ast::Program) -> cfg::Program {
    let mut builder = CfgBuilder::new(ast);
    builder.build()
}

// ============================================================================
// --- CfgBuilder and supporting structs
// ============================================================================

/// A builder that holds the state for the entire AST to CFG conversion process.
struct CfgBuilder {
    ast: ast::Program,
    cfg: cfg::Program,

    // Mappings from AST IDs to CFG IDs
    type_map: HashMap<ast::TypeDeclId, cfg::UserDefinedTypeId>,
    table_map: HashMap<ast::TableId, cfg::TableId>,
    field_map: HashMap<ast::FieldId, cfg::FieldId>,
    const_map: HashMap<ast::ConstId, cfg::GlobalConstId>,
    func_map: HashMap<ast::FunctionId, cfg::FunctionId>,
    generic_param_map: HashMap<ast::GenericParamId, cfg::GenericParamId>,

    // Cache for type conversion to avoid duplicating type nodes
    resolved_type_cache: HashMap<ast::ResolvedType, cfg::TypeId>,

    // Quick access to primitive and core built-in types
    builtin_types: BuiltinTypes,
}

/// Holds the CFG IDs for primitive and core built-in types for quick lookup.
#[derive(Debug, Default)]
struct BuiltinTypes {
    int: cfg::TypeId,
    float: cfg::TypeId,
    bool: cfg::TypeId,
    string: cfg::TypeId,
    void: cfg::TypeId,
    // Map from name to UserDefinedTypeId for generic built-ins like `List`
    generic_builtins: HashMap<String, cfg::UserDefinedTypeId>,
}

/// Context for building the body of a single function. Manages scopes, blocks, and state.
struct FunctionContext<'a> {
    builder: &'a mut CfgBuilder,
    func_id: cfg::FunctionId,
    current_hop: Option<cfg::HopId>,
    current_block: Option<cfg::BasicBlockId>,

    // Scoped variable mapping (from AST's VarId to CFG's VariableId)
    // A stack of scopes, where each scope is a map.
    var_scopes: Vec<HashMap<ast::VarId, cfg::VariableId>>,
    param_map: HashMap<ast::ParamId, cfg::VariableId>,

    // Counter for generating unique temporary variables
    temp_counter: u32,
}


impl CfgBuilder {
    /// Creates a new, empty CfgBuilder.
    fn new(ast: ast::Program) -> Self {
        Self {
            ast,
            cfg: cfg::Program::default(),
            type_map: HashMap::new(),
            table_map: HashMap::new(),
            field_map: HashMap::new(),
            const_map: HashMap::new(),
            func_map: HashMap::new(),
            generic_param_map: HashMap::new(),
            resolved_type_cache: HashMap::new(),
            builtin_types: BuiltinTypes::default(),
        }
    }

    /// Performs the full, multi-pass conversion from AST to CFG.
    fn build(&mut self) -> cfg::Program {
        // 1. Pre-populate built-in types from the prelude.
        self.populate_builtin_types();

        // 2. Process top-level declarations in an order that respects dependencies.
        self.build_type_declarations();
        self.build_table_declarations(); // This must happen before consts/funcs that might use table types
        self.build_const_declarations();

        // 3. Create function shells (signatures only) first to handle recursion and forward calls.
        self.build_function_shells();
        
        // 4. Build synthesized functions (e.g., from table invariants and function assumptions).
        self.build_synthesized_functions();

        // 5. Build the bodies of all user-defined functions and transactions.
        self.build_function_bodies();
        
        // Return the completed CFG program.
        std::mem::take(&mut self.cfg)
    }

    // --- Pass 1: Declaration Processing ---

    /// Populates the CFG with built-in primitive types and registers their IDs.
    fn populate_builtin_types(&mut self) {
        let primitives = [
            ("int", cfg::Type::Primitive(cfg::PrimitiveType::Int)),
            ("float", cfg::Type::Primitive(cfg::PrimitiveType::Float)),
            ("bool", cfg::Type::Primitive(cfg::PrimitiveType::Bool)),
            ("string", cfg::Type::Primitive(cfg::PrimitiveType::String)),
            ("void", cfg::Type::Void),
        ];

        for (name, ty) in primitives {
            let id = self.cfg.types.alloc(ty);
            match name {
                "int" => self.builtin_types.int = id,
                "float" => self.builtin_types.float = id,
                "bool" => self.builtin_types.bool = id,
                "string" => self.builtin_types.string = id,
                "void" => self.builtin_types.void = id,
                _ => {}
            }
        }
        
        // Register generic built-in types like `List`, `Row`, etc.
        let generic_builtins = ["List", "Row", "Table", "UUID"];
        for name in generic_builtins {
            let udt = cfg::UserDefinedType {
                name: name.to_string(),
                generic_params: Vec::new(), // These are handled specially during type conversion
                decorators: Vec::new(),
            };
            let udt_id = self.cfg.user_defined_types.alloc(udt);
            self.cfg.types_map.insert(name.to_string(), udt_id);
            self.builtin_types.generic_builtins.insert(name.to_string(), udt_id);
        }
    }

    /// Builds all user-defined type declarations, filtering out built-ins.
    fn build_type_declarations(&mut self) {
        for (id, ast_type_decl) in self.ast.type_decls.iter() {
            if self.is_builtin_decorator(&ast_type_decl.decorators) { continue; }

            let udt = cfg::UserDefinedType {
                name: ast_type_decl.name.name.clone(),
                generic_params: ast_type_decl.generic_params.iter().map(|p_id| self.convert_generic_param(*p_id)).collect(),
                decorators: self.convert_decorators(&ast_type_decl.decorators),
            };
            let udt_id = self.cfg.user_defined_types.alloc(udt);
            self.cfg.types_map.insert(ast_type_decl.name.name.clone(), udt_id);
            self.type_map.insert(id, udt_id);
        }
    }

    /// Builds all table declarations and their associated fields.
    fn build_table_declarations(&mut self) {
        for (id, ast_table) in self.ast.table_decls.iter() {
            let cfg_table = cfg::Table {
                name: ast_table.name.name.clone(),
                primary_key_fields: Vec::new(), // populated below
                other_fields: Vec::new(),       // populated below
                node_partition: Id::from(u32::MAX), // populated below
                node_partition_args: Vec::new(),    // populated below
                invariants: Vec::new(),             // populated in synthesize pass
            };
            let table_id = self.cfg.tables.alloc(cfg_table);
            self.cfg.tables_map.insert(ast_table.name.name.clone(), table_id);
            self.table_map.insert(id, table_id);

            // Now iterate through elements to create fields
            for element in &ast_table.elements {
                if let ast::TableElement::Field(field_id) = element {
                    let ast_field = &self.ast.fields[*field_id];
                    let cfg_field = cfg::TableField {
                        name: ast_field.name.name.clone(),
                        field_type: self.convert_ast_type(ast_field.ty),
                        table_id,
                    };
                    let new_field_id = self.cfg.table_fields.alloc(cfg_field);
                    self.field_map.insert(*field_id, new_field_id);

                    if ast_field.is_primary {
                        self.cfg.tables[table_id].primary_key_fields.push(new_field_id);
                    } else {
                        self.cfg.tables[table_id].other_fields.push(new_field_id);
                    }
                }
            }
            
            // Now resolve node partition (must be done after fields are mapped)
            for element in &ast_table.elements {
                if let ast::TableElement::Node(node) = element {
                    // The AST has resolved_partitions to a FunctionId
                    let ast_part_func_id = node.resolved_partitions.first().expect("Node partition must be resolved");
                    self.cfg.tables[table_id].node_partition = self.func_map[ast_part_func_id];
                    
                    // Args must be fields
                    for arg_expr_id in &node.args {
                        if let ast::Expression::Identifier { resolved_declarations, .. } = &self.ast.expressions[*arg_expr_id] {
                            if let Some(ast::IdentifierResolution::Field(field_id)) = resolved_declarations.first() {
                                self.cfg.tables[table_id].node_partition_args.push(self.field_map[field_id]);
                            } else {
                                panic!("Node partition argument is not a field");
                            }
                        } else {
                            panic!("Node partition argument must be a simple field identifier");
                        }
                    }
                }
            }
            self.cfg.all_tables.push(table_id);
        }
    }

    /// Builds all global constant declarations.
    fn build_const_declarations(&mut self) {
        for (id, ast_const) in self.ast.const_decls.iter() {
            let ty = self.convert_resolved_type(ast_const.resolved_type.as_ref().unwrap());
            let init = self.evaluate_const_expr(ast_const.value);

            let global_const = cfg::GlobalConst {
                name: ast_const.name.name.clone(),
                ty,
                init,
            };
            let const_id = self.cfg.global_consts.alloc(global_const);
            self.cfg.global_consts_map.insert(ast_const.name.name.clone(), const_id);
            self.const_map.insert(id, const_id);
        }
    }

    /// Creates function shells (signatures without bodies), filtering out built-in operators.
    fn build_function_shells(&mut self) {
        for (id, ast_func) in self.ast.functions.iter() {
            if self.is_builtin_op(&ast_func.decorators) { continue; }

            let signature = self.convert_type_scheme(ast_func.resolved_function_type.as_ref().unwrap());
            
            let cfg_func = cfg::Function {
                name: ast_func.name.name.clone(),
                signature,
                kind: self.convert_callable_kind(ast_func.kind),
                params: Vec::new(), // populated in body-building pass
                assumptions: Vec::new(), // populated in synthesize pass
                entry_block: None,
                all_blocks: Vec::new(),
                decorators: self.convert_decorators(&ast_func.decorators),
                entry_hop: None,
                hops: Vec::new(),
            };

            let func_id = self.cfg.functions.alloc(cfg_func);
            self.func_map.insert(id, func_id);
            
            match ast_func.kind {
                ast::CallableKind::Transaction => self.cfg.all_transactions.push(func_id),
                _ => self.cfg.all_functions.push(func_id),
            }
        }
    }

    // --- Pass 2: Synthesize Functions ---
    
    /// Builds bodies for synthesized functions like invariants and assumptions.
    fn build_synthesized_functions(&mut self) {
        // This is a complex task. For simplicity in this example, we will just note
        // where it would happen. A full implementation would create new `cfg::Function`
        // entries of kind `Invariant` or `Assumption` and then build their bodies
        // based on the corresponding AST expressions.
        
        // Example for table invariants:
        for (ast_table_id, cfg_table_id) in &self.table_map {
            let ast_table = &self.ast.table_decls[*ast_table_id];
            for element in &ast_table.elements {
                if let ast::TableElement::Invariant(expr_id) = element {
                    // 1. Create a new cfg::Function of kind Invariant.
                    // 2. Its signature takes a Row<ThisTable> and returns bool.
                    // 3. Add its ID to self.cfg.tables[cfg_table_id].invariants.
                    // 4. Create a FunctionContext and build the body from *expr_id.
                }
            }
        }
    }

    // --- Pass 3: Build Function Bodies ---

    /// Iterates through all non-builtin AST functions and builds their CFG bodies.
    fn build_function_bodies(&mut self) {
        let func_ids: Vec<ast::FunctionId> = self.ast.functions.iter().map(|(id, _)| id).collect();

        for ast_func_id in func_ids {
            let ast_func = &self.ast.functions[ast_func_id];
            if self.is_builtin_op(&ast_func.decorators) || ast_func.body.is_none() {
                continue;
            }

            let cfg_func_id = self.func_map[&ast_func_id];
            let mut context = FunctionContext::new(self, cfg_func_id);
            
            // Create and map parameters to CFG variables
            let ast_func_clone = ast_func.clone(); // Clone to avoid borrow issues
            let cfg_func_params = &mut self.cfg.functions[cfg_func_id].params;
            for ast_param_id in &ast_func_clone.params {
                let ast_param = &self.ast.params[*ast_param_id];
                let var = cfg::Variable {
                    name: ast_param.name.name.clone(),
                    ty: context.builder.convert_resolved_type(ast_param.resolved_type.as_ref().unwrap()),
                    kind: cfg::VariableKind::Parameter,
                };
                let var_id = context.builder.cfg.variables.alloc(var);
                cfg_func_params.push(var_id);
                context.param_map.insert(*ast_param_id, var_id);
            }

            // Delegate to the context to build the body
            let body_block_id = ast_func_clone.body.unwrap();
            if ast_func_clone.kind == ast::CallableKind::Transaction {
                context.build_transaction_body(body_block_id);
            } else {
                context.build_function_body(body_block_id);
            }
        }
    }
    
    // --- Type Conversion ---

    fn convert_type_scheme(&mut self, scheme: &ast::TypeScheme) -> cfg::TypeScheme {
        cfg::TypeScheme {
            quantified_params: scheme.quantified_params.iter().map(|p| self.convert_generic_param(*p)).collect(),
            ty: self.convert_resolved_type(&scheme.ty),
        }
    }

    fn convert_ast_type(&mut self, ast_type_id: ast::AstTypeId) -> cfg::TypeId {
        // This is a simplified conversion for types that are not yet fully resolved
        // into ResolvedType. This is common for field/const declarations.
        // A full implementation would need a more robust way to handle this, but
        // for now we assume we can get a ResolvedType.
        let ty = self.ast.types[ast_type_id].clone();
        match ty {
            ast::AstType::Named { name, .. } => {
                if let Some(id) = self.builtin_types.generic_builtins.get(&name.name) {
                    self.cfg.types.alloc(cfg::Type::Declared { type_id: *id, args: vec![]})
                } else {
                    match name.name.as_str() {
                         "int" => self.builtin_types.int,
                         "float" => self.builtin_types.float,
                         "bool" => self.builtin_types.bool,
                         "string" => self.builtin_types.string,
                         _ => cfg_unimplemented!("Unresolved named type '{}' during early conversion", name.name)
                    }
                }
            }
            _ => cfg_unimplemented!("Unsupported AstType shape for early conversion")
        }
    }
    
    fn convert_resolved_type(&mut self, ty: &ast::ResolvedType) -> cfg::TypeId {
        if let Some(id) = self.resolved_type_cache.get(ty) {
            return *id;
        }

        let new_ty = match ty {
            ast::ResolvedType::Declared { decl_id, args } => {
                let ast_decl = &self.ast.type_decls[*decl_id];
                let args_converted = args.iter().map(|arg| self.convert_resolved_type(arg)).collect();
                
                if self.is_builtin_decorator(&ast_decl.decorators) {
                    match ast_decl.name.name.as_str() {
                        "int" => return self.builtin_types.int,
                        "float" => return self.builtin_types.float,
                        "bool" => return self.builtin_types.bool,
                        "string" => return self.builtin_types.string,
                        "void" => return self.builtin_types.void,
                        // Handle generic builtins like `List<T>`
                        name => {
                            let udt_id = self.builtin_types.generic_builtins.get(name).expect("Unknown generic builtin");
                            cfg::Type::Declared { type_id: *udt_id, args: args_converted }
                        }
                    }
                } else {
                     cfg::Type::Declared { type_id: self.type_map[decl_id], args: args_converted }
                }
            },
            ast::ResolvedType::Table { table_id } => cfg::Type::Table(self.table_map[table_id]),
            ast::ResolvedType::GenericParam(id) => cfg::Type::GenericParam(self.convert_generic_param(*id)),
            ast::ResolvedType::Function { param_types, return_type } => cfg::Type::Function {
                param_types: param_types.iter().map(|p| self.convert_resolved_type(p)).collect(),
                return_type: Box::new(self.convert_resolved_type(return_type)),
            },
            ast::ResolvedType::InferVar(_) => panic!("Inference variable found during CFG build."),
        };

        let id = self.cfg.types.alloc(new_ty);
        self.resolved_type_cache.insert(ty.clone(), id);
        id
    }
    
    fn convert_generic_param(&mut self, ast_id: ast::GenericParamId) -> cfg::GenericParamId {
        if let Some(id) = self.generic_param_map.get(&ast_id) {
            return *id;
        }
        let ast_param = &self.ast.generic_params[ast_id];
        let id = self.cfg.generic_params.alloc(cfg::GenericParam { name: ast_param.name.name.clone() });
        self.generic_param_map.insert(ast_id, id);
        id
    }

    // --- Helpers ---

    fn convert_decorators(&self, decorators: &[ast::Decorator]) -> Vec<cfg::Decorator> {
        decorators.iter()
            .filter(|d| d.name.name != "builtin" && d.name.name != "builtinop")
            .map(|d| cfg::Decorator { name: d.name.name.clone() })
            .collect()
    }
    
    fn is_builtin_decorator(&self, decorators: &[ast::Decorator]) -> bool {
        decorators.iter().any(|d| d.name.name == "builtin")
    }
    
    fn is_builtin_op(&self, decorators: &[ast::Decorator]) -> bool {
        decorators.iter().any(|d| d.name.name == "builtinop")
    }

    fn convert_callable_kind(&self, kind: ast::CallableKind) -> cfg::FunctionKind {
        match kind {
            ast::CallableKind::Function => cfg::FunctionKind::Function,
            ast::CallableKind::Transaction => cfg::FunctionKind::Transaction,
            ast::CallableKind::Partition => cfg::FunctionKind::Partition,
            ast::CallableKind::Operator => panic!("Operators should be filtered out"),
        }
    }

    /// Evaluates a constant AST expression into a CFG constant value.
    fn evaluate_const_expr(&self, expr_id: ast::ExprId) -> cfg::ConstantValue {
        match &self.ast.expressions[expr_id] {
            ast::Expression::Literal { value, .. } => match value {
                ast::Literal::Integer(s) => cfg::ConstantValue::Int(s.parse().unwrap()),
                ast::Literal::Float(s) => cfg::ConstantValue::Float(OrderedFloat(s.parse().unwrap())),
                ast::Literal::String(s) => cfg::ConstantValue::String(s.clone()),
                ast::Literal::Bool(b) => cfg::ConstantValue::Bool(*b),
                _ => panic!("Unsupported literal type for constant evaluation."),
            },
            _ => panic!("Constant expression must be a literal."),
        }
    }
}

// ============================================================================
// --- FunctionContext: The workhorse for building function bodies
// ============================================================================

impl<'a> FunctionContext<'a> {
    fn new(builder: &'a mut CfgBuilder, func_id: cfg::FunctionId) -> Self {
        Self {
            builder,
            func_id,
            current_hop: None,
            current_block: None,
            var_scopes: vec![HashMap::new()], // Start with one global scope for the function
            param_map: HashMap::new(),
            temp_counter: 0,
        }
    }

    /// Build the body for a standard (non-transaction) function.
    fn build_function_body(&mut self, body_id: ast::BlockId) {
        let hop_id = self.new_hop(Vec::new());
        self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);
        
        let entry_block = self.new_basic_block(hop_id);
        self.builder.cfg.functions[self.func_id].entry_block = Some(entry_block);
        self.builder.cfg.hops[hop_id].entry_block = entry_block;
        
        self.current_block = Some(entry_block);
        self.build_block(body_id);
        
        // Ensure the last block is terminated if it hasn't been already (e.g., by a return)
        if let Some(block_id) = self.current_block {
            let func_return_type = self.builder.cfg.functions[self.func_id].signature.ty;
            let is_void = matches!(self.builder.cfg.types[func_return_type], cfg::Type::Void);
            if is_void {
                self.terminate(block_id, cfg::Terminator::Return(None));
            } else {
                 // A non-void function falling off the end is an error, but here we just abort.
                self.terminate(block_id, cfg::Terminator::Abort);
            }
        }
    }
    
    /// Build the body for a transaction, processing `hop` and `hopsfor` statements.
    fn build_transaction_body(&mut self, body_id: ast::BlockId) {
        let body_stmts = self.builder.ast.blocks[body_id].statements.clone();

        for stmt_id in body_stmts {
            let stmt = &self.builder.ast.statements[*stmt_id].clone();
            match stmt {
                ast::Statement::Hop { body, decorators, .. } => {
                    let hop_id = self.new_hop(self.builder.convert_decorators(decorators));
                    if self.builder.cfg.functions[self.func_id].entry_hop.is_none() {
                        self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);
                    }
                    
                    let entry_block = self.new_basic_block(hop_id);
                    self.builder.cfg.hops[hop_id].entry_block = entry_block;

                    if let Some(prev_block_id) = self.current_block.take() {
                        self.terminate(prev_block_id, cfg::Terminator::HopExit { next_block: entry_block });
                    }
                    
                    self.current_block = Some(entry_block);
                    self.build_block(*body);
                },
                ast::Statement::HopsFor { var, start, end, body, decorators, .. } => {
                    let start_val = self.builder.evaluate_const_expr(*start).as_int().expect("hopsfor start must be const int");
                    let end_val = self.builder.evaluate_const_expr(*end).as_int().expect("hopsfor end must be const int");

                    for i in start_val..end_val {
                        let hop_id = self.new_hop(self.builder.convert_decorators(decorators));
                        if self.builder.cfg.functions[self.func_id].entry_hop.is_none() {
                            self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);
                        }

                        let entry_block = self.new_basic_block(hop_id);
                        self.builder.cfg.hops[hop_id].entry_block = entry_block;

                        if let Some(prev_block_id) = self.current_block.take() {
                            self.terminate(prev_block_id, cfg::Terminator::HopExit { next_block: entry_block });
                        }
                        
                        self.current_block = Some(entry_block);

                        self.push_scope();
                        
                        let ast_var_decl = &self.builder.ast.var_decls[*var];
                        let var_ty = self.builder.convert_resolved_type(ast_var_decl.resolved_type.as_ref().unwrap());
                        let loop_var_id = self.new_variable(ast_var_decl.name.name.clone(), var_ty, cfg::VariableKind::Local);
                        self.add_var_to_scope(*var, loop_var_id);

                        self.add_instruction(cfg::Instruction::Assign {
                            dest: loop_var_id,
                            src: cfg::Operand::Constant(cfg::ConstantValue::Int(i)),
                        });

                        self.build_block(*body);
                        self.pop_scope();
                    }
                },
                _ => panic!("All top-level statements in a transaction must be `hop` or `hopsfor`."),
            }
        }
    }
    
    /// Recursively build statements within a block.
    fn build_block(&mut self, block_id: ast::BlockId) {
        self.push_scope();
        let block_stmts = self.builder.ast.blocks[block_id].statements.clone();
        for stmt_id in block_stmts {
            if self.current_block.is_some() {
                self.build_statement(*stmt_id);
            }
        }
        self.pop_scope();
    }
    
    fn build_statement(&mut self, stmt_id: ast::StmtId) {
        let stmt = self.builder.ast.statements[stmt_id].clone();
        match stmt {
            ast::Statement::VarDecl(var_id) => {
                let decl = &self.builder.ast.var_decls[var_id];
                let ty = self.builder.convert_resolved_type(decl.resolved_type.as_ref().unwrap());
                let new_var_id = self.new_variable(decl.name.name.clone(), ty, cfg::VariableKind::Local);
                self.add_var_to_scope(var_id, new_var_id);

                if let Some(init_expr_id) = decl.init {
                    let src_operand = self.build_expression(init_expr_id);
                    self.add_instruction(cfg::Instruction::Assign { dest: new_var_id, src: src_operand });
                }
            },
            ast::Statement::If { condition, then_block, else_block, .. } => {
                let start_block = self.current_block.take().unwrap();
                let cond_operand = self.build_expression(condition);

                let then_bb = self.new_basic_block(self.current_hop.unwrap());
                let merge_bb = self.new_basic_block(self.current_hop.unwrap());

                if let Some(else_id) = else_block {
                    let else_bb = self.new_basic_block(self.current_hop.unwrap());
                    self.terminate(start_block, cfg::Terminator::Branch { condition: cond_operand, if_true: then_bb, if_false: else_bb });

                    self.current_block = Some(then_bb);
                    self.build_block(then_block);
                    if self.current_block.is_some() { self.terminate(self.current_block.unwrap(), cfg::Terminator::Jump(merge_bb)); }

                    self.current_block = Some(else_bb);
                    self.build_block(else_id);
                    if self.current_block.is_some() { self.terminate(self.current_block.unwrap(), cfg::Terminator::Jump(merge_bb)); }
                } else {
                    self.terminate(start_block, cfg::Terminator::Branch { condition: cond_operand, if_true: then_bb, if_false: merge_bb });

                    self.current_block = Some(then_bb);
                    self.build_block(then_block);
                     if self.current_block.is_some() { self.terminate(self.current_block.unwrap(), cfg::Terminator::Jump(merge_bb)); }
                }

                self.current_block = Some(merge_bb);
            },
            ast::Statement::Return { value, .. } => {
                let operand = value.map(|expr_id| self.build_expression(expr_id));
                self.terminate(self.current_block.take().unwrap(), cfg::Terminator::Return(operand));
            },
            ast::Statement::Assert { expr, .. } => {
                let cond = self.build_expression(expr);
                self.add_instruction(cfg::Instruction::Assert { condition: cond, message: "Assertion failed".to_string() });
            },
            ast::Statement::Expression { expr, .. } => {
                self.build_expression(expr); // Build for side effects, discard result
            },
            ast::Statement::Block(block_id) => {
                self.build_block(block_id);
            }
            // `For` and `HopsFor` are more complex and omitted for brevity, but would follow a similar pattern of creating blocks and jumps.
            _ => cfg_unimplemented!("Statement type: {:?}", stmt)
        }
    }

    /// Build an AST expression, generating instructions and returning the final operand.
    fn build_expression(&mut self, expr_id: ast::ExprId) -> cfg::Operand {
        let expr = self.builder.ast.expressions[expr_id].clone();
        match expr {
            ast::Expression::Literal { value, .. } => {
                cfg::Operand::Constant(self.builder.evaluate_const_expr(expr_id))
            },
            ast::Expression::Identifier { resolved_declarations, .. } => {
                let res = resolved_declarations.first().expect("Identifier not resolved");
                match res {
                    ast::IdentifierResolution::Var(id) => cfg::Operand::Variable(self.find_var(*id)),
                    ast::IdentifierResolution::Param(id) => cfg::Operand::Variable(self.param_map[id]),
                    ast::IdentifierResolution::Const(id) => cfg::Operand::Global(self.builder.const_map[id]),
                    _ => cfg_unimplemented!("Identifier resolution type: {:?}", res),
                }
            },
            ast::Expression::Binary { left, op, right, resolved_callables, resolved_type, ..} => {
                // This is where we handle builtin operators
                let left_op = self.build_expression(left);
                let right_op = self.build_expression(right);
                
                let bin_op = match op.name.as_str() {
                    "+" => cfg::BinaryOp::AddInt, // Should check type
                    "-" => cfg::BinaryOp::SubInt,
                    "*" => cfg::BinaryOp::MulInt,
                    "/" => cfg::BinaryOp::DivInt,
                    "==" => cfg::BinaryOp::Eq,
                    "!=" => cfg::BinaryOp::Neq,
                    "<" => cfg::BinaryOp::LtInt,
                    "<=" => cfg::BinaryOp::LeqInt,
                    ">" => cfg::BinaryOp::GtInt,
                    ">=" => cfg::BinaryOp::GeqInt,
                    "&&" => cfg::BinaryOp::And,
                    "||" => cfg::BinaryOp::Or,
                    _ => cfg_unimplemented!("Binary operator {}", op.name)
                };

                let dest_ty = self.builder.convert_resolved_type(resolved_type.as_ref().unwrap());
                let dest_var = self.new_temporary(dest_ty);
                self.add_instruction(cfg::Instruction::BinaryOp { dest: dest_var, op: bin_op, left: left_op, right: right_op });
                cfg::Operand::Variable(dest_var)
            },
            ast::Expression::Call { callee, args, resolved_callables, resolved_type, ..} => {
                let func_id = self.builder.func_map[&resolved_callables[0]];
                let args_ops = args.iter().map(|arg_id| self.build_expression(*arg_id)).collect();
                
                let return_type = self.builder.convert_resolved_type(resolved_type.as_ref().unwrap());
                let is_void = matches!(self.builder.cfg.types[return_type], cfg::Type::Void);

                if is_void {
                    self.add_instruction(cfg::Instruction::Call { dest: None, func: func_id, args: args_ops });
                    cfg::Operand::Constant(cfg::ConstantValue::Bool(true)) // Placeholder, won't be used
                } else {
                    let dest_var = self.new_temporary(return_type);
                    self.add_instruction(cfg::Instruction::Call { dest: Some(dest_var), func: func_id, args: args_ops });
                    cfg::Operand::Variable(dest_var)
                }
            },
            ast::Expression::Assignment { lhs, rhs, .. } => {
                let src_op = self.build_expression(rhs);
                if let ast::Expression::Identifier { resolved_declarations, .. } = &self.builder.ast.expressions[lhs] {
                    if let ast::IdentifierResolution::Var(var_id) = resolved_declarations[0] {
                        let dest_var = self.find_var(var_id);
                        self.add_instruction(cfg::Instruction::Assign { dest: dest_var, src: src_op.clone() });
                        return src_op; // Assignment expression evaluates to the assigned value
                    }
                }
                panic!("LHS of assignment is not a variable");
            },
            _ => cfg_unimplemented!("Expression type: {:?}", expr)
        }
    }
    
    // --- CFG Structure Helpers ---
    
    fn new_hop(&mut self, decorators: Vec<cfg::Decorator>) -> cfg::HopId {
        let hop = cfg::Hop { function_id: self.func_id, entry_block: Id::from(u32::MAX), blocks: Vec::new(), decorators };
        let hop_id = self.builder.cfg.hops.alloc(hop);
        self.builder.cfg.functions[self.func_id].hops.push(hop_id);
        self.current_hop = Some(hop_id);
        hop_id
    }

    fn new_basic_block(&mut self, hop_id: cfg::HopId) -> cfg::BasicBlockId {
        let bb = cfg::BasicBlock { hop_id, function_id: self.func_id, predecessors: Vec::new(), instructions: Vec::new(), terminator: cfg::Terminator::Abort };
        let bb_id = self.builder.cfg.basic_blocks.alloc(bb);
        self.builder.cfg.functions[self.func_id].all_blocks.push(bb_id);
        self.builder.cfg.hops[hop_id].blocks.push(bb_id);
        bb_id
    }
    
    fn add_instruction(&mut self, instr: cfg::Instruction) {
        if let Some(block_id) = self.current_block {
            self.builder.cfg.basic_blocks[block_id].instructions.push(instr);
        }
    }

    fn terminate(&mut self, block_id: cfg::BasicBlockId, term: cfg::Terminator) {
        if let cfg::Terminator::Branch { if_true, if_false, .. } = &term {
            self.builder.cfg.basic_blocks[*if_true].predecessors.push(block_id);
            self.builder.cfg.basic_blocks[*if_false].predecessors.push(block_id);
        } else if let cfg::Terminator::Jump(target) = &term {
             self.builder.cfg.basic_blocks[*target].predecessors.push(block_id);
        }
        self.builder.cfg.basic_blocks[block_id].terminator = term;
    }

    // --- Variable and Scope Management ---
    
    fn new_variable(&mut self, name: String, ty: cfg::TypeId, kind: cfg::VariableKind) -> cfg::VariableId {
        self.builder.cfg.variables.alloc(cfg::Variable { name, ty, kind })
    }
    
    fn new_temporary(&mut self, ty: cfg::TypeId) -> cfg::VariableId {
        let name = format!("$tmp{}", self.temp_counter);
        self.temp_counter += 1;
        self.new_variable(name, ty, cfg::VariableKind::Temporary)
    }

    fn push_scope(&mut self) { self.var_scopes.push(HashMap::new()); }
    fn pop_scope(&mut self) { self.var_scopes.pop(); }
    
    fn add_var_to_scope(&mut self, ast_id: ast::VarId, cfg_id: cfg::VariableId) {
        self.var_scopes.last_mut().unwrap().insert(ast_id, cfg_id);
    }
    
    fn find_var(&self, ast_id: ast::VarId) -> cfg::VariableId {
        for scope in self.var_scopes.iter().rev() {
            if let Some(id) = scope.get(&ast_id) {
                return *id;
            }
        }
        panic!("Variable with AST id {:?} not found in any scope.", ast_id);
    }
}