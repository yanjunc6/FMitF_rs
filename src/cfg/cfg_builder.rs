//! cfg/cfg_builder.rs
//!
//! This module is responsible for converting the semantic-analyzed Abstract Syntax Tree (AST)
//! into a Control Flow Graph (CFG) Intermediate Representation.
//! It handles the translation of all language constructs, including the special scoping
//! and unrolling rules for transactional hops.

use crate::ast::{self, CallableDecl};
use crate::cfg;
use crate::util::Span;
use ordered_float::OrderedFloat;
use std::collections::{HashMap, HashSet};

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
    var_map: HashMap<ast::VarId, cfg::VariableId>,

    // Cache for type conversion to avoid duplicating type nodes
    resolved_type_cache: HashMap<ast::ResolvedType, cfg::TypeId>,

    // Global counter for generating unique lambda names
    lambda_counter: u32,

    // Global counter for generating unique temporary variables across all contexts
    temp_counter: u32,
}

/// Context for building the body of a single function. Manages scopes, blocks, and state.
enum SpecialExprContext {
    TableInvariant {
        table_id: cfg::TableId,
        key_params: Vec<(String, ast::ResolvedType)>,
    },
}

struct FunctionContext<'a> {
    builder: &'a mut CfgBuilder,
    func_id: cfg::FunctionId,
    current_hop: Option<cfg::HopId>,
    current_block: Option<cfg::BasicBlockId>,

    // Mapping for parameters, local to the function body build process.
    param_map: HashMap<ast::ParamId, cfg::VariableId>,

    // Tracking Row<T> variables and their field decompositions
    // Maps AST variable ID to a map of field names to CFG variable IDs
    row_field_map: HashMap<ast::VarId, HashMap<String, cfg::VariableId>>,

    // Tracking Row<T> parameters and their field decompositions
    // Maps AST parameter ID to a map of field names to CFG variable IDs
    param_row_field_map: HashMap<ast::ParamId, HashMap<String, cfg::VariableId>>,

    // Additional context for evaluating table invariants
    table_invariant_context: Option<TableInvariantContext>,
}

#[derive(Clone)]
struct TableInvariantContext {
    table_id: cfg::TableId,
    key_vars: Vec<cfg::VariableId>,
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
            var_map: HashMap::new(),
            resolved_type_cache: HashMap::new(),
            lambda_counter: 0,
            temp_counter: 0,
        }
    }

    /// Performs the full, multi-pass conversion from AST to CFG.
    fn build(&mut self) -> cfg::Program {
        // Pass 1: Build all declarations to populate maps.
        // Order is important to resolve dependencies.
        self.build_type_declarations();
        self.build_table_declarations();
        self.build_const_declarations();
        self.build_function_shells();

        // Pass 2: Resolve inter-declaration dependencies that couldn't be handled in pass 1.
        self.resolve_table_partitions();

        // Pass 3: Build bodies of functions, which can now refer to any declaration.
        self.build_function_bodies();

        // Return the completed CFG program.
        std::mem::take(&mut self.cfg)
    }

    // --- Pass 1: Declaration Processing ---

    /// Builds all user-defined type declarations.
    /// Built-in types are handled on-the-fly during type conversion.
    fn build_type_declarations(&mut self) {
        let type_decls: Vec<_> = self
            .ast
            .type_decls
            .iter()
            .map(|(id, decl)| (id, decl.clone()))
            .collect();
        for (id, ast_type_decl) in type_decls {
            // Built-in types are not added to the user-defined types arena.
            // They are identified by their `@builtin` decorator.
            if self.is_builtin_decorator(&ast_type_decl.decorators) {
                continue;
            }

            let generic_param_ids: Vec<_> = ast_type_decl.generic_params.clone();
            let cfg_generic_params: Vec<_> = generic_param_ids
                .iter()
                .map(|p_id| self.convert_generic_param(*p_id))
                .collect();

            let udt = cfg::UserDefinedType {
                name: ast_type_decl.name.name.clone(),
                generic_params: cfg_generic_params,
                decorators: self.convert_decorators(&ast_type_decl.decorators),
            };
            let udt_id = self.cfg.user_defined_types.alloc(udt);
            self.cfg
                .types_map
                .insert(ast_type_decl.name.name.clone(), udt_id);
            self.type_map.insert(id, udt_id);
        }
    }

    /// Builds all table declarations and their associated fields.
    fn build_table_declarations(&mut self) {
        let table_decls: Vec<_> = self
            .ast
            .table_decls
            .iter()
            .map(|(id, tbl)| (id, tbl.clone()))
            .collect();
        for (id, ast_table) in table_decls {
            let elements = ast_table.elements.clone();

            // Collect field declarations first so we can materialize the table structure
            let mut field_decls: Vec<(ast::FieldId, ast::TableField)> = Vec::new();
            for element in &elements {
                if let ast::TableElement::Field(field_id) = element {
                    let ast_field = self.ast.fields[*field_id].clone();
                    field_decls.push((*field_id, ast_field));
                }
            }

            // Create the CFG table shell with empty invariants; fields will be populated below
            let cfg_table = cfg::Table {
                name: ast_table.name.name.clone(),
                schedule_key_fields: Vec::new(),
                primary_key_fields: Vec::new(),
                other_fields: Vec::new(),
                node_partition: None,
                node_partition_args: Vec::new(),
                invariants: Vec::new(),
            };
            let table_id = self.cfg.tables.alloc(cfg_table);
            self.cfg
                .tables_map
                .insert(ast_table.name.name.clone(), table_id);
            self.table_map.insert(id, table_id);

            // Materialize table fields into CFG and populate primary/other lists
            for (field_id, ast_field) in &field_decls {
                let field_type = self
                    .convert_resolved_type(ast_field.resolved_type.as_ref().unwrap())
                    .unwrap();

                let cfg_field = cfg::TableField {
                    name: ast_field.name.name.clone(),
                    field_type,
                    table_id,
                };
                let new_field_id = self.cfg.table_fields.alloc(cfg_field);
                self.field_map.insert(*field_id, new_field_id);

                if ast_field.is_primary {
                    self.cfg.tables[table_id]
                        .primary_key_fields
                        .push(new_field_id);
                } else {
                    self.cfg.tables[table_id].other_fields.push(new_field_id);
                }
            }

            // Prepare primary key parameter specifications for invariants
            let primary_key_specs: Vec<(String, ast::ResolvedType)> = field_decls
                .iter()
                .filter(|(_, field)| field.is_primary)
                .map(|(_, field)| {
                    (
                        field.name.name.clone(),
                        field
                            .resolved_type
                            .clone()
                            .expect("Primary key field must have resolved type"),
                    )
                })
                .collect();

            // Build invariant helper functions now that field and key metadata are available
            for element in &elements {
                if let ast::TableElement::Invariant(expr_id) = element {
                    let inv_index = self.cfg.tables[table_id].invariants.len();
                    let context = SpecialExprContext::TableInvariant {
                        table_id,
                        key_params: primary_key_specs.clone(),
                    };
                    let inv_func_id = self.build_special_expr_function(
                        &format!("{}_inv_{}", ast_table.name.name, inv_index),
                        *expr_id,
                        cfg::FunctionKind::Invariant,
                        &[],
                        Some(context),
                    );
                    self.cfg.tables[table_id].invariants.push(inv_func_id);
                }
            }

            self.cfg.all_tables.push(table_id);
        }
    }

    /// Builds all global constant declarations.
    fn build_const_declarations(&mut self) {
        let const_decls: Vec<_> = self
            .ast
            .const_decls
            .iter()
            .map(|(id, c)| (id, c.clone()))
            .collect();
        for (id, ast_const) in const_decls {
            // Handle constants with missing resolved types by trying to infer the type
            let ty = if let Some(resolved_type) = &ast_const.resolved_type {
                self.convert_resolved_type(resolved_type).unwrap()
            } else {
                // Try to infer type from the constant value
                match &self.ast.expressions[ast_const.value] {
                    ast::Expression::Literal { value, .. } => match value {
                        ast::Literal::Integer(_) => {
                            // Create int type
                            self.cfg
                                .types
                                .alloc(cfg::Type::Primitive(cfg::PrimitiveType::Int))
                        }
                        ast::Literal::Float(_) => {
                            // Create float type
                            self.cfg
                                .types
                                .alloc(cfg::Type::Primitive(cfg::PrimitiveType::Float))
                        }
                        ast::Literal::String(_) => {
                            // Create string type
                            self.cfg
                                .types
                                .alloc(cfg::Type::Primitive(cfg::PrimitiveType::String))
                        }
                        ast::Literal::Bool(_) => {
                            // Create bool type
                            self.cfg
                                .types
                                .alloc(cfg::Type::Primitive(cfg::PrimitiveType::Bool))
                        }
                        _ => {
                            panic!("Constant '{}' has unresolved type and unsupported literal - should have been resolved in frontend", ast_const.name.name);
                        }
                    },
                    _ => {
                        panic!("Constant '{}' has unresolved type and non-literal value - should have been resolved in frontend", ast_const.name.name);
                    }
                }
            };

            let init = self.evaluate_const_expr(ast_const.value);

            let global_const = cfg::GlobalConst {
                name: ast_const.name.name.clone(),
                ty,
                init,
            };
            let const_id = self.cfg.global_consts.alloc(global_const);
            self.cfg
                .global_consts_map
                .insert(ast_const.name.name.clone(), const_id);
            self.const_map.insert(id, const_id);
        }
    }

    /// Creates function shells (signatures without bodies).
    fn build_function_shells(&mut self) {
        let functions: Vec<_> = self
            .ast
            .functions
            .iter()
            .map(|(id, f)| (id, f.clone()))
            .collect();
        for (id, ast_func) in functions {
            // Skip built-in operators - they should not be added as functions
            if self.is_builtin_op(&ast_func.decorators) {
                continue;
            }

            let signature =
                self.convert_type_scheme(ast_func.resolved_function_type.as_ref().unwrap());

            // Process parameters for all functions (not just those with bodies)
            let param_data: Vec<_> = ast_func
                .params
                .iter()
                .map(|param_id| {
                    let param = &self.ast.params[*param_id];
                    (
                        param.name.name.clone(),
                        param.resolved_type.clone().unwrap(),
                    )
                })
                .collect();

            let mut params = Vec::new();
            for (name, resolved_type) in param_data {
                let var = cfg::Variable {
                    name,
                    ty: self.convert_resolved_type(&resolved_type).unwrap(),
                    kind: cfg::VariableKind::Parameter,
                };
                let var_id = self.cfg.variables.alloc(var);
                params.push(var_id);
            }

            let mut assumptions = Vec::new();
            for assume_expr_id in &ast_func.assumptions {
                let assume_func_id = self.build_special_expr_function(
                    &format!("{}_assume_{}", ast_func.name.name, assumptions.len()),
                    *assume_expr_id,
                    cfg::FunctionKind::Assumption,
                    &ast_func.params,
                    None,
                );
                assumptions.push(assume_func_id);
            }

            let cfg_func = cfg::Function {
                name: ast_func.name.name.clone(),
                signature,
                kind: self.convert_callable_kind(&ast_func),
                params,
                assumptions,
                entry_block: None,
                all_blocks: Vec::new(),
                decorators: self.convert_decorators(&ast_func.decorators),
                entry_hop: None,
                hops: Vec::new(),
            };

            let func_id = self.cfg.functions.alloc(cfg_func);
            self.func_map.insert(id, func_id);

            if ast_func.kind == ast::CallableKind::Transaction {
                self.cfg.all_transactions.push(func_id);
            } else {
                self.cfg.all_functions.push(func_id);
            }
        }
    }

    // --- Pass 2: Resolution ---

    /// Resolves table partitions after all function shells have been created.
    fn resolve_table_partitions(&mut self) {
        let table_decls: Vec<_> = self.ast.table_decls.iter().collect();
        for (ast_table_id, ast_table) in table_decls {
            let cfg_table_id = self.table_map[&ast_table_id];
            for element in &ast_table.elements {
                if let ast::TableElement::Node(node) = element {
                    let ast_part_func_id = node
                        .resolved_partitions
                        .first()
                        .expect("Node partition must be resolved");
                    let cfg_part_func_id = self.func_map[ast_part_func_id];
                    self.cfg.tables[cfg_table_id].node_partition = Some(cfg_part_func_id);

                    let mut schedule_fields: HashSet<cfg::FieldId> = HashSet::new();

                    for arg_expr_id in &node.args {
                        match &self.ast.expressions[*arg_expr_id] {
                            ast::Expression::Identifier {
                                resolved_declarations,
                                ..
                            } => {
                                if let Some(ast::IdentifierResolution::Field(field_id)) =
                                    resolved_declarations.first()
                                {
                                    self.cfg.tables[cfg_table_id]
                                        .node_partition_args
                                        .push(self.field_map[field_id]);
                                    schedule_fields.insert(self.field_map[field_id]);
                                } else {
                                    // For now, just skip non-field identifiers
                                    // This could be expanded to handle other identifier types
                                }
                            }
                            ast::Expression::Literal { .. } => {
                                // Skip literal expressions in partition arguments
                                // These are handled differently in the partition function itself
                            }
                            _ => {
                                // Skip other complex expressions
                                // The partition logic will need to handle these appropriately
                            }
                        }
                    }

                    self.cfg.tables[cfg_table_id].schedule_key_fields =
                        schedule_fields.into_iter().collect();
                }
            }
        }
    }

    // --- Pass 3: Build Function Bodies ---

    /// Iterates through all AST functions and builds their CFG bodies.
    fn build_function_bodies(&mut self) {
        // Collect function IDs to avoid borrowing issues with `self.ast.functions`
        let func_ids: Vec<ast::FunctionId> = self.ast.functions.iter().map(|(id, _)| id).collect();

        for ast_func_id in func_ids {
            let ast_func = self.ast.functions[ast_func_id].clone();
            if ast_func.body.is_none() {
                continue;
            }

            let cfg_func_id = self.func_map[&ast_func_id];

            // Get the parameters before creating the context
            let cfg_params = self.cfg.functions[cfg_func_id].params.clone();

            let mut context = FunctionContext::new(self, cfg_func_id);

            // Map existing parameters to CFG variables for functions with bodies
            // (Parameters were already created in build_function_shells)
            for (i, ast_param_id) in ast_func.params.iter().enumerate() {
                // Map the AST parameter to the corresponding CFG variable
                if let Some(&cfg_var_id) = cfg_params.get(i) {
                    context.param_map.insert(*ast_param_id, cfg_var_id);
                }
            }

            // Delegate to the context to build the body
            let body_block_id = ast_func.body.unwrap();
            if ast_func.kind == ast::CallableKind::Transaction {
                context.build_transaction_body(body_block_id);
            } else {
                context.build_function_body(body_block_id);
            }
        }
    }

    // --- Type Conversion ---

    fn convert_type_scheme(&mut self, scheme: &ast::TypeScheme) -> cfg::TypeScheme {
        cfg::TypeScheme {
            quantified_params: scheme
                .quantified_params
                .iter()
                .map(|p| self.convert_generic_param(*p))
                .collect(),
            ty: self.convert_resolved_type(&scheme.ty).unwrap(),
        }
    }

    fn convert_resolved_type(&mut self, ty: &ast::ResolvedType) -> Option<cfg::TypeId> {
        if let Some(id) = self.resolved_type_cache.get(ty) {
            return Some(*id);
        }

        let new_ty = match ty {
            ast::ResolvedType::Declared { decl_id, args } => {
                let ast_decl = self.ast.type_decls[*decl_id].clone();
                let args_converted: Vec<_> = args
                    .iter()
                    .map(|arg| self.convert_resolved_type(arg).unwrap())
                    .collect();

                if self.is_builtin_decorator(&ast_decl.decorators) {
                    // Handle built-in types by name
                    match ast_decl.name.name.as_str() {
                        "int" => cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        "float" => cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        "bool" => cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                        "string" => cfg::Type::Primitive(cfg::PrimitiveType::String),
                        "void" => cfg::Type::Void,
                        "List" => {
                            assert_eq!(args_converted.len(), 1);
                            cfg::Type::List(args_converted[0])
                        }
                        "Row" => {
                            assert_eq!(args_converted.len(), 1);
                            let arg_type = &self.cfg.types[args_converted[0]];
                            if let cfg::Type::Table(table_id) = arg_type {
                                cfg::Type::Row {
                                    table_id: *table_id,
                                }
                            } else if let cfg::Type::GenericParam(param_id) = arg_type {
                                // This handles `Row<T>` where T is a generic parameter.
                                // The CFG `Row` type requires a concrete `TableId`, which we don't have.
                                // This indicates a place where the CFG type system could be more expressive
                                // (e.g., with a `GenericRow(GenericParamId)` variant).
                                // For now, to allow building generic function shells like `scan`,
                                // we will represent `Row<T>` as just `T` (the generic parameter itself).
                                cfg::Type::GenericParam(*param_id)
                            } else {
                                panic!("Row generic argument must be a table or generic type");
                            }
                        }
                        "Table" => {
                            assert_eq!(args_converted.len(), 1);
                            let arg_type = &self.cfg.types[args_converted[0]];
                            if let cfg::Type::Table(table_id) = arg_type {
                                // This handles cases like `Table<Row<MyTable>>` which resolves to `Table<MyTable>`
                                // and also the incorrect double-wrapping `Table<Table<MyTable>>`.
                                cfg::Type::Table(*table_id)
                            } else if let cfg::Type::Row { table_id } = arg_type {
                                cfg::Type::Table(*table_id)
                            } else if let cfg::Type::Declared { type_id, .. } = arg_type {
                                let udt = &self.cfg.user_defined_types[*type_id];
                                if let Some(table_id) = self.cfg.tables_map.get(&udt.name) {
                                    cfg::Type::Table(*table_id)
                                } else {
                                    panic!("Table generic argument is a Declared type but no matching table found.");
                                }
                            } else if let cfg::Type::GenericParam(param_id) = arg_type {
                                // This handles the case for generic functions like `scan<T>(t: Table<T>)`
                                // where the argument is a generic parameter. We can't resolve it to a
                                // concrete table, so we keep it as a generic table type.
                                // For the CFG, we might need a new Type variant, but for now,
                                // let's see if we can represent it with what we have.
                                // A proper solution might be `Type::GenericTable(param_id)`.
                                // For now, let's just panic with a better message.
                                // Let's try to create a placeholder or just pass it.
                                // The issue is that `cfg::Type::Table` expects a `TableId`.
                                // We can't create one from a generic param.
                                // This indicates a deeper modeling issue.
                                // However, for the purpose of just getting it to run, let's see.
                                // The `scan` function is what is likely causing this.
                                // Let's just return a placeholder type for now.
                                // The best we can do is probably another generic param type.
                                cfg::Type::GenericParam(*param_id)
                            } else {
                                panic!(
                                    "Table generic argument must be a Row, Table, or Declared type that is a table. Found: {:?}",
                                    arg_type
                                );
                            }
                        }
                        _ => panic!("Unknown builtin type: {}", ast_decl.name.name),
                    }
                } else {
                    // Handle user-defined types
                    cfg::Type::Declared {
                        type_id: self.type_map[decl_id],
                        args: args_converted,
                    }
                }
            }
            ast::ResolvedType::Table { table_id } => cfg::Type::Table(self.table_map[table_id]),
            ast::ResolvedType::GenericParam(id) => {
                cfg::Type::GenericParam(self.convert_generic_param(*id))
            }
            ast::ResolvedType::Function {
                param_types,
                return_type,
            } => cfg::Type::Function {
                param_types: param_types
                    .iter()
                    .map(|p| self.convert_resolved_type(p).unwrap())
                    .collect(),
                return_type: Box::new(self.convert_resolved_type(return_type).unwrap()),
            },
            ast::ResolvedType::InferVar(_) => {
                panic!("Inference variable found during CFG build.")
            }
        };

        let id = self.cfg.types.alloc(new_ty);
        self.resolved_type_cache.insert(ty.clone(), id);
        Some(id)
    }

    fn convert_generic_param(&mut self, ast_id: ast::GenericParamId) -> cfg::GenericParamId {
        if let Some(id) = self.generic_param_map.get(&ast_id) {
            return *id;
        }
        let ast_param = &self.ast.generic_params[ast_id];
        let id = self.cfg.generic_params.alloc(cfg::GenericParam {
            name: ast_param.name.name.clone(),
        });
        self.generic_param_map.insert(ast_id, id);
        id
    }

    // --- Helpers ---

    fn convert_decorators(&self, decorators: &[ast::Decorator]) -> Vec<cfg::Decorator> {
        decorators
            .iter()
            .filter(|d| d.name.name != "builtin" && d.name.name != "builtinop")
            .map(|d| cfg::Decorator {
                name: d.name.name.clone(),
            })
            .collect()
    }

    fn is_builtin_decorator(&self, decorators: &[ast::Decorator]) -> bool {
        decorators.iter().any(|d| d.name.name == "builtin")
    }

    fn is_builtin_op(&self, decorators: &[ast::Decorator]) -> bool {
        decorators.iter().any(|d| d.name.name == "builtinop")
    }

    fn convert_callable_kind(&self, func: &CallableDecl) -> cfg::FunctionKind {
        if self.is_builtin_op(&func.decorators) {
            return cfg::FunctionKind::Operator;
        }
        match func.kind {
            ast::CallableKind::Function => cfg::FunctionKind::Function,
            ast::CallableKind::Transaction => cfg::FunctionKind::Transaction,
            ast::CallableKind::Partition => cfg::FunctionKind::Partition,
            // This case should not be hit due to the check above, but is here for completeness.
            ast::CallableKind::Operator => cfg::FunctionKind::Operator,
        }
    }

    /// Evaluates a constant AST expression into a CFG constant value.
    fn evaluate_const_expr(&self, expr_id: ast::ExprId) -> cfg::ConstantValue {
        match &self.ast.expressions[expr_id] {
            ast::Expression::Literal { value, .. } => match value {
                ast::Literal::Integer(s) => cfg::ConstantValue::Int(s.parse().unwrap()),
                ast::Literal::Float(s) => {
                    cfg::ConstantValue::Float(OrderedFloat(s.parse().unwrap()))
                }
                ast::Literal::String(s) => cfg::ConstantValue::String(s.clone()),
                ast::Literal::Bool(b) => cfg::ConstantValue::Bool(*b),
                ast::Literal::List(_exprs) => {
                    // For now, we don't support list literals in constant evaluation
                    // This would require evaluating each expression and creating a list constant
                    panic!("List literals are not supported in constant evaluation");
                }
                ast::Literal::RowLiteral(_) => {
                    // Row literals are not supported in constant evaluation
                    panic!("Row literals are not supported in constant evaluation");
                }
            },
            ast::Expression::Identifier {
                resolved_declarations,
                ..
            } => {
                // Handle constant identifier references
                if let Some(ast::IdentifierResolution::Const(const_id)) =
                    resolved_declarations.first()
                {
                    let const_decl = &self.ast.const_decls[*const_id];
                    // Recursively evaluate the constant's value
                    self.evaluate_const_expr(const_decl.value)
                } else {
                    panic!("Non-constant identifier in constant expression");
                }
            }
            _ => panic!("Constant expression must be a literal or constant identifier."),
        }
    }

    fn build_special_expr_function(
        &mut self,
        name: &str,
        expr_id: ast::ExprId,
        kind: cfg::FunctionKind,
        param_ids: &[ast::ParamId],
        context: Option<SpecialExprContext>,
    ) -> cfg::FunctionId {
        // 1. Create signature and params
        let expr_resolved_type = self.ast.expressions[expr_id]
            .resolved_type()
            .cloned()
            .expect("Special expression function must have a resolved type");
        let bool_cfg_type = self
            .convert_resolved_type(&expr_resolved_type)
            .expect("Failed to convert special expression return type");

        let mut cfg_params = Vec::new();
        let mut param_types = Vec::new();
        let mut param_map = HashMap::new();
        let mut table_invariant_context = None;

        for ast_param_id in param_ids {
            let ast_param = self.ast.params[*ast_param_id].clone();
            let ty = self
                .convert_resolved_type(&ast_param.resolved_type.as_ref().unwrap())
                .unwrap();
            let var = cfg::Variable {
                name: ast_param.name.name.clone(),
                ty,
                kind: cfg::VariableKind::Parameter,
            };
            let var_id = self.cfg.variables.alloc(var);
            cfg_params.push(var_id);
            param_types.push(ty);
            param_map.insert(*ast_param_id, var_id);
        }

        if let Some(SpecialExprContext::TableInvariant {
            table_id,
            key_params,
        }) = context
        {
            let mut key_vars = Vec::new();
            for (name, key_type) in key_params {
                let ty = self.convert_resolved_type(&key_type).unwrap();
                let var = cfg::Variable {
                    name,
                    ty,
                    kind: cfg::VariableKind::Parameter,
                };
                let var_id = self.cfg.variables.alloc(var);
                cfg_params.push(var_id);
                param_types.push(ty);
                key_vars.push(var_id);
            }
            table_invariant_context = Some(TableInvariantContext { table_id, key_vars });
        }

        let func_type = self.cfg.types.alloc(cfg::Type::Function {
            param_types,
            return_type: Box::new(bool_cfg_type),
        });

        let signature = cfg::TypeScheme {
            quantified_params: vec![],
            ty: func_type,
        };

        // 2. Create function shell
        let func = cfg::Function {
            name: name.to_string(),
            signature,
            kind,
            params: cfg_params,
            assumptions: vec![],
            entry_block: None,
            all_blocks: vec![],
            decorators: vec![],
            entry_hop: None,
            hops: vec![],
        };
        let func_id = self.cfg.functions.alloc(func);

        // 3. Build function body
        let mut context = FunctionContext::new(self, func_id);
        context.param_map = param_map;
        context.table_invariant_context = table_invariant_context;

        let hop_id = context.new_hop(vec![]);
        context.builder.cfg.functions[func_id].entry_hop = Some(hop_id);
        let entry_block = context.new_basic_block(hop_id);
        context.builder.cfg.functions[func_id].entry_block = Some(entry_block);
        context.builder.cfg.hops[hop_id].entry_block = Some(entry_block);
        context.current_block = Some(entry_block);

        let result_operand = context.build_expression(expr_id);
        let current_block = context.current_block.take().unwrap();
        context.terminate(current_block, cfg::Terminator::Return(Some(result_operand)));

        func_id
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
            param_map: HashMap::new(),
            row_field_map: HashMap::new(),
            param_row_field_map: HashMap::new(),
            table_invariant_context: None,
        }
    }

    /// Build the body for a standard (non-transaction) function.
    fn build_function_body(&mut self, body_id: ast::BlockId) {
        let hop_id = self.new_hop(Vec::new());
        self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);

        let entry_block = self.new_basic_block(hop_id);
        self.builder.cfg.functions[self.func_id].entry_block = Some(entry_block);
        self.builder.cfg.hops[hop_id].entry_block = Some(entry_block);

        self.current_block = Some(entry_block);
        self.build_block(body_id);

        // Ensure the last block is terminated if it hasn't been already (e.g., by a return)
        if let Some(block_id) = self.current_block {
            let func_signature_type = self.builder.cfg.functions[self.func_id].signature.ty;
            let func_type = &self.builder.cfg.types[func_signature_type];

            let is_void = if let cfg::Type::Function { return_type, .. } = func_type {
                matches!(self.builder.cfg.types[**return_type], cfg::Type::Void)
            } else {
                false // Non-function types shouldn't happen for function signatures
            };

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
            let stmt = self.builder.ast.statements[stmt_id].clone();
            match stmt {
                ast::Statement::Hop {
                    body, decorators, ..
                } => {
                    let hop_id = self.new_hop(self.builder.convert_decorators(&decorators));
                    if self.builder.cfg.functions[self.func_id].entry_hop.is_none() {
                        self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);
                    }

                    let entry_block = self.new_basic_block(hop_id);
                    self.builder.cfg.hops[hop_id].entry_block = Some(entry_block);

                    if let Some(prev_block_id) = self.current_block.take() {
                        self.terminate(
                            prev_block_id,
                            cfg::Terminator::HopExit { next_hop: hop_id },
                        );
                    }

                    self.current_block = Some(entry_block);
                    self.build_block(body);
                }
                ast::Statement::HopsFor {
                    var,
                    start,
                    end,
                    body,
                    decorators,
                    ..
                } => {
                    let start_val = self
                        .builder
                        .evaluate_const_expr(start)
                        .as_int()
                        .expect("hopsfor start must be const int");
                    let end_val = self
                        .builder
                        .evaluate_const_expr(end)
                        .as_int()
                        .expect("hopsfor end must be const int");

                    for i in start_val..end_val {
                        let hop_id = self.new_hop(self.builder.convert_decorators(&decorators));
                        if self.builder.cfg.functions[self.func_id].entry_hop.is_none() {
                            self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);
                        }

                        let entry_block = self.new_basic_block(hop_id);
                        self.builder.cfg.hops[hop_id].entry_block = Some(entry_block);

                        if let Some(prev_block_id) = self.current_block.take() {
                            self.terminate(
                                prev_block_id,
                                cfg::Terminator::HopExit { next_hop: hop_id },
                            );
                        }

                        self.current_block = Some(entry_block);

                        let ast_var_decl = self.builder.ast.var_decls[var].clone();
                        let var_ty = self
                            .builder
                            .convert_resolved_type(ast_var_decl.resolved_type.as_ref().unwrap())
                            .unwrap();
                        let loop_var_id = self.new_variable(
                            ast_var_decl.name.name.clone(),
                            var_ty,
                            cfg::VariableKind::Local,
                        );
                        self.builder.var_map.insert(var, loop_var_id);

                        // HopsFor loop variable initialization; use a span from the loop bounds
                        let init_span = self.pick_span(&[
                            self.builder.ast.expressions[start].span(),
                            self.builder.ast.expressions[end].span(),
                        ]);
                        self.add_instruction(cfg::Instruction {
                            kind: cfg::InstructionKind::Assign {
                                dest: loop_var_id,
                                src: cfg::Operand::Constant(cfg::ConstantValue::Int(i)),
                            },
                            span: init_span,
                        });

                        self.build_block(body);
                    }
                }
                // Handle regular statements in transactions too (like variable declarations)
                _ => {
                    // For non-hop statements, create a default hop if needed
                    if self.builder.cfg.functions[self.func_id].entry_hop.is_none() {
                        let hop_id = self.new_hop(Vec::new()); // No decorators for default hop
                        self.builder.cfg.functions[self.func_id].entry_hop = Some(hop_id);

                        let entry_block = self.new_basic_block(hop_id);
                        self.builder.cfg.hops[hop_id].entry_block = Some(entry_block);
                        self.current_block = Some(entry_block);
                    }

                    // Process the statement using the regular statement handler
                    self.build_statement(stmt_id);
                }
            }
        }

        // Ensure the last block is terminated if it hasn't been already (e.g., by a return)
        if let Some(block_id) = self.current_block {
            // Transactions should return unless there's an explicit abort
            self.terminate(block_id, cfg::Terminator::Return(None));
        }
    }

    /// Recursively build statements within a block.
    fn build_block(&mut self, block_id: ast::BlockId) {
        let block_stmts = self.builder.ast.blocks[block_id].statements.clone();
        for stmt_id in block_stmts {
            if self.current_block.is_some() {
                self.build_statement(stmt_id);
            }
        }
    }

    fn build_statement(&mut self, stmt_id: ast::StmtId) {
        let stmt = self.builder.ast.statements[stmt_id].clone();
        match stmt {
            ast::Statement::VarDecl(var_id) => {
                let decl = self.builder.ast.var_decls[var_id].clone();
                let resolved_type = decl.resolved_type.as_ref().unwrap();
                let ty = self.builder.convert_resolved_type(resolved_type).unwrap();
                let var_name = decl.name.name.clone();

                // For Row<T> variables, treat them as regular variables initially
                // Decomposition will happen dynamically when member access is encountered
                let new_var_id = self.new_variable(var_name.clone(), ty, cfg::VariableKind::Local);
                self.builder.var_map.insert(var_id, new_var_id);

                if let Some(table_id) = self.is_row_type(resolved_type) {
                    if !self.row_field_map.contains_key(&var_id) {
                        self.create_row_field_variables(var_id, &var_name, table_id);
                    }
                }

                if let Some(init_expr_id) = decl.init {
                    let src_operand = self.build_expression(init_expr_id);
                    let span = self.pick_span(&[self.builder.ast.expressions[init_expr_id].span()]);
                    self.add_instruction(cfg::Instruction {
                        kind: cfg::InstructionKind::Assign {
                            dest: new_var_id,
                            src: src_operand,
                        },
                        span,
                    });

                    if let ast::Expression::TableRowAccess { .. } =
                        &self.builder.ast.expressions[init_expr_id]
                    {
                        if self.is_row_type(resolved_type).is_some() {
                            // Ensure field variables exist before populating them
                            if let Some(table_id) = self.is_row_type(resolved_type) {
                                if !self.row_field_map.contains_key(&var_id) {
                                    self.create_row_field_variables(var_id, &var_name, table_id);
                                }
                            }
                            self.handle_row_assignment(var_id, init_expr_id);
                        }
                    }
                }
            }
            ast::Statement::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                // Build condition expression before taking the current block
                let cond_operand = self.build_expression(condition);
                let start_block = self.current_block.take().unwrap();

                let then_bb = self.new_basic_block(self.current_hop.unwrap());
                let merge_bb = self.new_basic_block(self.current_hop.unwrap());

                if let Some(else_id) = else_block {
                    let else_bb = self.new_basic_block(self.current_hop.unwrap());
                    self.terminate(
                        start_block,
                        cfg::Terminator::Branch {
                            condition: cond_operand,
                            if_true: then_bb,
                            if_false: else_bb,
                        },
                    );

                    self.current_block = Some(then_bb);
                    self.build_block(then_block);
                    if self.current_block.is_some() {
                        self.terminate(
                            self.current_block.unwrap(),
                            cfg::Terminator::Jump(merge_bb),
                        );
                    }

                    self.current_block = Some(else_bb);
                    self.build_block(else_id);
                    if self.current_block.is_some() {
                        self.terminate(
                            self.current_block.unwrap(),
                            cfg::Terminator::Jump(merge_bb),
                        );
                    }
                } else {
                    self.terminate(
                        start_block,
                        cfg::Terminator::Branch {
                            condition: cond_operand,
                            if_true: then_bb,
                            if_false: merge_bb,
                        },
                    );

                    self.current_block = Some(then_bb);
                    self.build_block(then_block);
                    if self.current_block.is_some() {
                        self.terminate(
                            self.current_block.unwrap(),
                            cfg::Terminator::Jump(merge_bb),
                        );
                    }
                }

                self.current_block = Some(merge_bb);
            }
            ast::Statement::Return { value, .. } => {
                let operand = value.map(|expr_id| self.build_expression(expr_id));
                let current_block = self.current_block.take().unwrap();
                self.terminate(current_block, cfg::Terminator::Return(operand));
            }
            ast::Statement::Assert { expr, span } => {
                let cond = self.build_expression(expr);
                // Prefer the statement's span; otherwise, use the condition expression span
                let span = self.pick_span(&[span, self.builder.ast.expressions[expr].span()]);
                self.add_instruction(cfg::Instruction {
                    kind: cfg::InstructionKind::Assert {
                        condition: cond,
                        message: "Assertion failed".to_string(),
                    },
                    span,
                });
            }
            ast::Statement::Expression { expr, .. } => {
                self.build_expression(expr); // Build for side effects, discard result
            }
            ast::Statement::Block(block_id) => {
                self.build_block(block_id);
            }
            ast::Statement::Hop { body, .. } => {
                // Temporarily treat hop as a simple block to get CFG output
                self.build_block(body);
            }
            ast::Statement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                // Build initialization if present
                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(var_id) => {
                            let var_stmt_id = self
                                .builder
                                .ast
                                .statements
                                .alloc(ast::Statement::VarDecl(var_id));
                            self.build_statement(var_stmt_id);
                        }
                        ast::ForInit::Expression(expr_id) => {
                            self.build_expression(expr_id);
                        }
                    }
                }

                // Create loop structure
                let loop_entry = self.new_basic_block(self.current_hop.unwrap());
                let loop_body = self.new_basic_block(self.current_hop.unwrap());
                let loop_update = if update.is_some() {
                    Some(self.new_basic_block(self.current_hop.unwrap()))
                } else {
                    None
                };
                let loop_exit = self.new_basic_block(self.current_hop.unwrap());

                // Connect current block to loop entry
                if let Some(current) = self.current_block.take() {
                    self.terminate(current, cfg::Terminator::Jump(loop_entry));
                }

                self.current_block = Some(loop_entry);

                // Build condition if present
                if let Some(condition_id) = condition {
                    let condition_operand = self.build_expression(condition_id);
                    let current = self.current_block.take().unwrap();
                    self.terminate(
                        current,
                        cfg::Terminator::Branch {
                            condition: condition_operand,
                            if_true: loop_body,
                            if_false: loop_exit,
                        },
                    );
                } else {
                    // No condition means infinite loop
                    let current = self.current_block.take().unwrap();
                    self.terminate(current, cfg::Terminator::Jump(loop_body));
                }

                // Build loop body
                self.current_block = Some(loop_body);
                self.build_block(body);

                // Connect fallthrough of loop body to update (if present) or back to entry
                if let Some(current) = self.current_block.take() {
                    let next_block = loop_update.unwrap_or(loop_entry);
                    self.terminate(current, cfg::Terminator::Jump(next_block));
                }

                // Build update block if present, then jump back to entry
                if let (Some(update_bb), Some(update_id)) = (loop_update, update) {
                    self.current_block = Some(update_bb);
                    self.build_expression_or_void(update_id);
                    if let Some(current) = self.current_block.take() {
                        self.terminate(current, cfg::Terminator::Jump(loop_entry));
                    }
                }

                self.current_block = Some(loop_exit);
            }
            ast::Statement::HopsFor { .. } => {
                // This is handled in `build_transaction_body`, so we can ignore it here.
            }
        }
    }

    /// Builds an AST expression into a CFG operand.
    /// This is the core of the expression-to-instruction logic.
    fn build_expression(&mut self, expr_id: ast::ExprId) -> cfg::Operand {
        let expr = self.builder.ast.expressions[expr_id].clone();
        match expr {
            ast::Expression::Literal { value, .. } => {
                match value {
                    // Simple literals can be constants
                    ast::Literal::Integer(_)
                    | ast::Literal::Float(_)
                    | ast::Literal::String(_)
                    | ast::Literal::Bool(_) => {
                        cfg::Operand::Constant(self.builder.evaluate_const_expr(expr_id))
                    }
                    // Complex literals need to be built as runtime expressions
                    ast::Literal::List(_) => {
                        panic!("List literals in expressions should have been handled in frontend");
                    }
                    ast::Literal::RowLiteral(_key_values) => {
                        // Row literals are used in table assignments like:
                        // Table[key] = { field1: value1, field2: value2 }
                        // We need to handle this specially depending on context
                        panic!("Row literals should be handled in assignment context, not as standalone expressions")
                    }
                }
            }
            ast::Expression::Identifier {
                resolved_declarations,
                span,
                ..
            } => {
                let res = resolved_declarations
                    .first()
                    .expect("Identifier not resolved");
                match res {
                    ast::IdentifierResolution::Var(id) => {
                        cfg::Operand::Variable(self.builder.var_map[id])
                    }
                    ast::IdentifierResolution::Param(id) => {
                        cfg::Operand::Variable(self.param_map[id])
                    }
                    ast::IdentifierResolution::Const(id) => {
                        if let Some(cfg_const_id) = self.builder.const_map.get(id) {
                            cfg::Operand::Global(*cfg_const_id)
                        } else {
                            // Constant was skipped during building phase (likely no resolved type)
                            panic!("Constant '{}' not found in const_map - likely has no resolved type", 
                                self.builder.ast.const_decls[*id].name.name);
                        }
                    }
                    ast::IdentifierResolution::Table(id) => {
                        let cfg_table_id = self.builder.table_map[&id];
                        // Lower table identifiers directly to a Table operand
                        cfg::Operand::Table(cfg_table_id)
                    }
                    ast::IdentifierResolution::Field(field_id) => {
                        if let Some(inv_ctx) = &self.table_invariant_context {
                            let cfg_field_id = *self
                                .builder
                                .field_map
                                .get(field_id)
                                .expect("Field should be registered in field_map before invariants are built");
                            let field_type = self.builder.cfg.table_fields[cfg_field_id].field_type;
                            let table_id = inv_ctx.table_id;
                            let key_operands: Vec<_> = inv_ctx
                                .key_vars
                                .iter()
                                .map(|var_id| cfg::Operand::Variable(*var_id))
                                .collect();
                            let instr_span = self.pick_span(&[span]);
                            let temp_var = self.new_temporary(field_type);
                            self.add_instruction(cfg::Instruction {
                                kind: cfg::InstructionKind::TableGet {
                                    dest: temp_var,
                                    table: table_id,
                                    keys: key_operands,
                                    field: Some(cfg_field_id),
                                },
                                span: instr_span,
                            });
                            cfg::Operand::Variable(temp_var)
                        } else {
                            panic!(
                                "Field identifier '{}' used outside of a table invariant context",
                                self.builder.ast.fields[*field_id].name.name
                            );
                        }
                    }
                    _ => panic!("Unknown identifier resolution type: {:?}", res),
                }
            }
            ast::Expression::Binary {
                left,
                op,
                right,
                resolved_callables,
                resolved_type,
                span,
                ..
            } => {
                let left_op = self.build_expression(left);
                let right_op = self.build_expression(right);

                // Binary operators MUST resolve to exactly one function
                if resolved_callables.is_empty() {
                    panic!("Binary operator '{}' has no resolved function - should have been resolved in frontend", op.name);
                }
                if resolved_callables.len() > 1 {
                    panic!("Binary operator '{}' resolves to multiple functions - type checker should ensure only one", op.name);
                }

                let func_id = resolved_callables[0];
                let ast_func = &self.builder.ast.functions[func_id];

                // Check if this function is a @builtinop
                let is_builtin = self.builder.is_builtin_op(&ast_func.decorators);

                if is_builtin {
                    // This is a built-in operator - use BinaryOp with proper type checking
                    // Get types before the borrow checker complains
                    let left_resolved_type = self.builder.ast.expressions[left]
                        .resolved_type()
                        .unwrap()
                        .clone();
                    let right_resolved_type = self.builder.ast.expressions[right]
                        .resolved_type()
                        .unwrap()
                        .clone();
                    let left_type_id = self
                        .builder
                        .convert_resolved_type(&left_resolved_type)
                        .unwrap();
                    let right_type_id = self
                        .builder
                        .convert_resolved_type(&right_resolved_type)
                        .unwrap();
                    let left_type = &self.builder.cfg.types[left_type_id];
                    let right_type = &self.builder.cfg.types[right_type_id];

                    let bin_op = match (op.name.as_str(), left_type, right_type) {
                        (
                            "+",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::AddInt,
                        (
                            "+",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::AddFloat,
                        (
                            "-",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::SubInt,
                        (
                            "-",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::SubFloat,
                        (
                            "*",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::MulInt,
                        (
                            "*",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::MulFloat,
                        (
                            "/",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::DivInt,
                        (
                            "/",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::DivFloat,
                        (
                            "%",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::ModInt,
                        (
                            "==",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::EqInt,
                        (
                            "==",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::EqFloat,
                        (
                            "==",
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                        ) => cfg::BinaryOp::Eq,
                        (
                            "!=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::NeqInt,
                        (
                            "!=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::NeqFloat,
                        (
                            "!=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                        ) => cfg::BinaryOp::Neq,
                        (
                            "<",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::LtInt,
                        (
                            "<",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::LtFloat,
                        (
                            "<=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::LeqInt,
                        (
                            "<=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::LeqFloat,
                        (
                            ">",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::GtInt,
                        (
                            ">",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::GtFloat,
                        (
                            ">=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                            cfg::Type::Primitive(cfg::PrimitiveType::Int),
                        ) => cfg::BinaryOp::GeqInt,
                        (
                            ">=",
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                            cfg::Type::Primitive(cfg::PrimitiveType::Float),
                        ) => cfg::BinaryOp::GeqFloat,
                        (
                            "&&",
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                        ) => cfg::BinaryOp::And,
                        (
                            "||",
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                            cfg::Type::Primitive(cfg::PrimitiveType::Bool),
                        ) => cfg::BinaryOp::Or,
                        ("===", _, _) => cfg::BinaryOp::Eq, // Generic strict equality
                        ("!==", _, _) => cfg::BinaryOp::Neq, // Generic strict inequality
                        _ => panic!(
                            "Unsupported @builtinop binary operator '{}' with types {:?} and {:?}",
                            op.name, left_type, right_type
                        ),
                    };

                    let dest_ty = self
                        .builder
                        .convert_resolved_type(resolved_type.as_ref().unwrap())
                        .unwrap();
                    let dest_var = self.new_temporary(dest_ty);
                    let span = self.pick_span(&[
                        span,
                        self.builder.ast.expressions[left].span(),
                        self.builder.ast.expressions[right].span(),
                    ]);
                    self.add_instruction(cfg::Instruction {
                        kind: cfg::InstructionKind::BinaryOp {
                            dest: dest_var,
                            op: bin_op,
                            left: left_op,
                            right: right_op,
                        },
                        span,
                    });
                    cfg::Operand::Variable(dest_var)
                } else {
                    // This is a regular function (like string concatenation with @Boogie)
                    if let Some(&cfg_func_id) = self.builder.func_map.get(&func_id) {
                        let return_type = self
                            .builder
                            .convert_resolved_type(resolved_type.as_ref().unwrap())
                            .unwrap();
                        let temp_var = self.new_temporary(return_type);
                        let call_span = span;
                        let span = self.pick_span(&[
                            call_span,
                            self.builder.ast.expressions[left].span(),
                            self.builder.ast.expressions[right].span(),
                        ]);

                        self.add_instruction(cfg::Instruction {
                            kind: cfg::InstructionKind::Call {
                                dest: Some(temp_var),
                                func: cfg_func_id,
                                args: vec![left_op, right_op],
                            },
                            span,
                        });
                        cfg::Operand::Variable(temp_var)
                    } else {
                        panic!("Binary operator function '{}' should have been mapped during function shell building", op.name);
                    }
                }
            }
            ast::Expression::Unary {
                op,
                expr,
                resolved_callables,
                resolved_type,
                span,
                ..
            } => {
                let operand = self.build_expression(expr);

                // Unary operators MUST resolve to exactly one function
                if resolved_callables.is_empty() {
                    panic!("Unary operator '{}' has no resolved function - should have been resolved in frontend", op.name);
                }
                if resolved_callables.len() > 1 {
                    panic!("Unary operator '{}' resolves to multiple functions - type checker should ensure only one", op.name);
                }

                let func_id = resolved_callables[0];
                let ast_func = &self.builder.ast.functions[func_id];

                // Check if this function is a @builtinop
                let is_builtin = self.builder.is_builtin_op(&ast_func.decorators);

                if is_builtin {
                    // This is a built-in operator - use UnaryOp with proper type checking
                    let expr_resolved_type = self.builder.ast.expressions[expr]
                        .resolved_type()
                        .unwrap()
                        .clone();
                    let expr_type_id = self
                        .builder
                        .convert_resolved_type(&expr_resolved_type)
                        .unwrap();
                    let expr_type = &self.builder.cfg.types[expr_type_id];

                    let unary_op = match (op.name.as_str(), expr_type) {
                        ("-", cfg::Type::Primitive(cfg::PrimitiveType::Int)) => {
                            cfg::UnaryOp::NegInt
                        }
                        ("-", cfg::Type::Primitive(cfg::PrimitiveType::Float)) => {
                            cfg::UnaryOp::NegFloat
                        }
                        ("!", cfg::Type::Primitive(cfg::PrimitiveType::Bool)) => {
                            cfg::UnaryOp::NotBool
                        }
                        _ => panic!(
                            "Unsupported @builtinop unary operator '{}' with type {:?}",
                            op.name, expr_type
                        ),
                    };

                    let dest_ty = self
                        .builder
                        .convert_resolved_type(resolved_type.as_ref().unwrap())
                        .unwrap();
                    let dest_var = self.new_temporary(dest_ty);
                    let span = self.pick_span(&[span, self.builder.ast.expressions[expr].span()]);
                    self.add_instruction(cfg::Instruction {
                        kind: cfg::InstructionKind::UnaryOp {
                            dest: dest_var,
                            op: unary_op,
                            operand,
                        },
                        span,
                    });
                    cfg::Operand::Variable(dest_var)
                } else {
                    // This is a regular function
                    if let Some(&cfg_func_id) = self.builder.func_map.get(&func_id) {
                        let return_type = self
                            .builder
                            .convert_resolved_type(resolved_type.as_ref().unwrap())
                            .unwrap();
                        let temp_var = self.new_temporary(return_type);
                        let call_span = span;
                        let span =
                            self.pick_span(&[call_span, self.builder.ast.expressions[expr].span()]);

                        self.add_instruction(cfg::Instruction {
                            kind: cfg::InstructionKind::Call {
                                dest: Some(temp_var),
                                func: cfg_func_id,
                                args: vec![operand],
                            },
                            span,
                        });
                        cfg::Operand::Variable(temp_var)
                    } else {
                        panic!("Unary operator function '{}' should have been mapped during function shell building", op.name);
                    }
                }
            }
            ast::Expression::Call {
                callee,
                args,
                resolved_type,
                resolved_callables,
                span,
                ..
            } => {
                // Get the function ID from resolved_callables (already narrowed down by type checker)
                if resolved_callables.is_empty() {
                    let callee_expr = &self.builder.ast.expressions[callee];
                    panic!("Function call has no resolved callables - should have been resolved in frontend: {:?}", callee_expr);
                }
                if resolved_callables.len() > 1 {
                    panic!("Function call resolves to multiple functions - type checker should ensure only one: {:?}", resolved_callables);
                }

                let ast_func_id = resolved_callables[0];
                let func_id = if let Some(&cfg_id) = self.builder.func_map.get(&ast_func_id) {
                    cfg_id
                } else {
                    // Built-in functions should have been resolved in frontend
                    let ast_func = &self.builder.ast.functions[ast_func_id];
                    panic!(
                        "Built-in function call should have been resolved: {}",
                        ast_func.name.name
                    );
                };

                // Build arguments uniformly; table identifiers lower to Operand::Table via build_expression
                let arg_operands: Vec<cfg::Operand> = args
                    .iter()
                    .map(|arg_expr_id| self.build_expression(*arg_expr_id))
                    .collect();

                let return_type = self
                    .builder
                    .convert_resolved_type(resolved_type.as_ref().unwrap())
                    .unwrap();

                let is_void = matches!(self.builder.cfg.types[return_type], cfg::Type::Void);

                if is_void {
                    let call_span = span;
                    let span = self.pick_span(&[
                        call_span,
                        self.builder.ast.expressions[callee].span(),
                        args.first()
                            .and_then(|id| self.builder.ast.expressions[*id].span()),
                    ]);
                    let instr = cfg::Instruction {
                        kind: cfg::InstructionKind::Call {
                            dest: None,
                            func: func_id,
                            args: arg_operands,
                        },
                        span,
                    };
                    self.add_instruction(instr);
                    // Void functions used as expressions should not happen in well-formed AST
                    panic!("Void function call used as expression - should have been caught in frontend")
                } else {
                    let temp_var = self.new_temporary(return_type);
                    let call_span = span;
                    let span = self.pick_span(&[
                        call_span,
                        self.builder.ast.expressions[callee].span(),
                        args.first()
                            .and_then(|id| self.builder.ast.expressions[*id].span()),
                    ]);
                    let instr = cfg::Instruction {
                        kind: cfg::InstructionKind::Call {
                            dest: Some(temp_var),
                            func: func_id,
                            args: arg_operands,
                        },
                        span,
                    };
                    self.add_instruction(instr);
                    cfg::Operand::Variable(temp_var)
                }
            }
            ast::Expression::Assignment { lhs, rhs, span, .. } => {
                let lhs_expr = self.builder.ast.expressions[lhs].clone();
                match lhs_expr {
                    ast::Expression::Identifier {
                        resolved_declarations,
                        ..
                    } => {
                        // Regular variable assignment
                        if let ast::IdentifierResolution::Var(var_id) = resolved_declarations[0] {
                            // Check if this is a Row<T> variable that has been decomposed
                            if self.row_field_map.contains_key(&var_id) {
                                // This is assigning to a decomposed Row<T> variable, handle specially
                                self.handle_row_assignment(var_id, rhs);
                                // For decomposed rows, return a placeholder value
                                return cfg::Operand::Constant(cfg::ConstantValue::Int(0));
                            } else {
                                // Regular assignment (including non-decomposed Row<T> variables)
                                let src_op = self.build_expression(rhs);
                                let dest_var = self.builder.var_map[&var_id];
                                let assign_span = span;
                                let span = self.pick_span(&[
                                    assign_span,
                                    self.builder.ast.expressions[lhs].span(),
                                    self.builder.ast.expressions[rhs].span(),
                                ]);
                                self.add_instruction(cfg::Instruction {
                                    kind: cfg::InstructionKind::Assign {
                                        dest: dest_var,
                                        src: src_op.clone(),
                                    },
                                    span,
                                });
                                return src_op; // Assignment expression evaluates to the assigned value
                            }
                        }
                        panic!("LHS of assignment is not a variable");
                    }
                    ast::Expression::MemberAccess {
                        object,
                        member,
                        resolved_fields,
                        ..
                    } => {
                        // Handle member assignment: obj.field = value
                        let object_expr = self.builder.ast.expressions[object].clone();
                        let src_op = self.build_expression(rhs);

                        match object_expr {
                            ast::Expression::Identifier {
                                resolved_declarations,
                                ..
                            } => {
                                // Check if this is assigning to a field of a Row<T> variable
                                if let Some(ast::IdentifierResolution::Var(var_id)) =
                                    resolved_declarations.first()
                                {
                                    // Get the necessary data first to avoid borrowing conflicts
                                    let var_decl = self.builder.ast.var_decls[*var_id].clone();
                                    let var_id_val = *var_id;

                                    // Check if this variable is of Row<T> type
                                    if let Some(table_id) =
                                        self.is_row_type(var_decl.resolved_type.as_ref().unwrap())
                                    {
                                        // This is a Row<T> variable - create field variables if not already done
                                        if !self.row_field_map.contains_key(&var_id_val) {
                                            self.create_row_field_variables(
                                                var_id_val,
                                                &var_decl.name.name,
                                                table_id,
                                            );
                                        }

                                        // Now assign to the field variable
                                        if let Some(field_map) = self.row_field_map.get(&var_id_val)
                                        {
                                            if let Some(&field_var_id) = field_map.get(&member.name)
                                            {
                                                let assign_span = span;
                                                let span = self.pick_span(&[
                                                    assign_span,
                                                    self.builder.ast.expressions[object].span(),
                                                    self.builder.ast.expressions[rhs].span(),
                                                ]);
                                                self.add_instruction(cfg::Instruction {
                                                    kind: cfg::InstructionKind::Assign {
                                                        dest: field_var_id,
                                                        src: src_op.clone(),
                                                    },
                                                    span,
                                                });
                                                return src_op;
                                            }
                                        }
                                    }
                                }
                                panic!("Member assignment to non-Row variable should have been caught in frontend");
                            }
                            ast::Expression::TableRowAccess {
                                table, key_values, ..
                            } => {
                                // Handle Table[...].field = value - direct table field assignment
                                let table_id = self
                                    .get_table_id_from_expr(table)
                                    .expect("Table expression should resolve to table ID");

                                let key_operands: Vec<_> = key_values
                                    .iter()
                                    .map(|key| self.build_expression(key.value))
                                    .collect();

                                // Use the resolved field information from AST
                                if let Some(ast::IdentifierResolution::Field(field_id)) =
                                    resolved_fields.first()
                                {
                                    let cfg_field_id = self.builder.field_map[field_id];
                                    let assign_span = span;
                                    let span = self.pick_span(&[
                                        assign_span,
                                        self.builder.ast.expressions[table].span(),
                                        key_values
                                            .first()
                                            .map(|kv| self.builder.ast.expressions[kv.value].span())
                                            .flatten(),
                                        self.builder.ast.expressions[rhs].span(),
                                    ]);
                                    self.add_instruction(cfg::Instruction {
                                        kind: cfg::InstructionKind::TableSet {
                                            table: table_id,
                                            keys: key_operands,
                                            field: Some(cfg_field_id),
                                            value: src_op.clone(),
                                        },
                                        span,
                                    });
                                }

                                return src_op;
                            }
                            _ => panic!("Complex member assignment not supported"),
                        }
                    }
                    ast::Expression::TableRowAccess {
                        table, key_values, ..
                    } => {
                        // Handle table assignment: Table[key1: val1, key2: val2] = value
                        let table_id = self
                            .get_table_id_from_expr(table)
                            .expect("Table expression should resolve to table ID");

                        let key_operands: Vec<_> = key_values
                            .iter()
                            .map(|key| self.build_expression(key.value))
                            .collect();

                        // Check if the RHS is a Row<T> variable or row literal - if so, we need to decompose
                        let rhs_expr = self.builder.ast.expressions[rhs].clone();
                        match rhs_expr {
                            ast::Expression::Identifier {
                                resolved_declarations,
                                ..
                            } => {
                                let src_op = self.build_expression(rhs);
                                if let Some(ast::IdentifierResolution::Var(var_id)) =
                                    resolved_declarations.first()
                                {
                                    if let Some(field_map) = self.row_field_map.get(var_id).cloned()
                                    {
                                        // Decompose: Table[...] = row_var becomes Table[...].field1 = row_var#field1; etc.
                                        // Get all table fields to map field names to field IDs
                                        let table_fields = self.get_table_fields(table_id);
                                        let field_name_to_id: HashMap<String, cfg::FieldId> =
                                            table_fields.into_iter().collect();

                                        for (field_name, field_var_id) in field_map {
                                            if let Some(&field_id) =
                                                field_name_to_id.get(&field_name)
                                            {
                                                let assign_span = span;
                                                let span = self.pick_span(&[
                                                    assign_span,
                                                    self.builder.ast.expressions[lhs].span(),
                                                    self.builder.ast.expressions[rhs].span(),
                                                ]);
                                                self.add_instruction(cfg::Instruction {
                                                    kind: cfg::InstructionKind::TableSet {
                                                        table: table_id,
                                                        keys: key_operands.clone(),
                                                        field: Some(field_id),
                                                        value: cfg::Operand::Variable(field_var_id),
                                                    },
                                                    span,
                                                });
                                            }
                                        }
                                        return src_op;
                                    }
                                }
                                // Regular identifier assignment
                                let assign_span = span;
                                let span = self.pick_span(&[
                                    assign_span,
                                    self.builder.ast.expressions[lhs].span(),
                                    self.builder.ast.expressions[rhs].span(),
                                ]);
                                self.add_instruction(cfg::Instruction {
                                    kind: cfg::InstructionKind::TableSet {
                                        table: table_id,
                                        keys: key_operands,
                                        field: None, // None means set the whole row
                                        value: src_op.clone(),
                                    },
                                    span,
                                });
                                return src_op;
                            }
                            ast::Expression::Literal {
                                value: ast::Literal::RowLiteral(key_values),
                                ..
                            } => {
                                // Handle row literal assignment: Table[...] = { field1: value1, field2: value2 }
                                // Build a name->FieldId map once for fallback lookup.
                                let table_fields = self.get_table_fields(table_id);
                                let mut field_name_to_id: HashMap<String, cfg::FieldId> =
                                    HashMap::new();
                                for (fname, fid) in table_fields {
                                    field_name_to_id.insert(fname, fid);
                                }

                                for key_value in key_values {
                                    let field_value_op = self.build_expression(key_value.value);

                                    // Prefer resolved field from AST if present.
                                    if let Some(ast::IdentifierResolution::Field(field_id)) =
                                        &key_value.resolved_field
                                    {
                                        let cfg_field_id = self.builder.field_map[field_id];
                                        let assign_span = span;
                                        let span = self.pick_span(&[
                                            assign_span,
                                            self.builder.ast.expressions[lhs].span(),
                                            self.builder.ast.expressions[key_value.value].span(),
                                        ]);
                                        self.add_instruction(cfg::Instruction {
                                            kind: cfg::InstructionKind::TableSet {
                                                table: table_id,
                                                keys: key_operands.clone(),
                                                field: Some(cfg_field_id),
                                                value: field_value_op,
                                            },
                                            span,
                                        });
                                    } else {
                                        // Fallback: attempt to match by field name (the identifier key).
                                        // key_value.key is an Identifier; obtain its string name for fallback lookup
                                        let field_name = key_value.key.name.clone();
                                        if let Some(fid) = field_name_to_id.get(&field_name) {
                                            let assign_span = span;
                                            let span = self.pick_span(&[
                                                assign_span,
                                                self.builder.ast.expressions[lhs].span(),
                                                self.builder.ast.expressions[key_value.value]
                                                    .span(),
                                            ]);
                                            self.add_instruction(cfg::Instruction {
                                                kind: cfg::InstructionKind::TableSet {
                                                    table: table_id,
                                                    keys: key_operands.clone(),
                                                    field: Some(*fid),
                                                    value: field_value_op,
                                                },
                                                span,
                                            });
                                        } else {
                                            // If field still not found, emit a panic for now to surface issue.
                                            panic!("RowLiteral assignment field '{}' not resolved and not found in table.", field_name);
                                        }
                                    }
                                }
                                // Return a placeholder value since row assignments don't have a meaningful return value
                                return cfg::Operand::Constant(cfg::ConstantValue::Int(0));
                            }
                            _ => {
                                // Regular table assignment for other expression types
                                let src_op = self.build_expression(rhs);
                                let assign_span = span;
                                let span = self.pick_span(&[
                                    assign_span,
                                    self.builder.ast.expressions[lhs].span(),
                                    self.builder.ast.expressions[rhs].span(),
                                ]);
                                self.add_instruction(cfg::Instruction {
                                    kind: cfg::InstructionKind::TableSet {
                                        table: table_id,
                                        keys: key_operands,
                                        field: None, // None means set the whole row
                                        value: src_op.clone(),
                                    },
                                    span,
                                });
                                return src_op;
                            }
                        }
                    }
                    _ => {
                        panic!("Unsupported assignment LHS");
                    }
                }
            }
            ast::Expression::Lambda {
                params,
                body,
                resolved_type,
                ..
            } => {
                // Generate a globally unique lambda name
                let lambda_name = format!("lambda_{}", self.builder.lambda_counter);
                self.builder.lambda_counter += 1;

                // Convert the lambda type to get the function signature
                let func_type = self
                    .builder
                    .convert_resolved_type(resolved_type.as_ref().unwrap())
                    .unwrap();

                // Collect parameter information first to avoid borrowing issues
                let param_data: Vec<_> = params
                    .iter()
                    .map(|param_id| {
                        let param = &self.builder.ast.params[*param_id];
                        (
                            *param_id,
                            param.name.name.clone(),
                            param.resolved_type.clone().unwrap(),
                        )
                    })
                    .collect();

                // Process lambda parameters
                let mut lambda_params = Vec::new();
                for (_param_id, param_name, param_type) in &param_data {
                    let param_type_id = self.builder.convert_resolved_type(param_type).unwrap();
                    let param_var = cfg::Variable {
                        name: param_name.clone(),
                        ty: param_type_id,
                        kind: cfg::VariableKind::Parameter,
                    };
                    let param_var_id = self.builder.cfg.variables.alloc(param_var);
                    lambda_params.push(param_var_id);
                }

                // Create the lambda function
                let lambda_func = cfg::Function {
                    name: lambda_name.clone(),
                    signature: cfg::TypeScheme {
                        quantified_params: Vec::new(), // Lambdas typically don't have generic params
                        ty: func_type,
                    },
                    kind: cfg::FunctionKind::Lambda,
                    params: lambda_params.clone(),
                    assumptions: Vec::new(),
                    entry_block: None,
                    all_blocks: Vec::new(),
                    decorators: Vec::new(),
                    entry_hop: None,
                    hops: Vec::new(),
                };

                let lambda_func_id = self.builder.cfg.functions.alloc(lambda_func);

                // Build lambda body
                let mut lambda_context = FunctionContext::new(self.builder, lambda_func_id);

                // Copy outer function parameter mappings to lambda context
                lambda_context.param_map = self.param_map.clone();
                lambda_context.param_row_field_map = self.param_row_field_map.clone();
                lambda_context.table_invariant_context = self.table_invariant_context.clone();

                // Map lambda parameters and create Row field mappings if needed
                for (i, (param_id, param_name, param_type)) in param_data.iter().enumerate() {
                    if let Some(&param_var_id) = lambda_params.get(i) {
                        lambda_context.param_map.insert(*param_id, param_var_id);

                        // Check if this parameter is a Row type and create field variables
                        if let Some(table_id) = lambda_context.is_row_type(param_type) {
                            lambda_context
                                .create_param_row_field_variables(*param_id, param_name, table_id);
                        }
                    }
                }

                // Build the lambda body
                lambda_context.build_function_body(body);

                // Create a variable to represent this lambda function
                let lambda_var = self.new_variable(
                    lambda_name,
                    func_type,
                    cfg::VariableKind::Lambda(lambda_func_id),
                );

                cfg::Operand::Variable(lambda_var)
            }
            ast::Expression::TableRowAccess {
                table,
                key_values,
                resolved_type,
                span,
                ..
            } => {
                // Handle table row access: Table[key1: val1, key2: val2]
                let table_id = self
                    .get_table_id_from_expr(table)
                    .expect("Table expression should resolve to table ID");

                let key_operands: Vec<_> = key_values
                    .iter()
                    .map(|key| self.build_expression(key.value))
                    .collect();

                let return_type = self
                    .builder
                    .convert_resolved_type(resolved_type.as_ref().unwrap())
                    .unwrap();

                let temp_var = self.new_temporary(return_type);

                // For direct table access, we don't specify a field (get the whole row)
                let tr_span = self.pick_span(&[
                    span,
                    self.builder.ast.expressions[table].span(),
                    key_values
                        .first()
                        .map(|kv| self.builder.ast.expressions[kv.value].span())
                        .flatten(),
                ]);
                self.add_instruction(cfg::Instruction {
                    kind: cfg::InstructionKind::TableGet {
                        dest: temp_var,
                        table: table_id,
                        keys: key_operands,
                        field: None, // None means get the whole row
                    },
                    span: tr_span,
                });

                cfg::Operand::Variable(temp_var)
            }
            ast::Expression::MemberAccess {
                object,
                member,
                resolved_fields,
                resolved_type,
                span,
                ..
            } => {
                // Handle member access: obj.field
                let object_expr = self.builder.ast.expressions[object].clone();

                match object_expr {
                    ast::Expression::Identifier {
                        resolved_declarations,
                        ..
                    } => {
                        // Check if this is accessing a field of a Row<T> variable or parameter
                        match resolved_declarations.first() {
                            Some(ast::IdentifierResolution::Var(var_id)) => {
                                // Get the necessary data first to avoid borrowing conflicts
                                let var_decl = self.builder.ast.var_decls[*var_id].clone();
                                let var_id_val = *var_id;

                                // Check if this variable is of Row<T> type
                                if let Some(table_id) =
                                    self.is_row_type(var_decl.resolved_type.as_ref().unwrap())
                                {
                                    // This is a Row<T> variable - create field variables if not already done
                                    if !self.row_field_map.contains_key(&var_id_val) {
                                        self.create_row_field_variables(
                                            var_id_val,
                                            &var_decl.name.name,
                                            table_id,
                                        );
                                    }

                                    // Now get the field variable
                                    if let Some(field_map) = self.row_field_map.get(&var_id_val) {
                                        if let Some(&field_var_id) = field_map.get(&member.name) {
                                            return cfg::Operand::Variable(field_var_id);
                                        }
                                    }
                                }
                            }
                            Some(ast::IdentifierResolution::Param(param_id)) => {
                                // Get the parameter data
                                let param_decl = self.builder.ast.params[*param_id].clone();
                                let param_id_val = *param_id;

                                // Check if this parameter is of Row<T> type
                                if let Some(table_id) =
                                    self.is_row_type(param_decl.resolved_type.as_ref().unwrap())
                                {
                                    // This is a Row<T> parameter - create field variables if not already done
                                    if !self.param_row_field_map.contains_key(&param_id_val) {
                                        self.create_param_row_field_variables(
                                            param_id_val,
                                            &param_decl.name.name,
                                            table_id,
                                        );
                                    }

                                    // Now get the field variable
                                    if let Some(field_map) =
                                        self.param_row_field_map.get(&param_id_val)
                                    {
                                        if let Some(&field_var_id) = field_map.get(&member.name) {
                                            return cfg::Operand::Variable(field_var_id);
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }

                        // Regular member access - not supported yet for non-Row types
                        panic!(
                            "Member access on non-Row type should have been caught in frontend: {}",
                            member.name
                        );
                    }
                    ast::Expression::TableRowAccess {
                        table, key_values, ..
                    } => {
                        // Handle Table[...].field - direct table field access
                        let table_id = self
                            .get_table_id_from_expr(table)
                            .expect("Table expression should resolve to table ID");

                        let key_operands: Vec<_> = key_values
                            .iter()
                            .map(|key| self.build_expression(key.value))
                            .collect();

                        let return_type = self
                            .builder
                            .convert_resolved_type(resolved_type.as_ref().unwrap())
                            .unwrap();
                        let temp_var = self.new_temporary(return_type);

                        // Use the resolved field information from AST
                        if let Some(ast::IdentifierResolution::Field(field_id)) =
                            resolved_fields.first()
                        {
                            let cfg_field_id = self.builder.field_map[field_id];
                            let ma_span = self
                                .pick_span(&[span, self.builder.ast.expressions[object].span()]);
                            self.add_instruction(cfg::Instruction {
                                kind: cfg::InstructionKind::TableGet {
                                    dest: temp_var,
                                    table: table_id,
                                    keys: key_operands,
                                    field: Some(cfg_field_id),
                                },
                                span: ma_span,
                            });
                        }

                        cfg::Operand::Variable(temp_var)
                    }
                    _ => {
                        panic!("Member access on complex expression not supported");
                    }
                }
            }
            ast::Expression::Grouped { expr, .. } => {
                // Grouped expressions are just parentheses, so we unwrap and build the inner expression
                self.build_expression(expr)
            }
        }
    }

    /// Build an expression that might return void (used in contexts like for-loop updates)
    fn build_expression_or_void(&mut self, expr_id: ast::ExprId) {
        // Extract the necessary data first to avoid borrow checker issues
        let expr = self.builder.ast.expressions[expr_id].clone();
        if let ast::Expression::Call {
            callee,
            args,
            span,
            resolved_type,
            ..
        } = expr
        {
            // Check if this returns void
            if let Some(resolved_type) = resolved_type {
                let return_type = self.builder.convert_resolved_type(&resolved_type).unwrap();
                let is_void = matches!(self.builder.cfg.types[return_type], cfg::Type::Void);

                if is_void {
                    // Handle void function call directly (similar to build_expression but without panic)
                    let callee_expr = self.builder.ast.expressions[callee].clone();
                    let func_id = match callee_expr {
                        ast::Expression::Identifier {
                            resolved_declarations,
                            ..
                        } => {
                            // Find the function in resolved declarations
                            let mut found_func = None;
                            for resolution in resolved_declarations {
                                if let ast::IdentifierResolution::Function(id) = resolution {
                                    if let Some(cfg_id) = self.builder.func_map.get(&id) {
                                        found_func = Some(*cfg_id);
                                        break;
                                    }
                                }
                            }
                            if let Some(id) = found_func {
                                id
                            } else {
                                panic!("Function should have been resolved in frontend");
                            }
                        }
                        _ => panic!("Dynamic callee should not reach CFG builder"),
                    };

                    // Build arguments
                    let arg_operands: Vec<cfg::Operand> = args
                        .iter()
                        .map(|arg_expr_id| self.build_expression(*arg_expr_id))
                        .collect();

                    // Create void function call instruction
                    let call_span = self.pick_span(&[
                        span,
                        self.builder.ast.expressions[callee].span(),
                        args.first()
                            .and_then(|id| self.builder.ast.expressions[*id].span()),
                    ]);
                    let instr = cfg::Instruction {
                        kind: cfg::InstructionKind::Call {
                            dest: None,
                            func: func_id,
                            args: arg_operands,
                        },
                        span: call_span,
                    };
                    self.add_instruction(instr);
                    return;
                }
            }
        }

        // Not a void function call, use regular expression building (discard the result)
        self.build_expression(expr_id);
    }

    // --- CFG Structure Helpers ---

    fn new_hop(&mut self, decorators: Vec<cfg::Decorator>) -> cfg::HopId {
        let hop = cfg::Hop {
            function_id: self.func_id,
            entry_block: None,
            blocks: Vec::new(),
            decorators,
        };
        let hop_id = self.builder.cfg.hops.alloc(hop);
        self.builder.cfg.functions[self.func_id].hops.push(hop_id);
        self.current_hop = Some(hop_id);
        hop_id
    }

    fn new_basic_block(&mut self, hop_id: cfg::HopId) -> cfg::BasicBlockId {
        let bb = cfg::BasicBlock {
            hop_id,
            function_id: self.func_id,
            predecessors: Vec::new(),
            instructions: Vec::new(),
            terminator: cfg::Terminator::Abort,
        };
        let bb_id = self.builder.cfg.basic_blocks.alloc(bb);
        self.builder.cfg.functions[self.func_id]
            .all_blocks
            .push(bb_id);
        self.builder.cfg.hops[hop_id].blocks.push(bb_id);
        bb_id
    }

    fn add_instruction(&mut self, instr: cfg::Instruction) {
        if let Some(block_id) = self.current_block {
            self.builder.cfg.basic_blocks[block_id]
                .instructions
                .push(instr);
        }
    }

    fn terminate(&mut self, block_id: cfg::BasicBlockId, term: cfg::Terminator) {
        if let cfg::Terminator::Branch {
            if_true, if_false, ..
        } = &term
        {
            self.builder.cfg.basic_blocks[*if_true]
                .predecessors
                .push(block_id);
            self.builder.cfg.basic_blocks[*if_false]
                .predecessors
                .push(block_id);
        } else if let cfg::Terminator::Jump(target) = &term {
            self.builder.cfg.basic_blocks[*target]
                .predecessors
                .push(block_id);
        }
        self.builder.cfg.basic_blocks[block_id].terminator = term;
    }

    /// Pick the first available span from a list of candidates.
    /// This helps attach a meaningful source location for generated instructions.
    fn pick_span(&self, candidates: &[Option<Span>]) -> Span {
        for c in candidates {
            if let Some(s) = c {
                return *s;
            }
        }
        // As a last resort, return an "unknown" span instead of a synthetic "<generated>".
        Span::new(0, 0, "unknown")
    }

    // --- Variable and Scope Management ---

    /// Check if a resolved type is a Row<T> type
    fn is_row_type(&self, resolved_type: &ast::ResolvedType) -> Option<cfg::TableId> {
        match resolved_type {
            ast::ResolvedType::Declared { decl_id, args } => {
                let ast_decl = &self.builder.ast.type_decls[*decl_id];
                if self.builder.is_builtin_decorator(&ast_decl.decorators)
                    && ast_decl.name.name == "Row"
                    && args.len() == 1
                {
                    // This is Row<T>, get the table ID from T
                    match &args[0] {
                        ast::ResolvedType::Table { table_id } => {
                            Some(self.builder.table_map[table_id])
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Get all fields for a table
    fn get_table_fields(&self, table_id: cfg::TableId) -> Vec<(String, cfg::FieldId)> {
        let table = &self.builder.cfg.tables[table_id];
        let mut fields = Vec::new();

        // Add primary key fields
        for &field_id in &table.primary_key_fields {
            let field = &self.builder.cfg.table_fields[field_id];
            fields.push((field.name.clone(), field_id));
        }

        // Add other fields
        for &field_id in &table.other_fields {
            let field = &self.builder.cfg.table_fields[field_id];
            fields.push((field.name.clone(), field_id));
        }

        fields
    }

    /// Create field variables for a Row<T> variable
    fn create_row_field_variables(
        &mut self,
        var_id: ast::VarId,
        var_name: &str,
        table_id: cfg::TableId,
    ) {
        let fields = self.get_table_fields(table_id);
        let mut field_map = HashMap::new();

        for (field_name, field_id) in fields {
            let field_var_name = format!("{}#{}", var_name, field_name);
            let field_type = self.builder.cfg.table_fields[field_id].field_type;
            let cfg_var_id =
                self.new_variable(field_var_name, field_type, cfg::VariableKind::Local);
            field_map.insert(field_name, cfg_var_id);
        }

        self.row_field_map.insert(var_id, field_map);
    }

    fn create_param_row_field_variables(
        &mut self,
        param_id: ast::ParamId,
        param_name: &str,
        table_id: cfg::TableId,
    ) {
        let fields = self.get_table_fields(table_id);
        let mut field_map = HashMap::new();

        for (field_name, field_id) in fields {
            let field_var_name = format!("{}#{}", param_name, field_name);
            let field_type = self.builder.cfg.table_fields[field_id].field_type;
            let cfg_var_id =
                self.new_variable(field_var_name, field_type, cfg::VariableKind::Local);
            field_map.insert(field_name, cfg_var_id);
        }

        self.param_row_field_map.insert(param_id, field_map);
    }

    /// Handle assignment to a Row<T> variable by decomposing into field assignments
    fn handle_row_assignment(&mut self, row_var_id: ast::VarId, source_expr_id: ast::ExprId) {
        // Get the field variables for this row variable
        let field_map = self
            .row_field_map
            .get(&row_var_id)
            .cloned()
            .expect("Row variable should have field map");

        let source_expr = self.builder.ast.expressions[source_expr_id].clone();
        match source_expr {
            ast::Expression::TableRowAccess {
                table,
                key_values,
                span,
                ..
            } => {
                // This is an assignment like: var c: Row<Customer> = Customer[...];
                // Decompose into: c#field1 = Customer[...].field1; c#field2 = Customer[...].field2; etc.

                // First, collect key expressions
                let key_expr_ids: Vec<_> = key_values.iter().map(|key| key.value).collect();

                // Get table ID
                let table_id = self
                    .get_table_id_from_expr(table)
                    .expect("Table expression should resolve to table ID");

                for (field_name, &field_var_id) in &field_map {
                    // Build key operands for each field (they're the same for all fields)
                    let key_operands: Vec<_> = key_expr_ids
                        .iter()
                        .map(|&key_expr_id| self.build_expression(key_expr_id))
                        .collect();

                    // Get field ID from table structure instead of name lookup
                    let table_fields = self.get_table_fields(table_id);
                    let field_name_to_id: HashMap<String, cfg::FieldId> =
                        table_fields.into_iter().collect();

                    if let Some(&field_id) = field_name_to_id.get(field_name) {
                        let tr_span = self.pick_span(&[
                            span,
                            self.builder.ast.expressions[table].span(),
                            key_expr_ids
                                .first()
                                .and_then(|id| self.builder.ast.expressions[*id].span()),
                        ]);
                        self.add_instruction(cfg::Instruction {
                            kind: cfg::InstructionKind::TableGet {
                                dest: field_var_id,
                                table: table_id,
                                keys: key_operands,
                                field: Some(field_id),
                            },
                            span: tr_span,
                        });
                    }
                }
            }
            _ => {
                // For other expressions, build the source and decompose
                panic!("Row assignment from non-TableRowAccess expression not supported");
            }
        }
    }

    /// Get table ID from a table expression
    fn get_table_id_from_expr(&self, table_expr_id: ast::ExprId) -> Option<cfg::TableId> {
        match &self.builder.ast.expressions[table_expr_id] {
            ast::Expression::Identifier {
                resolved_declarations,
                ..
            } => {
                if let Some(ast::IdentifierResolution::Table(table_id)) =
                    resolved_declarations.first()
                {
                    Some(self.builder.table_map[table_id])
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn new_variable(
        &mut self,
        name: String,
        ty: cfg::TypeId,
        kind: cfg::VariableKind,
    ) -> cfg::VariableId {
        self.builder
            .cfg
            .variables
            .alloc(cfg::Variable { name, ty, kind })
    }

    fn new_temporary(&mut self, ty: cfg::TypeId) -> cfg::VariableId {
        let name = format!("#tmp{}", self.builder.temp_counter);
        self.builder.temp_counter += 1;
        self.new_variable(name, ty, cfg::VariableKind::Temporary)
    }
}
