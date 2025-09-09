//! AST Builder
//!
//! This module provides the `AstBuilder` struct which converts Pest parse trees
//! into the AST defined in `mod.rs`. It handles:
//! - Converting Pest pairs to AST nodes
//! - Creating proper arena-based storage for all nodes
//! - Maintaining span information for error reporting
//! - Building the complete program structure

use pest::iterators::{Pair, Pairs};
use pest::{Parser, RuleType};
use pest_derive::Parser;

use crate::ast::errors::*;
use crate::ast::*;

// ============================================================================
// --- Pest Parser Definition
// ============================================================================

#[derive(Parser)]
#[grammar = "ast/grammar.pest"]
pub struct DslParser;

// ============================================================================
// --- AST Builder
// ============================================================================

pub struct AstBuilder {
    pub program: Program,
    errors: ErrorCollector,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self {
            program: Program::default(),
            errors: ErrorCollector::new(),
        }
    }

    /// Create a new builder with prelude (built-in types and operators)
    pub fn new_with_prelude() -> Self {
        let mut builder = Self::new();
        builder.add_prelude();
        builder
    }

    /// Add prelude declarations (built-in types and operators)
    fn add_prelude(&mut self) {
        self.add_builtin_types();
        self.add_builtin_operators();
        self.add_builtin_functions();
    }

    /// Add built-in primitive types
    fn add_builtin_types(&mut self) {
        let builtin_types = [
            ("int", "32-bit signed integer"),
            ("bool", "Boolean true/false value"),
            ("string", "UTF-8 string"),
            ("float", "64-bit floating point number"),
            ("void", "No return value"),
        ];

        for (name, _description) in builtin_types {
            let type_decl = TypeDecl {
                decorators: vec![self.intrinsic_decorator()],
                name: self.spanned_string(name.to_string()),
                generic_params: vec![],
                span: None,
            };

            let type_id = self.program.type_decls.alloc(type_decl);
            self.program.declarations.push(Declaration::Type(type_id));
        }
    }

    /// Add built-in operators
    fn add_builtin_operators(&mut self) {
        let binary_ops = [
            ("+", "Addition", true), // infix
            ("-", "Subtraction", true),
            ("*", "Multiplication", true),
            ("/", "Division", true),
            ("%", "Modulo", true),
            ("==", "Equality", true),
            ("!=", "Inequality", true),
            ("<", "Less than", true),
            ("<=", "Less than or equal", true),
            (">", "Greater than", true),
            (">=", "Greater than or equal", true),
            ("&&", "Logical AND", true),
            ("||", "Logical OR", true),
            ("=", "Assignment", true),
        ];

        let unary_ops = [
            ("-", "Negation", false), // prefix
            ("!", "Logical NOT", false),
        ];

        // Add binary operators
        for (symbol, _description, _is_infix) in binary_ops {
            let mut decorators = vec![self.intrinsic_decorator()];
            decorators.push(self.infix_decorator()); // @infix can be omitted but we include it for clarity

            let operator = CallableDecl {
                decorators,
                kind: CallableKind::Operator,
                name: self.spanned_string(symbol.to_string()),
                generic_params: vec![],
                params: vec![],    // Will be filled with generic params later
                return_type: None, // Will be inferred
                assumptions: vec![],
                body: None, // Built-in, no body
                span: None,
            };

            let func_id = self.program.functions.alloc(operator);
            self.program
                .declarations
                .push(Declaration::Callable(func_id));
        }

        // Add unary operators
        for (symbol, _description, _is_prefix) in unary_ops {
            let mut decorators = vec![self.intrinsic_decorator()];
            decorators.push(self.prefix_decorator());

            let operator = CallableDecl {
                decorators,
                kind: CallableKind::Operator,
                name: self.spanned_string(symbol.to_string()),
                generic_params: vec![],
                params: vec![],
                return_type: None,
                assumptions: vec![],
                body: None,
                span: None,
            };

            let func_id = self.program.functions.alloc(operator);
            self.program
                .declarations
                .push(Declaration::Callable(func_id));
        }
    }

    /// Add built-in functions
    fn add_builtin_functions(&mut self) {
        let builtin_functions = [
            ("print", "Print value to console"),
            ("len", "Get length of string or array"),
            ("min", "Minimum of two values"),
            ("max", "Maximum of two values"),
            ("abs", "Absolute value"),
        ];

        for (name, _description) in builtin_functions {
            let function = CallableDecl {
                decorators: vec![self.intrinsic_decorator()],
                kind: CallableKind::Function,
                name: self.spanned_string(name.to_string()),
                generic_params: vec![],
                params: vec![],
                return_type: None,
                assumptions: vec![],
                body: None,
                span: None,
            };

            let func_id = self.program.functions.alloc(function);
            self.program
                .declarations
                .push(Declaration::Callable(func_id));
        }
    }

    /// Create @intrinsic decorator
    fn intrinsic_decorator(&self) -> Spanned<String> {
        self.spanned_string("intrinsic".to_string())
    }

    /// Create @infix decorator
    fn infix_decorator(&self) -> Spanned<String> {
        self.spanned_string("infix".to_string())
    }

    /// Create @prefix decorator
    fn prefix_decorator(&self) -> Spanned<String> {
        self.spanned_string("prefix".to_string())
    }

    /// Create a spanned string with no span
    fn spanned_string(&self, value: String) -> Spanned<String> {
        Spanned { value, span: None }
    }

    /// Create a parse error
    fn create_parse_error(message: String, span: Option<Span>) -> SpannedError {
        SpannedError::new(AstError::Parse(ParseError::PestError { message }), span)
    }

    /// Parse a source string into an AST Program
    pub fn parse(source: &str) -> Results<Program> {
        let mut builder = Self::new_with_prelude();

        match DslParser::parse(Rule::program, source) {
            Ok(mut pairs) => {
                if let Some(program_pair) = pairs.next() {
                    builder.build_program(program_pair);
                }
                builder.errors.into_result(Some(builder.program))
            }
            Err(e) => {
                let error = Self::create_parse_error(e.to_string(), None);
                Results::failure(vec![error])
            }
        }
    }

    fn build_program(&mut self, pair: Pair<Rule>) {
        debug_assert_eq!(pair.as_rule(), Rule::program);

        for decl_pair in pair.into_inner() {
            match decl_pair.as_rule() {
                Rule::callable_declaration => {
                    if let Some(decl) = self.build_callable_declaration(decl_pair) {
                        self.program.declarations.push(Declaration::Callable(decl));
                    }
                }
                Rule::type_declaration => {
                    if let Some(decl) = self.build_type_declaration(decl_pair) {
                        self.program.declarations.push(Declaration::Type(decl));
                    }
                }
                Rule::const_declaration => {
                    if let Some(decl) = self.build_const_declaration(decl_pair) {
                        self.program.declarations.push(Declaration::Const(decl));
                    }
                }
                Rule::table_declaration => {
                    if let Some(decl) = self.build_table_declaration(decl_pair) {
                        self.program.declarations.push(Declaration::Table(decl));
                    }
                }
                Rule::EOI => break,
                _ => {
                    self.error(
                        format!("Unexpected rule in program: {:?}", decl_pair.as_rule()),
                        self.span_from_pair(&decl_pair),
                    );
                }
            }
        }
    }

    // ========================================================================
    // --- Declaration Builders
    // ========================================================================

    fn build_callable_declaration(&mut self, pair: Pair<Rule>) -> Option<FunctionId> {
        debug_assert_eq!(pair.as_rule(), Rule::callable_declaration);

        let span = self.span_from_pair(&pair);
        let mut decorators = Vec::new();
        let mut kind = CallableKind::Function;
        let mut name = String::new();
        let mut generic_params = Vec::new();
        let mut params = Vec::new();
        let mut return_type = None;
        let mut assumptions = Vec::new();
        let mut body = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::decorator => {
                    decorators.push(self.spanned_string_from_pair(inner_pair));
                }
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                    // Determine kind based on context - will be set by previous keyword
                }
                Rule::operator_symbol => {
                    name = inner_pair.as_str().to_string();
                    kind = CallableKind::Operator;
                }
                Rule::generic_param_list => {
                    generic_params = self.build_generic_param_list(inner_pair);
                }
                Rule::parameter_list => {
                    params = self.build_parameter_list(inner_pair);
                }
                Rule::r#type => {
                    return_type = self.build_type(inner_pair);
                }
                Rule::assume_statement => {
                    if let Some(expr) = self.build_assume_statement(inner_pair) {
                        assumptions.push(expr);
                    }
                }
                Rule::block => {
                    body = self.build_block(inner_pair);
                }
                _ => {
                    // Handle keywords to determine callable kind
                    match inner_pair.as_str() {
                        "function" => kind = CallableKind::Function,
                        "partition" => kind = CallableKind::Partition,
                        "transaction" => kind = CallableKind::Transaction,
                        "operator" => kind = CallableKind::Operator,
                        _ => {}
                    }
                }
            }
        }

        let decl = CallableDecl {
            decorators,
            kind,
            name: Spanned { value: name, span },
            generic_params,
            params,
            return_type,
            assumptions,
            body,
            span,
        };

        Some(self.program.functions.alloc(decl))
    }

    fn build_type_declaration(&mut self, pair: Pair<Rule>) -> Option<TypeDeclId> {
        debug_assert_eq!(pair.as_rule(), Rule::type_declaration);

        let span = self.span_from_pair(&pair);
        let mut decorators = Vec::new();
        let mut name = String::new();
        let mut generic_params = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::decorator => {
                    decorators.push(self.spanned_string_from_pair(inner_pair));
                }
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::generic_param_list => {
                    generic_params = self.build_generic_param_list(inner_pair);
                }
                _ => {}
            }
        }

        let decl = TypeDecl {
            decorators,
            name: Spanned { value: name, span },
            generic_params,
            span,
        };

        Some(self.program.type_decls.alloc(decl))
    }

    fn build_const_declaration(&mut self, pair: Pair<Rule>) -> Option<ConstId> {
        debug_assert_eq!(pair.as_rule(), Rule::const_declaration);

        let span = self.span_from_pair(&pair);
        let mut name = String::new();
        let mut ty = None;
        let mut value = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::r#type => {
                    ty = self.build_type(inner_pair);
                }
                Rule::expression => {
                    value = self.build_expression(inner_pair);
                }
                _ => {}
            }
        }

        if let (Some(ty), Some(value)) = (ty, value) {
            let decl = ConstDecl {
                name: Spanned { value: name, span },
                ty,
                value,
                span,
            };
            Some(self.program.const_decls.alloc(decl))
        } else {
            self.error("Incomplete const declaration".to_string(), span);
            None
        }
    }

    fn build_table_declaration(&mut self, pair: Pair<Rule>) -> Option<TableId> {
        debug_assert_eq!(pair.as_rule(), Rule::table_declaration);

        let span = self.span_from_pair(&pair);
        let mut name = String::new();
        let mut fields = Vec::new();
        let mut nodes = Vec::new();
        let mut invariants = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::table_field => {
                    self.build_table_field(inner_pair, &mut fields, &mut nodes, &mut invariants);
                }
                _ => {}
            }
        }

        let decl = TableDecl {
            name: Spanned { value: name, span },
            fields,
            nodes,
            invariants,
            span,
        };

        Some(self.program.table_decls.alloc(decl))
    }

    fn build_table_field(
        &mut self,
        pair: Pair<Rule>,
        fields: &mut Vec<TableField>,
        nodes: &mut Vec<TableNode>,
        invariants: &mut Vec<ExprId>,
    ) {
        debug_assert_eq!(pair.as_rule(), Rule::table_field);

        let span = self.span_from_pair(&pair);
        let mut is_primary = false;
        let mut is_node = false;
        let mut is_invariant = false;
        let mut name = String::new();
        let mut ty = None;
        let mut args = Vec::new();
        let mut expr = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if name.is_empty() {
                        name = inner_pair.as_str().to_string();
                    }
                }
                Rule::r#type => {
                    ty = self.build_type(inner_pair);
                }
                Rule::expression => {
                    expr = self.build_expression(inner_pair);
                }
                Rule::expression_list => {
                    args = self.build_expression_list(inner_pair);
                }
                _ => match inner_pair.as_str() {
                    "primary" => is_primary = true,
                    "node" => is_node = true,
                    "invariant" => is_invariant = true,
                    _ => {}
                },
            }
        }

        if is_node {
            let node = TableNode {
                name: Spanned { value: name, span },
                args,
                resolved_partition: None,
                span,
            };
            nodes.push(node);
        } else if is_invariant {
            if let Some(expr) = expr {
                invariants.push(expr);
            }
        } else if let Some(ty) = ty {
            let field = TableField {
                is_primary,
                name: Spanned { value: name, span },
                ty,
                span,
            };
            fields.push(field);
        }
    }

    // ========================================================================
    // --- Type Builders
    // ========================================================================

    fn build_type(&mut self, pair: Pair<Rule>) -> Option<TypeId> {
        debug_assert_eq!(pair.as_rule(), Rule::r#type);

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::primitive_type => {
                    return self.build_primitive_type(inner_pair);
                }
                Rule::generic_type => {
                    return self.build_generic_type(inner_pair);
                }
                Rule::function_type => {
                    return self.build_function_type(inner_pair);
                }
                _ => {}
            }
        }
        None
    }

    fn build_primitive_type(&mut self, pair: Pair<Rule>) -> Option<TypeId> {
        debug_assert_eq!(pair.as_rule(), Rule::primitive_type);

        let span = self.span_from_pair(&pair);
        let identifier = self.build_identifier_from_string(pair.as_str().to_string(), span);

        let ty = Type::Named(identifier);
        Some(self.program.types.alloc(ty))
    }

    fn build_generic_type(&mut self, pair: Pair<Rule>) -> Option<TypeId> {
        debug_assert_eq!(pair.as_rule(), Rule::generic_type);

        let span = self.span_from_pair(&pair);
        let mut base = None;
        let mut args = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    base = Some(self.build_identifier_from_string(
                        inner_pair.as_str().to_string(),
                        self.span_from_pair(&inner_pair),
                    ));
                }
                Rule::type_list => {
                    args = self.build_type_list(inner_pair);
                }
                _ => {}
            }
        }

        if let Some(base) = base {
            let ty = Type::Generic { base, args, span };
            Some(self.program.types.alloc(ty))
        } else {
            None
        }
    }

    fn build_function_type(&mut self, pair: Pair<Rule>) -> Option<TypeId> {
        debug_assert_eq!(pair.as_rule(), Rule::function_type);

        let span = self.span_from_pair(&pair);
        let mut params = Vec::new();
        let mut return_type = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_list => {
                    params = self.build_type_list(inner_pair);
                }
                Rule::r#type => {
                    return_type = self.build_type(inner_pair);
                }
                _ => {}
            }
        }

        if let Some(return_type) = return_type {
            let ty = Type::Function {
                params,
                return_type,
                span,
            };
            Some(self.program.types.alloc(ty))
        } else {
            None
        }
    }

    fn build_type_list(&mut self, pair: Pair<Rule>) -> Vec<TypeId> {
        debug_assert_eq!(pair.as_rule(), Rule::type_list);

        let mut types = Vec::new();
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::r#type {
                if let Some(ty) = self.build_type(inner_pair) {
                    types.push(ty);
                }
            }
        }
        types
    }

    // ========================================================================
    // --- Statement Builders
    // ========================================================================

    fn build_block(&mut self, pair: Pair<Rule>) -> Option<BlockId> {
        debug_assert_eq!(pair.as_rule(), Rule::block);

        let span = self.span_from_pair(&pair);
        let mut statements = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::statement {
                if let Some(stmt) = self.build_statement(inner_pair) {
                    statements.push(stmt);
                }
            }
        }

        let block = Block { statements, span };
        Some(self.program.blocks.alloc(block))
    }

    fn build_statement(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::var_declaration => {
                    if let Some(var_id) = self.build_var_declaration(inner_pair) {
                        let stmt = Statement::VarDecl(var_id);
                        return Some(self.program.statements.alloc(stmt));
                    }
                }
                Rule::if_statement => {
                    return self.build_if_statement(inner_pair);
                }
                Rule::for_statement => {
                    return self.build_for_statement(inner_pair);
                }
                Rule::return_statement => {
                    return self.build_return_statement(inner_pair);
                }
                Rule::assert_statement => {
                    return self.build_assert_statement(inner_pair);
                }
                Rule::hop_block => {
                    return self.build_hop_block(inner_pair);
                }
                Rule::hops_for_loop => {
                    return self.build_hops_for_loop(inner_pair);
                }
                Rule::block => {
                    if let Some(block_id) = self.build_block(inner_pair) {
                        let stmt = Statement::Block(block_id);
                        return Some(self.program.statements.alloc(stmt));
                    }
                }
                Rule::expression_statement => {
                    return self.build_expression_statement(inner_pair);
                }
                _ => {}
            }
        }
        None
    }

    fn build_var_declaration(&mut self, pair: Pair<Rule>) -> Option<VarId> {
        debug_assert_eq!(pair.as_rule(), Rule::var_declaration);

        let span = self.span_from_pair(&pair);
        let mut name = String::new();
        let mut ty = None;
        let mut init = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::r#type => {
                    ty = self.build_type(inner_pair);
                }
                Rule::expression => {
                    init = self.build_expression(inner_pair);
                }
                _ => {}
            }
        }

        let var_decl = VarDecl {
            name: Spanned { value: name, span },
            ty,
            init,
            span,
        };

        Some(self.program.var_decls.alloc(var_decl))
    }

    fn build_if_statement(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::if_statement);

        let span = self.span_from_pair(&pair);
        let mut condition = None;
        let mut then_block = None;
        let mut else_block = None;

        let mut pairs = pair.into_inner();

        // First should be the condition
        if let Some(expr_pair) = pairs.next() {
            condition = self.build_expression(expr_pair);
        }

        // Second should be the then block
        if let Some(block_pair) = pairs.next() {
            then_block = self.build_block(block_pair);
        }

        // Third (optional) is the else block
        if let Some(else_pair) = pairs.next() {
            else_block = self.build_block(else_pair);
        }

        if let (Some(condition), Some(then_block)) = (condition, then_block) {
            let stmt = Statement::If {
                condition,
                then_block,
                else_block,
                span,
            };
            Some(self.program.statements.alloc(stmt))
        } else {
            None
        }
    }

    fn build_for_statement(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::for_statement);

        let span = self.span_from_pair(&pair);
        let mut init = None;
        let mut condition = None;
        let mut update = None;
        let mut body = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::var_declaration => {
                    if let Some(var_id) = self.build_var_declaration(inner_pair) {
                        init = Some(ForInit::VarDecl(var_id));
                    }
                }
                Rule::expression_statement => {
                    // This could be init or update depending on position
                    if let Some(expr) = self.build_expression_from_statement(inner_pair) {
                        if init.is_none() {
                            init = Some(ForInit::Expression(expr));
                        } else if condition.is_some() && update.is_none() {
                            update = Some(expr);
                        }
                    }
                }
                Rule::expression => {
                    if condition.is_none() {
                        condition = self.build_expression(inner_pair);
                    } else if update.is_none() {
                        update = self.build_expression(inner_pair);
                    }
                }
                Rule::block => {
                    body = self.build_block(inner_pair);
                }
                _ => {}
            }
        }

        if let Some(body) = body {
            let stmt = Statement::For {
                init,
                condition,
                update,
                body,
                span,
            };
            Some(self.program.statements.alloc(stmt))
        } else {
            None
        }
    }

    fn build_return_statement(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::return_statement);

        let span = self.span_from_pair(&pair);
        let mut value = None;

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                value = self.build_expression(inner_pair);
                break;
            }
        }

        let stmt = Statement::Return { value, span };
        Some(self.program.statements.alloc(stmt))
    }

    fn build_assert_statement(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::assert_statement);

        let span = self.span_from_pair(&pair);
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                if let Some(expr) = self.build_expression(inner_pair) {
                    let stmt = Statement::Assert { expr, span };
                    return Some(self.program.statements.alloc(stmt));
                }
            }
        }
        None
    }

    fn build_hop_block(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::hop_block);

        let span = self.span_from_pair(&pair);
        let mut decorators = Vec::new();
        let mut body = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::decorator => {
                    decorators.push(self.spanned_string_from_pair(inner_pair));
                }
                Rule::block => {
                    body = self.build_block(inner_pair);
                }
                _ => {}
            }
        }

        if let Some(body) = body {
            let stmt = Statement::Hop {
                decorators,
                body,
                span,
            };
            Some(self.program.statements.alloc(stmt))
        } else {
            None
        }
    }

    fn build_hops_for_loop(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::hops_for_loop);

        let span = self.span_from_pair(&pair);
        let mut decorators = Vec::new();
        let mut var: Option<VarId> = None;
        let mut start = None;
        let mut end = None;
        let mut body = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::decorator => {
                    decorators.push(self.spanned_string_from_pair(inner_pair));
                }
                Rule::identifier => {
                    // This should be part of the variable declaration
                }
                Rule::r#type => {
                    // This should be part of the variable declaration
                }
                Rule::expression => {
                    if start.is_none() {
                        start = self.build_expression(inner_pair);
                    } else if end.is_none() {
                        end = self.build_expression(inner_pair);
                    }
                }
                Rule::block => {
                    body = self.build_block(inner_pair);
                }
                _ => {}
            }
        }

        // Need to build the variable from the identifier and type
        if let (Some(start), Some(end), Some(body)) = (start, end, body) {
            // Create a dummy variable for now - would need better parsing to extract name and type
            let var_decl = VarDecl {
                name: Spanned {
                    value: "loop_var".to_string(),
                    span,
                },
                ty: None,
                init: None,
                span,
            };
            let var_id = self.program.var_decls.alloc(var_decl);

            let stmt = Statement::HopsFor {
                decorators,
                var: var_id,
                start,
                end,
                body,
                span,
            };
            Some(self.program.statements.alloc(stmt))
        } else {
            None
        }
    }

    fn build_expression_statement(&mut self, pair: Pair<Rule>) -> Option<StmtId> {
        debug_assert_eq!(pair.as_rule(), Rule::expression_statement);

        let span = self.span_from_pair(&pair);
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                if let Some(expr) = self.build_expression(inner_pair) {
                    let stmt = Statement::Expression { expr, span };
                    return Some(self.program.statements.alloc(stmt));
                }
            }
        }
        None
    }

    // ========================================================================
    // --- Expression Builders
    // ========================================================================

    fn build_expression(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::expression);

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::assignment {
                return self.build_assignment(inner_pair);
            }
        }
        None
    }

    fn build_assignment(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::assignment);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(lhs_pair) = pairs.next() {
            let lhs = self.build_level10_expr(lhs_pair)?;

            if let Some(_eq_pair) = pairs.next() {
                // This is an assignment
                if let Some(rhs_pair) = pairs.next() {
                    let rhs = self.build_assignment(rhs_pair)?;
                    let expr = Expression::Assignment { lhs, rhs, span };
                    return Some(self.program.expressions.alloc(expr));
                }
            } else {
                // Just a regular expression
                return Some(lhs);
            }
        }
        None
    }

    fn build_level10_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level10_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let mut left = self.build_level9_expr(first_pair)?;

            while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level9_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                left = self.program.expressions.alloc(expr);
            }

            Some(left)
        } else {
            None
        }
    }

    fn build_level9_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level9_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let left = self.build_level8_expr(first_pair)?;

            if let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level9_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                Some(self.program.expressions.alloc(expr))
            } else {
                Some(left)
            }
        } else {
            None
        }
    }

    fn build_level8_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level8_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let mut left = self.build_level7_expr(first_pair)?;

            while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level7_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                left = self.program.expressions.alloc(expr);
            }

            Some(left)
        } else {
            None
        }
    }

    fn build_level7_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level7_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let left = self.build_level6_expr(first_pair)?;

            if let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level7_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                Some(self.program.expressions.alloc(expr))
            } else {
                Some(left)
            }
        } else {
            None
        }
    }

    fn build_level6_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level6_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let mut left = self.build_level5_expr(first_pair)?;

            while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level5_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                left = self.program.expressions.alloc(expr);
            }

            Some(left)
        } else {
            None
        }
    }

    fn build_level5_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level5_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let left = self.build_level4_expr(first_pair)?;

            if let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level4_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                Some(self.program.expressions.alloc(expr))
            } else {
                Some(left)
            }
        } else {
            None
        }
    }

    fn build_level4_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level4_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let mut left = self.build_level3_expr(first_pair)?;

            while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_level3_expr(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                left = self.program.expressions.alloc(expr);
            }

            Some(left)
        } else {
            None
        }
    }

    fn build_level3_expr(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::level3_expr);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            let mut left = self.build_prefix(first_pair)?;

            while let (Some(op_pair), Some(right_pair)) = (pairs.next(), pairs.next()) {
                let right = self.build_prefix(right_pair)?;
                let op = self.spanned_string_from_pair(op_pair);
                let expr = Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                };
                left = self.program.expressions.alloc(expr);
            }

            Some(left)
        } else {
            None
        }
    }

    fn build_prefix(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::prefix);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(first_pair) = pairs.next() {
            match first_pair.as_rule() {
                Rule::prefix_op => {
                    if let Some(expr_pair) = pairs.next() {
                        let expr = self.build_prefix(expr_pair)?;
                        let op = self.spanned_string_from_pair(first_pair);
                        let unary_expr = Expression::Unary { op, expr, span };
                        Some(self.program.expressions.alloc(unary_expr))
                    } else {
                        None
                    }
                }
                Rule::postfix => self.build_postfix(first_pair),
                _ => None,
            }
        } else {
            None
        }
    }

    fn build_postfix(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::postfix);

        let span = self.span_from_pair(&pair);
        let mut pairs = pair.into_inner();

        if let Some(primary_pair) = pairs.next() {
            let mut expr = self.build_primary(primary_pair)?;

            for postfix_pair in pairs {
                match postfix_pair.as_rule() {
                    Rule::call => {
                        let args = self.build_call_args(postfix_pair);
                        let call_expr = Expression::Call {
                            callee: expr,
                            args,
                            resolved_callable: None,
                            span,
                        };
                        expr = self.program.expressions.alloc(call_expr);
                    }
                    Rule::member_access => {
                        if let Some(member) = self.build_member_access(postfix_pair) {
                            let member_expr = Expression::MemberAccess {
                                object: expr,
                                member,
                                resolved_field: None,
                                span,
                            };
                            expr = self.program.expressions.alloc(member_expr);
                        }
                    }
                    Rule::table_row_access => {
                        let key_values = self.build_table_row_access(postfix_pair);
                        let access_expr = Expression::TableRowAccess {
                            table: expr,
                            key_values,
                            span,
                        };
                        expr = self.program.expressions.alloc(access_expr);
                    }
                    _ => {}
                }
            }

            Some(expr)
        } else {
            None
        }
    }

    fn build_primary(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::primary);

        let span = self.span_from_pair(&pair);

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::literal => {
                    return self.build_literal(inner_pair);
                }
                Rule::identifier => {
                    let identifier = self.build_identifier_from_string(
                        inner_pair.as_str().to_string(),
                        self.span_from_pair(&inner_pair),
                    );
                    let expr = Expression::Identifier(identifier);
                    return Some(self.program.expressions.alloc(expr));
                }
                Rule::expression => {
                    if let Some(inner_expr) = self.build_expression(inner_pair) {
                        let grouped_expr = Expression::Grouped {
                            expr: inner_expr,
                            span,
                        };
                        return Some(self.program.expressions.alloc(grouped_expr));
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn build_literal(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::literal);

        let span = self.span_from_pair(&pair);

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::primitive_literal => {
                    return self.build_primitive_literal(inner_pair, span);
                }
                Rule::list_literal => {
                    return self.build_list_literal(inner_pair, span);
                }
                Rule::row_literal => {
                    return self.build_row_literal(inner_pair, span);
                }
                _ => {}
            }
        }
        None
    }

    fn build_primitive_literal(&mut self, pair: Pair<Rule>, span: Option<Span>) -> Option<ExprId> {
        for inner_pair in pair.into_inner() {
            let literal = match inner_pair.as_rule() {
                Rule::integer => Literal::Integer(inner_pair.as_str().to_string()),
                Rule::float => Literal::Float(inner_pair.as_str().to_string()),
                Rule::string => Literal::String(inner_pair.as_str().to_string()),
                Rule::bool => {
                    let value = inner_pair.as_str() == "true";
                    Literal::Bool(value)
                }
                _ => continue,
            };

            let expr = Expression::Literal {
                value: literal,
                span,
            };
            return Some(self.program.expressions.alloc(expr));
        }
        None
    }

    fn build_list_literal(&mut self, pair: Pair<Rule>, span: Option<Span>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::list_literal);

        let elements = self.build_expression_list_from_pair(pair);
        let literal = Literal::List(elements);
        let expr = Expression::Literal {
            value: literal,
            span,
        };
        Some(self.program.expressions.alloc(expr))
    }

    fn build_row_literal(&mut self, pair: Pair<Rule>, span: Option<Span>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::row_literal);

        let key_values = self.build_key_value_pairs(pair);
        let literal = Literal::RowLiteral(key_values);
        let expr = Expression::Literal {
            value: literal,
            span,
        };
        Some(self.program.expressions.alloc(expr))
    }

    // ========================================================================
    // --- Helper Functions
    // ========================================================================

    fn build_identifier_from_string(&self, name: String, span: Option<Span>) -> Identifier {
        Identifier {
            name,
            span,
            resolved: None,
            resolved_type: None,
        }
    }

    fn build_generic_param_list(&mut self, pair: Pair<Rule>) -> Vec<GenericParamId> {
        debug_assert_eq!(pair.as_rule(), Rule::generic_param_list);

        let mut params = Vec::new();
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::identifier {
                let span = self.span_from_pair(&inner_pair);
                let param = GenericParam {
                    name: Spanned {
                        value: inner_pair.as_str().to_string(),
                        span,
                    },
                    span,
                };
                let param_id = self.program.generic_params.alloc(param);
                params.push(param_id);
            }
        }
        params
    }

    fn build_parameter_list(&mut self, pair: Pair<Rule>) -> Vec<ParamId> {
        debug_assert_eq!(pair.as_rule(), Rule::parameter_list);

        let mut params = Vec::new();
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::parameter {
                if let Some(param_id) = self.build_parameter(inner_pair) {
                    params.push(param_id);
                }
            }
        }
        params
    }

    fn build_parameter(&mut self, pair: Pair<Rule>) -> Option<ParamId> {
        debug_assert_eq!(pair.as_rule(), Rule::parameter);

        let span = self.span_from_pair(&pair);
        let mut name = String::new();
        let mut ty = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                Rule::r#type => {
                    ty = self.build_type(inner_pair);
                }
                _ => {}
            }
        }

        if let Some(ty) = ty {
            let param = Parameter {
                name: Spanned { value: name, span },
                ty,
                span,
            };
            Some(self.program.params.alloc(param))
        } else {
            None
        }
    }

    fn build_assume_statement(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::assume_statement);

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                return self.build_expression(inner_pair);
            }
        }
        None
    }

    fn build_expression_list(&mut self, pair: Pair<Rule>) -> Vec<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::expression_list);

        let mut expressions = Vec::new();
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                if let Some(expr) = self.build_expression(inner_pair) {
                    expressions.push(expr);
                }
            }
        }
        expressions
    }

    fn build_expression_list_from_pair(&mut self, pair: Pair<Rule>) -> Vec<ExprId> {
        let mut expressions = Vec::new();
        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression_list {
                return self.build_expression_list(inner_pair);
            } else if inner_pair.as_rule() == Rule::expression {
                if let Some(expr) = self.build_expression(inner_pair) {
                    expressions.push(expr);
                }
            }
        }
        expressions
    }

    fn build_expression_from_statement(&mut self, pair: Pair<Rule>) -> Option<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::expression_statement);

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression {
                return self.build_expression(inner_pair);
            }
        }
        None
    }

    fn build_call_args(&mut self, pair: Pair<Rule>) -> Vec<ExprId> {
        debug_assert_eq!(pair.as_rule(), Rule::call);

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::expression_list {
                return self.build_expression_list(inner_pair);
            }
        }
        Vec::new()
    }

    fn build_member_access(&mut self, pair: Pair<Rule>) -> Option<Spanned<String>> {
        debug_assert_eq!(pair.as_rule(), Rule::member_access);

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::identifier {
                let span = self.span_from_pair(&inner_pair);
                return Some(Spanned {
                    value: inner_pair.as_str().to_string(),
                    span,
                });
            }
        }
        None
    }

    fn build_table_row_access(&mut self, pair: Pair<Rule>) -> Vec<KeyValue> {
        debug_assert_eq!(pair.as_rule(), Rule::table_row_access);

        self.build_key_value_pairs(pair)
    }

    fn build_key_value_pairs(&mut self, pair: Pair<Rule>) -> Vec<KeyValue> {
        let mut key_values = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::key_value_pair {
                if let Some(kv) = self.build_key_value_pair(inner_pair) {
                    key_values.push(kv);
                }
            }
        }

        key_values
    }

    fn build_key_value_pair(&mut self, pair: Pair<Rule>) -> Option<KeyValue> {
        debug_assert_eq!(pair.as_rule(), Rule::key_value_pair);

        let span = self.span_from_pair(&pair);
        let mut key = None;
        let mut value = None;

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    key = Some(Spanned {
                        value: inner_pair.as_str().to_string(),
                        span: self.span_from_pair(&inner_pair),
                    });
                }
                Rule::expression => {
                    value = self.build_expression(inner_pair);
                }
                _ => {}
            }
        }

        if let (Some(key), Some(value)) = (key, value) {
            Some(KeyValue {
                key,
                value,
                resolved_field: None,
                span,
            })
        } else {
            None
        }
    }

    fn spanned_string_from_pair(&self, pair: Pair<Rule>) -> Spanned<String> {
        let span = self.span_from_pair(&pair);
        Spanned {
            value: pair.as_str().to_string(),
            span,
        }
    }

    fn span_from_pair(&self, pair: &Pair<Rule>) -> Option<Span> {
        let span = pair.as_span();
        let (line, column) = span.start_pos().line_col();
        Some(Span {
            start: span.start(),
            end: span.end(),
            line,
            column,
        })
    }

    fn error(&mut self, message: String, span: Option<Span>) {
        self.errors
            .add_error(AstError::Parse(ParseError::PestError { message }), span);
    }
}

impl Default for AstBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Parse a source string into an AST Program  
pub fn parse_program(source: &str) -> Results<Program> {
    AstBuilder::parse(source)
}

/// Main entry point for parsing and initial analysis
pub fn parse_and_analyze(source: &str) -> Results<Program> {
    parse_program(source)
}
