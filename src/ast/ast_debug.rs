//! AST Debug and Pretty Printing
//!
//! This module provides enhanced debug output for AST structures.
//! It excludes arena internals and focuses on the logical structure
//! of the AST for development and debugging purposes.

use std::fmt;
use std::collections::HashMap;

use crate::ast::*;

// ============================================================================
// --- Debug Configuration
// ============================================================================

/// Configuration for AST debug output
#[derive(Debug, Clone)]
pub struct DebugConfig {
    /// Show spans in output
    pub show_spans: bool,
    
    /// Show resolved references
    pub show_resolved: bool,
    
    /// Show type annotations
    pub show_types: bool,
    
    /// Maximum depth for nested structures
    pub max_depth: usize,
    
    /// Indentation string
    pub indent: String,
}

impl Default for DebugConfig {
    fn default() -> Self {
        Self {
            show_spans: false,
            show_resolved: true,
            show_types: true,
            max_depth: 20,
            indent: "  ".to_string(),
        }
    }
}

// ============================================================================
// --- AST Debug Printer
// ============================================================================

pub struct AstDebugPrinter<'a> {
    program: &'a Program,
    config: DebugConfig,
    current_depth: usize,
}

impl<'a> AstDebugPrinter<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            config: DebugConfig::default(),
            current_depth: 0,
        }
    }

    pub fn with_config(program: &'a Program, config: DebugConfig) -> Self {
        Self {
            program,
            config,
            current_depth: 0,
        }
    }

    /// Print the entire program
    pub fn print_program(&self) -> String {
        let mut output = String::new();
        
        output.push_str("Program {\n");
        
        // Print declarations
        if !self.program.declarations.is_empty() {
            output.push_str(&format!("{}declarations: [\n", self.config.indent));
            for (i, decl) in self.program.declarations.iter().enumerate() {
                output.push_str(&format!("{}{}[{}] {}\n", 
                    self.config.indent.repeat(2), 
                    self.config.indent,
                    i,
                    self.print_declaration(decl, 2)
                ));
            }
            output.push_str(&format!("{}],\n", self.config.indent));
        }
        
        output.push_str("}\n");
        output
    }

    /// Print a single declaration
    pub fn print_declaration(&self, decl: &Declaration, depth: usize) -> String {
        if depth > self.config.max_depth {
            return "...".to_string();
        }

        let indent = self.config.indent.repeat(depth);
        
        match decl {
            Declaration::Callable(id) => {
                let callable = &self.program.callables[*id];
                format!("Callable({}) {{ name: \"{}\", {} }}", 
                    id.index(),
                    callable.name.value,
                    self.format_callable_details(callable, depth + 1)
                )
            }
            
            Declaration::Type(id) => {
                let type_decl = &self.program.type_decls[*id];
                format!("Type({}) {{ name: \"{}\", {} }}", 
                    id.index(),
                    type_decl.name.value,
                    self.format_type_decl_details(type_decl, depth + 1)
                )
            }
            
            Declaration::Const(id) => {
                let const_decl = &self.program.const_decls[*id];
                format!("Const({}) {{ name: \"{}\", {} }}", 
                    id.index(),
                    const_decl.name.value,
                    self.format_const_details(const_decl, depth + 1)
                )
            }
            
            Declaration::Table(id) => {
                let table = &self.program.tables[*id];
                format!("Table({}) {{ name: \"{}\", {} }}", 
                    id.index(),
                    table.name.value,
                    self.format_table_details(table, depth + 1)
                )
            }
        }
    }

    // ========================================================================
    // --- Declaration Details
    // ========================================================================

    fn format_callable_details(&self, callable: &CallableDecl, depth: usize) -> String {
        let indent = self.config.indent.repeat(depth);
        let mut details = Vec::new();
        
        // Parameters
        if !callable.params.is_empty() {
            let param_strings: Vec<String> = callable.params.iter()
                .map(|p| format!("{}: {}", p.name.value, self.print_type_ref(&p.param_type, depth + 1)))
                .collect();
            details.push(format!("params: [{}]", param_strings.join(", ")));
        }
        
        // Return type
        if let Some(ref ret_type) = callable.return_type {
            details.push(format!("return_type: {}", self.print_type_ref(ret_type, depth + 1)));
        }
        
        // Body
        if let Some(body_id) = callable.body {
            details.push(format!("body: {}", self.print_statement(&body_id, depth + 1)));
        }
        
        // Span
        if self.config.show_spans {
            if let Some(span) = callable.span {
                details.push(format!("span: {:?}", span));
            }
        }
        
        details.join(", ")
    }

    fn format_type_decl_details(&self, type_decl: &TypeDecl, depth: usize) -> String {
        let mut details = Vec::new();
        
        // Type definition
        details.push(format!("type_def: {}", self.print_type_ref(&type_decl.type_def, depth + 1)));
        
        // Span
        if self.config.show_spans {
            if let Some(span) = type_decl.span {
                details.push(format!("span: {:?}", span));
            }
        }
        
        details.join(", ")
    }

    fn format_const_details(&self, const_decl: &ConstDecl, depth: usize) -> String {
        let mut details = Vec::new();
        
        // Type annotation
        if let Some(ref type_ann) = const_decl.type_annotation {
            details.push(format!("type: {}", self.print_type_ref(type_ann, depth + 1)));
        }
        
        // Value
        details.push(format!("value: {}", self.print_expression(&const_decl.value, depth + 1)));
        
        // Span
        if self.config.show_spans {
            if let Some(span) = const_decl.span {
                details.push(format!("span: {:?}", span));
            }
        }
        
        details.join(", ")
    }

    fn format_table_details(&self, table: &TableDecl, depth: usize) -> String {
        let mut details = Vec::new();
        
        // Fields
        if !table.fields.is_empty() {
            let field_strings: Vec<String> = table.fields.iter()
                .map(|f| format!("{}: {}", f.name.value, self.print_type_ref(&f.field_type, depth + 1)))
                .collect();
            details.push(format!("fields: [{}]", field_strings.join(", ")));
        }
        
        // Constraints
        if !table.constraints.is_empty() {
            let constraint_strings: Vec<String> = table.constraints.iter()
                .map(|c| self.print_table_constraint(c, depth + 1))
                .collect();
            details.push(format!("constraints: [{}]", constraint_strings.join(", ")));
        }
        
        // Span
        if self.config.show_spans {
            if let Some(span) = table.span {
                details.push(format!("span: {:?}", span));
            }
        }
        
        details.join(", ")
    }

    // ========================================================================
    // --- Expression Printing
    // ========================================================================

    pub fn print_expression(&self, expr_id: &ExprId, depth: usize) -> String {
        if depth > self.config.max_depth {
            return "...".to_string();
        }

        let expr = &self.program.expressions[*expr_id];
        
        match expr {
            Expression::Literal { value, span } => {
                let mut result = format!("Literal({})", self.print_literal(value));
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Expression::Identifier(identifier) => {
                let mut result = format!("Identifier(\"{}\")", identifier.name);
                
                if self.config.show_resolved {
                    if let Some(ref resolved) = identifier.resolved {
                        result.push_str(&format!(" -> {:?}", resolved));
                    }
                }
                
                if self.config.show_spans && identifier.span.is_some() {
                    result.push_str(&format!(" @{:?}", identifier.span));
                }
                
                result
            }
            
            Expression::Binary { left, op, right, span } => {
                let left_str = self.print_expression(left, depth + 1);
                let right_str = self.print_expression(right, depth + 1);
                let mut result = format!("Binary {{ left: {}, op: \"{}\", right: {} }}", 
                    left_str, op.value, right_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                
                result
            }
            
            Expression::Unary { op, expr, span } => {
                let expr_str = self.print_expression(expr, depth + 1);
                let mut result = format!("Unary {{ op: \"{}\", expr: {} }}", op.value, expr_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                
                result
            }
            
            Expression::Call { callee, args, span } => {
                let callee_str = self.print_expression(callee, depth + 1);
                let arg_strings: Vec<String> = args.iter()
                    .map(|arg| self.print_expression(arg, depth + 1))
                    .collect();
                let mut result = format!("Call {{ callee: {}, args: [{}] }}", 
                    callee_str, arg_strings.join(", "));
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                
                result
            }
            
            Expression::FieldAccess { base, field, span } => {
                let base_str = self.print_expression(base, depth + 1);
                let mut result = format!("FieldAccess {{ base: {}, field: \"{}\" }}", 
                    base_str, field.value);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                
                result
            }
            
            Expression::IndexAccess { base, index, span } => {
                let base_str = self.print_expression(base, depth + 1);
                let index_str = self.print_expression(index, depth + 1);
                let mut result = format!("IndexAccess {{ base: {}, index: {} }}", 
                    base_str, index_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                
                result
            }
            
            Expression::Grouped { expr, span } => {
                let expr_str = self.print_expression(expr, depth + 1);
                let mut result = format!("Grouped({})", expr_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                
                result
            }
        }
    }

    fn print_literal(&self, literal: &Literal) -> String {
        match literal {
            Literal::Integer(s) => format!("Integer({})", s),
            Literal::Float(s) => format!("Float({})", s),
            Literal::String(s) => format!("String({})", s),
            Literal::Bool(b) => format!("Bool({})", b),
            Literal::List(elements) => {
                let element_strings: Vec<String> = elements.iter()
                    .map(|e| self.print_expression(e, self.current_depth + 1))
                    .collect();
                format!("List([{}])", element_strings.join(", "))
            }
            Literal::RowLiteral(key_values) => {
                let kv_strings: Vec<String> = key_values.iter()
                    .map(|kv| format!("{}: {}", 
                        kv.key.value, 
                        self.print_expression(&kv.value, self.current_depth + 1)))
                    .collect();
                format!("Row({{{}}})", kv_strings.join(", "))
            }
        }
    }

    // ========================================================================
    // --- Statement Printing
    // ========================================================================

    pub fn print_statement(&self, stmt_id: &StmtId, depth: usize) -> String {
        if depth > self.config.max_depth {
            return "...".to_string();
        }

        let stmt = &self.program.statements[*stmt_id];
        
        match stmt {
            Statement::Expression { expr, span } => {
                let mut result = format!("ExprStmt({})", self.print_expression(expr, depth + 1));
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::VarDecl { name, var_type, init, span } => {
                let mut details = vec![format!("name: \"{}\"", name.value)];
                
                if let Some(ref t) = var_type {
                    details.push(format!("type: {}", self.print_type_ref(t, depth + 1)));
                }
                
                if let Some(ref init_expr) = init {
                    details.push(format!("init: {}", self.print_expression(init_expr, depth + 1)));
                }
                
                let mut result = format!("VarDecl {{ {} }}", details.join(", "));
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::Assignment { target, value, span } => {
                let target_str = self.print_expression(target, depth + 1);
                let value_str = self.print_expression(value, depth + 1);
                let mut result = format!("Assignment {{ target: {}, value: {} }}", 
                    target_str, value_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::If { condition, then_stmt, else_stmt, span } => {
                let condition_str = self.print_expression(condition, depth + 1);
                let then_str = self.print_statement(then_stmt, depth + 1);
                let mut result = format!("If {{ condition: {}, then: {} }}", 
                    condition_str, then_str);
                
                if let Some(ref else_id) = else_stmt {
                    let else_str = self.print_statement(else_id, depth + 1);
                    result.push_str(&format!(", else: {}", else_str));
                }
                
                result.push_str(" }");
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::While { condition, body, span } => {
                let condition_str = self.print_expression(condition, depth + 1);
                let body_str = self.print_statement(body, depth + 1);
                let mut result = format!("While {{ condition: {}, body: {} }}", 
                    condition_str, body_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::For { var, iterable, body, span } => {
                let var_str = format!("\"{}\"", var.value);
                let iterable_str = self.print_expression(iterable, depth + 1);
                let body_str = self.print_statement(body, depth + 1);
                let mut result = format!("For {{ var: {}, iterable: {}, body: {} }}", 
                    var_str, iterable_str, body_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::Block { stmts, span } => {
                let stmt_strings: Vec<String> = stmts.iter()
                    .map(|s| self.print_statement(s, depth + 1))
                    .collect();
                let mut result = format!("Block([{}])", stmt_strings.join(", "));
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::Return { value, span } => {
                let mut result = if let Some(ref val) = value {
                    format!("Return({})", self.print_expression(val, depth + 1))
                } else {
                    "Return()".to_string()
                };
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
            
            Statement::Hop { target, action, span } => {
                let target_str = self.print_expression(target, depth + 1);
                let action_str = self.print_expression(action, depth + 1);
                let mut result = format!("Hop {{ target: {}, action: {} }}", 
                    target_str, action_str);
                
                if self.config.show_spans && span.is_some() {
                    result.push_str(&format!(" @{:?}", span));
                }
                result
            }
        }
    }

    // ========================================================================
    // --- Type Printing
    // ========================================================================

    pub fn print_type_ref(&self, type_ref: &TypeRef, depth: usize) -> String {
        if depth > self.config.max_depth {
            return "...".to_string();
        }

        match type_ref {
            TypeRef::Named { name, args, span } => {
                let mut result = format!("\"{}\"", name.value);
                
                if !args.is_empty() {
                    let arg_strings: Vec<String> = args.iter()
                        .map(|arg| self.print_type_ref(arg, depth + 1))
                        .collect();
                    result.push_str(&format!("<{}>", arg_strings.join(", ")));
                }
                
                if self.config.show_resolved && self.config.show_types {
                    if let Some(ref resolved) = name.resolved {
                        result.push_str(&format!(" -> {:?}", resolved));
                    }
                }
                
                result
            }
            
            TypeRef::List { element_type, span } => {
                format!("List<{}>", self.print_type_ref(element_type, depth + 1))
            }
            
            TypeRef::Row { fields, span } => {
                let field_strings: Vec<String> = fields.iter()
                    .map(|f| format!("{}: {}", 
                        f.name.value, 
                        self.print_type_ref(&f.field_type, depth + 1)))
                    .collect();
                format!("Row {{ {} }}", field_strings.join(", "))
            }
            
            TypeRef::Callable { params, return_type, span } => {
                let param_strings: Vec<String> = params.iter()
                    .map(|p| self.print_type_ref(p, depth + 1))
                    .collect();
                
                let mut result = format!("({}) -> {}", 
                    param_strings.join(", "),
                    self.print_type_ref(return_type, depth + 1));
                
                result
            }
        }
    }

    // ========================================================================
    // --- Helper Methods
    // ========================================================================

    fn print_table_constraint(&self, constraint: &TableConstraint, depth: usize) -> String {
        match constraint {
            TableConstraint::PrimaryKey { fields, span } => {
                let field_strings: Vec<String> = fields.iter()
                    .map(|f| format!("\"{}\"", f.value))
                    .collect();
                format!("PrimaryKey([{}])", field_strings.join(", "))
            }
            
            TableConstraint::Unique { fields, span } => {
                let field_strings: Vec<String> = fields.iter()
                    .map(|f| format!("\"{}\"", f.value))
                    .collect();
                format!("Unique([{}])", field_strings.join(", "))
            }
            
            TableConstraint::Check { condition, span } => {
                format!("Check({})", self.print_expression(condition, depth + 1))
            }
        }
    }
}

// ============================================================================
// --- Public Interface Functions
// ============================================================================

/// Print the entire program with default configuration
pub fn print_program(program: &Program) -> String {
    let printer = AstDebugPrinter::new(program);
    printer.print_program()
}

/// Print the entire program with custom configuration
pub fn print_program_with_config(program: &Program, config: DebugConfig) -> String {
    let printer = AstDebugPrinter::with_config(program, config);
    printer.print_program()
}

/// Print a single declaration
pub fn print_declaration(program: &Program, decl: &Declaration) -> String {
    let printer = AstDebugPrinter::new(program);
    printer.print_declaration(decl, 0)
}

/// Print a single expression
pub fn print_expression(program: &Program, expr_id: ExprId) -> String {
    let printer = AstDebugPrinter::new(program);
    printer.print_expression(&expr_id, 0)
}

/// Print a single statement
pub fn print_statement(program: &Program, stmt_id: StmtId) -> String {
    let printer = AstDebugPrinter::new(program);
    printer.print_statement(&stmt_id, 0)
}

/// Print a type reference
pub fn print_type_ref(program: &Program, type_ref: &TypeRef) -> String {
    let printer = AstDebugPrinter::new(program);
    printer.print_type_ref(type_ref, 0)
}

// ============================================================================
// --- Compact Debug Formats
// ============================================================================

/// Compact debug representation for expressions (single line)
pub fn debug_expression_compact(program: &Program, expr_id: ExprId) -> String {
    let expr = &program.expressions[expr_id];
    
    match expr {
        Expression::Literal { value, .. } => {
            match value {
                Literal::Integer(s) => s.clone(),
                Literal::Float(s) => s.clone(),
                Literal::String(s) => s.clone(),
                Literal::Bool(b) => b.to_string(),
                _ => "complex_literal".to_string(),
            }
        }
        
        Expression::Identifier(identifier) => identifier.name.clone(),
        
        Expression::Binary { left, op, right, .. } => {
            format!("({} {} {})", 
                debug_expression_compact(program, *left),
                op.value,
                debug_expression_compact(program, *right))
        }
        
        Expression::Unary { op, expr, .. } => {
            format!("({}{})", op.value, debug_expression_compact(program, *expr))
        }
        
        Expression::Call { callee, args, .. } => {
            let arg_strs: Vec<String> = args.iter()
                .map(|arg| debug_expression_compact(program, *arg))
                .collect();
            format!("{}({})", 
                debug_expression_compact(program, *callee),
                arg_strs.join(", "))
        }
        
        Expression::FieldAccess { base, field, .. } => {
            format!("{}.{}", debug_expression_compact(program, *base), field.value)
        }
        
        Expression::IndexAccess { base, index, .. } => {
            format!("{}[{}]", 
                debug_expression_compact(program, *base),
                debug_expression_compact(program, *index))
        }
        
        Expression::Grouped { expr, .. } => {
            format!("({})", debug_expression_compact(program, *expr))
        }
    }
}

/// Compact debug representation for statements (single line)
pub fn debug_statement_compact(program: &Program, stmt_id: StmtId) -> String {
    let stmt = &program.statements[stmt_id];
    
    match stmt {
        Statement::Expression { expr, .. } => {
            debug_expression_compact(program, *expr)
        }
        
        Statement::VarDecl { name, var_type, init, .. } => {
            let mut result = format!("let {}", name.value);
            if let Some(t) = var_type {
                result.push_str(&format!(": {}", debug_type_compact(t)));
            }
            if let Some(init_expr) = init {
                result.push_str(&format!(" = {}", debug_expression_compact(program, *init_expr)));
            }
            result
        }
        
        Statement::Assignment { target, value, .. } => {
            format!("{} = {}", 
                debug_expression_compact(program, *target),
                debug_expression_compact(program, *value))
        }
        
        Statement::Block { stmts, .. } => {
            format!("{{ {} stmts }}", stmts.len())
        }
        
        Statement::Return { value, .. } => {
            if let Some(val) = value {
                format!("return {}", debug_expression_compact(program, *val))
            } else {
                "return".to_string()
            }
        }
        
        _ => format!("{:?}", stmt).chars().take(50).collect::<String>() + "...",
    }
}

/// Compact debug representation for types (single line)
pub fn debug_type_compact(type_ref: &TypeRef) -> String {
    match type_ref {
        TypeRef::Named { name, args, .. } => {
            if args.is_empty() {
                name.value.clone()
            } else {
                let arg_strs: Vec<String> = args.iter()
                    .map(|arg| debug_type_compact(arg))
                    .collect();
                format!("{}<{}>", name.value, arg_strs.join(", "))
            }
        }
        
        TypeRef::List { element_type, .. } => {
            format!("List<{}>", debug_type_compact(element_type))
        }
        
        TypeRef::Row { fields, .. } => {
            format!("Row({} fields)", fields.len())
        }
        
        TypeRef::Callable { params, return_type, .. } => {
            format!("({} args) -> {}", params.len(), debug_type_compact(return_type))
        }
    }
}
