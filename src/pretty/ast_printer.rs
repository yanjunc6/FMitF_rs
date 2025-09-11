//! ast_printer.rs
//!
//! Implementation of pretty printing for AST nodes using the visitor pattern.
//!
//! This module provides a comprehensive AST printer that traverses the entire AST
//! using the visitor pattern and outputs formatted code. The printer supports:
//!
//! - All AST node types (items, statements, expressions, types)
//! - Configurable indentation
//! - Proper syntax highlighting through formatted output
//! - Individual node printing capabilities
//!
//!
//! # Implementation Details
//!
//! The printer uses the visitor pattern from `crate::ast::visit::Visitor` to traverse
//! the AST. This ensures that all nodes are visited in the correct order and allows
//! for easy extension of the printing logic.
//!
//! The implementation handles:
//! - Proper indentation and formatting
//! - Different callable kinds (functions, operators, partitions, transactions)
//! - Complex expressions including binary operations, function calls, and lambdas
//! - Table declarations with fields, nodes, and invariants
//! - All statement types including control flow
//! - Type annotations and generic parameters

use std::io::{self, Write};
use crate::ast::*;
use crate::ast::visit::Visitor;
use super::PrettyPrint;

/// A pretty printer that uses the visitor pattern to traverse and format AST nodes.
pub struct AstPrinter<W: Write> {
    writer: W,
    indent_level: usize,
    indent_size: usize,
}

impl<W: Write> AstPrinter<W> {
    /// Creates a new AST printer with the given writer.
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            indent_level: 0,
            indent_size: 2,
        }
    }

    /// Creates a new AST printer with custom indentation size.
    pub fn with_indent_size(mut self, indent_size: usize) -> Self {
        self.indent_size = indent_size;
        self
    }

    /// Writes the current indentation to the output.
    fn write_indent(&mut self) -> io::Result<()> {
        let spaces = " ".repeat(self.indent_level * self.indent_size);
        write!(self.writer, "{}", spaces)
    }

    /// Increases the indentation level.
    fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decreases the indentation level.
    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    /// Writes a line with proper indentation.
    fn writeln(&mut self, text: &str) -> io::Result<()> {
        self.write_indent()?;
        writeln!(self.writer, "{}", text)
    }

    /// Writes text without indentation or newline.
    fn write_text(&mut self, text: &str) -> io::Result<()> {
        write!(self.writer, "{}", text)
    }

    /// Prints decorators.
    fn print_decorators(&mut self, decorators: &[Decorator]) -> io::Result<()> {
        for decorator in decorators {
            self.writeln(&format!("@{}", decorator.name.name))?;
        }
        Ok(())
    }

    /// Prints a generic parameter list.
    fn print_generic_params(&mut self, prog: &Program, params: &[GenericParamId]) -> io::Result<()> {
        if !params.is_empty() {
            self.write_text("<")?;
            for (i, param_id) in params.iter().enumerate() {
                if i > 0 {
                    self.write_text(", ")?;
                }
                let param = &prog.generic_params[*param_id];
                self.write_text(&param.name.name)?;
            }
            self.write_text(">")?;
        }
        Ok(())
    }

    /// Prints a parameter list.
    fn print_params(&mut self, prog: &Program, params: &[ParamId]) -> io::Result<()> {
        self.write_text("(")?;
        for (i, param_id) in params.iter().enumerate() {
            if i > 0 {
                self.write_text(", ")?;
            }
            let param = &prog.params[*param_id];
            self.write_text(&param.name.name)?;
            self.write_text(": ")?;
            self.print_ast_type(prog, param.ty)?;
        }
        self.write_text(")")?;
        Ok(())
    }

    /// Prints a type annotation.
    fn print_ast_type(&mut self, prog: &Program, type_id: AstTypeId) -> io::Result<()> {
        match &prog.types[type_id] {
            AstType::Named { name, .. } => {
                self.write_text(&name.name)?;
            }
            AstType::Generic { base, args, .. } => {
                self.write_text(&base.name)?;
                self.write_text("<")?;
                for (i, arg_id) in args.iter().enumerate() {
                    if i > 0 {
                        self.write_text(", ")?;
                    }
                    self.print_ast_type(prog, *arg_id)?;
                }
                self.write_text(">")?;
            }
            AstType::Function { params, return_type, .. } => {
                self.write_text("(")?;
                for (i, param_id) in params.iter().enumerate() {
                    if i > 0 {
                        self.write_text(", ")?;
                    }
                    self.print_ast_type(prog, *param_id)?;
                }
                self.write_text(") -> ")?;
                self.print_ast_type(prog, *return_type)?;
            }
        }
        Ok(())
    }

    /// Prints an expression.
    fn print_expr(&mut self, prog: &Program, expr_id: ExprId) -> io::Result<()> {
        match &prog.expressions[expr_id] {
            Expression::Literal { value, .. } => {
                self.print_literal(prog, value)?;
            }
            Expression::Identifier { name, .. } => {
                self.write_text(&name.name)?;
            }
            Expression::Binary { left, op, right, .. } => {
                self.print_expr(prog, *left)?;
                self.write_text(&format!(" {} ", op.value))?;
                self.print_expr(prog, *right)?;
            }
            Expression::Unary { op, expr, .. } => {
                self.write_text(&op.value)?;
                self.print_expr(prog, *expr)?;
            }
            Expression::Assignment { lhs, rhs, .. } => {
                self.print_expr(prog, *lhs)?;
                self.write_text(" = ")?;
                self.print_expr(prog, *rhs)?;
            }
            Expression::Call { callee, args, .. } => {
                self.print_expr(prog, *callee)?;
                self.write_text("(")?;
                for (i, arg_id) in args.iter().enumerate() {
                    if i > 0 {
                        self.write_text(", ")?;
                    }
                    self.print_expr(prog, *arg_id)?;
                }
                self.write_text(")")?;
            }
            Expression::MemberAccess { object, member, .. } => {
                self.print_expr(prog, *object)?;
                self.write_text(".")?;
                self.write_text(&member.name)?;
            }
            Expression::TableRowAccess { table, key_values, .. } => {
                self.print_expr(prog, *table)?;
                self.write_text("[")?;
                for (i, kv) in key_values.iter().enumerate() {
                    if i > 0 {
                        self.write_text(", ")?;
                    }
                    self.write_text(&kv.key.name)?;
                    self.write_text(": ")?;
                    self.print_expr(prog, kv.value)?;
                }
                self.write_text("]")?;
            }
            Expression::Grouped { expr, .. } => {
                self.write_text("(")?;
                self.print_expr(prog, *expr)?;
                self.write_text(")")?;
            }
            Expression::Lambda { params, return_type, body, .. } => {
                self.write_text("|")?;
                self.print_params(prog, params)?;
                self.write_text("| -> ")?;
                self.print_ast_type(prog, *return_type)?;
                self.write_text(" ")?;
                self.print_block(prog, *body)?;
            }
        }
        Ok(())
    }

    /// Prints a literal value.
    fn print_literal(&mut self, prog: &Program, literal: &Literal) -> io::Result<()> {
        match literal {
            Literal::Integer(value) => self.write_text(value)?,
            Literal::Float(value) => self.write_text(value)?,
            Literal::String(value) => self.write_text(&format!("\"{}\"", value))?,
            Literal::Bool(value) => self.write_text(&value.to_string())?,
            Literal::List(items) => {
                self.write_text("[")?;
                for (i, item_id) in items.iter().enumerate() {
                    if i > 0 {
                        self.write_text(", ")?;
                    }
                    self.print_expr(prog, *item_id)?;
                }
                self.write_text("]")?;
            }
            Literal::RowLiteral(key_values) => {
                self.write_text("{")?;
                for (i, kv) in key_values.iter().enumerate() {
                    if i > 0 {
                        self.write_text(", ")?;
                    }
                    self.write_text(&kv.key.name)?;
                    self.write_text(": ")?;
                    self.print_expr(prog, kv.value)?;
                }
                self.write_text("}")?;
            }
        }
        Ok(())
    }

    /// Prints a statement.
    fn print_stmt(&mut self, prog: &Program, stmt_id: StmtId) -> io::Result<()> {
        match &prog.statements[stmt_id] {
            Statement::VarDecl(var_id) => {
                let var_decl = &prog.var_decls[*var_id];
                self.write_indent()?;
                self.write_text("let ")?;
                self.write_text(&var_decl.name.name)?;
                
                if let Some(type_id) = var_decl.ty {
                    self.write_text(": ")?;
                    self.print_ast_type(prog, type_id)?;
                }
                
                if let Some(init_id) = var_decl.init {
                    self.write_text(" = ")?;
                    self.print_expr(prog, init_id)?;
                }
                
                writeln!(self.writer, ";")?;
            }
            Statement::If { condition, then_block, else_block, .. } => {
                self.write_indent()?;
                self.write_text("if ")?;
                self.print_expr(prog, *condition)?;
                self.write_text(" ")?;
                self.print_block(prog, *then_block)?;
                
                if let Some(else_id) = else_block {
                    self.write_text(" else ")?;
                    self.print_block(prog, *else_id)?;
                }
                writeln!(self.writer)?;
            }
            Statement::For { init, condition, update, body, .. } => {
                self.write_indent()?;
                self.write_text("for (")?;
                
                if let Some(init) = init {
                    match init {
                        ForInit::VarDecl(var_id) => {
                            let var_decl = &prog.var_decls[*var_id];
                            self.write_text("let ")?;
                            self.write_text(&var_decl.name.name)?;
                            if let Some(type_id) = var_decl.ty {
                                self.write_text(": ")?;
                                self.print_ast_type(prog, type_id)?;
                            }
                            if let Some(init_id) = var_decl.init {
                                self.write_text(" = ")?;
                                self.print_expr(prog, init_id)?;
                            }
                        }
                        ForInit::Expression(expr_id) => {
                            self.print_expr(prog, *expr_id)?;
                        }
                    }
                }
                
                self.write_text("; ")?;
                
                if let Some(cond_id) = condition {
                    self.print_expr(prog, *cond_id)?;
                }
                
                self.write_text("; ")?;
                
                if let Some(update_id) = update {
                    self.print_expr(prog, *update_id)?;
                }
                
                self.write_text(") ")?;
                self.print_block(prog, *body)?;
                writeln!(self.writer)?;
            }
            Statement::Return { value, .. } => {
                self.write_indent()?;
                self.write_text("return")?;
                if let Some(expr_id) = value {
                    self.write_text(" ")?;
                    self.print_expr(prog, *expr_id)?;
                }
                writeln!(self.writer, ";")?;
            }
            Statement::Assert { expr, .. } => {
                self.write_indent()?;
                self.write_text("assert ")?;
                self.print_expr(prog, *expr)?;
                writeln!(self.writer, ";")?;
            }
            Statement::Hop { decorators, body, .. } => {
                self.print_decorators(decorators)?;
                self.write_indent()?;
                self.write_text("hop ")?;
                self.print_block(prog, *body)?;
                writeln!(self.writer)?;
            }
            Statement::HopsFor { decorators, var, start, end, body, .. } => {
                self.print_decorators(decorators)?;
                self.write_indent()?;
                self.write_text("hops for ")?;
                let var_decl = &prog.var_decls[*var];
                self.write_text(&var_decl.name.name)?;
                self.write_text(" in ")?;
                self.print_expr(prog, *start)?;
                self.write_text("..")?;
                self.print_expr(prog, *end)?;
                self.write_text(" ")?;
                self.print_block(prog, *body)?;
                writeln!(self.writer)?;
            }
            Statement::Expression { expr, .. } => {
                self.write_indent()?;
                self.print_expr(prog, *expr)?;
                writeln!(self.writer, ";")?;
            }
            Statement::Block(block_id) => {
                self.print_block(prog, *block_id)?;
            }
        }
        Ok(())
    }

    /// Prints a block of statements.
    fn print_block(&mut self, prog: &Program, block_id: BlockId) -> io::Result<()> {
        let block = &prog.blocks[block_id];
        writeln!(self.writer, "{{")?;
        self.indent();
        
        for stmt_id in &block.statements {
            self.print_stmt(prog, *stmt_id)?;
        }
        
        self.dedent();
        self.write_indent()?;
        self.write_text("}")?;
        Ok(())
    }
}

impl<'ast, W: Write> Visitor<'ast> for AstPrinter<W> {
    fn visit_program(&mut self, prog: &'ast Program) {
        // Print all top-level declarations
        for item in &prog.declarations {
            if let Err(_) = self.visit_item(prog, item) {
                // Handle error silently for now
                break;
            }
            if let Err(_) = writeln!(self.writer) {
                break;
            }
        }
    }

    fn visit_callable_decl(&mut self, prog: &'ast Program, _id: FunctionId, decl: &'ast CallableDecl) {
        let _ = self.print_decorators(&decl.decorators);
        let _ = self.write_indent();
        
        let kind_str = match decl.kind {
            CallableKind::Function => "fn",
            CallableKind::Operator => "op",
            CallableKind::Partition => "partition",
            CallableKind::Transaction => "transaction",
        };
        
        let _ = self.write_text(kind_str);
        let _ = self.write_text(" ");
        let _ = self.write_text(&decl.name.name);
        let _ = self.print_generic_params(prog, &decl.generic_params);
        let _ = self.print_params(prog, &decl.params);
        
        if let Some(return_type) = decl.return_type {
            let _ = self.write_text(" -> ");
            let _ = self.print_ast_type(prog, return_type);
        }
        
        if !decl.assumptions.is_empty() {
            let _ = writeln!(self.writer);
            let _ = self.write_indent();
            let _ = self.write_text("assumes ");
            for (i, assumption_id) in decl.assumptions.iter().enumerate() {
                if i > 0 {
                    let _ = self.write_text(", ");
                }
                let _ = self.print_expr(prog, *assumption_id);
            }
        }
        
        if let Some(body_id) = decl.body {
            let _ = self.write_text(" ");
            let _ = self.print_block(prog, body_id);
        } else {
            let _ = self.write_text(";");
        }
        
        let _ = writeln!(self.writer);
    }

    fn visit_type_decl(&mut self, prog: &'ast Program, _id: TypeDeclId, decl: &'ast TypeDecl) {
        let _ = self.print_decorators(&decl.decorators);
        let _ = self.write_indent();
        let _ = self.write_text("type ");
        let _ = self.write_text(&decl.name.name);
        let _ = self.print_generic_params(prog, &decl.generic_params);
        let _ = writeln!(self.writer, ";");
    }

    fn visit_const_decl(&mut self, prog: &'ast Program, _id: ConstId, decl: &'ast ConstDecl) {
        let _ = self.write_indent();
        let _ = self.write_text("const ");
        let _ = self.write_text(&decl.name.name);
        let _ = self.write_text(": ");
        let _ = self.print_ast_type(prog, decl.ty);
        let _ = self.write_text(" = ");
        let _ = self.print_expr(prog, decl.value);
        let _ = writeln!(self.writer, ";");
    }

    fn visit_table_decl(&mut self, prog: &'ast Program, _id: TableId, decl: &'ast TableDecl) {
        let _ = self.write_indent();
        let _ = self.write_text("table ");
        let _ = self.write_text(&decl.name.name);
        let _ = writeln!(self.writer, " {{");
        self.indent();
        
        for element in &decl.elements {
            match element {
                TableElement::Field(field) => {
                    let _ = self.write_indent();
                    if field.is_primary {
                        let _ = self.write_text("primary ");
                    }
                    let _ = self.write_text(&field.name.name);
                    let _ = self.write_text(": ");
                    let _ = self.print_ast_type(prog, field.ty);
                    let _ = writeln!(self.writer, ",");
                }
                TableElement::Node(node) => {
                    let _ = self.write_indent();
                    let _ = self.write_text("node ");
                    let _ = self.write_text(&node.name.name);
                    let _ = self.write_text("(");
                    for (i, arg_id) in node.args.iter().enumerate() {
                        if i > 0 {
                            let _ = self.write_text(", ");
                        }
                        let _ = self.print_expr(prog, *arg_id);
                    }
                    let _ = writeln!(self.writer, "),");
                }
                TableElement::Invariant(expr_id) => {
                    let _ = self.write_indent();
                    let _ = self.write_text("invariant ");
                    let _ = self.print_expr(prog, *expr_id);
                    let _ = writeln!(self.writer, ",");
                }
            }
        }
        
        self.dedent();
        let _ = self.write_indent();
        let _ = writeln!(self.writer, "}}");
    }
}

impl<'ast, W: Write> AstPrinter<W> {
    fn visit_item(&mut self, prog: &'ast Program, item: &'ast Item) -> io::Result<()> {
        match item {
            Item::Callable(id) => {
                self.visit_callable_decl(prog, *id, &prog.functions[*id]);
                Ok(())
            }
            Item::Type(id) => {
                self.visit_type_decl(prog, *id, &prog.type_decls[*id]);
                Ok(())
            }
            Item::Const(id) => {
                self.visit_const_decl(prog, *id, &prog.const_decls[*id]);
                Ok(())
            }
            Item::Table(id) => {
                self.visit_table_decl(prog, *id, &prog.table_decls[*id]);
                Ok(())
            }
        }
    }
}

// Implement PrettyPrint for Program using the visitor
impl PrettyPrint for Program {
    fn pretty_print(&self, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.visit_program(self);
        Ok(())
    }
}

// Convenience functions for pretty printing individual AST nodes
impl Program {
    /// Pretty print a single expression.
    pub fn pretty_print_expr(&self, expr_id: ExprId, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.print_expr(self, expr_id)
    }

    /// Pretty print a single statement.
    pub fn pretty_print_stmt(&self, stmt_id: StmtId, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.print_stmt(self, stmt_id)
    }

    /// Pretty print a single type.
    pub fn pretty_print_type(&self, type_id: AstTypeId, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.print_ast_type(self, type_id)
    }
}
