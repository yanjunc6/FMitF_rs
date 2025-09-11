//! AST Pretty Printer using the Visitor pattern
//!
//! This module demonstrates how to use the visitor pattern to traverse and print the AST.
//! It uses the new visitor traits from ast::visit to implement a clean, extensible printer.

use crate::ast::visit::{Visitor, walk_program};
use crate::ast::*;
use std::io::Write;

/// A pretty printer that uses the visitor pattern to traverse the AST
pub struct AstPrinter<W: Write> {
    writer: W,
    indent_level: usize,
    indent_size: usize,
    show_ids: bool,
}

impl<W: Write> AstPrinter<W> {
    /// Creates a new visitor-based AST printer
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            indent_level: 0,
            indent_size: 2,
            show_ids: true,
        }
    }

    /// Creates a new visitor-based AST printer with custom settings
    pub fn with_settings(writer: W, indent_size: usize, show_ids: bool) -> Self {
        Self {
            writer,
            indent_level: 0,
            indent_size,
            show_ids,
        }
    }

    /// Print a program using the visitor pattern
    pub fn print_program(mut self, program: &Program) -> Result<W, std::io::Error> {
        self.visit_program(program);
        Ok(self.writer)
    }

    fn write_indent(&mut self) -> std::io::Result<()> {
        for _ in 0..(self.indent_level * self.indent_size) {
            write!(self.writer, " ")?;
        }
        Ok(())
    }

    fn increase_indent(&mut self) {
        self.indent_level += 1;
    }

    fn decrease_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    fn format_identifier(&self, identifier: &Identifier) -> String {
        identifier.name.clone()
    }

    fn format_identifier_with_id<T>(&self, identifier: &Identifier, id: T) -> String 
    where 
        T: std::fmt::Display 
    {
        if self.show_ids {
            format!("{} ({})", identifier.name, id)
        } else {
            identifier.name.clone()
        }
    }

    fn write_decorators(&mut self, decorators: &[Decorator]) -> std::io::Result<()> {
        for decorator in decorators {
            write!(self.writer, "@{} ", self.format_identifier(&decorator.name))?;
        }
        Ok(())
    }
}

impl<'ast, W: Write> Visitor<'ast> for AstPrinter<W> {
    fn visit_program(&mut self, prog: &'ast Program) {
        writeln!(self.writer, "AST Program (Visitor Pattern):").unwrap();
        writeln!(self.writer).unwrap();
        
        self.write_indent().unwrap();
        writeln!(
            self.writer,
            "Declarations: {} total",
            prog.declarations.len()
        ).unwrap();
        writeln!(self.writer).unwrap();

        // Use the default walker to traverse items
        walk_program(self, prog);
    }

    fn visit_item(&mut self, prog: &'ast Program, item: &'ast Item) {
        self.write_indent().unwrap();
        match item {
            Item::Callable(_id) => {
                writeln!(self.writer, "Function Declaration:").unwrap();
            }
            Item::Type(_id) => {
                writeln!(self.writer, "Type Declaration:").unwrap();
            }
            Item::Const(_id) => {
                writeln!(self.writer, "Const Declaration:").unwrap();
            }
            Item::Table(_id) => {
                writeln!(self.writer, "Table Declaration:").unwrap();
            }
        }
        
        self.increase_indent();
        // Let the default walker handle the specific item type
        crate::ast::visit::walk_item(self, prog, item);
        self.decrease_indent();
        writeln!(self.writer).unwrap();
    }

    fn visit_callable_decl(&mut self, prog: &'ast Program, id: FunctionId, decl: &'ast CallableDecl) {
        self.write_indent().unwrap();

        // Print decorators
        self.write_decorators(&decl.decorators).unwrap();

        // Print callable kind and name
        match decl.kind {
            CallableKind::Function => write!(self.writer, "function ").unwrap(),
            CallableKind::Operator => write!(self.writer, "operator ").unwrap(),
            CallableKind::Partition => write!(self.writer, "partition ").unwrap(),
            CallableKind::Transaction => write!(self.writer, "transaction ").unwrap(),
        }

        write!(
            self.writer,
            "{}",
            self.format_identifier_with_id(&decl.name, id.index())
        ).unwrap();

        // Print generic parameters
        if !decl.generic_params.is_empty() {
            write!(self.writer, "<").unwrap();
            for (i, &param_id) in decl.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ").unwrap();
                }
                let param = &prog.generic_params[param_id];
                write!(self.writer, "{}", self.format_identifier(&param.name)).unwrap();
            }
            write!(self.writer, ">").unwrap();
        }

        // Print parameters
        write!(self.writer, "(").unwrap();
        for (i, &param_id) in decl.params.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ").unwrap();
            }
            let param = &prog.params[param_id];
            write!(
                self.writer,
                "{}: {}",
                self.format_identifier(&param.name),
                self.format_type_id(prog, param.ty)
            ).unwrap();
        }
        write!(self.writer, ")").unwrap();

        // Print return type
        if let Some(return_type) = decl.return_type {
            write!(self.writer, " -> {}", self.format_type_id(prog, return_type)).unwrap();
        }

        writeln!(self.writer).unwrap();

        // Print assumptions
        if !decl.assumptions.is_empty() {
            self.increase_indent();
            for &assumption_id in &decl.assumptions {
                self.write_indent().unwrap();
                writeln!(
                    self.writer,
                    "assume {};",
                    self.format_expression_id(prog, assumption_id)
                ).unwrap();
            }
            self.decrease_indent();
        }

        // Let the default walker handle the body and other parts
        crate::ast::visit::walk_callable_decl(self, prog, id, decl);
    }

    fn visit_type_decl(&mut self, prog: &'ast Program, id: TypeDeclId, decl: &'ast TypeDecl) {
        self.write_indent().unwrap();

        // Print decorators
        self.write_decorators(&decl.decorators).unwrap();

        write!(
            self.writer,
            "type {}",
            self.format_identifier(&decl.name)
        ).unwrap();
        
        if self.show_ids {
            write!(self.writer, " ({})", id.index()).unwrap();
        }

        // Print generic parameters
        if !decl.generic_params.is_empty() {
            write!(self.writer, "<").unwrap();
            for (i, &param_id) in decl.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ").unwrap();
                }
                let param = &prog.generic_params[param_id];
                write!(self.writer, "{}", self.format_identifier(&param.name)).unwrap();
            }
            write!(self.writer, ">").unwrap();
        }

        writeln!(self.writer, ";").unwrap();
    }

    fn visit_const_decl(&mut self, prog: &'ast Program, id: ConstId, decl: &'ast ConstDecl) {
        self.write_indent().unwrap();
        write!(
            self.writer,
            "const {}: {} = {};",
            self.format_identifier(&decl.name),
            self.format_type_id(prog, decl.ty),
            self.format_expression_id(prog, decl.value)
        ).unwrap();
        
        if self.show_ids {
            write!(self.writer, " // ({})", id.index()).unwrap();
        }
        writeln!(self.writer).unwrap();
    }

    fn visit_table_decl(&mut self, prog: &'ast Program, id: TableId, decl: &'ast TableDecl) {
        self.write_indent().unwrap();
        write!(self.writer, "table {}", self.format_identifier(&decl.name)).unwrap();
        
        if self.show_ids {
            write!(self.writer, " ({})", id.index()).unwrap();
        }
        writeln!(self.writer, " {{").unwrap();

        self.increase_indent();
        for element in &decl.elements {
            match element {
                TableElement::Field(field) => {
                    self.write_indent().unwrap();
                    if field.is_primary {
                        write!(self.writer, "primary ").unwrap();
                    }
                    writeln!(
                        self.writer,
                        "{}: {};",
                        self.format_identifier(&field.name),
                        self.format_type_id(prog, field.ty)
                    ).unwrap();
                }
                TableElement::Node(node) => {
                    self.write_indent().unwrap();
                    write!(self.writer, "node {}(", self.format_identifier(&node.name)).unwrap();
                    for (i, &arg_id) in node.args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ").unwrap();
                        }
                        write!(self.writer, "{}", self.format_expression_id(prog, arg_id)).unwrap();
                    }
                    writeln!(self.writer, ");").unwrap();
                }
                TableElement::Invariant(expr_id) => {
                    self.write_indent().unwrap();
                    writeln!(
                        self.writer,
                        "invariant {};",
                        self.format_expression_id(prog, *expr_id)
                    ).unwrap();
                }
            }
        }
        self.decrease_indent();

        self.write_indent().unwrap();
        writeln!(self.writer, "}}").unwrap();
    }

    fn visit_var_decl(&mut self, prog: &'ast Program, _id: VarId, decl: &'ast VarDecl) {
        self.write_indent().unwrap();
        write!(self.writer, "var {}", self.format_identifier(&decl.name)).unwrap();
        
        if let Some(ty) = decl.ty {
            write!(self.writer, ": {}", self.format_type_id(prog, ty)).unwrap();
        }
        
        if let Some(init) = decl.init {
            write!(self.writer, " = {}", self.format_expression_id(prog, init)).unwrap();
        }
        
        writeln!(self.writer, ";").unwrap();
    }

    fn visit_stmt(&mut self, prog: &'ast Program, id: StmtId) {
        let statement = &prog.statements[id];
        match statement {
            Statement::VarDecl(_) => {
                // Let the default walker handle this
                crate::ast::visit::walk_stmt(self, prog, id);
            }
            Statement::If { condition, then_block, else_block, .. } => {
                self.write_indent().unwrap();
                writeln!(
                    self.writer,
                    "if ({}) {{",
                    self.format_expression_id(prog, *condition)
                ).unwrap();
                self.increase_indent();
                self.visit_block(prog, *then_block);
                self.decrease_indent();
                
                if let Some(else_block_id) = else_block {
                    self.write_indent().unwrap();
                    writeln!(self.writer, "}} else {{").unwrap();
                    self.increase_indent();
                    self.visit_block(prog, *else_block_id);
                    self.decrease_indent();
                }
                
                self.write_indent().unwrap();
                writeln!(self.writer, "}}").unwrap();
            }
            Statement::Return { value, .. } => {
                self.write_indent().unwrap();
                if let Some(val) = value {
                    writeln!(self.writer, "return {};", self.format_expression_id(prog, *val)).unwrap();
                } else {
                    writeln!(self.writer, "return;").unwrap();
                }
            }
            Statement::Assert { expr, .. } => {
                self.write_indent().unwrap();
                writeln!(self.writer, "assert {};", self.format_expression_id(prog, *expr)).unwrap();
            }
            Statement::Hop { decorators, body, .. } => {
                self.write_indent().unwrap();
                self.write_decorators(decorators).unwrap();
                writeln!(self.writer, "hop {{").unwrap();
                self.increase_indent();
                self.visit_block(prog, *body);
                self.decrease_indent();
                self.write_indent().unwrap();
                writeln!(self.writer, "}}").unwrap();
            }
            Statement::Expression { expr, .. } => {
                self.write_indent().unwrap();
                writeln!(self.writer, "{};", self.format_expression_id(prog, *expr)).unwrap();
            }
            _ => {
                // For other statement types, use the default walker
                crate::ast::visit::walk_stmt(self, prog, id);
            }
        }
    }

    fn visit_block(&mut self, prog: &'ast Program, id: BlockId) {
        // Use the default walker for blocks
        crate::ast::visit::walk_block(self, prog, id);
    }
}

impl<W: Write> AstPrinter<W> {
    fn format_type_id(&self, prog: &Program, type_id: AstTypeId) -> String {
        let ast_type = &prog.types[type_id];
        match ast_type {
            AstType::Named { name, .. } => {
                if self.show_ids {
                    format!("{} ({})", self.format_identifier(name), type_id.index())
                } else {
                    self.format_identifier(name)
                }
            }
            AstType::Generic { base, args, .. } => {
                let arg_strs: Vec<String> = args
                    .iter()
                    .map(|&id| self.format_type_id(prog, id))
                    .collect();
                if self.show_ids {
                    format!(
                        "{}<{}> ({})",
                        self.format_identifier(base),
                        arg_strs.join(", "),
                        type_id.index()
                    )
                } else {
                    format!("{}<{}>", self.format_identifier(base), arg_strs.join(", "))
                }
            }
            AstType::Function { params, return_type, .. } => {
                let param_strs: Vec<String> = params
                    .iter()
                    .map(|&id| self.format_type_id(prog, id))
                    .collect();
                if self.show_ids {
                    format!(
                        "({}) -> {} ({})",
                        param_strs.join(", "),
                        self.format_type_id(prog, *return_type),
                        type_id.index()
                    )
                } else {
                    format!(
                        "({}) -> {}",
                        param_strs.join(", "),
                        self.format_type_id(prog, *return_type)
                    )
                }
            }
        }
    }

    fn format_expression_id(&self, prog: &Program, expr_id: ExprId) -> String {
        let expr = &prog.expressions[expr_id];
        match expr {
            Expression::Literal { value, .. } => match value {
                Literal::Integer(i) => i.clone(),
                Literal::Float(f) => f.clone(),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Bool(b) => b.to_string(),
                Literal::List(exprs) => {
                    let elements: Vec<String> = exprs
                        .iter()
                        .map(|&id| self.format_expression_id(prog, id))
                        .collect();
                    format!("[{}]", elements.join(", "))
                }
                Literal::RowLiteral(key_values) => {
                    let pairs: Vec<String> = key_values
                        .iter()
                        .map(|kv| {
                            format!(
                                "{}: {}",
                                self.format_identifier(&kv.key),
                                self.format_expression_id(prog, kv.value)
                            )
                        })
                        .collect();
                    format!("{{{}}}", pairs.join(", "))
                }
            },
            Expression::Identifier { name, .. } => self.format_identifier(name),
            Expression::Binary { left, op, right, .. } => {
                format!(
                    "({} {} {})",
                    self.format_expression_id(prog, *left),
                    op.value,
                    self.format_expression_id(prog, *right)
                )
            }
            Expression::Unary { op, expr, .. } => {
                format!("({}{})", op.value, self.format_expression_id(prog, *expr))
            }
            Expression::Assignment { lhs, rhs, .. } => {
                format!(
                    "{} = {}",
                    self.format_expression_id(prog, *lhs),
                    self.format_expression_id(prog, *rhs)
                )
            }
            Expression::Call { callee, args, .. } => {
                let arg_strs: Vec<String> = args
                    .iter()
                    .map(|&id| self.format_expression_id(prog, id))
                    .collect();
                format!(
                    "{}({})",
                    self.format_expression_id(prog, *callee),
                    arg_strs.join(", ")
                )
            }
            Expression::MemberAccess { object, member, .. } => {
                format!(
                    "{}.{}",
                    self.format_expression_id(prog, *object),
                    self.format_identifier(member)
                )
            }
            Expression::TableRowAccess { table, key_values, .. } => {
                let pairs: Vec<String> = key_values
                    .iter()
                    .map(|kv| {
                        format!(
                            "{}: {}",
                            self.format_identifier(&kv.key),
                            self.format_expression_id(prog, kv.value)
                        )
                    })
                    .collect();
                format!(
                    "{}[{}]",
                    self.format_expression_id(prog, *table),
                    pairs.join(", ")
                )
            }
            Expression::Grouped { expr, .. } => {
                format!("({})", self.format_expression_id(prog, *expr))
            }
            Expression::Lambda { params, return_type, .. } => {
                let param_strs: Vec<String> = params
                    .iter()
                    .map(|&param_id| {
                        let param = &prog.params[param_id];
                        format!(
                            "{}: {}",
                            self.format_identifier(&param.name),
                            self.format_type_id(prog, param.ty)
                        )
                    })
                    .collect();

                format!(
                    "({}) -> {} {{ ... }}",
                    param_strs.join(", "),
                    self.format_type_id(prog, *return_type)
                )
            }
        }
    }
}

/// Convenience function to pretty-print a program using the visitor pattern
pub fn print_program_visitor(program: &Program) -> String {
    let mut buffer = Vec::new();
    let printer = AstPrinter::new(&mut buffer);
    
    match printer.print_program(program) {
        Ok(_) => String::from_utf8(buffer).unwrap_or_else(|_| "Error: Invalid UTF-8".to_string()),
        Err(e) => format!("Error printing program: {}", e),
    }
}

/// Convenience function to pretty-print a program using the visitor pattern with custom settings
pub fn print_program_visitor_custom(program: &Program, indent_size: usize, show_ids: bool) -> String {
    let mut buffer = Vec::new();
    let printer = AstPrinter::with_settings(&mut buffer, indent_size, show_ids);
    
    match printer.print_program(program) {
        Ok(_) => String::from_utf8(buffer).unwrap_or_else(|_| "Error: Invalid UTF-8".to_string()),
        Err(e) => format!("Error printing program: {}", e),
    }
}
