//! AST Pretty Printer

use super::PrettyPrinter;
use crate::ast::*;
use std::cell::RefCell;
use std::io::Write;

// ============== ID DISPLAY CONTROL CONSTANTS ===================
// Controls whether IDs are shown after names
const SHOW_IDS: bool = true;
// ===============================================================

/// Pretty printer for AST structures using visitor pattern.
/// Provides human-readable output of the abstract syntax tree.
pub struct AstPrinter;

impl AstPrinter {
    /// Creates a new AST printer.
    pub fn new() -> Self {
        Self
    }
}

/// AST printer that uses the visitor pattern to traverse and print the AST
pub struct AstPrintVisitor<'a> {
    writer: &'a mut dyn std::io::Write,
    program: &'a Program,
    indent_level: RefCell<usize>,
    indent_size: usize,
}

impl<'a> AstPrintVisitor<'a> {
    fn new(writer: &'a mut dyn Write, program: &'a Program) -> Self {
        Self {
            writer,
            program,
            indent_level: RefCell::new(0),
            indent_size: 2,
        }
    }

    fn write_indent(&mut self) -> std::io::Result<()> {
        let level = *self.indent_level.borrow();
        for _ in 0..(level * self.indent_size) {
            write!(self.writer, " ")?;
        }
        Ok(())
    }

    fn increase_indent(&self) {
        *self.indent_level.borrow_mut() += 1;
    }

    fn decrease_indent(&self) {
        let mut level = self.indent_level.borrow_mut();
        if *level > 0 {
            *level -= 1;
        }
    }

    fn visit_program(&mut self) -> std::io::Result<()> {
        writeln!(self.writer, "AST Program:")?;
        writeln!(self.writer)?;

        self.write_indent()?;
        writeln!(
            self.writer,
            "Declarations: {} total",
            self.program.declarations.len()
        )?;
        writeln!(self.writer)?;

        for (i, declaration) in self.program.declarations.iter().enumerate() {
            self.write_indent()?;
            writeln!(self.writer, "Declaration {}:", i + 1)?;
            self.increase_indent();
            self.visit_declaration(declaration)?;
            self.decrease_indent();
            writeln!(self.writer)?;
        }

        Ok(())
    }

    fn visit_declaration(&mut self, declaration: &Declaration) -> std::io::Result<()> {
        match declaration {
            Declaration::Callable(function_id) => {
                self.visit_callable_decl(*function_id)?;
            }
            Declaration::Type(type_id) => {
                self.visit_type_decl(*type_id)?;
            }
            Declaration::Const(const_id) => {
                self.visit_const_decl(*const_id)?;
            }
            Declaration::Table(table_id) => {
                self.visit_table_decl(*table_id)?;
            }
        }
        Ok(())
    }

    fn visit_callable_decl(&mut self, function_id: FunctionId) -> std::io::Result<()> {
        if let Some(function) = self.program.functions.get(function_id) {
            self.write_indent()?;

            // Print decorators
            if !function.decorators.is_empty() {
                for decorator in &function.decorators {
                    write!(self.writer, "@{} ", self.format_identifier(&decorator.name))?;
                }
            }

            // Print callable kind and name
            match function.kind {
                CallableKind::Function => write!(self.writer, "function ")?,
                CallableKind::Operator => write!(self.writer, "operator ")?,
                CallableKind::Partition => write!(self.writer, "partition ")?,
                CallableKind::Transaction => write!(self.writer, "transaction ")?,
            }

            match &function.name {
                CallableName::Identifier(id) => {
                    write!(
                        self.writer,
                        "{}",
                        self.format_identifier_with_id(id, function_id)
                    )?;
                }
                CallableName::Operator(op) => {
                    write!(self.writer, "{}", op.value)?;
                    if SHOW_IDS {
                        write!(self.writer, " ({})", function_id.index())?;
                    }
                }
            }

            // Print generic parameters
            if !function.generic_params.is_empty() {
                write!(self.writer, "<")?;
                for (i, &param_id) in function.generic_params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    if let Some(param) = self.program.generic_params.get(param_id) {
                        write!(self.writer, "{}", self.format_identifier(&param.name))?;
                    }
                }
                write!(self.writer, ">")?;
            }

            // Print parameters
            write!(self.writer, "(")?;
            for (i, &param_id) in function.params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                if let Some(param) = self.program.params.get(param_id) {
                    write!(
                        self.writer,
                        "{}: {}",
                        self.format_identifier(&param.name),
                        self.format_type_id(param.ty)
                    )?;
                }
            }
            write!(self.writer, ")")?;

            // Print return type
            if let Some(return_type) = function.return_type {
                write!(self.writer, " -> {}", self.format_type_id(return_type))?;
            }

            writeln!(self.writer)?;

            // Print assumptions
            if !function.assumptions.is_empty() {
                self.increase_indent();
                for &assumption_id in &function.assumptions {
                    self.write_indent()?;
                    writeln!(
                        self.writer,
                        "assume {};",
                        self.format_expression_id(assumption_id)
                    )?;
                }
                self.decrease_indent();
            }

            // Print body
            if let Some(body_id) = function.body {
                self.increase_indent();
                self.visit_block(body_id)?;
                self.decrease_indent();
            } else {
                self.write_indent()?;
                writeln!(self.writer, "// Forward declaration")?;
            }
        }
        Ok(())
    }

    fn visit_type_decl(&mut self, type_id: TypeDeclId) -> std::io::Result<()> {
        if let Some(type_decl) = self.program.type_decls.get(type_id) {
            self.write_indent()?;

            // Print decorators
            for decorator in &type_decl.decorators {
                write!(self.writer, "@{} ", self.format_identifier(&decorator.name))?;
            }

            write!(
                self.writer,
                "type {}",
                self.format_identifier(&type_decl.name)
            )?;
            if SHOW_IDS {
                write!(self.writer, " ({})", type_id.index())?;
            }

            // Print generic parameters
            if !type_decl.generic_params.is_empty() {
                write!(self.writer, "<")?;
                for (i, &param_id) in type_decl.generic_params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    if let Some(param) = self.program.generic_params.get(param_id) {
                        write!(self.writer, "{}", self.format_identifier(&param.name))?;
                    }
                }
                write!(self.writer, ">")?;
            }

            writeln!(self.writer, ";")?;
        }
        Ok(())
    }

    fn visit_const_decl(&mut self, const_id: ConstId) -> std::io::Result<()> {
        if let Some(const_decl) = self.program.const_decls.get(const_id) {
            self.write_indent()?;
            write!(
                self.writer,
                "const {}: {} = {};",
                self.format_identifier(&const_decl.name),
                self.format_type_id(const_decl.ty),
                self.format_expression_id(const_decl.value)
            )?;
            if SHOW_IDS {
                write!(self.writer, " // ({})", const_id.index())?;
            }
            writeln!(self.writer)?;
        }
        Ok(())
    }

    fn visit_table_decl(&mut self, table_id: TableId) -> std::io::Result<()> {
        if let Some(table) = self.program.table_decls.get(table_id) {
            self.write_indent()?;
            write!(self.writer, "table {}", self.format_identifier(&table.name))?;
            if SHOW_IDS {
                write!(self.writer, " ({})", table_id.index())?;
            }
            writeln!(self.writer, " {{")?;

            self.increase_indent();
            for element in &table.elements {
                match element {
                    TableElement::Field(field) => {
                        self.write_indent()?;
                        if field.is_primary {
                            write!(self.writer, "primary ")?;
                        }
                        writeln!(
                            self.writer,
                            "{}: {};",
                            self.format_identifier(&field.name),
                            self.format_type_id(field.ty)
                        )?;
                    }
                    TableElement::Node(node) => {
                        self.write_indent()?;
                        write!(self.writer, "node {}(", self.format_identifier(&node.name))?;
                        for (i, &arg_id) in node.args.iter().enumerate() {
                            if i > 0 {
                                write!(self.writer, ", ")?;
                            }
                            write!(self.writer, "{}", self.format_expression_id(arg_id))?;
                        }
                        writeln!(self.writer, ");")?;
                    }
                    TableElement::Invariant(expr_id) => {
                        self.write_indent()?;
                        writeln!(
                            self.writer,
                            "invariant {};",
                            self.format_expression_id(*expr_id)
                        )?;
                    }
                }
            }
            self.decrease_indent();

            self.write_indent()?;
            writeln!(self.writer, "}}")?;
        }
        Ok(())
    }

    fn visit_block(&mut self, block_id: BlockId) -> std::io::Result<()> {
        if let Some(block) = self.program.blocks.get(block_id) {
            for stmt_id in &block.statements {
                self.visit_statement(*stmt_id)?;
            }
        }
        Ok(())
    }

    fn visit_statement(&mut self, stmt_id: StmtId) -> std::io::Result<()> {
        if let Some(statement) = self.program.statements.get(stmt_id) {
            match statement {
                Statement::VarDecl(var_id) => {
                    if let Some(var) = self.program.var_decls.get(*var_id) {
                        self.write_indent()?;
                        write!(self.writer, "var {}", self.format_identifier(&var.name))?;
                        if let Some(ty) = var.ty {
                            write!(self.writer, ": {}", self.format_type_id(ty))?;
                        }
                        if let Some(init) = var.init {
                            write!(self.writer, " = {}", self.format_expression_id(init))?;
                        }
                        writeln!(self.writer, ";")?;
                    }
                }
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                    ..
                } => {
                    self.write_indent()?;
                    writeln!(
                        self.writer,
                        "if ({}) {{",
                        self.format_expression_id(*condition)
                    )?;
                    self.increase_indent();
                    self.visit_block(*then_block)?;
                    self.decrease_indent();
                    if let Some(else_block_id) = else_block {
                        self.write_indent()?;
                        writeln!(self.writer, "}} else {{")?;
                        self.increase_indent();
                        self.visit_block(*else_block_id)?;
                        self.decrease_indent();
                    }
                    self.write_indent()?;
                    writeln!(self.writer, "}}")?;
                }
                Statement::For {
                    init,
                    condition,
                    update,
                    body,
                    ..
                } => {
                    self.write_indent()?;
                    write!(self.writer, "for (")?;
                    if let Some(init) = init {
                        match init {
                            ForInit::VarDecl(var_id) => {
                                if let Some(var) = self.program.var_decls.get(*var_id) {
                                    write!(
                                        self.writer,
                                        "var {}",
                                        self.format_identifier(&var.name)
                                    )?;
                                    if let Some(ty) = var.ty {
                                        write!(self.writer, ": {}", self.format_type_id(ty))?;
                                    }
                                    if let Some(init_expr) = var.init {
                                        write!(
                                            self.writer,
                                            " = {}",
                                            self.format_expression_id(init_expr)
                                        )?;
                                    }
                                }
                            }
                            ForInit::Expression(expr_id) => {
                                write!(self.writer, "{}", self.format_expression_id(*expr_id))?;
                            }
                        }
                    }
                    write!(self.writer, "; ")?;
                    if let Some(cond) = condition {
                        write!(self.writer, "{}", self.format_expression_id(*cond))?;
                    }
                    write!(self.writer, "; ")?;
                    if let Some(upd) = update {
                        write!(self.writer, "{}", self.format_expression_id(*upd))?;
                    }
                    writeln!(self.writer, ") {{")?;
                    self.increase_indent();
                    self.visit_block(*body)?;
                    self.decrease_indent();
                    self.write_indent()?;
                    writeln!(self.writer, "}}")?;
                }
                Statement::Return { value, .. } => {
                    self.write_indent()?;
                    if let Some(val) = value {
                        writeln!(self.writer, "return {};", self.format_expression_id(*val))?;
                    } else {
                        writeln!(self.writer, "return;")?;
                    }
                }
                Statement::Assert { expr, .. } => {
                    self.write_indent()?;
                    writeln!(self.writer, "assert {};", self.format_expression_id(*expr))?;
                }
                Statement::Hop {
                    decorators, body, ..
                } => {
                    self.write_indent()?;
                    for decorator in decorators {
                        write!(self.writer, "@{} ", self.format_identifier(&decorator.name))?;
                    }
                    writeln!(self.writer, "hop {{")?;
                    self.increase_indent();
                    self.visit_block(*body)?;
                    self.decrease_indent();
                    self.write_indent()?;
                    writeln!(self.writer, "}}")?;
                }
                Statement::HopsFor {
                    decorators,
                    var,
                    start,
                    end,
                    body,
                    ..
                } => {
                    self.write_indent()?;
                    for decorator in decorators {
                        write!(self.writer, "@{} ", self.format_identifier(&decorator.name))?;
                    }
                    if let Some(var_decl) = self.program.var_decls.get(*var) {
                        write!(
                            self.writer,
                            "hops for {}",
                            self.format_identifier(&var_decl.name)
                        )?;
                        if let Some(ty) = var_decl.ty {
                            write!(self.writer, ": {}", self.format_type_id(ty))?;
                        }
                        writeln!(
                            self.writer,
                            " = ({}) to ({}) {{",
                            self.format_expression_id(*start),
                            self.format_expression_id(*end)
                        )?;
                    }
                    self.increase_indent();
                    self.visit_block(*body)?;
                    self.decrease_indent();
                    self.write_indent()?;
                    writeln!(self.writer, "}}")?;
                }
                Statement::Expression { expr, .. } => {
                    self.write_indent()?;
                    writeln!(self.writer, "{};", self.format_expression_id(*expr))?;
                }
                Statement::Block(block_id) => {
                    self.visit_block(*block_id)?;
                }
            }
        }
        Ok(())
    }

    fn format_type_id(&self, type_id: TypeId) -> String {
        if let Some(ty) = self.program.types.get(type_id) {
            match ty {
                Type::Named(identifier) => {
                    if SHOW_IDS {
                        format!(
                            "{} ({})",
                            self.format_identifier(identifier),
                            type_id.index()
                        )
                    } else {
                        self.format_identifier(identifier)
                    }
                }
                Type::Generic { base, args, .. } => {
                    let arg_strs: Vec<String> =
                        args.iter().map(|&id| self.format_type_id(id)).collect();
                    if SHOW_IDS {
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
                Type::Function {
                    params,
                    return_type,
                    ..
                } => {
                    let param_strs: Vec<String> =
                        params.iter().map(|&id| self.format_type_id(id)).collect();
                    if SHOW_IDS {
                        format!(
                            "({}) -> {} ({})",
                            param_strs.join(", "),
                            self.format_type_id(*return_type),
                            type_id.index()
                        )
                    } else {
                        format!(
                            "({}) -> {}",
                            param_strs.join(", "),
                            self.format_type_id(*return_type)
                        )
                    }
                }
            }
        } else {
            if SHOW_IDS {
                format!("<unknown_type> ({})", type_id.index())
            } else {
                "<unknown_type>".to_string()
            }
        }
    }

    fn format_expression_id(&self, expr_id: ExprId) -> String {
        if let Some(expr) = self.program.expressions.get(expr_id) {
            match expr {
                Expression::Literal { value, .. } => match value {
                    Literal::Integer(i) => i.clone(),
                    Literal::Float(f) => f.clone(),
                    Literal::String(s) => format!("\"{}\"", s),
                    Literal::Bool(b) => b.to_string(),
                    Literal::List(exprs) => {
                        let elements: Vec<String> = exprs
                            .iter()
                            .map(|&id| self.format_expression_id(id))
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
                                    self.format_expression_id(kv.value)
                                )
                            })
                            .collect();
                        format!("{{{}}}", pairs.join(", "))
                    }
                },
                Expression::Identifier(id) => self.format_identifier(id),
                Expression::Binary {
                    left, op, right, ..
                } => {
                    let indent = *self.indent_level.borrow();
                    format!(
                        "BinaryOp(\n{}  left: {},\n{}  op: {},\n{}  right: {}\n{})",
                        "  ".repeat(indent + 1),
                        self.format_expression_id(*left),
                        "  ".repeat(indent + 1),
                        op.value,
                        "  ".repeat(indent + 1),
                        self.format_expression_id(*right),
                        "  ".repeat(indent)
                    )
                }
                Expression::Unary { op, expr, .. } => {
                    let indent = *self.indent_level.borrow();
                    format!(
                        "UnaryOp(\n{}  op: {},\n{}  expr: {}\n{})",
                        "  ".repeat(indent + 1),
                        op.value,
                        "  ".repeat(indent + 1),
                        self.format_expression_id(*expr),
                        "  ".repeat(indent)
                    )
                }
                Expression::Assignment { lhs, rhs, .. } => {
                    format!(
                        "{} = {}",
                        self.format_expression_id(*lhs),
                        self.format_expression_id(*rhs)
                    )
                }
                Expression::Call { callee, args, .. } => {
                    let arg_strs: Vec<String> = args
                        .iter()
                        .map(|&id| self.format_expression_id(id))
                        .collect();
                    format!(
                        "{}({})",
                        self.format_expression_id(*callee),
                        arg_strs.join(", ")
                    )
                }
                Expression::MemberAccess { object, member, .. } => {
                    format!(
                        "{}.{}",
                        self.format_expression_id(*object),
                        self.format_identifier(member)
                    )
                }
                Expression::TableRowAccess {
                    table, key_values, ..
                } => {
                    let pairs: Vec<String> = key_values
                        .iter()
                        .map(|kv| {
                            format!(
                                "{}: {}",
                                self.format_identifier(&kv.key),
                                self.format_expression_id(kv.value)
                            )
                        })
                        .collect();
                    format!(
                        "{}[{}]",
                        self.format_expression_id(*table),
                        pairs.join(", ")
                    )
                }
                Expression::Grouped { expr, .. } => {
                    format!("({})", self.format_expression_id(*expr))
                }
                Expression::Lambda { params, return_type, .. } => {
                    let param_strs: Vec<String> = params
                        .iter()
                        .map(|&param_id| {
                            if let Some(param) = self.program.params.get(param_id) {
                                format!("{}: {}", self.format_identifier(&param.name), self.format_type_id(param.ty))
                            } else {
                                format!("<unknown_param_{}>", param_id.index())
                            }
                        })
                        .collect();
                    
                    format!(
                        "({}) -> {} {{ ... }}",
                        param_strs.join(", "),
                        self.format_type_id(*return_type)
                    )
                }
            }
        } else {
            if SHOW_IDS {
                format!("<unknown_expr> ({})", expr_id.index())
            } else {
                "<unknown_expr>".to_string()
            }
        }
    }

    fn format_identifier(&self, identifier: &Identifier) -> String {
        let resolved_info = match &identifier.resolved {
            Some(decl_ref) => match decl_ref {
                DeclRef::Function(id) => format!("func#{}", id.index()),
                DeclRef::Type(id) => format!("type#{}", id.index()),
                DeclRef::Const(id) => format!("const#{}", id.index()),
                DeclRef::Table(id) => format!("table#{}", id.index()),
                DeclRef::Var(id) => format!("var#{}", id.index()),
                DeclRef::Param(id) => format!("param#{}", id.index()),
                DeclRef::GenericParam(id) => format!("generic#{}", id.index()),
            },
            None => "*".to_string(),
        };

        let type_info = match &identifier.resolved_type {
            Some(resolved_type) => match resolved_type {
                ResolvedType::Type(id) => format!(":type#{}", id.index()),
                ResolvedType::Table(id) => format!(":table#{}", id.index()),
                ResolvedType::TypeVar(id) => format!(":typevar#{}", id.index()),
                ResolvedType::Generic { base, .. } => format!(":generic#{}", base.index()),
                ResolvedType::Function { .. } => ":function".to_string(),
                ResolvedType::Void => ":void".to_string(),
            },
            None => ":*".to_string(),
        };

        format!("{}[{}{}]", identifier.name, resolved_info, type_info)
    }

    fn format_identifier_with_id(&self, identifier: &Identifier, id: FunctionId) -> String {
        if SHOW_IDS {
            format!("{} ({})", identifier.name, id.index())
        } else {
            identifier.name.clone()
        }
    }
}

impl PrettyPrinter<Program> for AstPrinter {
    fn print(&self, program: &Program, writer: &mut dyn Write) -> std::io::Result<()> {
        let mut visitor = AstPrintVisitor::new(writer, program);
        visitor.visit_program()
    }
}

impl Default for AstPrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to pretty-print a program
pub fn print_program(program: &Program) -> String {
    let printer = AstPrinter::new();
    printer
        .print_to_string(program)
        .unwrap_or_else(|e| format!("Error printing program: {}", e))
}
