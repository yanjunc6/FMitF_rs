use super::PrettyPrinter;
use crate::ast::{
    AssignmentOperator, BinaryOp, Expression, ExpressionKind, FunctionDeclaration, FunctionId,
    HopBlock, HopId, HopType, LValue, PartitionDeclaration, PartitionId, Program, ReturnType,
    Statement, StatementKind, TableDeclaration, TableId, TypeName, UnaryOp, VarDecl, VarId,
};
use std::io::Write;

/// Output format for AST printing
#[derive(Debug, Clone, Copy)]
pub enum PrintMode {
    /// Compact format with minimal spacing
    Compact,
    /// Pretty format with proper indentation
    Pretty,
    /// Debug format with detailed information
    Debug,
}

/// Options for configuring AST output
#[derive(Debug, Clone)]
pub struct PrintOptions {
    pub mode: PrintMode,
    pub show_spans: bool,
    pub show_resolved_info: bool,
    pub indent_size: usize,
}

impl Default for PrintOptions {
    fn default() -> Self {
        Self {
            mode: PrintMode::Pretty,
            show_spans: false,
            show_resolved_info: false,
            indent_size: 2,
        }
    }
}

/// Pretty printer for AST structures.
/// Provides human-readable output of the abstract syntax tree.
pub struct AstPrinter {
    options: PrintOptions,
}

impl AstPrinter {
    /// Creates a new AST printer with default settings.
    pub fn new() -> Self {
        Self {
            options: PrintOptions::default(),
        }
    }

    /// Creates a new AST printer with custom options.
    pub fn with_options(options: PrintOptions) -> Self {
        Self { options }
    }

    /// Helper to write indentation
    fn write_indent(&self, writer: &mut dyn Write, level: usize) -> std::io::Result<()> {
        for _ in 0..(level * self.options.indent_size) {
            write!(writer, " ")?;
        }
        Ok(())
    }

    /// Print the entire AST program
    fn print_program(&self, program: &Program, writer: &mut dyn Write) -> std::io::Result<()> {
        writeln!(writer, "AST Program:")?;
        writeln!(writer)?;

        // Print partitions
        if !program.root_partitions.is_empty() {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Partitions:")?;
            for &partition_id in &program.root_partitions {
                if let Some(partition) = program.partitions.get(partition_id) {
                    self.print_partition(partition, partition_id, program, writer, 2)?;
                    writeln!(writer)?;
                }
            }
        }

        // Print constants
        if !program.root_constants.is_empty() {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Constants:")?;
            for &const_id in &program.root_constants {
                if let Some(const_var) = program.variables.get(const_id) {
                    self.print_constant(const_var, const_id, program, writer, 2)?;
                }
            }
            writeln!(writer)?;
        }

        // Print tables
        if !program.root_tables.is_empty() {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Tables:")?;
            for &table_id in &program.root_tables {
                if let Some(table) = program.tables.get(table_id) {
                    self.print_table(table, table_id, program, writer, 2)?;
                    writeln!(writer)?;
                }
            }
        }

        // Print functions
        if !program.root_functions.is_empty() {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Functions:")?;
            for &func_id in &program.root_functions {
                if let Some(function) = program.functions.get(func_id) {
                    self.print_function(function, func_id, program, writer, 2)?;
                    writeln!(writer)?;
                }
            }
        }

        Ok(())
    }

    /// Print a partition declaration
    fn print_partition(
        &self,
        partition: &PartitionDeclaration,
        partition_id: PartitionId,
        program: &Program,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        write!(
            writer,
            "partition {} (id: {:?})",
            partition.name, partition_id
        )?;

        // Print parameters
        write!(writer, "(")?;
        for (i, &param_id) in partition.parameters.iter().enumerate() {
            if i > 0 {
                write!(writer, ", ")?;
            }
            if let Some(param) = program.parameters.get(param_id) {
                write!(
                    writer,
                    "{} {}",
                    self.format_type(&param.param_type),
                    param.param_name
                )?;
            }
        }
        write!(writer, ")")?;

        if let Some(impl_expr_id) = partition.implementation {
            write!(writer, " = ")?;
            if let Some(expr) = program.expressions.get(impl_expr_id) {
                self.print_expression(expr, program, writer)?;
            }
        }

        if self.options.show_spans {
            write!(writer, " @{:?}", partition.span)?;
        }

        writeln!(writer)?;
        Ok(())
    }

    /// Print a constant declaration
    fn print_constant(
        &self,
        const_var: &VarDecl,
        const_id: VarId,
        _program: &Program,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        write!(
            writer,
            "const {} {} (id: {:?})",
            self.format_type(&const_var.ty),
            const_var.name,
            const_id
        )?;

        if self.options.show_spans {
            write!(writer, " @{:?}", const_var.defined_at)?;
        }

        writeln!(writer)?;
        Ok(())
    }

    /// Print a table declaration
    fn print_table(
        &self,
        table: &TableDeclaration,
        table_id: TableId,
        program: &Program,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        writeln!(writer, "table {} (id: {:?}) {{", table.name, table_id)?;

        // Print fields
        for &field_id in &table.fields {
            if let Some(field) = program.fields.get(field_id) {
                self.write_indent(writer, level + 1)?;
                let primary_marker = if field.is_primary { " (primary)" } else { "" };
                writeln!(
                    writer,
                    "{} {}{};",
                    self.format_type(&field.field_type),
                    field.field_name,
                    primary_marker
                )?;
            }
        }

        // Print node partition if present
        if let Some(ref node_partition) = table.node_partition {
            self.write_indent(writer, level + 1)?;
            write!(writer, "node {}(", node_partition.partition_name)?;
            for (i, arg) in node_partition.arguments.iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }
                write!(writer, "{}", arg)?;
            }
            write!(writer, ")")?;

            if self.options.show_resolved_info {
                if let Some(resolved_id) = node_partition.resolved_partition {
                    write!(writer, " -> {:?}", resolved_id)?;
                }
            }
            writeln!(writer, ";")?;
        }

        if self.options.show_spans {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "// span: {:?}", table.span)?;
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a function declaration
    fn print_function(
        &self,
        function: &FunctionDeclaration,
        func_id: FunctionId,
        program: &Program,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        write!(
            writer,
            "{} {} (id: {:?})",
            self.format_return_type(&function.return_type),
            function.name,
            func_id
        )?;

        // Print parameters
        write!(writer, "(")?;
        for (i, &param_id) in function.parameters.iter().enumerate() {
            if i > 0 {
                write!(writer, ", ")?;
            }
            if let Some(param) = program.parameters.get(param_id) {
                write!(
                    writer,
                    "{} {}",
                    self.format_type(&param.param_type),
                    param.param_name
                )?;
            }
        }
        writeln!(writer, ") {{")?;

        // Print hops
        for (hop_index, &hop_id) in function.hops.iter().enumerate() {
            if let Some(hop) = program.hops.get(hop_id) {
                self.print_hop(hop, hop_id, hop_index, program, writer, level + 1)?;
                writeln!(writer)?;
            }
        }

        if self.options.show_spans {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "// span: {:?}", function.span)?;
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a hop block
    fn print_hop(
        &self,
        hop: &HopBlock,
        hop_id: HopId,
        hop_index: usize,
        program: &Program,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;

        match &hop.hop_type {
            HopType::Simple => {
                writeln!(writer, "hop #{} (id: {:?}) {{", hop_index, hop_id)?;
            }
            HopType::ForLoop {
                loop_var,
                loop_var_type,
                start,
                end,
                start_value: _,
                end_value: _,
            } => {
                write!(
                    writer,
                    "hops for {} {} = ",
                    self.format_type(loop_var_type),
                    loop_var
                )?;
                if let Some(start_expr) = program.expressions.get(*start) {
                    self.print_expression(start_expr, program, writer)?;
                }
                write!(writer, " to ")?;
                if let Some(end_expr) = program.expressions.get(*end) {
                    self.print_expression(end_expr, program, writer)?;
                }
                writeln!(writer, " {{ // hop #{} (id: {:?})", hop_index, hop_id)?;
            }
        }

        // Print statements
        for &stmt_id in &hop.statements {
            if let Some(stmt) = program.statements.get(stmt_id) {
                self.print_statement(stmt, program, writer, level + 1)?;
            }
        }

        if self.options.show_spans {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "// span: {:?}", hop.span)?;
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a statement
    fn print_statement(
        &self,
        stmt: &Statement,
        program: &Program,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;

        match &stmt.node {
            StatementKind::Assignment(assign) => {
                self.print_lvalue(&assign.lvalue, program, writer)?;
                write!(writer, " {} ", self.format_assignment_op(&assign.operator))?;
                if let Some(rhs) = program.expressions.get(assign.rhs) {
                    self.print_expression(rhs, program, writer)?;
                }
                writeln!(writer, ";")?;
            }
            StatementKind::VarDecl(var_decl) => {
                write!(
                    writer,
                    "{} {}",
                    self.format_type(&var_decl.var_type),
                    var_decl.var_name
                )?;
                if let Some(init_expr_id) = var_decl.init_value {
                    if let Some(init) = program.expressions.get(init_expr_id) {
                        write!(writer, " = ")?;
                        self.print_expression(init, program, writer)?;
                    }
                }
                writeln!(writer, ";")?;
            }
            StatementKind::IfStmt(if_stmt) => {
                write!(writer, "if (")?;
                if let Some(cond) = program.expressions.get(if_stmt.condition) {
                    self.print_expression(cond, program, writer)?;
                }
                writeln!(writer, ") {{")?;

                for &stmt_id in &if_stmt.then_branch {
                    if let Some(stmt) = program.statements.get(stmt_id) {
                        self.print_statement(stmt, program, writer, level + 1)?;
                    }
                }

                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.write_indent(writer, level)?;
                    writeln!(writer, "}} else {{")?;
                    for &stmt_id in else_branch {
                        if let Some(stmt) = program.statements.get(stmt_id) {
                            self.print_statement(stmt, program, writer, level + 1)?;
                        }
                    }
                }

                self.write_indent(writer, level)?;
                writeln!(writer, "}}")?;
            }
            StatementKind::ForStmt(for_stmt) => {
                write!(
                    writer,
                    "for ({} {} = ",
                    self.format_type(&for_stmt.loop_var_type),
                    for_stmt.loop_var
                )?;
                if let Some(init) = program.expressions.get(for_stmt.init) {
                    self.print_expression(init, program, writer)?;
                }
                write!(writer, "; ")?;
                if let Some(cond) = program.expressions.get(for_stmt.condition) {
                    self.print_expression(cond, program, writer)?;
                }
                write!(writer, "; ")?;
                if let Some(inc) = program.expressions.get(for_stmt.increment) {
                    self.print_expression(inc, program, writer)?;
                }
                writeln!(writer, ") {{")?;

                for &stmt_id in &for_stmt.body {
                    if let Some(stmt) = program.statements.get(stmt_id) {
                        self.print_statement(stmt, program, writer, level + 1)?;
                    }
                }

                self.write_indent(writer, level)?;
                writeln!(writer, "}}")?;
            }
            StatementKind::WhileStmt(while_stmt) => {
                write!(writer, "while (")?;
                if let Some(cond) = program.expressions.get(while_stmt.condition) {
                    self.print_expression(cond, program, writer)?;
                }
                writeln!(writer, ") {{")?;

                for &stmt_id in &while_stmt.body {
                    if let Some(stmt) = program.statements.get(stmt_id) {
                        self.print_statement(stmt, program, writer, level + 1)?;
                    }
                }

                self.write_indent(writer, level)?;
                writeln!(writer, "}}")?;
            }
            StatementKind::Return(ret_stmt) => {
                write!(writer, "return")?;
                if let Some(value) = ret_stmt.value {
                    write!(writer, " ")?;
                    if let Some(expr) = program.expressions.get(value) {
                        self.print_expression(expr, program, writer)?;
                    }
                }
                writeln!(writer, ";")?;
            }
            StatementKind::Abort(_) => {
                writeln!(writer, "abort;")?;
            }
            StatementKind::Break(_) => {
                writeln!(writer, "break;")?;
            }
            StatementKind::Continue(_) => {
                writeln!(writer, "continue;")?;
            }
            StatementKind::Expression(expr_stmt) => {
                if let Some(expr) = program.expressions.get(expr_stmt.expression) {
                    self.print_expression(expr, program, writer)?;
                }
                writeln!(writer, ";")?;
            }
            StatementKind::Empty => {
                writeln!(writer, ";")?;
            }
        }

        Ok(())
    }

    /// Print an lvalue
    fn print_lvalue(
        &self,
        lvalue: &LValue,
        program: &Program,
        writer: &mut dyn Write,
    ) -> std::io::Result<()> {
        match lvalue {
            LValue::Var { name, resolved_var } => {
                write!(writer, "{}", name)?;
                if self.options.show_resolved_info {
                    if let Some(var_id) = resolved_var {
                        write!(writer, "/*{:?}*/", var_id)?;
                    }
                }
            }
            LValue::TableField {
                table_name,
                pk_exprs,
                field_name,
                ..
            } => {
                write!(writer, "{}[", table_name)?;
                for (i, &pk_expr_id) in pk_exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    if let Some(expr) = program.expressions.get(pk_expr_id) {
                        self.print_expression(expr, program, writer)?;
                    }
                }
                write!(writer, "].{}", field_name)?;
            }
            LValue::TableRecord {
                table_name,
                pk_exprs,
                ..
            } => {
                write!(writer, "{}[", table_name)?;
                for (i, &pk_expr_id) in pk_exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    if let Some(expr) = program.expressions.get(pk_expr_id) {
                        self.print_expression(expr, program, writer)?;
                    }
                }
                write!(writer, "]")?;
            }
            LValue::ArrayElement {
                array_name, index, ..
            } => {
                write!(writer, "{}[", array_name)?;
                if let Some(idx_expr) = program.expressions.get(*index) {
                    self.print_expression(idx_expr, program, writer)?;
                }
                write!(writer, "]")?;
            }
            LValue::FieldAccess {
                object_name,
                field_name,
                ..
            } => {
                write!(writer, "{}.{}", object_name, field_name)?;
            }
        }
        Ok(())
    }

    /// Print an expression
    fn print_expression(
        &self,
        expr: &Expression,
        program: &Program,
        writer: &mut dyn Write,
    ) -> std::io::Result<()> {
        match &expr.node {
            ExpressionKind::Ident(name) => {
                write!(writer, "{}", name)?;
            }
            ExpressionKind::IntLit(value) => {
                write!(writer, "{}", value)?;
            }
            ExpressionKind::FloatLit(value) => {
                write!(writer, "{}", value)?;
            }
            ExpressionKind::StringLit(value) => {
                write!(writer, "\"{}\"", value)?;
            }
            ExpressionKind::BoolLit(value) => {
                write!(writer, "{}", value)?;
            }
            ExpressionKind::TableFieldAccess {
                table_name,
                pk_exprs,
                field_name,
                ..
            } => {
                write!(writer, "{}[", table_name)?;
                for (i, &pk_expr_id) in pk_exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    if let Some(expr) = program.expressions.get(pk_expr_id) {
                        self.print_expression(expr, program, writer)?;
                    }
                }
                write!(writer, "].{}", field_name)?;
            }
            ExpressionKind::TableAccess {
                table_name,
                pk_exprs,
                ..
            } => {
                write!(writer, "{}[", table_name)?;
                for (i, &pk_expr_id) in pk_exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    if let Some(expr) = program.expressions.get(pk_expr_id) {
                        self.print_expression(expr, program, writer)?;
                    }
                }
                write!(writer, "]")?;
            }
            ExpressionKind::ArrayAccess {
                array_name, index, ..
            } => {
                write!(writer, "{}[", array_name)?;
                if let Some(idx_expr) = program.expressions.get(*index) {
                    self.print_expression(idx_expr, program, writer)?;
                }
                write!(writer, "]")?;
            }
            ExpressionKind::RecordLiteral { fields, .. } => {
                write!(writer, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    write!(writer, "{}: ", field.field_name)?;
                    if let Some(value_expr) = program.expressions.get(field.value) {
                        self.print_expression(value_expr, program, writer)?;
                    }
                }
                write!(writer, "}}")?;
            }
            ExpressionKind::ArrayLiteral { elements, .. } => {
                write!(writer, "{{")?;
                for (i, element_id) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    if let Some(element_expr) = program.expressions.get(*element_id) {
                        self.print_expression(element_expr, program, writer)?;
                    }
                }
                write!(writer, "}}")?;
            }
            ExpressionKind::UnaryOp { op, expr, .. } => {
                write!(writer, "{}", self.format_unary_op(op))?;
                if let Some(inner_expr) = program.expressions.get(*expr) {
                    self.print_expression(inner_expr, program, writer)?;
                }
            }
            ExpressionKind::FieldAccess {
                object_name,
                field_name,
                ..
            } => {
                write!(writer, "{}.{}", object_name, field_name)?;
            }
            ExpressionKind::BinaryOp {
                left, op, right, ..
            } => {
                write!(writer, "(")?;
                if let Some(left_expr) = program.expressions.get(*left) {
                    self.print_expression(left_expr, program, writer)?;
                }
                write!(writer, " {} ", self.format_binary_op(op))?;
                if let Some(right_expr) = program.expressions.get(*right) {
                    self.print_expression(right_expr, program, writer)?;
                }
                write!(writer, ")")?;
            }
        }
        Ok(())
    }

    /// Format type name
    fn format_type(&self, ty: &TypeName) -> String {
        match ty {
            TypeName::Int => "int".to_string(),
            TypeName::Float => "float".to_string(),
            TypeName::String => "string".to_string(),
            TypeName::Bool => "bool".to_string(),
            TypeName::Array {
                element_type, size, ..
            } => match size {
                Some(n) => format!("{}[{}]", self.format_type(element_type), n),
                None => format!("{}[]", self.format_type(element_type)),
            },
            TypeName::Table(name) => name.clone(),
        }
    }

    /// Format return type
    fn format_return_type(&self, ret_ty: &ReturnType) -> String {
        match ret_ty {
            ReturnType::Void => "void".to_string(),
            ReturnType::Type(ty) => self.format_type(ty),
        }
    }

    /// Format assignment operator
    fn format_assignment_op(&self, op: &AssignmentOperator) -> &'static str {
        match op {
            AssignmentOperator::Assign => "=",
            AssignmentOperator::AddAssign => "+=",
            AssignmentOperator::SubAssign => "-=",
            AssignmentOperator::MulAssign => "*=",
            AssignmentOperator::DivAssign => "/=",
        }
    }

    /// Format unary operator
    fn format_unary_op(&self, op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
            UnaryOp::PreIncrement => "++",
            UnaryOp::PostIncrement => "++",
            UnaryOp::PreDecrement => "--",
            UnaryOp::PostDecrement => "--",
        }
    }

    /// Format binary operator
    fn format_binary_op(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

impl Default for AstPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl PrettyPrinter<Program> for AstPrinter {
    fn print(&self, program: &Program, writer: &mut dyn Write) -> std::io::Result<()> {
        self.print_program(program, writer)
    }
}

/// Convenience function to print a program with default options
pub fn print_program(program: &Program, writer: &mut dyn Write) -> std::io::Result<()> {
    let printer = AstPrinter::new();
    printer.print(program, writer)
}

/// Convenience function to print a program to a string
pub fn print_program_to_string(program: &Program) -> Result<String, std::io::Error> {
    let mut buffer = Vec::new();
    print_program(program, &mut buffer)?;
    Ok(String::from_utf8_lossy(&buffer).to_string())
}

/// Print program to a writer with custom options
pub fn print_program_to_writer(
    program: &Program,
    options: &PrintOptions,
    writer: &mut dyn Write,
) -> std::io::Result<()> {
    let printer = AstPrinter::with_options(options.clone());
    printer.print(program, writer)
}
