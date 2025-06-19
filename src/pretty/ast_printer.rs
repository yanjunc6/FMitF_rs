use crate::ast::*;
use std::io::{Result, Write};

#[derive(Debug, Clone)]
pub struct PrintOptions {
    pub mode: PrintMode,
    pub show_spans: bool,
}

#[derive(Debug, Clone)]
pub enum PrintMode {
    Verbose,
    Summary,
}

impl Default for PrintOptions {
    fn default() -> Self {
        Self {
            mode: PrintMode::Verbose,
            show_spans: false,
        }
    }
}

pub fn print_program(program: &Program, opts: &PrintOptions) {
    let mut printer = Printer::new(opts);
    printer.print_program(program);
}

pub fn print_program_to_writer(
    program: &Program,
    opts: &PrintOptions,
    writer: &mut impl Write,
) -> Result<()> {
    let mut printer = WriterPrinter::new(opts, writer);
    printer.print_program(program)
}

struct Printer<'a> {
    opts: &'a PrintOptions,
    depth: usize,
}

impl<'a> Printer<'a> {
    fn new(opts: &'a PrintOptions) -> Self {
        Self { opts, depth: 0 }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.depth)
    }

    fn with_depth(&mut self, depth: usize) -> &mut Self {
        self.depth = depth;
        self
    }

    fn span(&self, span: &Span) -> String {
        if self.opts.show_spans {
            format!(" @{}:{}", span.line, span.column)
        } else {
            String::new()
        }
    }

    fn print_program(&mut self, program: &Program) {
        match self.opts.mode {
            PrintMode::Summary => self.print_summary(program),
            PrintMode::Verbose => self.print_verbose(program),
        }
    }

    fn print_summary(&mut self, program: &Program) {
        println!("AST Summary:");

        println!(" - Total Nodes: {}", program.root_nodes.len());
        println!(" - Total Tables: {}", program.root_tables.len());
        println!(" - Total Functions: {}", program.root_functions.len());

        println!("Tables:");
        for &table_id in &program.root_tables {
            let table = &program.tables[table_id];
            let node_name = &program.nodes[table.node].name;

            let fields: Vec<String> = table
                .fields
                .iter()
                .map(|&field_id| {
                    let field = &program.fields[field_id];
                    let prefix = if field.is_primary { "primary " } else { "" };
                    format!(
                        "{}{}:{}",
                        prefix,
                        field.field_name,
                        type_name(&field.field_type)
                    )
                })
                .collect();
            println!(" - {} on {}: {}", table.name, node_name, fields.join(", "));
        }

        println!("Functions:");
        for &func_id in &program.root_functions {
            let func = &program.functions[func_id];
            let params: Vec<String> = func
                .parameters
                .iter()
                .map(|&param_id| {
                    let param = &program.parameters[param_id];
                    format!("{}:{}", param.param_name, type_name(&param.param_type))
                })
                .collect();

            println!(
                " - {}({}) -> {}",
                func.name,
                params.join(", "),
                return_type(&func.return_type)
            );
        }
    }

    fn print_verbose(&mut self, program: &Program) {
        println!("Program");
        self.with_depth(1).print_nodes(program);
        self.with_depth(1).print_tables(program);
        self.with_depth(1).print_functions(program);
    }

    fn print_nodes(&mut self, program: &Program) {
        println!("{}nodes[{}]", self.indent(), program.root_nodes.len());
        for (i, &node_id) in program.root_nodes.iter().enumerate() {
            let node = &program.nodes[node_id];
            println!(
                "{}[{}] NodeDef{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&node.span)
            );
            println!(
                "{}name: {}",
                self.with_depth(self.depth + 2).indent(),
                node.name
            );
        }
    }

    fn print_tables(&mut self, program: &Program) {
        println!("{}tables[{}]", self.indent(), program.root_tables.len());
        for (i, &table_id) in program.root_tables.iter().enumerate() {
            let table = &program.tables[table_id];
            let node = &program.nodes[table.node];
            
            println!(
                "{}[{}] TableDeclaration{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&table.span)
            );
            println!(
                "{}name: {}",
                self.with_depth(self.depth + 2).indent(),
                table.name
            );
            println!(
                "{}node: {}",
                self.with_depth(self.depth + 2).indent(),
                node.name
            );
            
            // Print all primary key fields
            print!(
                "{}primary_keys: [",
                self.with_depth(self.depth + 2).indent()
            );
            for (j, &pk_field_id) in table.primary_keys.iter().enumerate() {
                let pk_field = &program.fields[pk_field_id];
                if j > 0 { print!(", "); }
                print!("{}", pk_field.field_name);
            }
            println!("]");
            
            self.with_depth(self.depth + 2)
                .print_fields(program, &table.fields);
        }
    }

    fn print_fields(&mut self, program: &Program, field_ids: &[FieldId]) {
        println!("{}fields[{}]", self.indent(), field_ids.len());
        for (i, &field_id) in field_ids.iter().enumerate() {
            let field = &program.fields[field_id];
            println!(
                "{}[{}] FieldDeclaration{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&field.span)
            );
            println!(
                "{}field_type: {}",
                self.with_depth(self.depth + 2).indent(),
                type_name(&field.field_type)
            );
            println!(
                "{}field_name: {}",
                self.with_depth(self.depth + 2).indent(),
                field.field_name
            );
            println!(
                "{}is_primary: {}",
                self.with_depth(self.depth + 2).indent(),
                field.is_primary
            );
        }
    }

    fn print_functions(&mut self, program: &Program) {
        println!(
            "{}functions[{}]",
            self.indent(),
            program.root_functions.len()
        );
        for (i, &func_id) in program.root_functions.iter().enumerate() {
            let func = &program.functions[func_id];
            println!(
                "{}[{}] FunctionDeclaration{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&func.span)
            );
            println!(
                "{}return_type: {}",
                self.with_depth(self.depth + 2).indent(),
                return_type(&func.return_type)
            );
            println!(
                "{}name: {}",
                self.with_depth(self.depth + 2).indent(),
                func.name
            );
            self.with_depth(self.depth + 2)
                .print_parameters(program, &func.parameters);
            self.with_depth(self.depth + 2)
                .print_hops(program, &func.hops);
        }
    }

    fn print_parameters(&mut self, program: &Program, param_ids: &[ParameterId]) {
        println!("{}parameters[{}]", self.indent(), param_ids.len());
        for (i, &param_id) in param_ids.iter().enumerate() {
            let param = &program.parameters[param_id];
            println!(
                "{}[{}] ParameterDecl{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&param.span)
            );
            println!(
                "{}param_type: {}",
                self.with_depth(self.depth + 2).indent(),
                type_name(&param.param_type)
            );
            println!(
                "{}param_name: {}",
                self.with_depth(self.depth + 2).indent(),
                param.param_name
            );
        }
    }

    fn print_hops(&mut self, program: &Program, hop_ids: &[HopId]) {
        println!("{}hops[{}]", self.indent(), hop_ids.len());
        for (i, &hop_id) in hop_ids.iter().enumerate() {
            let hop = &program.hops[hop_id];

            println!(
                "{}[{}] HopBlock{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&hop.span)
            );
            println!(
                "{}node_name: {}",
                self.with_depth(self.depth + 2).indent(),
                hop.node_name
            );

            if let Some(resolved_node) = hop.resolved_node {
                let node = &program.nodes[resolved_node];
                println!(
                    "{}resolved_node: {} ({})",
                    self.with_depth(self.depth + 2).indent(),
                    node.name,
                    resolved_node.index()
                );
            } else {
                println!(
                    "{}resolved_node: None",
                    self.with_depth(self.depth + 2).indent()
                );
            }

            self.with_depth(self.depth + 2)
                .print_statements(program, &hop.statements);
        }
    }

    fn print_statements(&mut self, program: &Program, stmt_ids: &[StatementId]) {
        println!("{}statements[{}]", self.indent(), stmt_ids.len());
        for (i, &stmt_id) in stmt_ids.iter().enumerate() {
            let stmt = &program.statements[stmt_id];
            self.with_depth(self.depth + 1)
                .print_statement_with_index(program, i, stmt);
        }
    }

    fn print_statement_with_index(&mut self, program: &Program, index: usize, stmt: &Statement) {
        match &stmt.node {
            StatementKind::VarDecl(v) => {
                println!(
                    "{}[{}] VarDeclStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!(
                    "{}var_type: {}",
                    self.with_depth(self.depth + 1).indent(),
                    type_name(&v.var_type)
                );
                println!(
                    "{}var_name: {}",
                    self.with_depth(self.depth + 1).indent(),
                    v.var_name
                );
                println!("{}init_value:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, v.init_value);
            }
            StatementKind::VarAssignment(v) => {
                println!(
                    "{}[{}] VarAssignmentStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!(
                    "{}var_name: {}",
                    self.with_depth(self.depth + 1).indent(),
                    v.var_name
                );

                if let Some(resolved_var) = v.resolved_var {
                    let var = &program.variables[resolved_var];
                    println!(
                        "{}resolved_var: {} ({}) - {:?}",
                        self.with_depth(self.depth + 1).indent(),
                        var.name,
                        resolved_var.index(),
                        var.kind
                    );
                } else {
                    println!(
                        "{}resolved_var: None",
                        self.with_depth(self.depth + 1).indent()
                    );
                }

                println!("{}rhs:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, v.rhs);
            }
            StatementKind::Assignment(a) => {
                println!(
                    "{}[{}] AssignmentStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!(
                    "{}table_name: {}",
                    self.with_depth(self.depth + 1).indent(),
                    a.table_name
                );
                
                // Print primary key fields
                print!("{}pk_fields: [", self.with_depth(self.depth + 1).indent());
                for (i, pk_field) in a.pk_fields.iter().enumerate() {
                    if i > 0 { print!(", "); }
                    print!("{}", pk_field);
                }
                println!("]");
                
                println!(
                    "{}field_name: {}",
                    self.with_depth(self.depth + 1).indent(),
                    a.field_name
                );

                if let Some(resolved_table) = a.resolved_table {
                    let table = &program.tables[resolved_table];
                    println!(
                        "{}resolved_table: {} ({})",
                        self.with_depth(self.depth + 1).indent(),
                        table.name,
                        resolved_table.index()
                    );
                } else {
                    println!(
                        "{}resolved_table: None",
                        self.with_depth(self.depth + 1).indent()
                    );
                }

                // Print resolved primary key fields
                print!("{}resolved_pk_fields: [", self.with_depth(self.depth + 1).indent());
                for (i, resolved_pk_field) in a.resolved_pk_fields.iter().enumerate() {
                    if i > 0 { print!(", "); }
                    if let Some(pk_field_id) = resolved_pk_field {
                        let pk_field = &program.fields[*pk_field_id];
                        print!("{} ({})", pk_field.field_name, pk_field_id.index());
                    } else {
                        print!("None");
                    }
                }
                println!("]");

                if let Some(resolved_field) = a.resolved_field {
                    let field = &program.fields[resolved_field];
                    println!(
                        "{}resolved_field: {} ({})",
                        self.with_depth(self.depth + 1).indent(),
                        field.field_name,
                        resolved_field.index()
                    );
                } else {
                    println!(
                        "{}resolved_field: None",
                        self.with_depth(self.depth + 1).indent()
                    );
                }

                // Print primary key expressions
                println!("{}pk_exprs:", self.with_depth(self.depth + 1).indent());
                for (i, &pk_expr) in a.pk_exprs.iter().enumerate() {
                    println!("{}[{}]:", self.with_depth(self.depth + 2).indent(), i);
                    self.with_depth(self.depth + 3).print_expression(program, pk_expr);
                }
                
                println!("{}rhs:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, a.rhs);
            }
            StatementKind::Return(r) => {
                println!(
                    "{}[{}] ReturnStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!("{}value:", self.with_depth(self.depth + 1).indent());
                match &r.value {
                    Some(expr_id) => self
                        .with_depth(self.depth + 2)
                        .print_expression(program, *expr_id),
                    None => println!("{}None", self.with_depth(self.depth + 2).indent()),
                }
            }
            StatementKind::Abort(_) => {
                println!(
                    "{}[{}] AbortStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
            }
            StatementKind::IfStmt(i) => {
                println!(
                    "{}[{}] IfStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!("{}condition:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, i.condition);
                println!("{}then_branch:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_statements(program, &i.then_branch);
                println!("{}else_branch:", self.with_depth(self.depth + 1).indent());
                match &i.else_branch {
                    Some(else_stmts) => self
                        .with_depth(self.depth + 2)
                        .print_statements(program, else_stmts),
                    None => println!("{}None", self.with_depth(self.depth + 2).indent()),
                }
            }
            StatementKind::WhileStmt(w) => {
                println!(
                    "{}[{}] WhileStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!("{}condition:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, w.condition);
                println!("{}body:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_statements(program, &w.body);
            }
            StatementKind::Break(_) => {
                println!(
                    "{}[{}] BreakStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
            }
            StatementKind::Continue(_) => {
                println!(
                    "{}[{}] ContinueStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
            }
            StatementKind::Empty => {
                println!(
                    "{}[{}] Empty{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
            }
        }
    }

    fn print_expression(&mut self, program: &Program, expr_id: ExpressionId) {
        let expr = &program.expressions[expr_id];
        println!("{}Expression{}", self.indent(), self.span(&expr.span));
        self.with_depth(self.depth + 1)
            .print_expression_kind(program, &expr.node);
    }

    fn print_expression_kind(&mut self, program: &Program, expr: &ExpressionKind) {
        match expr {
            ExpressionKind::Ident(name) => {
                println!("{}Ident {}", self.indent(), name);
            }
            ExpressionKind::IntLit(value) => {
                println!("{}IntLit {}", self.indent(), value);
            }
            ExpressionKind::FloatLit(value) => {
                println!("{}FloatLit {}", self.indent(), value);
            }
            ExpressionKind::StringLit(value) => {
                println!("{}StringLit \"{}\"", self.indent(), value);
            }
            ExpressionKind::BoolLit(value) => {
                println!("{}BoolLit {}", self.indent(), value);
            }
            ExpressionKind::TableFieldAccess {
                table_name,
                pk_fields,
                pk_exprs,
                field_name,
                resolved_table,
                resolved_pk_fields,
                resolved_field,
            } => {
                println!("{}TableFieldAccess", self.indent());
                println!(
                    "{}table_name: {}",
                    self.with_depth(self.depth + 1).indent(),
                    table_name
                );
                
                // Print primary key fields
                print!("{}pk_fields: [", self.with_depth(self.depth + 1).indent());
                for (i, pk_field) in pk_fields.iter().enumerate() {
                    if i > 0 { print!(", "); }
                    print!("{}", pk_field);
                }
                println!("]");
                
                println!(
                    "{}field_name: {}",
                    self.with_depth(self.depth + 1).indent(),
                    field_name
                );

                if let Some(resolved_table_id) = resolved_table {
                    let table = &program.tables[*resolved_table_id];
                    println!(
                        "{}resolved_table: {} ({})",
                        self.with_depth(self.depth + 1).indent(),
                        table.name,
                        resolved_table_id.index()
                    );
                } else {
                    println!(
                        "{}resolved_table: None",
                        self.with_depth(self.depth + 1).indent()
                    );
                }

                // Print resolved primary key fields
                print!("{}resolved_pk_fields: [", self.with_depth(self.depth + 1).indent());
                for (i, resolved_pk_field) in resolved_pk_fields.iter().enumerate() {
                    if i > 0 { print!(", "); }
                    if let Some(pk_field_id) = resolved_pk_field {
                        let pk_field = &program.fields[*pk_field_id];
                        print!("{} ({})", pk_field.field_name, pk_field_id.index());
                    } else {
                        print!("None");
                    }
                }
                println!("]");

                if let Some(resolved_field_id) = resolved_field {
                    let field = &program.fields[*resolved_field_id];
                    println!(
                        "{}resolved_field: {} ({})",
                        self.with_depth(self.depth + 1).indent(),
                        field.field_name,
                        resolved_field_id.index()
                    );
                } else {
                    println!(
                        "{}resolved_field: None",
                        self.with_depth(self.depth + 1).indent()
                    );
                }

                // Print primary key expressions
                println!("{}pk_exprs:", self.with_depth(self.depth + 1).indent());
                for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                    println!("{}[{}]:", self.with_depth(self.depth + 2).indent(), i);
                    self.with_depth(self.depth + 3).print_expression(program, pk_expr);
                }
            }
            ExpressionKind::UnaryOp { op, expr } => {
                println!("{}UnaryOp", self.indent());
                println!("{}op: {:?}", self.with_depth(self.depth + 1).indent(), op);
                println!("{}expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, *expr);
            }
            ExpressionKind::BinaryOp { left, op, right } => {
                println!("{}BinaryOp", self.indent());
                println!("{}left:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, *left);
                println!("{}op: {:?}", self.with_depth(self.depth + 1).indent(), op);
                println!("{}right:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2)
                    .print_expression(program, *right);
            }
        }
    }
}

// WriterPrinter for writing to any writer
struct WriterPrinter<'a, W: Write> {
    opts: &'a PrintOptions,
    depth: usize,
    writer: &'a mut W,
}

impl<'a, W: Write> WriterPrinter<'a, W> {
    fn new(opts: &'a PrintOptions, writer: &'a mut W) -> Self {
        Self {
            opts,
            depth: 0,
            writer,
        }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.depth)
    }

    fn span(&self, span: &Span) -> String {
        if self.opts.show_spans {
            format!(" @{}:{}", span.line, span.column)
        } else {
            String::new()
        }
    }

    fn print_program(&mut self, program: &Program) -> Result<()> {
        match self.opts.mode {
            PrintMode::Summary => self.print_summary(program),
            PrintMode::Verbose => self.print_verbose(program),
        }
    }

    fn print_summary(&mut self, program: &Program) -> Result<()> {
        writeln!(self.writer, "AST Summary:")?;

        writeln!(self.writer, " - Total Nodes: {}", program.root_nodes.len())?;
        writeln!(self.writer, " - Total Tables: {}", program.root_tables.len())?;
        writeln!(self.writer, " - Total Functions: {}", program.root_functions.len())?;

        writeln!(self.writer, "Tables:")?;
        for &table_id in &program.root_tables {
            let table = &program.tables[table_id];
            let node_name = &program.nodes[table.node].name;

            let fields: Vec<String> = table
                .fields
                .iter()
                .map(|&field_id| {
                    let field = &program.fields[field_id];
                    let prefix = if field.is_primary { "primary " } else { "" };
                    format!(
                        "{}{}:{}",
                        prefix,
                        field.field_name,
                        type_name(&field.field_type)
                    )
                })
                .collect();
            writeln!(
                self.writer,
                " - {} on {}: {}",
                table.name,
                node_name,
                fields.join(", ")
            )?;
        }

        writeln!(self.writer, "Functions:")?;
        for &func_id in &program.root_functions {
            let func = &program.functions[func_id];
            let params: Vec<String> = func
                .parameters
                .iter()
                .map(|&param_id| {
                    let param = &program.parameters[param_id];
                    format!("{}:{}", param.param_name, type_name(&param.param_type))
                })
                .collect();

            writeln!(
                self.writer,
                " - {}({}) -> {}",
                func.name,
                params.join(", "),
                return_type(&func.return_type)
            )?;
        }
        Ok(())
    }

    fn print_verbose(&mut self, program: &Program) -> Result<()> {
        writeln!(self.writer, "Program")?;
        self.depth = 1;
        self.print_nodes(program)?;
        self.print_tables(program)?;
        self.print_functions(program)?;
        Ok(())
    }

    fn print_nodes(&mut self, program: &Program) -> Result<()> {
        writeln!(
            self.writer,
            "{}nodes[{}]",
            self.indent(),
            program.root_nodes.len()
        )?;
        for (i, &node_id) in program.root_nodes.iter().enumerate() {
            let node = &program.nodes[node_id];
            let indent1 = "  ".repeat(self.depth + 1);
            let indent2 = "  ".repeat(self.depth + 2);
            writeln!(
                self.writer,
                "{}[{}] NodeDef{}",
                indent1,
                i,
                self.span(&node.span)
            )?;
            writeln!(self.writer, "{}name: {}", indent2, node.name)?;
        }
        Ok(())
    }

    fn print_tables(&mut self, program: &Program) -> Result<()> {
        writeln!(
            self.writer,
            "{}tables[{}]",
            self.indent(),
            program.root_tables.len()
        )?;
        for (i, &table_id) in program.root_tables.iter().enumerate() {
            let table = &program.tables[table_id];
            let node = &program.nodes[table.node];

            let indent1 = "  ".repeat(self.depth + 1);
            let indent2 = "  ".repeat(self.depth + 2);

            writeln!(
                self.writer,
                "{}[{}] TableDeclaration{}",
                indent1,
                i,
                self.span(&table.span)
            )?;
            writeln!(self.writer, "{}name: {}", indent2, table.name)?;
            writeln!(self.writer, "{}node: {}", indent2, node.name)?;
            
            // Write all primary key fields
            write!(self.writer, "{}primary_keys: [", indent2)?;
            for (j, &pk_field_id) in table.primary_keys.iter().enumerate() {
                let pk_field = &program.fields[pk_field_id];
                if j > 0 { write!(self.writer, ", ")?; }
                write!(self.writer, "{}", pk_field.field_name)?;
            }
            writeln!(self.writer, "]")?;
            
            self.depth += 2;
            self.print_fields(program, &table.fields)?;
            self.depth -= 2;
        }
        Ok(())
    }

    fn print_fields(&mut self, program: &Program, field_ids: &[FieldId]) -> Result<()> {
        writeln!(self.writer, "{}fields[{}]", self.indent(), field_ids.len())?;
        for (i, &field_id) in field_ids.iter().enumerate() {
            let field = &program.fields[field_id];
            let indent1 = "  ".repeat(self.depth + 1);
            let indent2 = "  ".repeat(self.depth + 2);
            writeln!(
                self.writer,
                "{}[{}] FieldDeclaration{}",
                indent1,
                i,
                self.span(&field.span)
            )?;
            writeln!(
                self.writer,
                "{}field_type: {}",
                indent2,
                type_name(&field.field_type)
            )?;
            writeln!(self.writer, "{}field_name: {}", indent2, field.field_name)?;
            writeln!(self.writer, "{}is_primary: {}", indent2, field.is_primary)?;
        }
        Ok(())
    }

    fn print_functions(&mut self, program: &Program) -> Result<()> {
        writeln!(
            self.writer,
            "{}functions[{}]",
            self.indent(),
            program.root_functions.len()
        )?;
        for (i, &func_id) in program.root_functions.iter().enumerate() {
            let func = &program.functions[func_id];
            let indent1 = "  ".repeat(self.depth + 1);
            let indent2 = "  ".repeat(self.depth + 2);
            writeln!(
                self.writer,
                "{}[{}] FunctionDeclaration{}",
                indent1,
                i,
                self.span(&func.span)
            )?;
            writeln!(
                self.writer,
                "{}return_type: {}",
                indent2,
                return_type(&func.return_type)
            )?;
            writeln!(self.writer, "{}name: {}", indent2, func.name)?;
            self.depth += 2;
            self.print_parameters(program, &func.parameters)?;
            self.print_hops(program, &func.hops)?;
            self.depth -= 2;
        }
        Ok(())
    }

    fn print_parameters(&mut self, program: &Program, param_ids: &[ParameterId]) -> Result<()> {
        writeln!(
            self.writer,
            "{}parameters[{}]",
            self.indent(),
            param_ids.len()
        )?;
        for (i, &param_id) in param_ids.iter().enumerate() {
            let param = &program.parameters[param_id];
            let indent1 = "  ".repeat(self.depth + 1);
            let indent2 = "  ".repeat(self.depth + 2);
            writeln!(
                self.writer,
                "{}[{}] ParameterDecl{}",
                indent1,
                i,
                self.span(&param.span)
            )?;
            writeln!(
                self.writer,
                "{}param_type: {}",
                indent2,
                type_name(&param.param_type)
            )?;
            writeln!(self.writer, "{}param_name: {}", indent2, param.param_name)?;
        }
        Ok(())
    }

    fn print_hops(&mut self, program: &Program, hop_ids: &[HopId]) -> Result<()> {
        writeln!(self.writer, "{}hops[{}]", self.indent(), hop_ids.len())?;
        for (i, &hop_id) in hop_ids.iter().enumerate() {
            let hop = &program.hops[hop_id];
            let indent1 = "  ".repeat(self.depth + 1);
            let indent2 = "  ".repeat(self.depth + 2);

            writeln!(
                self.writer,
                "{}[{}] HopBlock{}",
                indent1,
                i,
                self.span(&hop.span)
            )?;
            writeln!(self.writer, "{}node_name: {}", indent2, hop.node_name)?;

            if let Some(resolved_node) = hop.resolved_node {
                let node = &program.nodes[resolved_node];
                writeln!(
                    self.writer,
                    "{}resolved_node: {} ({})",
                    indent2,
                    node.name,
                    resolved_node.index()
                )?;
            } else {
                writeln!(self.writer, "{}resolved_node: None", indent2)?;
            }

            self.depth += 2;
            self.print_statements(program, &hop.statements)?;
            self.depth -= 2;
        }
        Ok(())
    }

    fn print_statements(&mut self, program: &Program, stmt_ids: &[StatementId]) -> Result<()> {
        writeln!(
            self.writer,
            "{}statements[{}]",
            self.indent(),
            stmt_ids.len()
        )?;
        for (i, &stmt_id) in stmt_ids.iter().enumerate() {
            let stmt = &program.statements[stmt_id];
            self.depth += 1;
            self.print_statement_with_index(program, i, stmt)?;
            self.depth -= 1;
        }
        Ok(())
    }

    fn print_statement_with_index(
        &mut self,
        program: &Program,
        index: usize,
        stmt: &Statement,
    ) -> Result<()> {
        let indent = self.indent();
        let indent1 = "  ".repeat(self.depth + 1);
        let _indent2 = "  ".repeat(self.depth + 2); // Prefixed with _ to avoid unused warning

        match &stmt.node {
            StatementKind::VarDecl(v) => {
                writeln!(
                    self.writer,
                    "{}[{}] VarDeclStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
                writeln!(
                    self.writer,
                    "{}var_type: {}",
                    indent1,
                    type_name(&v.var_type)
                )?;
                writeln!(self.writer, "{}var_name: {}", indent1, v.var_name)?;
                writeln!(self.writer, "{}init_value:", indent1)?;
                self.depth += 2;
                self.print_expression(program, v.init_value)?;
                self.depth -= 2;
            }
            StatementKind::VarAssignment(v) => {
                writeln!(
                    self.writer,
                    "{}[{}] VarAssignmentStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
                writeln!(self.writer, "{}var_name: {}", indent1, v.var_name)?;

                if let Some(resolved_var) = v.resolved_var {
                    let var = &program.variables[resolved_var];
                    writeln!(
                        self.writer,
                        "{}resolved_var: {} ({}) - {:?}",
                        indent1,
                        var.name,
                        resolved_var.index(),
                        var.kind
                    )?;
                } else {
                    writeln!(self.writer, "{}resolved_var: None", indent1)?;
                }

                writeln!(self.writer, "{}rhs:", indent1)?;
                self.depth += 2;
                self.print_expression(program, v.rhs)?;
                self.depth -= 2;
            }
            StatementKind::Assignment(a) => {
                writeln!(
                    self.writer,
                    "{}[{}] AssignmentStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
                writeln!(self.writer, "{}table_name: {}", indent1, a.table_name)?;
                
                // Write primary key fields
                write!(self.writer, "{}pk_fields: [", indent1)?;
                for (i, pk_field) in a.pk_fields.iter().enumerate() {
                    if i > 0 { write!(self.writer, ", ")?; }
                    write!(self.writer, "{}", pk_field)?;
                }
                writeln!(self.writer, "]")?;
                
                writeln!(self.writer, "{}field_name: {}", indent1, a.field_name)?;

                if let Some(resolved_table) = a.resolved_table {
                    let table = &program.tables[resolved_table];
                    writeln!(
                        self.writer,
                        "{}resolved_table: {} ({})",
                        indent1,
                        table.name,
                        resolved_table.index()
                    )?;
                } else {
                    writeln!(self.writer, "{}resolved_table: None", indent1)?;
                }

                // Write resolved primary key fields
                write!(self.writer, "{}resolved_pk_fields: [", indent1)?;
                for (i, resolved_pk_field) in a.resolved_pk_fields.iter().enumerate() {
                    if i > 0 { write!(self.writer, ", ")?; }
                    if let Some(pk_field_id) = resolved_pk_field {
                        let pk_field = &program.fields[*pk_field_id];
                        write!(self.writer, "{} ({})", pk_field.field_name, pk_field_id.index())?;
                    } else {
                        write!(self.writer, "None")?;
                    }
                }
                writeln!(self.writer, "]")?;

                if let Some(resolved_field) = a.resolved_field {
                    let field = &program.fields[resolved_field];
                    writeln!(
                        self.writer,
                        "{}resolved_field: {} ({})",
                        indent1,
                        field.field_name,
                        resolved_field.index()
                    )?;
                } else {
                    writeln!(self.writer, "{}resolved_field: None", indent1)?;
                }

                // Write primary key expressions
                writeln!(self.writer, "{}pk_exprs:", indent1)?;
                for (i, &pk_expr) in a.pk_exprs.iter().enumerate() {
                    let indent2 = "  ".repeat(self.depth + 2);
                    writeln!(self.writer, "{}[{}]:", indent2, i)?;
                    self.depth += 3;
                    self.print_expression(program, pk_expr)?;
                    self.depth -= 3;
                }
                
                writeln!(self.writer, "{}rhs:", indent1)?;
                self.depth += 2;
                self.print_expression(program, a.rhs)?;
                self.depth -= 2;
            }
            StatementKind::Return(r) => {
                writeln!(
                    self.writer,
                    "{}[{}] ReturnStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
                writeln!(self.writer, "{}value:", indent1)?;
                match &r.value {
                    Some(expr_id) => {
                        self.depth += 2;
                        self.print_expression(program, *expr_id)?;
                        self.depth -= 2;
                    }
                    None => {
                        let indent2 = "  ".repeat(self.depth + 2);
                        writeln!(self.writer, "{}None", indent2)?;
                    }
                }
            }
            StatementKind::Abort(_) => {
                writeln!(
                    self.writer,
                    "{}[{}] AbortStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
            }
            StatementKind::IfStmt(i) => {
                writeln!(
                    self.writer,
                    "{}[{}] IfStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
                writeln!(self.writer, "{}condition:", indent1)?;
                self.depth += 2;
                self.print_expression(program, i.condition)?;
                self.depth -= 2;
                writeln!(self.writer, "{}then_branch:", indent1)?;
                self.depth += 2;
                self.print_statements(program, &i.then_branch)?;
                self.depth -= 2;
                writeln!(self.writer, "{}else_branch:", indent1)?;
                match &i.else_branch {
                    Some(else_stmts) => {
                        self.depth += 2;
                        self.print_statements(program, else_stmts)?;
                        self.depth -= 2;
                    }
                    None => {
                        let indent2 = "  ".repeat(self.depth + 2);
                        writeln!(self.writer, "{}None", indent2)?;
                    }
                }
            }
            StatementKind::WhileStmt(w) => {
                writeln!(
                    self.writer,
                    "{}[{}] WhileStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
                writeln!(self.writer, "{}condition:", indent1)?;
                self.depth += 2;
                self.print_expression(program, w.condition)?;
                self.depth -= 2;
                writeln!(self.writer, "{}body:", indent1)?;
                self.depth += 2;
                self.print_statements(program, &w.body)?;
                self.depth -= 2;
            }
            StatementKind::Break(_) => {
                writeln!(
                    self.writer,
                    "{}[{}] BreakStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
            }
            StatementKind::Continue(_) => {
                writeln!(
                    self.writer,
                    "{}[{}] ContinueStatement{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
            }
            StatementKind::Empty => {
                writeln!(
                    self.writer,
                    "{}[{}] Empty{}",
                    indent,
                    index,
                    self.span(&stmt.span)
                )?;
            }
        }
        Ok(())
    }

    fn print_expression(&mut self, program: &Program, expr_id: ExpressionId) -> Result<()> {
        let expr = &program.expressions[expr_id];
        writeln!(
            self.writer,
            "{}Expression{}",
            self.indent(),
            self.span(&expr.span)
        )?;
        self.depth += 1;
        self.print_expression_kind(program, &expr.node)?;
        self.depth -= 1;
        Ok(())
    }

    fn print_expression_kind(&mut self, program: &Program, expr: &ExpressionKind) -> Result<()> {
        let indent = self.indent();
        let indent1 = "  ".repeat(self.depth + 1);

        match expr {
            ExpressionKind::Ident(name) => {
                writeln!(self.writer, "{}Ident {}", indent, name)?;
            }
            ExpressionKind::IntLit(value) => {
                writeln!(self.writer, "{}IntLit {}", indent, value)?;
            }
            ExpressionKind::FloatLit(value) => {
                writeln!(self.writer, "{}FloatLit {}", indent, value)?;
            }
            ExpressionKind::StringLit(value) => {
                writeln!(self.writer, "{}StringLit \"{}\"", indent, value)?;
            }
            ExpressionKind::BoolLit(value) => {
                writeln!(self.writer, "{}BoolLit {}", indent, value)?;
            }
            ExpressionKind::TableFieldAccess {
                table_name,
                pk_fields,
                pk_exprs,
                field_name,
                resolved_table,
                resolved_pk_fields,
                resolved_field,
            } => {
                writeln!(self.writer, "{}TableFieldAccess", indent)?;
                writeln!(self.writer, "{}table_name: {}", indent1, table_name)?;
                
                // Write primary key fields
                write!(self.writer, "{}pk_fields: [", indent1)?;
                for (i, pk_field) in pk_fields.iter().enumerate() {
                    if i > 0 { write!(self.writer, ", ")?; }
                    write!(self.writer, "{}", pk_field)?;
                }
                writeln!(self.writer, "]")?;
                
                writeln!(self.writer, "{}field_name: {}", indent1, field_name)?;

                if let Some(resolved_table_id) = resolved_table {
                    let table = &program.tables[*resolved_table_id];
                    writeln!(
                        self.writer,
                        "{}resolved_table: {} ({})",
                        indent1,
                        table.name,
                        resolved_table_id.index()
                    )?;
                } else {
                    writeln!(self.writer, "{}resolved_table: None", indent1)?;
                }

                // Write resolved primary key fields
                write!(self.writer, "{}resolved_pk_fields: [", indent1)?;
                for (i, resolved_pk_field) in resolved_pk_fields.iter().enumerate() {
                    if i > 0 { write!(self.writer, ", ")?; }
                    if let Some(pk_field_id) = resolved_pk_field {
                        let pk_field = &program.fields[*pk_field_id];
                        write!(self.writer, "{} ({})", pk_field.field_name, pk_field_id.index())?;
                    } else {
                        write!(self.writer, "None")?;
                    }
                }
                writeln!(self.writer, "]")?;

                if let Some(resolved_field_id) = resolved_field {
                    let field = &program.fields[*resolved_field_id];
                    writeln!(
                        self.writer,
                        "{}resolved_field: {} ({})",
                        indent1,
                        field.field_name,
                        resolved_field_id.index()
                    )?;
                } else {
                    writeln!(self.writer, "{}resolved_field: None", indent1)?;
                }

                // Write primary key expressions
                writeln!(self.writer, "{}pk_exprs:", indent1)?;
                for (i, &pk_expr) in pk_exprs.iter().enumerate() {
                    let indent2 = "  ".repeat(self.depth + 2);
                    writeln!(self.writer, "{}[{}]:", indent2, i)?;
                    self.depth += 3;
                    self.print_expression(program, pk_expr)?;
                    self.depth -= 3;
                }
            }
            ExpressionKind::UnaryOp { op, expr } => {
                writeln!(self.writer, "{}UnaryOp", indent)?;
                writeln!(self.writer, "{}op: {:?}", indent1, op)?;
                writeln!(self.writer, "{}expr:", indent1)?;
                self.depth += 2;
                self.print_expression(program, *expr)?;
                self.depth -= 2;
            }
            ExpressionKind::BinaryOp { left, op, right } => {
                writeln!(self.writer, "{}BinaryOp", indent)?;
                writeln!(self.writer, "{}left:", indent1)?;
                self.depth += 2;
                self.print_expression(program, *left)?;
                self.depth -= 2;
                writeln!(self.writer, "{}op: {:?}", indent1, op)?;
                writeln!(self.writer, "{}right:", indent1)?;
                self.depth += 2;
                self.print_expression(program, *right)?;
                self.depth -= 2;
            }
        }
        Ok(())
    }
}

fn type_name(t: &TypeName) -> &'static str {
    match t {
        TypeName::Int => "int",
        TypeName::Float => "float",
        TypeName::String => "string",
        TypeName::Bool => "bool",
    }
}

fn return_type(ret: &ReturnType) -> &'static str {
    match ret {
        ReturnType::Void => "void",
        ReturnType::Type(t) => type_name(t),
    }
}
