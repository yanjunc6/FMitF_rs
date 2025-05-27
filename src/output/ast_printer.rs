use crate::ast::*;
use std::rc::Rc;

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
    Printer::new(opts).print_program(program);
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

    fn with_depth(&self, depth: usize) -> Self {
        Self { opts: self.opts, depth }
    }

    fn span(&self, span: &Span) -> String {
        if self.opts.show_spans {
            format!(" @{}:{}", span.line, span.column)
        } else {
            String::new()
        }
    }

    fn print_program(&self, program: &Program) {
        match self.opts.mode {
            PrintMode::Summary => self.print_summary(program),
            PrintMode::Verbose => self.print_verbose(program),
        }
    }

    fn print_summary(&self, program: &Program) {
        println!("Program Summary:");
        
        let nodes: Vec<&str> = program.nodes.iter().map(|n| n.name.as_str()).collect();
        println!("  Nodes: {}", nodes.join(", "));
        
        println!("  Tables:");
        for table in &program.tables {
            let fields: Vec<String> = table.fields.iter()
                .map(|f| {
                    let prefix = if f.is_primary { "primary " } else { "" };
                    format!("{}{}:{}", prefix, f.field_name, type_name(&f.field_type))
                })
                .collect();
            println!("    {} on {}: {}", table.name, table.node.name, fields.join(", "));
        }
        
        println!("  Functions:");
        for func in &program.functions {
            let params: Vec<String> = func.parameters.iter()
                .map(|p| format!("{}:{}", p.param_name, type_name(&p.param_type)))
                .collect();
            println!("    {}({}) -> {}", func.name, params.join(", "), return_type(&func.return_type));
        }
    }

    fn print_verbose(&self, program: &Program) {
        println!("Program");
        self.with_depth(1).print_nodes(&program.nodes);
        self.with_depth(1).print_tables(&program.tables);
        self.with_depth(1).print_functions(&program.functions);
    }

    fn print_nodes(&self, nodes: &[std::rc::Rc<NodeDef>]) {
        println!("{}nodes[{}]", self.indent(), nodes.len());
        for (i, node) in nodes.iter().enumerate() {
            println!("{}[{}] NodeDef{}", self.with_depth(self.depth + 1).indent(), i, self.span(&node.span));
            println!("{}name: {}", self.with_depth(self.depth + 2).indent(), node.name);
        }
    }

    fn print_tables(&self, tables: &[std::rc::Rc<TableDeclaration>]) {
        println!("{}tables[{}]", self.indent(), tables.len());
        for (i, table) in tables.iter().enumerate() {
            println!("{}[{}] TableDeclaration{}", self.with_depth(self.depth + 1).indent(), i, self.span(&table.span));
            println!("{}name: {}", self.with_depth(self.depth + 2).indent(), table.name);
            println!("{}node: {}", self.with_depth(self.depth + 2).indent(), table.node.name);
            println!("{}primary_key: {}", self.with_depth(self.depth + 2).indent(), table.primary_key.field_name); // NEW
            self.with_depth(self.depth + 2).print_fields(&table.fields);
        }
    }

    fn print_fields(&self, fields: &[FieldDeclaration]) {
        println!("{}fields[{}]", self.indent(), fields.len());
        for (i, field) in fields.iter().enumerate() {
            println!("{}[{}] FieldDeclaration{}", self.with_depth(self.depth + 1).indent(), i, self.span(&field.span));
            println!("{}field_type: {}", self.with_depth(self.depth + 2).indent(), type_name(&field.field_type));
            println!("{}field_name: {}", self.with_depth(self.depth + 2).indent(), field.field_name);
            println!("{}is_primary: {}", self.with_depth(self.depth + 2).indent(), field.is_primary); // NEW
        }
    }

    fn print_functions(&self, functions: &[FunctionDeclaration]) {
        println!("{}functions[{}]", self.indent(), functions.len());
        for (i, func) in functions.iter().enumerate() {
            println!("{}[{}] FunctionDeclaration{}", self.with_depth(self.depth + 1).indent(), i, self.span(&func.span));
            println!("{}return_type: {}", self.with_depth(self.depth + 2).indent(), return_type(&func.return_type));
            println!("{}name: {}", self.with_depth(self.depth + 2).indent(), func.name);
            self.with_depth(self.depth + 2).print_parameters(&func.parameters);
            self.with_depth(self.depth + 2).print_hops(&func.hops);
        }
    }

    fn print_parameters(&self, params: &[ParameterDecl]) {
        println!("{}parameters[{}]", self.indent(), params.len());
        for (i, param) in params.iter().enumerate() {
            println!("{}[{}] ParameterDecl{}", self.with_depth(self.depth + 1).indent(), i, self.span(&param.span));
            println!("{}param_type: {}", self.with_depth(self.depth + 2).indent(), type_name(&param.param_type));
            println!("{}param_name: {}", self.with_depth(self.depth + 2).indent(), param.param_name);
        }
    }

    fn print_hops(&self, hops: &[Rc<HopBlock>]) {
        println!("{}hops[{}]", self.indent(), hops.len());
        for (i, hop) in hops.iter().enumerate() {
            println!("{}[{}] HopBlock{}", self.with_depth(self.depth + 1).indent(), i, self.span(&hop.span));
            println!("{}node: {}", self.with_depth(self.depth + 2).indent(), hop.node.name);
            self.with_depth(self.depth + 2).print_statements(&hop.statements);
        }
    }

    fn print_statements(&self, statements: &[Statement]) {
        println!("{}statements[{}]", self.indent(), statements.len());
        for (i, stmt) in statements.iter().enumerate() {
            self.with_depth(self.depth + 1).print_statement_with_index(i, stmt);
        }
    }

    fn print_statement_with_index(&self, index: usize, stmt: &Statement) {
        match &stmt.node {
            StatementKind::VarDecl(v) => {
                println!("{}[{}] VarDeclStatement{}", self.indent(), index, self.span(&stmt.span));
                println!("{}var_type: {}", self.with_depth(self.depth + 1).indent(), type_name(&v.var_type));
                println!("{}var_name: {}", self.with_depth(self.depth + 1).indent(), v.var_name);
                println!("{}init_value:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(&v.init_value);
                println!("{}is_global: {}", self.with_depth(self.depth + 1).indent(), v.is_global);
            }
            StatementKind::VarAssignment(v) => {
                println!("{}[{}] VarAssignmentStatement{}", self.indent(), index, self.span(&stmt.span));
                println!("{}var_name: {}", self.with_depth(self.depth + 1).indent(), v.var_name);
                println!("{}rhs:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(&v.rhs);
            }
            StatementKind::Assignment(a) => {
                println!("{}[{}] AssignmentStatement{}", self.indent(), index, self.span(&stmt.span));
                println!("{}table: {}", self.with_depth(self.depth + 1).indent(), a.table.name);
                println!("{}pk_field: {}", self.with_depth(self.depth + 1).indent(), a.pk_field.field_name); // UPDATED
                println!("{}pk_expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(&a.pk_expr);
                println!("{}field: {}", self.with_depth(self.depth + 1).indent(), a.field.field_name); // UPDATED
                println!("{}rhs:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(&a.rhs);
            }
            StatementKind::Return(r) => {
                println!("{}[{}] ReturnStatement{}", self.indent(), index, self.span(&stmt.span));
                println!("{}value:", self.with_depth(self.depth + 1).indent());
                match &r.value {
                    Some(expr) => self.with_depth(self.depth + 2).print_expression(expr),
                    None => println!("{}None", self.with_depth(self.depth + 2).indent()),
                }
            }
            StatementKind::Abort(_) => { // NEW: Add abort statement printing
                println!("{}[{}] AbortStatement{}", self.indent(), index, self.span(&stmt.span));
            }
            StatementKind::IfStmt(i) => {
                println!("{}[{}] IfStatement{}", self.indent(), index, self.span(&stmt.span));
                println!("{}condition:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(&i.condition);
                println!("{}then_branch:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_statements(&i.then_branch);
                println!("{}else_branch:", self.with_depth(self.depth + 1).indent());
                match &i.else_branch {
                    Some(else_stmts) => self.with_depth(self.depth + 2).print_statements(else_stmts),
                    None => println!("{}None", self.with_depth(self.depth + 2).indent()),
                }
            }
            StatementKind::Empty => {
                println!("{}[{}] Empty{}", self.indent(), index, self.span(&stmt.span));
            }
        }
    }

    fn print_expression(&self, expr: &Expression) {
        println!("{}Expression{}", self.indent(), self.span(&expr.span));
        self.with_depth(self.depth + 1).print_expression_kind(&expr.node);
    }

    fn print_expression_kind(&self, expr: &ExpressionKind) {
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
                println!("{}StringLit {}", self.indent(), value);
            }
            ExpressionKind::BoolLit(value) => {
                println!("{}BoolLit {}", self.indent(), value);
            }
            ExpressionKind::TableFieldAccess { table, pk_field, pk_expr, field } => { // UPDATED
                println!("{}TableFieldAccess", self.indent());
                println!("{}table: {}", self.with_depth(self.depth + 1).indent(), table.name);
                println!("{}pk_field: {}", self.with_depth(self.depth + 1).indent(), pk_field.field_name);
                println!("{}pk_expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(pk_expr);
                println!("{}field: {}", self.with_depth(self.depth + 1).indent(), field.field_name);
            }
            ExpressionKind::UnaryOp { op, expr } => {
                println!("{}UnaryOp", self.indent());
                println!("{}op: {:?}", self.with_depth(self.depth + 1).indent(), op);
                println!("{}expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(expr);
            }
            ExpressionKind::BinaryOp { left, op, right } => {
                println!("{}BinaryOp", self.indent());
                println!("{}left:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(left);
                println!("{}op: {:?}", self.with_depth(self.depth + 1).indent(), op);
                println!("{}right:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(right);
            }
        }
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
