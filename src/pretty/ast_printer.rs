use crate::ast::*;

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
    program: Option<&'a Program>, // Store reference to program for ID lookups
}

impl<'a> Printer<'a> {
    fn new(opts: &'a PrintOptions) -> Self {
        Self { 
            opts, 
            depth: 0,
            program: None,
        }
    }

    fn with_program(&self, program: &'a Program) -> Self {
        Self {
            opts: self.opts,
            depth: self.depth,
            program: Some(program),
        }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.depth)
    }

    fn with_depth(&self, depth: usize) -> Self {
        Self {
            opts: self.opts,
            depth,
            program: self.program,
        }
    }

    fn span(&self, span: &Span) -> String {
        if self.opts.show_spans {
            format!(" @{}:{}", span.line, span.column)
        } else {
            String::new()
        }
    }

    fn print_program(&mut self, program: &'a Program) {
        self.program = Some(program);
        
        match self.opts.mode {
            PrintMode::Summary => self.print_summary(program),
            PrintMode::Verbose => self.print_verbose(program),
        }
    }

    fn print_summary(&self, program: &Program) {
        println!("Program Summary:");

        // Print nodes
        let nodes: Vec<&str> = program.root_nodes
            .iter()
            .map(|&node_id| program.nodes[node_id].name.as_str())
            .collect();
        println!("  Nodes: {}", nodes.join(", "));

        // Print tables
        println!("  Tables:");
        for &table_id in &program.root_tables {
            let table = &program.tables[table_id];
            let node_name = &program.nodes[table.node].name;
            
            let fields: Vec<String> = table.fields
                .iter()
                .map(|&field_id| {
                    let field = &program.fields[field_id];
                    let prefix = if field.is_primary { "primary " } else { "" };
                    format!("{}{}:{}", prefix, field.field_name, type_name(&field.field_type))
                })
                .collect();
            
            println!(
                "    {} on {}: {}",
                table.name,
                node_name,
                fields.join(", ")
            );
        }

        // Print functions
        println!("  Functions:");
        for &func_id in &program.root_functions {
            let func = &program.functions[func_id];
            let params: Vec<String> = func.parameters
                .iter()
                .map(|&param_id| {
                    let param = &program.parameters[param_id];
                    format!("{}:{}", param.param_name, type_name(&param.param_type))
                })
                .collect();
            
            println!(
                "    {}({}) -> {}",
                func.name,
                params.join(", "),
                return_type(&func.return_type)
            );
        }
    }

    fn print_verbose(&self, program: &Program) {
        println!("Program");
        self.with_depth(1).print_nodes(program);
        self.with_depth(1).print_tables(program);
        self.with_depth(1).print_functions(program);
    }

    fn print_nodes(&self, program: &Program) {
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

    fn print_tables(&self, program: &Program) {
        println!("{}tables[{}]", self.indent(), program.root_tables.len());
        for (i, &table_id) in program.root_tables.iter().enumerate() {
            let table = &program.tables[table_id];
            let node = &program.nodes[table.node];
            let primary_key_field = &program.fields[table.primary_key];
            
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
            println!(
                "{}primary_key: {}",
                self.with_depth(self.depth + 2).indent(),
                primary_key_field.field_name
            );
            self.with_depth(self.depth + 2).print_fields(program, &table.fields);
        }
    }

    fn print_fields(&self, program: &Program, field_ids: &[FieldId]) {
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

    fn print_functions(&self, program: &Program) {
        println!("{}functions[{}]", self.indent(), program.root_functions.len());
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
            self.with_depth(self.depth + 2).print_parameters(program, &func.parameters);
            self.with_depth(self.depth + 2).print_hops(program, &func.hops);
        }
    }

    fn print_parameters(&self, program: &Program, param_ids: &[ParameterId]) {
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

    fn print_hops(&self, program: &Program, hop_ids: &[HopId]) {
        println!("{}hops[{}]", self.indent(), hop_ids.len());
        for (i, &hop_id) in hop_ids.iter().enumerate() {
            let hop = &program.hops[hop_id];
            let node = &program.nodes[hop.node];
            
            println!(
                "{}[{}] HopBlock{}",
                self.with_depth(self.depth + 1).indent(),
                i,
                self.span(&hop.span)
            );
            println!(
                "{}node: {}",
                self.with_depth(self.depth + 2).indent(),
                node.name
            );
            self.with_depth(self.depth + 2).print_statements(program, &hop.statements);
        }
    }

    fn print_statements(&self, program: &Program, stmt_ids: &[StatementId]) {
        println!("{}statements[{}]", self.indent(), stmt_ids.len());
        for (i, &stmt_id) in stmt_ids.iter().enumerate() {
            let stmt = &program.statements[stmt_id];
            self.with_depth(self.depth + 1).print_statement_with_index(program, i, stmt);
        }
    }

    fn print_statement_with_index(&self, program: &Program, index: usize, stmt: &Statement) {
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
                self.with_depth(self.depth + 2).print_expression(program, v.init_value);
                println!(
                    "{}is_global: {}",
                    self.with_depth(self.depth + 1).indent(),
                    v.is_global
                );
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
                println!("{}rhs:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, v.rhs);
            }
            StatementKind::Assignment(a) => {
                let table = &program.tables[a.table];
                let pk_field = &program.fields[a.pk_field];
                let field = &program.fields[a.field];
                
                println!(
                    "{}[{}] AssignmentStatement{}",
                    self.indent(),
                    index,
                    self.span(&stmt.span)
                );
                println!(
                    "{}table: {}",
                    self.with_depth(self.depth + 1).indent(),
                    table.name
                );
                println!(
                    "{}pk_field: {}",
                    self.with_depth(self.depth + 1).indent(),
                    pk_field.field_name
                );
                println!("{}pk_expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, a.pk_expr);
                println!(
                    "{}field: {}",
                    self.with_depth(self.depth + 1).indent(),
                    field.field_name
                );
                println!("{}rhs:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, a.rhs);
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
                    Some(expr_id) => self.with_depth(self.depth + 2).print_expression(program, *expr_id),
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
                self.with_depth(self.depth + 2).print_expression(program, i.condition);
                println!("{}then_branch:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_statements(program, &i.then_branch);
                println!("{}else_branch:", self.with_depth(self.depth + 1).indent());
                match &i.else_branch {
                    Some(else_stmts) => {
                        self.with_depth(self.depth + 2).print_statements(program, else_stmts)
                    }
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
                self.with_depth(self.depth + 2).print_expression(program, w.condition);
                println!("{}body:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_statements(program, &w.body);
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

    fn print_expression(&self, program: &Program, expr_id: ExpressionId) {
        let expr = &program.expressions[expr_id];
        println!("{}Expression{}", self.indent(), self.span(&expr.span));
        self.with_depth(self.depth + 1).print_expression_kind(program, &expr.node);
    }

    fn print_expression_kind(&self, program: &Program, expr: &ExpressionKind) {
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
                table,
                pk_field,
                pk_expr,
                field,
            } => {
                let table_obj = &program.tables[*table];
                let pk_field_obj = &program.fields[*pk_field];
                let field_obj = &program.fields[*field];
                
                println!("{}TableFieldAccess", self.indent());
                println!(
                    "{}table: {}",
                    self.with_depth(self.depth + 1).indent(),
                    table_obj.name
                );
                println!(
                    "{}pk_field: {}",
                    self.with_depth(self.depth + 1).indent(),
                    pk_field_obj.field_name
                );
                println!("{}pk_expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, *pk_expr);
                println!(
                    "{}field: {}",
                    self.with_depth(self.depth + 1).indent(),
                    field_obj.field_name
                );
            }
            ExpressionKind::UnaryOp { op, expr } => {
                println!("{}UnaryOp", self.indent());
                println!("{}op: {:?}", self.with_depth(self.depth + 1).indent(), op);
                println!("{}expr:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, *expr);
            }
            ExpressionKind::BinaryOp { left, op, right } => {
                println!("{}BinaryOp", self.indent());
                println!("{}left:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, *left);
                println!("{}op: {:?}", self.with_depth(self.depth + 1).indent(), op);
                println!("{}right:", self.with_depth(self.depth + 1).indent());
                self.with_depth(self.depth + 2).print_expression(program, *right);
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
