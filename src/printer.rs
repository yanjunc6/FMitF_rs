use crate::ast::*; // Import your AST definitions from ast.rs

/// Print the entire TransAct program in a simple hierarchical way.
pub fn print_program(program: &Program) {
    println!("Program AST:");

    // 1) Print all nodes
    println!("  Nodes:");
    for (i, node) in program.nodes.iter().enumerate() {
        println!("    [{}] Node: {}", i, node.name);
    }

    // 2) Print all tables
    println!("  Tables:");
    for (i, table) in program.tables.iter().enumerate() {
        println!("    [{}] Table: {}", i, table.name);
        println!("        On node: {}", table.node.name);
        println!("        Fields:");
        for f in &table.fields {
            println!("            {} {}", type_name_to_str(&f.field_type), f.field_name);
        }
    }

    // 3) Print all functions
    println!("  Functions:");
    for (i, func) in program.functions.iter().enumerate() {
        println!("    [{}] Function: {}", i, func.name);
        match &func.return_type {
            ReturnType::Void => println!("        Returns: void"),
            ReturnType::Type(t) => println!("        Returns: {}", type_name_to_str(t)),
        }
        println!("        Parameters:");
        for param in &func.parameters {
            println!("            {} {}", type_name_to_str(&param.param_type), param.param_name);
        }

        println!("        Hop Blocks:");
        for (j, hop) in func.hops.iter().enumerate() {
            println!("            [{}] Hop on node: {}", j, hop.node.name);
            for stmt in &hop.statements {
                print_statement(stmt, 4);
            }
        }
    }

    println!("End of AST.");
}

/// Helper to print a single statement with indentation.
fn print_statement(stmt: &Statement, indent_level: usize) {
    let indent = "    ".repeat(indent_level);
    match stmt {
        Statement::Assignment(asg) => {
            println!("{}Assignment:", indent);
            println!("{}  Table = {}", indent, asg.table.name);
            println!("{}  PK column = {}", indent, asg.pk_column);
            println!("{}  PK expr:", indent);
            print_expression(&asg.pk_expr, indent_level + 2);
            println!("{}  Field = {}", indent, asg.field_name);
            println!("{}  Rhs expr:", indent);
            print_expression(&asg.rhs, indent_level + 2);
        }
        Statement::IfStmt(ifstmt) => {
            println!("{}IfStatement:", indent);
            println!("{}  Condition:", indent);
            print_expression(&ifstmt.condition, indent_level + 2);

            println!("{}  Then branch:", indent);
            for s in &ifstmt.then_branch {
                print_statement(s, indent_level + 2);
            }

            if let Some(else_stmts) = &ifstmt.else_branch {
                println!("{}  Else branch:", indent);
                for s in else_stmts {
                    print_statement(s, indent_level + 2);
                }
            }
        }
        Statement::Empty => {
            println!("{}(Empty Statement)", indent);
        }
    }
}

/// Helper to print an expression with indentation.
fn print_expression(expr: &Expression, indent_level: usize) {
    let indent = "    ".repeat(indent_level);
    match expr {
        Expression::Ident(name) => {
            println!("{}Ident({})", indent, name);
        }
        Expression::IntLit(i) => {
            println!("{}IntLit({})", indent, i);
        }
        Expression::FloatLit(f) => {
            println!("{}FloatLit({})", indent, f);
        }
        Expression::StringLit(s) => {
            println!("{}StringLit(\"{}\")", indent, s);
        }
        Expression::UnaryOp { op, expr } => {
            let op_str = match op {
                UnaryOp::Neg => "-",
                UnaryOp::Not => "!",
            };
            println!("{}UnaryOp({})", indent, op_str);
            print_expression(expr, indent_level + 1);
        }
        Expression::BinaryOp { left, op, right } => {
            let op_str = match op {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Eq  => "==",
                BinaryOp::Neq => "!=",
                BinaryOp::Lt  => "<",
                BinaryOp::Lte => "<=",
                BinaryOp::Gt  => ">",
                BinaryOp::Gte => ">=",
                BinaryOp::And => "&&",
                BinaryOp::Or  => "||",
            };
            println!("{}BinaryOp({})", indent, op_str);
            println!("{}  Left:", indent);
            print_expression(left, indent_level + 2);
            println!("{}  Right:", indent);
            print_expression(right, indent_level + 2);
        }
    }
}

/// Convert a TypeName enum variant to a string for printing.
fn type_name_to_str(t: &TypeName) -> &'static str {
    match t {
        TypeName::Int => "int",
        TypeName::Float => "float",
        TypeName::String => "string",
    }
}