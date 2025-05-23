use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;

#[derive(Parser)]
#[grammar = "TransAct.pest"]
pub struct TransActParser;

pub fn parse_program(source: &str) -> Program {
    let mut program = Program {
        nodes: Vec::new(),
        tables: Vec::new(),
        functions: Vec::new(),
    };

    let mut node_map = HashMap::new();
    let mut table_map = HashMap::new();

    let parse_result =
        TransActParser::parse(Rule::program, source).expect("Failed to parse the TransAct DSL.");

    let program_pair = parse_result
        .into_iter()
        .next()
        .expect("Expected a single program rule");
    assert_eq!(program_pair.as_rule(), Rule::program);

    // First pass: collect all nodes and tables
    for top_item in program_pair.clone().into_inner() {
        match top_item.as_rule() {
            Rule::nodes_block => {
                parse_nodes_block(top_item, &mut program, &mut node_map);
            }
            Rule::table_declaration => {
                parse_table_declaration(top_item, &mut program, &node_map, &mut table_map);
            }
            _ => {}
        }
    }

    // Second pass: parse functions
    for top_item in program_pair.into_inner() {
        match top_item.as_rule() {
            Rule::function_declaration => {
                parse_function_declaration(top_item, &mut program, &node_map, &table_map);
            }
            _ => {}
        }
    }

    program
}

// Basic parsing functions for each grammar rule

fn parse_identifier(pair: Pair<Rule>) -> String {
    assert_eq!(pair.as_rule(), Rule::identifier);
    pair.as_str().to_string()
}

fn parse_type_name(pair: Pair<Rule>) -> TypeName {
    assert_eq!(pair.as_rule(), Rule::type_name);
    match pair.as_str() {
        "int" => TypeName::Int,
        "float" => TypeName::Float,
        "string" => TypeName::String,
        "bool" => TypeName::Bool, // Add Boolean type
        other => panic!("Unknown type: {}", other),
    }
}

fn parse_return_type(pair: Pair<Rule>) -> ReturnType {
    assert_eq!(pair.as_rule(), Rule::ret_type);
    
    if pair.as_str() == "void" {
        ReturnType::Void
    } else {
        // For non-void return types, we need to extract the inner type_name rule
        let inner = pair.into_inner().next().unwrap();
        ReturnType::Type(parse_type_name(inner))
    }
}

fn parse_integer_literal(pair: Pair<Rule>) -> i64 {
    assert_eq!(pair.as_rule(), Rule::integer_literal);
    pair.as_str().parse().unwrap()
}

fn parse_float_literal(pair: Pair<Rule>) -> f64 {
    assert_eq!(pair.as_rule(), Rule::float_literal);
    pair.as_str().parse().unwrap()
}

fn parse_string_literal(pair: Pair<Rule>) -> String {
    assert_eq!(pair.as_rule(), Rule::string_literal);
    let raw = pair.as_str();
    let content = &raw[1..raw.len() - 1];
    content
        .to_string()
        .replace("\\\"", "\"")
        .replace("\\\\", "\\")
}

fn parse_bool_literal(pair: Pair<Rule>) -> bool {
    assert_eq!(pair.as_rule(), Rule::bool_literal);
    match pair.as_str() {
        "true" => true,
        "false" => false,
        other => panic!("Unknown boolean literal: {}", other),
    }
}

// Compound parsing functions

fn parse_nodes_block(
    pair: Pair<Rule>,
    program: &mut Program,
    node_map: &mut HashMap<String, Rc<NodeDef>>,
) {
    assert_eq!(pair.as_rule(), Rule::nodes_block);

    for item in pair.into_inner() {
        if item.as_rule() == Rule::node_list {
            parse_node_list(item, program, node_map);
        }
    }
}

fn parse_node_list(
    pair: Pair<Rule>,
    program: &mut Program,
    node_map: &mut HashMap<String, Rc<NodeDef>>,
) {
    assert_eq!(pair.as_rule(), Rule::node_list);

    for ident_item in pair.into_inner() {
        if ident_item.as_rule() == Rule::identifier {
            let name = parse_identifier(ident_item);
            let node = Rc::new(NodeDef { name: name.clone() });
            program.nodes.push(node.clone());
            node_map.insert(name, node);
        }
    }
}

fn parse_table_declaration(
    pair: Pair<Rule>,
    program: &mut Program,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &mut HashMap<String, Rc<TableDeclaration>>,
) {
    assert_eq!(pair.as_rule(), Rule::table_declaration);

    let mut inner = pair.into_inner();

    let table_name = parse_identifier(inner.next().unwrap());
    let node_name = parse_identifier(inner.next().unwrap());

    let fields: Vec<FieldDeclaration> = inner
        .filter(|item| item.as_rule() == Rule::field_declaration)
        .map(parse_field_declaration)
        .collect();

    let node = node_map
        .get(&node_name)
        .unwrap_or_else(|| panic!("No such node: {}", node_name))
        .clone();

    let table = Rc::new(TableDeclaration {
        name: table_name.clone(),
        node,
        fields,
    });

    program.tables.push(table.clone());
    table_map.insert(table_name, table);
}

fn parse_field_declaration(pair: Pair<Rule>) -> FieldDeclaration {
    assert_eq!(pair.as_rule(), Rule::field_declaration);

    let mut inner = pair.into_inner();

    let field_type = parse_type_name(inner.next().unwrap());
    let field_name = parse_identifier(inner.next().unwrap());

    FieldDeclaration {
        field_type,
        field_name,
    }
}

fn parse_function_declaration(
    pair: Pair<Rule>,
    program: &mut Program,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) {
    assert_eq!(pair.as_rule(), Rule::function_declaration);

    let mut inner = pair.into_inner();

    let return_type = parse_return_type(inner.next().unwrap());
    let name = parse_identifier(inner.next().unwrap());

    let mut parameters = Vec::new();
    let mut hops = Vec::new();

    for item in inner {
        match item.as_rule() {
            Rule::parameter_list => {
                parameters = parse_parameter_list(item);
            }
            Rule::function_body_item => {
                hops.push(parse_function_body_item(item, node_map, table_map));
            }
            _ => {}
        }
    }

    program.functions.push(FunctionDeclaration {
        return_type,
        name,
        parameters,
        hops,
    });
}

fn parse_parameter_list(pair: Pair<Rule>) -> Vec<ParameterDecl> {
    assert_eq!(pair.as_rule(), Rule::parameter_list);

    pair.into_inner()
        .filter(|child| child.as_rule() == Rule::parameter_decl)
        .map(parse_parameter_decl)
        .collect()
}

fn parse_parameter_decl(pair: Pair<Rule>) -> ParameterDecl {
    assert_eq!(pair.as_rule(), Rule::parameter_decl);

    let mut inner = pair.into_inner();

    let param_type = parse_type_name(inner.next().unwrap());
    let param_name = parse_identifier(inner.next().unwrap());

    ParameterDecl {
        param_type,
        param_name,
    }
}

fn parse_function_body_item(
    pair: Pair<Rule>,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) -> HopBlock {
    assert_eq!(pair.as_rule(), Rule::function_body_item);

    let hop_block = pair.into_inner().next().unwrap();
    parse_hop_block(hop_block, node_map, table_map)
}

fn parse_hop_block(
    pair: Pair<Rule>,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) -> HopBlock {
    assert_eq!(pair.as_rule(), Rule::hop_block);

    let mut inner = pair.into_inner();

    let node_name = parse_identifier(inner.next().unwrap());
    let statements = parse_block(inner.next().unwrap(), table_map);

    let node = node_map
        .get(&node_name)
        .unwrap_or_else(|| panic!("No such node: {}", node_name))
        .clone();

    HopBlock { node, statements }
}

fn parse_statement(
    pair: Pair<Rule>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) -> Statement {
    assert_eq!(pair.as_rule(), Rule::statement);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::assignment_statement => {
            Statement::Assignment(parse_assignment_statement(inner, table_map))
        }
        Rule::var_assignment_statement => {
            Statement::VarAssignment(parse_var_assignment_statement(inner))
        }
        Rule::if_statement => Statement::IfStmt(parse_if_statement(inner, table_map)),
        Rule::var_decl_statement => Statement::VarDecl(parse_var_decl_statement(inner)),
        Rule::return_statement => Statement::Return(parse_return_statement(inner)),
        Rule::empty_statement => Statement::Empty,
        _ => unreachable!("Unexpected rule in parse_statement: {:?}", inner.as_rule()),
    }
}

fn parse_assignment_statement(
    pair: Pair<Rule>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) -> AssignmentStatement {
    assert_eq!(pair.as_rule(), Rule::assignment_statement);

    let mut inner = pair.into_inner();

    let table_name = parse_identifier(inner.next().unwrap());
    let pk_column = parse_identifier(inner.next().unwrap());
    let pk_expr = parse_expression(inner.next().unwrap());
    let field_name = parse_identifier(inner.next().unwrap());
    let rhs = parse_expression(inner.next().unwrap());

    let table = table_map
        .get(&table_name)
        .unwrap_or_else(|| panic!("Unknown table: {}", table_name))
        .clone();

    AssignmentStatement {
        table,
        pk_column,
        pk_expr,
        field_name,
        rhs,
    }
}

fn parse_var_assignment_statement(pair: Pair<Rule>) -> VarAssignmentStatement {
    assert_eq!(pair.as_rule(), Rule::var_assignment_statement);

    let mut inner = pair.into_inner();

    let var_name = parse_identifier(inner.next().unwrap());
    let rhs = parse_expression(inner.next().unwrap());

    VarAssignmentStatement { var_name, rhs }
}

fn parse_if_statement(
    pair: Pair<Rule>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) -> IfStatement {
    assert_eq!(pair.as_rule(), Rule::if_statement);

    let mut inner = pair.into_inner();

    let condition = parse_expression(inner.next().unwrap());
    let then_branch = parse_block(inner.next().unwrap(), table_map);
    let else_branch = inner
        .next()
        .map(|else_block| parse_block(else_block, table_map));

    IfStatement {
        condition,
        then_branch,
        else_branch,
    }
}

fn parse_block(
    pair: Pair<Rule>,
    table_map: &HashMap<String, Rc<TableDeclaration>>,
) -> Vec<Statement> {
    assert_eq!(pair.as_rule(), Rule::block);

    pair.into_inner()
        .filter(|item| item.as_rule() == Rule::statement)
        .map(|stmt| parse_statement(stmt, table_map))
        .collect()
}

// Expression parsing functions

fn parse_expression(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::expression);
    parse_logic_or(pair.into_inner().next().unwrap())
}

fn parse_logic_or(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::logic_or);

    let mut inner = pair.into_inner();
    let mut expr = parse_logic_and(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if let Some(next_operand_pair) = inner.next() {
            expr = build_binary_expression(expr, op_pair, parse_logic_and, next_operand_pair);
        }
    }
    expr
}

fn parse_logic_and(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::logic_and);

    let mut inner = pair.into_inner();
    let mut expr = parse_equality(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if let Some(next_operand_pair) = inner.next() {
            expr = build_binary_expression(expr, op_pair, parse_equality, next_operand_pair);
        }
    }
    expr
}

fn parse_equality(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::equality);

    let mut inner = pair.into_inner();
    let mut expr = parse_comparison(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if let Some(next_operand_pair) = inner.next() {
            expr = build_binary_expression(expr, op_pair, parse_comparison, next_operand_pair);
        }
    }
    expr
}

fn parse_comparison(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::comparison);

    let mut inner = pair.into_inner();
    let mut expr = parse_addition(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if let Some(next_operand_pair) = inner.next() {
            expr = build_binary_expression(expr, op_pair, parse_addition, next_operand_pair);
        }
    }
    expr
}

fn parse_addition(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::addition);

    let mut inner = pair.into_inner();
    let mut expr = parse_multiplication(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if let Some(next_operand_pair) = inner.next() {
            expr = build_binary_expression(expr, op_pair, parse_multiplication, next_operand_pair);
        }
    }
    expr
}

fn parse_multiplication(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::multiplication);

    let mut inner = pair.into_inner();
    let mut expr = parse_unary(inner.next().unwrap());

    while let Some(op_pair) = inner.next() {
        if let Some(next_operand_pair) = inner.next() {
            expr = build_binary_expression(expr, op_pair, parse_unary, next_operand_pair);
        }
    }
    expr
}

fn parse_unary(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::unary);

    let choice = pair.into_inner().next().unwrap();
    match choice.as_rule() {
        Rule::primary => parse_primary(choice),
        Rule::unary_op => {
            // Parse the unary operation structure directly
            let mut inner = choice.into_inner();
            let op_token = inner.next().unwrap();
            let operand = inner.next().unwrap();

            let op = match op_token.as_str() {
                "!" => UnaryOp::Not,
                "-" => UnaryOp::Neg,
                s => unreachable!("Unknown unary operator: {}", s),
            };

            Expression::UnaryOp {
                op,
                expr: Box::new(parse_unary(operand)),
            }
        }
        _ => unreachable!("Unexpected rule in parse_unary: {:?}", choice.as_rule()),
    }
}

fn parse_primary(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::primary);

    let token = pair.into_inner().next().unwrap();
    match token.as_rule() {
        Rule::table_field_access => parse_table_field_access(token),
        Rule::identifier => Expression::Ident(parse_identifier(token)),
        Rule::integer_literal => Expression::IntLit(parse_integer_literal(token)),
        Rule::float_literal => Expression::FloatLit(parse_float_literal(token)),
        Rule::string_literal => Expression::StringLit(parse_string_literal(token)),
        Rule::bool_literal => Expression::BoolLit(parse_bool_literal(token)),
        Rule::expression => parse_expression(token),
        _ => unreachable!("Unknown rule in parse_primary: {:?}", token.as_rule()),
    }
}

// Add new parsing function:
fn parse_table_field_access(pair: Pair<Rule>) -> Expression {
    assert_eq!(pair.as_rule(), Rule::table_field_access);

    let mut inner = pair.into_inner();

    let table_name = parse_identifier(inner.next().unwrap());
    let pk_column = parse_identifier(inner.next().unwrap());
    let pk_expr = parse_expression(inner.next().unwrap());
    let field_name = parse_identifier(inner.next().unwrap());

    Expression::TableFieldAccess {
        table_name,
        pk_column,
        pk_expr: Box::new(pk_expr),
        field_name,
    }
}

// Operator parsing functions

fn parse_logic_or_op(pair: Pair<Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::logic_or_op);
    BinaryOp::Or
}

fn parse_logic_and_op(pair: Pair<Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::logic_and_op);
    BinaryOp::And
}

fn parse_equality_op(pair: Pair<Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::equality_op);
    match pair.as_str() {
        "==" => BinaryOp::Eq,
        "!=" => BinaryOp::Neq,
        s => unreachable!("Unknown equality operator: {}", s),
    }
}

fn parse_comparison_op(pair: Pair<Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::comparison_op);
    match pair.as_str() {
        "<" => BinaryOp::Lt,
        "<=" => BinaryOp::Lte,
        ">" => BinaryOp::Gt,
        ">=" => BinaryOp::Gte,
        s => unreachable!("Unknown comparison operator: {}", s),
    }
}

fn parse_addition_op(pair: Pair<Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::addition_op);
    match pair.as_str() {
        "+" => BinaryOp::Add,
        "-" => BinaryOp::Sub,
        s => unreachable!("Unknown addition operator: {}", s),
    }
}

fn parse_multiplication_op(pair: Pair<Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::multiplication_op);
    match pair.as_str() {
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        s => unreachable!("Unknown multiplication operator: {}", s),
    }
}

fn build_binary_expression(
    lhs: Expression,
    op_pair: Pair<Rule>,
    rhs_parser: impl Fn(Pair<Rule>) -> Expression,
    next_operand_pair: Pair<Rule>,
) -> Expression {
    let op = match op_pair.as_rule() {
        Rule::logic_or_op => parse_logic_or_op(op_pair),
        Rule::logic_and_op => parse_logic_and_op(op_pair),
        Rule::equality_op => parse_equality_op(op_pair),
        Rule::comparison_op => parse_comparison_op(op_pair),
        Rule::addition_op => parse_addition_op(op_pair),
        Rule::multiplication_op => parse_multiplication_op(op_pair),
        _ => unreachable!("Unknown binary operator rule: {:?}", op_pair.as_rule()),
    };

    let rhs = rhs_parser(next_operand_pair);

    Expression::BinaryOp {
        left: Box::new(lhs),
        op,
        right: Box::new(rhs),
    }
}

// New parsing functions:

fn parse_var_decl_statement(pair: Pair<Rule>) -> VarDeclStatement {
    assert_eq!(pair.as_rule(), Rule::var_decl_statement);
    
    let mut inner = pair.into_inner();
    
    // Check if first token is global keyword
    let first = inner.next().unwrap();
    let (is_global, var_type, mut remaining) = if first.as_rule() == Rule::global_keyword {
        // global type_name identifier = expression ;
        let var_type = parse_type_name(inner.next().unwrap());
        (true, var_type, inner)
    } else {
        // type_name identifier = expression ;
        let var_type = parse_type_name(first);
        (false, var_type, inner)
    };
    
    let var_name = parse_identifier(remaining.next().unwrap());
    let init_value = parse_expression(remaining.next().unwrap());
    
    VarDeclStatement {
        var_type,
        var_name,
        init_value,
        is_global,
    }
}

fn parse_return_statement(pair: Pair<Rule>) -> ReturnStatement {
    assert_eq!(pair.as_rule(), Rule::return_statement);

    let mut inner = pair.into_inner();

    // Check if there's an expression after "return"
    let value = inner.next().map(parse_expression);

    ReturnStatement { value }
}
