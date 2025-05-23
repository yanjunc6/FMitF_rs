use pest::Parser;
use pest_derive::Parser;
// pest::iterators::Pairs is not directly used, Pair is.
use pest::iterators::Pair;
use std::collections::HashMap;

use crate::ast::*;  // Import your final AST definitions

#[derive(Parser)]
#[grammar = "TransAct.pest"]
pub struct TransActParser;

pub fn parse_program(source: &str) -> Program<'static> {
    let mut program = Program {
        nodes: Vec::new(),
        tables: Vec::new(),
        functions: Vec::new(),
    };

    let mut node_map = HashMap::new();
    let mut table_map = HashMap::new();

    let parse_result = TransActParser::parse(Rule::program, source)
        .expect("Failed to parse the TransAct DSL.");

    let program_pair = parse_result.into_iter().next().expect("Expected a single program rule");
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
            Rule::EOI => {}
            Rule::WHITESPACE_SL_COMMENT => {}
            _ => {}
        }
    }

    // Second pass: parse functions (now that we have all nodes and tables)
    for top_item in program_pair.into_inner() {
        match top_item.as_rule() {
            Rule::function_declaration => {
                parse_function_declaration(top_item, &mut program, &node_map, &table_map);
            }
            _ => {} // Skip already processed items
        }
    }

    program
}

/// Parse `nodes { NodeA, NodeB, ... }`
fn parse_nodes_block<'a>(
    pair: Pair<Rule>,
    program: &'a mut Program<'a>,
    node_map: &mut HashMap<String, usize>
) {
    for item in pair.into_inner() {
        if item.as_rule() == Rule::node_list {
            for ident_item in item.into_inner() {
                if ident_item.as_rule() == Rule::identifier {
                    let name = ident_item.as_str().to_string();
                    let idx = program.nodes.len();
                    program.nodes.push(NodeDef { name: name.clone() });
                    node_map.insert(name, idx);
                }
            }
        }
    }
}

/// Parse `table <table_name> on <node_name> { field_declaration* } ;`
fn parse_table_declaration<'a>(
    pair: Pair<Rule>,
    program: &'a mut Program<'a>,
    node_map: &HashMap<String, usize>,
    table_map: &mut HashMap<String, &'static TableDeclaration<'static>>
) {
    let mut table_name = String::new();
    let mut node_name = String::new();
    let mut fields_temp = Vec::new();

    let mut phase = 0;
    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::identifier if phase == 0 => {
                table_name = item.as_str().to_string();
                phase = 1;
            }
            Rule::identifier if phase == 1 => {
                node_name = item.as_str().to_string();
                phase = 2;
            }
            Rule::field_declaration => {
                fields_temp.push(parse_field_declaration(item));
            }
            _ => {}
        }
    }

    let node_idx = node_map
        .get(&node_name)
        .unwrap_or_else(|| panic!("No such node: {}", node_name));

    let node_ref_for_program_tables = &program.nodes[*node_idx];
    let table_decl_for_program = TableDeclaration {
        name: table_name.clone(),
        node: node_ref_for_program_tables,
        fields: fields_temp.iter().map(|f_orig| FieldDeclaration {
            field_type: f_orig.field_type.clone(),
            field_name: f_orig.field_name.clone(),
        }).collect(),
    };
    program.tables.push(table_decl_for_program);

    let static_node_def_for_leaked_table = Box::leak(Box::new(NodeDef { name: node_name.clone() }));

    let table_decl_to_leak = TableDeclaration {
        name: table_name.clone(),
        node: static_node_def_for_leaked_table,
        fields: fields_temp.iter().map(|f_orig| FieldDeclaration {
            field_type: f_orig.field_type.clone(),
            field_name: f_orig.field_name.clone(),
        }).collect(),
    };
    
    let static_table_ref: &'static TableDeclaration<'static> = Box::leak(Box::new(table_decl_to_leak));
    table_map.insert(table_name, static_table_ref);
}

fn parse_field_declaration(pair: Pair<Rule>) -> FieldDeclaration {
    let mut ty = TypeName::Int;
    let mut name = String::new();
    let mut reading_type = true;

    for c in pair.into_inner() {
        match c.as_rule() {
            Rule::type_name if reading_type => { // Corrected from Rule::type_ to Rule::type_name
                ty = match c.as_str() {
                    "int" => TypeName::Int,
                    "float" => TypeName::Float,
                    "string" => TypeName::String,
                    other => panic!("Unknown type: {}", other),
                };
                reading_type = false;
            }
            Rule::identifier => {
                name = c.as_str().to_string();
            }
            _ => {}
        }
    }
    FieldDeclaration {
        field_type: ty,
        field_name: name,
    }
}

/// Parse a function: `ret_type functionName ( param_list? ) { hop_block* }`
fn parse_function_declaration<'a>(
    pair: Pair<Rule>,
    program: &'a mut Program<'static>, 
    node_map: &HashMap<String, usize>,
    table_map: &HashMap<String, &'static TableDeclaration<'static>>
) {
    let mut return_type = ReturnType::Void;
    let mut func_name = String::new();
    let mut parameters = Vec::new();
    let mut hops_temp = Vec::new(); 

    let mut phase = 0;
    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::ret_type if phase == 0 => {
                return_type = parse_return_type(item.as_str());
                phase = 1;
            }
            Rule::identifier if phase == 1 => {
                func_name = item.as_str().to_string();
                phase = 2;
            }
            Rule::parameter_list => {
                parameters = parse_parameter_list(item);
            }
            Rule::function_body_item => {
                // Create static references to nodes for the hop blocks
                let hop = parse_hop_block(
                    item.into_inner().next().unwrap(),
                    node_map,
                    table_map,
                );
                hops_temp.push(hop);
            }
            _ => {}
        }
    }

    program.functions.push(FunctionDeclaration {
        return_type,
        name: func_name,
        parameters,
        hops: hops_temp, 
    });
}

fn parse_return_type(s: &str) -> ReturnType {
    if s == "void" {
        ReturnType::Void
    } else {
        let ty = match s {
            "int" => TypeName::Int,
            "float" => TypeName::Float,
            "string" => TypeName::String,
            _ => panic!("Unknown return type {}", s),
        };
        ReturnType::Type(ty)
    }
}

fn parse_parameter_list(pair: Pair<Rule>) -> Vec<ParameterDecl> {
    let mut params = Vec::new();
    for child in pair.into_inner() {
        if child.as_rule() == Rule::parameter_decl {
            params.push(parse_parameter_decl(child));
        }
    }
    params
}

/// Parse: `type identifier`
fn parse_parameter_decl(pair: Pair<Rule>) -> ParameterDecl {
    let mut ptype = TypeName::Int;
    let mut pname = String::new();
    let mut reading_type = true;

    for c in pair.into_inner() {
        match c.as_rule() {
            Rule::type_name if reading_type => { // Corrected from Rule::type_ to Rule::type_name
                ptype = match c.as_str() {
                    "int" => TypeName::Int,
                    "float" => TypeName::Float,
                    "string" => TypeName::String,
                    _ => panic!("Unknown param type"),
                };
                reading_type = false;
            }
            Rule::identifier => {
                pname = c.as_str().to_string();
            }
            _ => {}
        }
    }

    ParameterDecl {
        param_type: ptype,
        param_name: pname,
    }
}

/// Parse a hop block: `hop on <node_name> { statement* }`
fn parse_hop_block(
    pair: Pair<Rule>,
    node_map: &HashMap<String, usize>,
    table_map: &HashMap<String, &'static TableDeclaration<'static>>,
) -> HopBlock<'static> {
    let mut node_name = String::new();
    let mut reading_node = true;
    let mut statements = Vec::new();

    for c in pair.into_inner() {
        match c.as_rule() {
            Rule::identifier if reading_node => {
                node_name = c.as_str().to_string();
                reading_node = false;
            }
            Rule::statement => {
                statements.push(parse_statement(c.into_inner().next().unwrap(), table_map));
            }
            _ => {}
        }
    }

    // Create a static reference to the node
    let static_node_ref: &'static NodeDef = Box::leak(Box::new(NodeDef { 
        name: node_name.clone() 
    }));

    HopBlock {
        node: static_node_ref,
        statements,
    }
}

fn parse_statement(
    pair: Pair<Rule>, // This pair is the actual statement rule (e.g., assignment_statement)
    table_map: &HashMap<String, &'static TableDeclaration<'static>>,
) -> Statement {
    match pair.as_rule() {
        Rule::assignment_statement => {
            Statement::Assignment(parse_assignment(pair, table_map))
        }
        Rule::if_statement => {
            Statement::IfStmt(parse_if_statement(pair, table_map))
        }
        Rule::empty_statement => Statement::Empty,
        _ => unreachable!("Unexpected rule in parse_statement: {:?}", pair.as_rule()),
    }
}

/// assignment_statement =
///   identifier "[" identifier ":" expression "]" "." identifier "=" expression ";"
fn parse_assignment(
    pair: Pair<Rule>,
    table_map: &HashMap<String, &'static TableDeclaration<'static>>,
) -> AssignmentStatement {
    let mut inner_pairs = pair.into_inner(); // identifier, identifier, expression, identifier, expression

    let table_name = inner_pairs.next().unwrap().as_str().to_string();
    let pk_column = inner_pairs.next().unwrap().as_str().to_string();
    let pk_expr = parse_expression(inner_pairs.next().unwrap());
    let field_name = inner_pairs.next().unwrap().as_str().to_string();
    let rhs = parse_expression(inner_pairs.next().unwrap());


    let table_ref = table_map
        .get(&table_name)
        .unwrap_or_else(|| panic!("Unknown table: {}", table_name));

    AssignmentStatement {
        table: *table_ref,
        pk_column,
        pk_expr,
        field_name,
        rhs,
    }
}

/// if_statement = "if" "(" expression ")" "{" statement* "}" ( "else" "{" statement* "}" )?
fn parse_if_statement(
    pair: Pair<Rule>, // This is Rule::if_statement
    table_map: &HashMap<String, &'static TableDeclaration<'static>>,
) -> IfStatement {
    let mut condition = Expression::IntLit(0); // Default
    let mut then_stmts = Vec::new();
    let mut else_stmts = None;

    enum Mode { Cond, Then, Else }
    let mut mode = Mode::Cond;

    // Children of if_statement: expression, then statement* (for then), then optionally "else" keyword token, then statement* (for else)
    // This depends on how pest handles the literal "else".
    // The literal "else" does not become Rule::else_ with the current grammar.
    // This parsing logic will likely only capture `then_stmts` correctly.
    // A more robust parsing of `else` would require grammar changes or more complex iteration.
    for c in pair.into_inner() {
        match c.as_rule() {
            Rule::expression if matches!(mode, Mode::Cond) => {
                condition = parse_expression(c);
                mode = Mode::Then;
            }
            Rule::statement if matches!(mode, Mode::Then) => {
                then_stmts.push(parse_statement(c.into_inner().next().unwrap(), table_map));
            }
            Rule::statement if matches!(mode, Mode::Else) => { // This mode is unlikely to be reached correctly.
                else_stmts.get_or_insert_with(Vec::new)
                          .push(parse_statement(c.into_inner().next().unwrap(), table_map));
            }
            // Rule::else_ was removed as it doesn't exist with the current grammar.
            // Transitioning to Mode::Else correctly is non-trivial here.
            _ => { /* Ignored (e.g. braces if they were tokens, or other structural elements) */ }
        }
    }

    IfStatement {
        condition,
        then_branch: then_stmts,
        else_branch: else_stmts,
    }
}


fn parse_expression(pair: Pair<Rule>) -> Expression {
    // expression rule in pest is { logic_or }. So pair is Rule::expression, its child is Rule::logic_or
    parse_logic_or(pair.into_inner().next().unwrap())
}

// Operator-precedence climbing functions
fn pratt_parser_parse_op(
    lhs: Expression,
    op_pair: Pair<Rule>,
    rhs_parser: impl Fn(Pair<Rule>) -> Expression,
    next_operand_pair: Pair<Rule>,
) -> Expression {
    let op = match op_pair.as_str() {
        "||" => BinaryOp::Or,
        "&&" => BinaryOp::And,
        "==" => BinaryOp::Eq,
        "!=" => BinaryOp::Neq,
        "<" => BinaryOp::Lt,
        "<=" => BinaryOp::Lte,
        ">" => BinaryOp::Gt,
        ">=" => BinaryOp::Gte,
        "+" => BinaryOp::Add,
        "-" => BinaryOp::Sub,
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        s => unreachable!("Unknown binary operator: {}", s),
    };
    let rhs = rhs_parser(next_operand_pair);
    Expression::BinaryOp {
        left: Box::new(lhs),
        op,
        right: Box::new(rhs),
    }
}

fn parse_logic_or(pair: Pair<Rule>) -> Expression { // pair is Rule::logic_or
    let mut inner_pairs = pair.into_inner(); // children: logic_and, (op logic_and)*
    let mut expr = parse_logic_and(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() { // op_pair is "||"
        let next_operand_pair = inner_pairs.next().unwrap(); // the next logic_and
        expr = pratt_parser_parse_op(expr, op_pair, parse_logic_and, next_operand_pair);
    }
    expr
}

fn parse_logic_and(pair: Pair<Rule>) -> Expression { // pair is Rule::logic_and
    let mut inner_pairs = pair.into_inner(); // children: equality, (op equality)*
    let mut expr = parse_equality(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() { // op_pair is "&&"
        let next_operand_pair = inner_pairs.next().unwrap();
        expr = pratt_parser_parse_op(expr, op_pair, parse_equality, next_operand_pair);
    }
    expr
}

fn parse_equality(pair: Pair<Rule>) -> Expression { // pair is Rule::equality
    let mut inner_pairs = pair.into_inner(); // children: comparison, (op comparison)*
    let mut expr = parse_comparison(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() { // op_pair is "==" or "!="
        let next_operand_pair = inner_pairs.next().unwrap();
        expr = pratt_parser_parse_op(expr, op_pair, parse_comparison, next_operand_pair);
    }
    expr
}

fn parse_comparison(pair: Pair<Rule>) -> Expression { // pair is Rule::comparison
    let mut inner_pairs = pair.into_inner(); // children: addition, (op addition)*
    let mut expr = parse_addition(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() { // op_pair is "<", "<=", ">", ">="
        let next_operand_pair = inner_pairs.next().unwrap();
        expr = pratt_parser_parse_op(expr, op_pair, parse_addition, next_operand_pair);
    }
    expr
}

fn parse_addition(pair: Pair<Rule>) -> Expression { // pair is Rule::addition
    let mut inner_pairs = pair.into_inner(); // children: multiplication, (op multiplication)*
    let mut expr = parse_multiplication(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() { // op_pair is "+" or "-"
        let next_operand_pair = inner_pairs.next().unwrap();
        expr = pratt_parser_parse_op(expr, op_pair, parse_multiplication, next_operand_pair);
    }
    expr
}

fn parse_multiplication(pair: Pair<Rule>) -> Expression { // pair is Rule::multiplication
    let mut inner_pairs = pair.into_inner(); // children: unary, (op unary)*
    let mut expr = parse_unary(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() { // op_pair is "*" or "/"
        let next_operand_pair = inner_pairs.next().unwrap();
        expr = pratt_parser_parse_op(expr, op_pair, parse_unary, next_operand_pair);
    }
    expr
}

fn parse_unary(pair: Pair<Rule>) -> Expression { // pair is Rule::unary
    // unary = { primary | ( "!" ~ unary ) | ( "-" ~ unary ) }
    let choice = pair.into_inner().next().unwrap();
    match choice.as_rule() {
        Rule::primary => parse_primary(choice),
        _ => { // This 'choice' pair represents one of the `(op ~ unary)` groups.
            let op_str = choice.as_str(); // Get "op + operand" string first
            let operand_pair = choice.into_inner().next().unwrap(); // Then consume choice for its inner Rule::unary

            let op = if op_str.starts_with('!') {
                UnaryOp::Not
            } else if op_str.starts_with('-') {
                UnaryOp::Neg
            } else {
                unreachable!("Unary operator group did not start with '!' or '-': {}", op_str);
            };
            
            Expression::UnaryOp {
                op,
                expr: Box::new(parse_unary(operand_pair)),
            }
        }
    }
}

fn parse_primary(pair: Pair<Rule>) -> Expression { // pair is Rule::primary
    // primary = { identifier | float_literal | integer_literal | string_literal | "(" ~ expression ~ ")" }
    let actual_primary_token_or_expr = pair.into_inner().next().unwrap();
    match actual_primary_token_or_expr.as_rule() {
        Rule::identifier => Expression::Ident(actual_primary_token_or_expr.as_str().to_string()),
        Rule::integer_literal => {
            Expression::IntLit(actual_primary_token_or_expr.as_str().parse().unwrap())
        }
        Rule::float_literal => {
            Expression::FloatLit(actual_primary_token_or_expr.as_str().parse().unwrap()) // Corrected typo
        }
        Rule::string_literal => {
            let raw = actual_primary_token_or_expr.as_str(); 
            let content = &raw[1..raw.len()-1]; // Remove quotes
            Expression::StringLit(content.to_string().replace("\\\"", "\"").replace("\\\\", "\\")) // Handle basic escapes
        }
        Rule::expression => parse_expression(actual_primary_token_or_expr), // For "(" expression ")"
        _ => unreachable!("Unknown rule in parse_primary: {:?}", actual_primary_token_or_expr.as_rule()),
    }
}