use pest::Parser;
use pest_derive::Parser;
// pest::iterators::Pairs is not directly used, Pair is.
use pest::iterators::Pair;
use std::collections::HashMap;

use crate::ast::*;  // Import your final AST definitions

#[derive(Parser)]
#[grammar = "TransAct.pest"]
pub struct TransActParser;

pub fn parse_program<'ast>(source: &str) -> Program<'ast> {
    let mut program = Program {
        nodes: Vec::new(),
        tables: Vec::new(),
        functions: Vec::new(),
    };

    let mut node_map = HashMap::new();
    let mut table_map = HashMap::new();

    let parse_result = TransActParser::parse(Rule::program, source)
        .expect("Failed to parse the TransAct DSL.");

    // Get the single top-level program pair
    let program_pair = parse_result.into_iter().next().expect("Expected a single program rule");
    assert_eq!(program_pair.as_rule(), Rule::program);

    for top_item in program_pair.into_inner() {
        match top_item.as_rule() {
            Rule::nodes_block => {
                parse_nodes_block(top_item, &mut program, &mut node_map);
            }
            Rule::table_declaration => {
                parse_table_declaration(top_item, &mut program, &node_map, &mut table_map);
            }
            Rule::function_declaration => {
                parse_function_declaration(top_item, &mut program, &node_map, &table_map);
            }
            Rule::EOI => {}
            // Allow WHITESPACE_SL_COMMENT to be ignored at top level if pest generates it
            Rule::WHITESPACE_SL_COMMENT => {}
            _ => { /* Optionally print or panic for unexpected top-level items: panic!("Unexpected top-level rule: {:?}", top_item.as_rule()) */ }
        }
    }

    program
}

/// Parse `nodes { NodeA, NodeB, ... }`
fn parse_nodes_block<'ast>(
    pair: Pair<Rule>,
    program: &mut Program<'ast>,
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
fn parse_table_declaration<'ast>(
    pair: Pair<Rule>,
    program: &mut Program<'ast>,
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

    // Ensure node_ref has a lifetime tied to 'ast for the static leak
    // This requires program.nodes to be stable for 'ast.
    // The reference here will be valid as long as program.nodes is.
    // When creating static_table_ref, this node_ref's lifetime needs to be 'static.
    // This is tricky. The NodeDef itself is owned by Program.
    // For &'static TableDeclaration<'static>, its node field must be &'static NodeDef.
    // This implies NodeDef must also be leaked or come from a static source.
    // A truly 'static NodeDef would mean nodes are also leaked or globally defined.
    // Let's proceed with the current structure, but be aware of this lifetime complexity.
    // The `node: node_ref` below will have lifetime of `program`, not `'static`.
    // This will cause a problem when `TableDeclaration` expects `node: &'ast NodeDef`
    // and we try to make `TableDeclaration` itself `'static`.
    // The `node` field in `TableDeclaration<'ast>` is `&'ast NodeDef`.
    // If `TableDeclaration` becomes `TableDeclaration<'static>`, then `node` must be `&'static NodeDef`.
    // This implies `program.nodes` must contain `&'static NodeDef` or `NodeDef` that can be referenced statically.
    // This is a fundamental lifetime issue with the current AST structure and leaking strategy.

    // To make `node: node_ref` work for a `TableDeclaration` that will be leaked to `'static`,
    // `node_ref` itself must effectively be `'static`. This means `program.nodes` would need to hold
    // `Box<NodeDef>` that are leaked, or `NodeDef` are stored in a static array.
    // Given the current `program.nodes: Vec<NodeDef>`, `&program.nodes[*node_idx]` is not `'static`.

    // Quick fix assuming NodeDef is simple and can be "copied" into the static context if needed,
    // or that the lifetime `'ast` for `TableDeclaration` in `program.tables` is sufficient and
    // the `'static` requirement for `AssignmentStatement.table` is the main driver for leaking.
    // The `node: node_ref` below will have the lifetime of `program`.
    // When we create `TableDeclaration` for leaking, its `node` field must be `&'static NodeDef`.
    // This is a conflict.

    // Let's assume `NodeDef` is simple (just a String name) and for the purpose of the
    // `'static TableDeclaration`, we can create a new, leaked `NodeDef`.
    // This is a significant workaround due to the lifetime constraints.
    let static_node_def_for_leaked_table = Box::leak(Box::new(NodeDef { name: node_name.clone() }));


    let table_decl_to_leak = TableDeclaration {
        name: table_name.clone(),
        node: static_node_def_for_leaked_table, // Now using a leaked NodeDef
        fields: fields_temp.iter().map(|f_orig| FieldDeclaration {
            field_type: f_orig.field_type.clone(),
            field_name: f_orig.field_name.clone(),
        }).collect(),
    };
    
    let static_table_ref: &'static TableDeclaration<'static> = Box::leak(Box::new(table_decl_to_leak));

    // program.tables stores TableDeclaration<'ast> where 'ast is tied to the Program's lifetime.
    // The node reference here should be from program.nodes.
    program.tables.push(table_decl_for_program);


    table_map.insert(table_name, static_table_ref);
}

fn parse_field_declaration(pair: Pair<Rule>) -> FieldDeclaration {
    let mut ty = TypeName::Int; 
    let mut name = String::new();
    let mut reading_type = true;

    for c in pair.into_inner() {
        match c.as_rule() {
            Rule::type_ if reading_type => { // Changed from Rule::type
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
fn parse_function_declaration<'ast>(
    pair: Pair<Rule>,
    program: &'ast mut Program<'ast>, // Added 'ast lifetime
    node_map: &HashMap<String, usize>,
    table_map: &HashMap<String, &'static TableDeclaration<'static>>
) {
    let mut return_type = ReturnType::Void;
    let mut func_name = String::new();
    let mut parameters = Vec::new();
    let mut hops = Vec::new();

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
                let hop = parse_hop_block(item.into_inner().next().unwrap(), node_map, table_map, program); // function_body_item = hop_block
                hops.push(hop);
            }
            _ => {}
        }
    }

    program.functions.push(FunctionDeclaration {
        return_type,
        name: func_name,
        parameters,
        hops,
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
            Rule::type_ if reading_type => { // Changed from Rule::type
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
fn parse_hop_block<'ast>(
    pair: Pair<Rule>, // This should be the hop_block pair itself
    node_map: &HashMap<String, usize>,
    table_map: &HashMap<String, &'static TableDeclaration<'static>>,
    program: &'ast Program<'ast>, // Added 'ast lifetime
) -> HopBlock<'ast> {
    let mut node_name = String::new();
    let mut reading_node = true;
    let mut statements = Vec::new();

    for c in pair.into_inner() { // Children of hop_block: identifier, then statement*
        match c.as_rule() {
            Rule::identifier if reading_node => {
                node_name = c.as_str().to_string();
                reading_node = false;
            }
            Rule::statement => {
                // parse_statement expects the rule that IS the statement (e.g. assignment_statement)
                // not Rule::statement itself. So we need to get the inner actual statement rule.
                statements.push(parse_statement(c.into_inner().next().unwrap(), table_map));
            }
            _ => {}
        }
    }

    let node_idx = node_map.get(&node_name)
        .unwrap_or_else(|| panic!("Unknown node: {}", node_name));
    let node_ref = &program.nodes[*node_idx];

    HopBlock {
        node: node_ref,
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
    let mut table_name = String::new();
    let mut pk_column = String::new();
    let mut pk_expr = Expression::IntLit(0); // Default
    let mut field_name = String::new();
    let mut rhs = Expression::IntLit(0); // Default

    let mut inner_pairs = pair.into_inner(); // identifier, identifier, expression, identifier, expression

    table_name = inner_pairs.next().unwrap().as_str().to_string();
    pk_column = inner_pairs.next().unwrap().as_str().to_string();
    pk_expr = parse_expression(inner_pairs.next().unwrap());
    field_name = inner_pairs.next().unwrap().as_str().to_string();
    rhs = parse_expression(inner_pairs.next().unwrap());


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
    // This depends on how pest handles the literal "else". If "else" becomes a token like Rule::else_
    // then this logic can work.
    for c in pair.into_inner() {
        match c.as_rule() {
            Rule::expression if matches!(mode, Mode::Cond) => {
                condition = parse_expression(c);
                mode = Mode::Then;
            }
            Rule::statement if matches!(mode, Mode::Then) => {
                then_stmts.push(parse_statement(c.into_inner().next().unwrap(), table_map));
            }
            Rule::statement if matches!(mode, Mode::Else) => {
                else_stmts.get_or_insert_with(Vec::new)
                          .push(parse_statement(c.into_inner().next().unwrap(), table_map));
            }
            // Attempting to match the "else" keyword. This assumes pest creates a rule like `else_` for the literal "else".
            // This is a guess. If `pest` consumes "else" silently or names the rule differently, this will fail.
            Rule::else_ => { // Tentative fix for `else_clause`
                mode = Mode::Else;
            }
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
    // So, pair.into_inner().next().unwrap() is either Rule::primary or a group representing the op+operand.
    let choice = pair.into_inner().next().unwrap();
    match choice.as_rule() {
        Rule::primary => parse_primary(choice),
        _ => {
            // This 'choice' pair represents one of the `(op ~ unary)` groups.
            // The operator ("!" or "-") is consumed by pest.
            // The inner content of 'choice' is the `Rule::unary` operand.
            let operand_pair = choice.into_inner().next().unwrap(); // This is the inner Rule::unary
            let op_str = choice.as_str(); // This will be like "!x" or "-y"

            let op = if op_str.starts_with('!') {
                UnaryOp::Not
            } else if op_str.starts_with('-') {
                UnaryOp::Neg
            } else {
                unreachable!("Unary operator group did not start with '!' or '-': {}", op_str);
            };
            
            Expression::UnaryOp {
                op,
                expr: Box::new(parse_unary(operand_pair)), // Recursive call for the operand
            }
        }
    }
}

fn parse_primary(pair: Pair<Rule>) -> Expression { // pair is Rule::primary
    // primary = { identifier | float_literal | integer_literal | string_literal | "(" ~ expression ~ ")" }
    // So, pair.into_inner().next().unwrap() is the actual token or the inner expression.
    let actual_primary_token_or_expr = pair.into_inner().next().unwrap();
    match actual_primary_token_or_expr.as_rule() {
        Rule::identifier => Expression::Ident(actual_primary_token_or_expr.as_str().to_string()),
        Rule::integer_literal => {
            Expression::IntLit(actual_primary_token_or_expr.as_str().parse().unwrap())
        }
        Rule::float_literal => {
            Expression::FloatLit(actual_primary_token_or__expr.as_str().parse().unwrap())
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