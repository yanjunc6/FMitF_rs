use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
pub use errors::{TransActError, Results, SpannedError, format_errors};

pub mod errors;

#[derive(Parser)]
#[grammar = "frontend/parse/grammar.pest"]  // Updated path
pub struct TransActParser;

fn error_at(pair: &Pair<Rule>, error: TransActError) -> SpannedError {
    SpannedError {
        error,
        span: Some(Span::from_pest(pair.as_span())),
    }
}

pub fn parse_program(source: &str) -> Results<Program> {
    let pairs = TransActParser::parse(Rule::program, source)
        .map_err(|e| vec![SpannedError { error: TransActError::ParseError(e.to_string()), span: None }])?;

    let program_pair = pairs.into_iter().next()
        .ok_or_else(|| vec![SpannedError { error: TransActError::ParseError("No program found".into()), span: None }])?;

    let mut nodes = Vec::new();
    let mut tables = Vec::new();
    let mut functions = Vec::new();
    let mut node_map = HashMap::new();
    let mut table_map = HashMap::new();
    let mut errors = Vec::new();

    // First pass: collect nodes and tables
    for item in program_pair.clone().into_inner() {
        match item.as_rule() {
            Rule::nodes_block => {
                if let Err(mut errs) = parse_nodes(item, &mut nodes, &mut node_map) {
                    errors.append(&mut errs);
                }
            }
            Rule::table_declaration => {
                if let Err(mut errs) = parse_table(item, &mut tables, &node_map, &mut table_map) {
                    errors.append(&mut errs);
                }
            }
            _ => {}
        }
    }

    // Second pass: parse functions
    for item in program_pair.into_inner() {
        if item.as_rule() == Rule::function_declaration {
            if let Err(mut errs) = parse_function(item, &mut functions, &node_map, &table_map) {
                errors.append(&mut errs);
            }
        }
    }

    if errors.is_empty() {
        Ok(Program { nodes, tables, functions })
    } else {
        Err(errors)
    }
}

fn parse_nodes(pair: Pair<Rule>, nodes: &mut Vec<Rc<NodeDef>>, node_map: &mut HashMap<String, Rc<NodeDef>>) -> Results<()> {
    for item in pair.into_inner() {
        if item.as_rule() == Rule::node_list {
            for node_pair in item.into_inner() {
                let name = node_pair.as_str().to_string();
                let span = Span::from_pest(node_pair.as_span());
                let node = Rc::new(NodeDef { name: name.clone(), span });
                nodes.push(node.clone());
                node_map.insert(name, node);
            }
        }
    }
    Ok(())
}

fn parse_table(
    pair: Pair<Rule>, 
    tables: &mut Vec<Rc<TableDeclaration>>, 
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &mut HashMap<String, Rc<TableDeclaration>>
) -> Results<()> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    let table_name = inner.next().unwrap().as_str().to_string();
    let node_name = inner.next().unwrap().as_str().to_string();
    
    let node = node_map.get(&node_name)
        .ok_or_else(|| vec![SpannedError { error: TransActError::UndeclaredNode(node_name), span: Some(span.clone()) }])?
        .clone();

    let mut fields = Vec::new();
    for field_pair in inner {
        if field_pair.as_rule() == Rule::field_declaration {
            let field_span = Span::from_pest(field_pair.as_span());
            let mut field_inner = field_pair.into_inner();
            let field_type = parse_type(field_inner.next().unwrap())?;
            let field_name = field_inner.next().unwrap().as_str().to_string();
            fields.push(FieldDeclaration { field_type, field_name, span: field_span });
        }
    }

    let table = Rc::new(TableDeclaration { 
        name: table_name.clone(), 
        node, 
        fields,
        span
    });
    tables.push(table.clone());
    table_map.insert(table_name, table);
    Ok(())
}

fn parse_function(
    pair: Pair<Rule>,
    functions: &mut Vec<FunctionDeclaration>,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>
) -> Results<()> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    let return_type = parse_return_type(inner.next().unwrap())?;
    let name = inner.next().unwrap().as_str().to_string();
    
    let mut parameters = Vec::new();
    let mut hops = Vec::new();

    for item in inner {
        match item.as_rule() {
            Rule::parameter_list => {
                for param_pair in item.into_inner() {
                    let param_span = Span::from_pest(param_pair.as_span());
                    let mut param_inner = param_pair.into_inner();
                    let param_type = parse_type(param_inner.next().unwrap())?;
                    let param_name = param_inner.next().unwrap().as_str().to_string();
                    parameters.push(ParameterDecl { param_type, param_name, span: param_span });
                }
            }
            Rule::function_body_item => {
                let hop_pair = item.into_inner().next().unwrap();
                hops.push(parse_hop(hop_pair, node_map, table_map)?);
            }
            _ => {}
        }
    }

    functions.push(FunctionDeclaration { return_type, name, parameters, hops, span });
    Ok(())
}

fn parse_hop(
    pair: Pair<Rule>,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>
) -> Result<HopBlock, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    let node_name = inner.next().unwrap().as_str().to_string();
    let block_pair = inner.next().unwrap();

    let node = node_map.get(&node_name)
        .ok_or_else(|| vec![SpannedError { error: TransActError::UndeclaredNode(node_name), span: Some(span.clone()) }])?
        .clone();

    let statements = parse_block(block_pair, table_map)?;
    Ok(HopBlock { node, statements, span })
}

fn parse_block(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Results<Vec<Statement>> {
    let mut statements = Vec::new();
    for stmt_pair in pair.into_inner() {
        statements.push(parse_statement(stmt_pair, table_map)?);
    }
    Ok(statements)
}

fn parse_statement(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<Statement, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let inner = pair.into_inner().next().unwrap();
    
    let kind = match inner.as_rule() {
        Rule::assignment_statement => StatementKind::Assignment(parse_assignment(inner, table_map)?),
        Rule::var_assignment_statement => StatementKind::VarAssignment(parse_var_assignment(inner)?),
        Rule::if_statement => StatementKind::IfStmt(parse_if(inner, table_map)?),
        Rule::var_decl_statement => StatementKind::VarDecl(parse_var_decl(inner)?),
        Rule::return_statement => StatementKind::Return(parse_return(inner)?),
        Rule::empty_statement => StatementKind::Empty,
        _ => return Err(vec![error_at(&inner, TransActError::ParseError(format!("Unknown statement: {:?}", inner.as_rule())))]),
    };
    
    Ok(Spanned { node: kind, span })
}

fn parse_assignment(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<AssignmentStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let table_name = inner.next().unwrap().as_str().to_string();
    let pk_column = inner.next().unwrap().as_str().to_string();
    let pk_expr = parse_expression(inner.next().unwrap())?;
    let field_name = inner.next().unwrap().as_str().to_string();
    let rhs = parse_expression(inner.next().unwrap())?;

    let table = table_map.get(&table_name)
        .ok_or_else(|| vec![SpannedError { error: TransActError::UndeclaredTable(table_name), span: None }])?
        .clone();

    Ok(AssignmentStatement { table, pk_column, pk_expr, field_name, rhs })
}

fn parse_var_assignment(pair: Pair<Rule>) -> Result<VarAssignmentStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let var_name = inner.next().unwrap().as_str().to_string();
    let rhs = parse_expression(inner.next().unwrap())?;
    Ok(VarAssignmentStatement { var_name, rhs })
}

fn parse_if(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<IfStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let condition = parse_expression(inner.next().unwrap())?;
    let then_branch = parse_block(inner.next().unwrap(), table_map)?;
    let else_branch = inner.next().map(|p| parse_block(p, table_map)).transpose()?;
    Ok(IfStatement { condition, then_branch, else_branch })
}

fn parse_var_decl(pair: Pair<Rule>) -> Result<VarDeclStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    
    let (is_global, var_type, var_name, init_value) = if first.as_rule() == Rule::global_keyword {
        let var_type = parse_type(inner.next().unwrap())?;
        let var_name = inner.next().unwrap().as_str().to_string();
        let init_value = parse_expression(inner.next().unwrap())?;
        (true, var_type, var_name, init_value)
    } else {
        let var_type = parse_type(first)?;
        let var_name = inner.next().unwrap().as_str().to_string();
        let init_value = parse_expression(inner.next().unwrap())?;
        (false, var_type, var_name, init_value)
    };

    Ok(VarDeclStatement { var_type, var_name, init_value, is_global })
}

fn parse_return(pair: Pair<Rule>) -> Result<ReturnStatement, Vec<SpannedError>> {
    let value = pair.into_inner().next().map(parse_expression).transpose()?;
    Ok(ReturnStatement { value })
}

fn parse_expression(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    
    let kind = match pair.as_rule() {
        Rule::identifier => ExpressionKind::Ident(pair.as_str().to_string()),
        Rule::integer_literal => ExpressionKind::IntLit(pair.as_str().parse().unwrap()),
        Rule::float_literal => ExpressionKind::FloatLit(pair.as_str().parse().unwrap()),
        Rule::string_literal => {
            let s = pair.as_str();
            ExpressionKind::StringLit(s[1..s.len()-1].to_string())
        }
        Rule::bool_literal => ExpressionKind::BoolLit(pair.as_str() == "true"),
        Rule::table_field_access => {
            let mut inner = pair.into_inner();
            let table_name = inner.next().unwrap().as_str().to_string();
            let pk_column = inner.next().unwrap().as_str().to_string();
            let pk_expr = Box::new(parse_expression(inner.next().unwrap())?);
            let field_name = inner.next().unwrap().as_str().to_string();
            ExpressionKind::TableFieldAccess { table_name, pk_column, pk_expr, field_name }
        }
        Rule::expression | Rule::logic_or | Rule::logic_and | Rule::equality | 
        Rule::comparison | Rule::addition | Rule::multiplication => {
            return parse_complex_expression(pair);
        }
        Rule::unary => {
            let mut inner = pair.into_inner();
            let first = inner.next().unwrap();
            if first.as_rule() == Rule::unary_op {
                let mut op_inner = first.into_inner();
                let op_str = op_inner.next().unwrap().as_str();
                let operand = parse_expression(inner.next().unwrap())?;
                let op = match op_str {
                    "!" => UnaryOp::Not,
                    "-" => UnaryOp::Neg,
                    _ => return Err(vec![SpannedError { 
                        error: TransActError::ParseError(format!("Unknown unary op: {}", op_str)),
                        span: Some(span)
                    }]),
                };
                ExpressionKind::UnaryOp { op, expr: Box::new(operand) }
            } else {
                return parse_expression(first);
            }
        }
        Rule::primary => {
            let inner = pair.into_inner().next().unwrap();
            return parse_expression(inner);
        }
        _ => return Err(vec![SpannedError { 
            error: TransActError::ParseError(format!("Unknown expression: {:?}", pair.as_rule())),
            span: Some(span)
        }]),
    };
    
    Ok(Spanned { node: kind, span })
}

fn parse_complex_expression(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    match pair.as_rule() {
        Rule::expression => parse_expression(pair.into_inner().next().unwrap()),
        Rule::logic_or => parse_binary_expr(pair, parse_logic_and, |op| op == "||"),
        Rule::logic_and => parse_binary_expr(pair, parse_equality, |op| op == "&&"),
        Rule::equality => parse_binary_expr(pair, parse_comparison, |op| matches!(op, "==" | "!=")),
        Rule::comparison => parse_binary_expr(pair, parse_addition, |op| matches!(op, "<" | "<=" | ">" | ">=")),
        Rule::addition => parse_binary_expr(pair, parse_multiplication, |op| matches!(op, "+" | "-")),
        Rule::multiplication => parse_binary_expr(pair, parse_unary_expr, |op| matches!(op, "*" | "/")),
        _ => {
            let span = Span::from_pest(pair.as_span());
            Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Unknown complex expression: {:?}", pair.as_rule())),
                span: Some(span)
            }])
        }
    }
}

fn parse_binary_expr<F, P>(
    pair: Pair<Rule>, 
    parse_next: F, 
    is_op: P
) -> Result<Expression, Vec<SpannedError>>
where
    F: Fn(Pair<Rule>) -> Result<Expression, Vec<SpannedError>>,
    P: Fn(&str) -> bool,
{
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    let mut expr = parse_next(inner.next().unwrap())?;

    while let Some(op_pair) = inner.next() {
        if let Some(right_pair) = inner.next() {
            let op_str = op_pair.as_str();
            if is_op(op_str) {
                let op = match op_str {
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
                    _ => return Err(vec![error_at(&op_pair, TransActError::ParseError(format!("Unknown operator: {}", op_str)))]),
                };
                let right = parse_next(right_pair)?;
                expr = Spanned {
                    node: ExpressionKind::BinaryOp {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    },
                    span: span.clone(),
                };
            }
        }
    }
    Ok(expr)
}

fn parse_logic_and(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, parse_equality, |op| op == "&&")
}

fn parse_equality(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, parse_comparison, |op| matches!(op, "==" | "!="))
}

fn parse_comparison(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, parse_addition, |op| matches!(op, "<" | "<=" | ">" | ">="))
}

fn parse_addition(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, parse_multiplication, |op| matches!(op, "+" | "-"))
}

fn parse_multiplication(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, parse_unary_expr, |op| matches!(op, "*" | "/"))
}

fn parse_unary_expr(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::primary => parse_expression(inner),
        _ => parse_expression(inner),
    }
}

fn parse_type(pair: Pair<Rule>) -> Result<TypeName, Vec<SpannedError>> {
    match pair.as_str() {
        "int" => Ok(TypeName::Int),
        "float" => Ok(TypeName::Float),
        "string" => Ok(TypeName::String),
        "bool" => Ok(TypeName::Bool),
        t => Err(vec![error_at(&pair, TransActError::ParseError(format!("Unknown type: {}", t)))]),
    }
}

fn parse_return_type(pair: Pair<Rule>) -> Result<ReturnType, Vec<SpannedError>> {
    if pair.as_str() == "void" {
        Ok(ReturnType::Void)
    } else {
        Ok(ReturnType::Type(parse_type(pair.into_inner().next().unwrap())?))
    }
}
