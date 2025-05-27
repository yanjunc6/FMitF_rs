use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
pub use errors::{TransActError, Results, SpannedError, format_errors};

pub mod errors;

#[derive(Parser)]
#[grammar = "frontend/parse/grammar.pest"]
pub struct TransActParser;

pub fn parse_program(source: &str) -> Results<Program> {
    let pairs = TransActParser::parse(Rule::program, source)
        .map_err(|e| vec![SpannedError { error: TransActError::ParseError(e.to_string()), span: None }])?;

    let program_pair = pairs.into_iter().next()
        .ok_or_else(|| vec![SpannedError { error: TransActError::ParseError("No program found".into()), span: None }])?;

    parse_program_rule(program_pair)
}

fn parse_program_rule(pair: Pair<Rule>) -> Results<Program> {
    let mut nodes = Vec::new();
    let mut tables = Vec::new();
    let mut functions = Vec::new();
    let mut node_map = HashMap::new();
    let mut table_map = HashMap::new();
    let mut errors = Vec::new();

    // First pass: collect nodes
    for item in pair.clone().into_inner() {
        if item.as_rule() == Rule::nodes_block {
            if let Err(mut errs) = parse_nodes_block(item, &mut nodes, &mut node_map) {
                errors.append(&mut errs);
            }
        }
    }

    // Second pass: collect tables
    for item in pair.clone().into_inner() {
        if item.as_rule() == Rule::table_declaration {
            if let Err(mut errs) = parse_table_declaration(item, &mut tables, &node_map, &mut table_map) {
                errors.append(&mut errs);
            }
        }
    }

    // Third pass: parse functions
    for item in pair.into_inner() {
        if item.as_rule() == Rule::function_declaration {
            if let Err(mut errs) = parse_function_declaration(item, &mut functions, &node_map, &table_map) {
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

fn parse_nodes_block(pair: Pair<Rule>, nodes: &mut Vec<Rc<NodeDef>>, node_map: &mut HashMap<String, Rc<NodeDef>>) -> Results<()> {
    for item in pair.into_inner() {
        if item.as_rule() == Rule::node_list {
            parse_node_list(item, nodes, node_map)?;
        }
    }
    Ok(())
}

fn parse_node_list(pair: Pair<Rule>, nodes: &mut Vec<Rc<NodeDef>>, node_map: &mut HashMap<String, Rc<NodeDef>>) -> Results<()> {
    for node_pair in pair.into_inner() {
        if node_pair.as_rule() == Rule::identifier {
            let name = parse_identifier(node_pair.clone())?;
            let span = Span::from_pest(node_pair.as_span());
            let node = Rc::new(NodeDef { name: name.clone(), span });
            nodes.push(node.clone());
            node_map.insert(name, node);
        }
    }
    Ok(())
}

fn parse_table_declaration(
    pair: Pair<Rule>, 
    tables: &mut Vec<Rc<TableDeclaration>>, 
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &mut HashMap<String, Rc<TableDeclaration>>
) -> Results<()> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let table_name = parse_identifier(inner.next().unwrap())?;
    let node_name = parse_identifier(inner.next().unwrap())?;
    
    let node = node_map.get(&node_name)
        .ok_or_else(|| vec![SpannedError { error: TransActError::UndeclaredNode(node_name), span: Some(span.clone()) }])?
        .clone();

    let mut fields = Vec::new();
    let mut primary_key = None;
    
    for field_pair in inner {
        if field_pair.as_rule() == Rule::field_declaration {
            let field = parse_field_declaration(field_pair)?;
            if field.is_primary {
                if primary_key.is_some() {
                    return Err(vec![SpannedError { 
                        error: TransActError::ParseError(format!("Table {} has multiple primary keys", table_name)), 
                        span: Some(span) 
                    }]);
                }
                primary_key = Some(Rc::new(field.clone()));
            }
            fields.push(field);
        }
    }

    let primary_key = primary_key.ok_or_else(|| vec![SpannedError { 
        error: TransActError::ParseError(format!("Table {} must have exactly one primary key", table_name)), 
        span: Some(span.clone()) 
    }])?;

    let table = Rc::new(TableDeclaration { 
        name: table_name.clone(), 
        node, 
        fields, 
        primary_key, 
        span 
    });
    
    tables.push(table.clone());
    table_map.insert(table_name, table);
    Ok(())
}

fn parse_field_declaration(pair: Pair<Rule>) -> Result<FieldDeclaration, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    // Check if first token is primary keyword
    let first = inner.next().unwrap();
    let (is_primary, field_type) = if first.as_rule() == Rule::primary_keyword {
        (true, parse_type_name(inner.next().unwrap())?)
    } else {
        (false, parse_type_name(first)?)
    };
    
    let field_name = parse_identifier(inner.next().unwrap())?;
    
    Ok(FieldDeclaration { 
        field_type, 
        field_name, 
        is_primary, 
        span 
    })
}

fn parse_function_declaration(
    pair: Pair<Rule>,
    functions: &mut Vec<FunctionDeclaration>,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>
) -> Results<()> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let return_type = parse_ret_type(inner.next().unwrap())?;
    let name = parse_identifier(inner.next().unwrap())?;
    
    let mut parameters = Vec::new();
    let mut hops = Vec::new();
    
    for item in inner {
        match item.as_rule() {
            Rule::parameter_list => {
                parameters = parse_parameter_list(item)?;
            }
            Rule::function_body_item => {
                for hop_item in item.into_inner() {
                    if hop_item.as_rule() == Rule::hop_block {
                        let hop = parse_hop_block(hop_item, node_map, table_map)?;
                        hops.push(Rc::new(hop));
                    }
                }
            }
            _ => {}
        }
    }
    
    functions.push(FunctionDeclaration { 
        return_type, 
        name, 
        parameters, 
        hops, 
        span 
    });
    Ok(())
}

fn parse_parameter_list(pair: Pair<Rule>) -> Result<Vec<ParameterDecl>, Vec<SpannedError>> {
    let mut parameters = Vec::new();
    for param_pair in pair.into_inner() {
        if param_pair.as_rule() == Rule::parameter_decl {
            parameters.push(parse_parameter_decl(param_pair)?);
        }
    }
    Ok(parameters)
}

fn parse_parameter_decl(pair: Pair<Rule>) -> Result<ParameterDecl, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let param_type = parse_type_name(inner.next().unwrap())?;
    let param_name = parse_identifier(inner.next().unwrap())?;
    
    Ok(ParameterDecl { param_type, param_name, span })
}

fn parse_hop_block(
    pair: Pair<Rule>,
    node_map: &HashMap<String, Rc<NodeDef>>,
    table_map: &HashMap<String, Rc<TableDeclaration>>
) -> Result<HopBlock, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let node_name = parse_identifier(inner.next().unwrap())?;
    let node = node_map.get(&node_name)
        .ok_or_else(|| vec![SpannedError { error: TransActError::UndeclaredNode(node_name), span: Some(span.clone()) }])?
        .clone();
    
    let mut statements = Vec::new();
    for item in inner {
        if item.as_rule() == Rule::block {
            statements = parse_block(item, table_map)?;
        }
    }
    
    Ok(HopBlock { node, statements, span })
}

fn parse_block(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<Vec<Statement>, Vec<SpannedError>> {
    let mut statements = Vec::new();
    for stmt_pair in pair.into_inner() {
        if stmt_pair.as_rule() == Rule::statement {
            statements.push(parse_statement(stmt_pair, table_map)?);
        }
    }
    Ok(statements)
}

fn parse_statement(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<Statement, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let inner = pair.into_inner().next().unwrap();
    
    let kind = match inner.as_rule() {
        Rule::var_decl_statement => {
            StatementKind::VarDecl(parse_var_decl_statement(inner)?)
        }
        Rule::var_assignment_statement => {
            StatementKind::VarAssignment(parse_var_assignment_statement(inner)?)
        }
        Rule::assignment_statement => {
            StatementKind::Assignment(parse_assignment_statement(inner, table_map)?)
        }
        Rule::if_statement => {
            StatementKind::IfStmt(parse_if_statement(inner, table_map)?)
        }
        Rule::return_statement => {
            StatementKind::Return(parse_return_statement(inner)?)
        }
        Rule::abort_statement => {
            StatementKind::Abort(parse_abort_statement(inner)?)
        }
        Rule::empty_statement => {
            StatementKind::Empty
        }
        _ => {
            return Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Unknown statement: {:?}", inner.as_rule())),
                span: Some(span)
            }]);
        }
    };
    
    Ok(Spanned { node: kind, span })
}

fn parse_var_decl_statement(pair: Pair<Rule>) -> Result<VarDeclStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    
    let (is_global, var_type, var_name, init_value) = if first.as_rule() == Rule::global_keyword {
        let var_type = parse_type_name(inner.next().unwrap())?;
        let var_name = parse_identifier(inner.next().unwrap())?;
        let init_value = parse_expression(inner.next().unwrap())?;
        (true, var_type, var_name, init_value)
    } else {
        let var_type = parse_type_name(first)?;
        let var_name = parse_identifier(inner.next().unwrap())?;
        let init_value = parse_expression(inner.next().unwrap())?;
        (false, var_type, var_name, init_value)
    };

    Ok(VarDeclStatement { var_type, var_name, init_value, is_global })
}

fn parse_var_assignment_statement(pair: Pair<Rule>) -> Result<VarAssignmentStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let var_name = parse_identifier(inner.next().unwrap())?;
    let rhs = parse_expression(inner.next().unwrap())?;
    
    Ok(VarAssignmentStatement { var_name, rhs })
}

fn parse_assignment_statement(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<AssignmentStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let table_name = parse_identifier(inner.next().unwrap())?;
    let pk_column = parse_identifier(inner.next().unwrap())?;
    let pk_expr = parse_expression(inner.next().unwrap())?;
    let field_name = parse_identifier(inner.next().unwrap())?;
    let rhs = parse_expression(inner.next().unwrap())?;

    let table = table_map.get(&table_name)
        .ok_or_else(|| vec![SpannedError { 
            error: TransActError::UndeclaredTable(table_name.clone()), 
            span: None 
        }])?
        .clone();

    // Find the primary key field
    let pk_field = table.fields.iter()
        .find(|f| f.field_name == pk_column && f.is_primary)
        .ok_or_else(|| vec![SpannedError { 
            error: TransActError::ParseError(format!("Field {} is not the primary key of table {}", pk_column, table_name)), 
            span: None 
        }])?
        .clone();

    // Find the field being assigned
    let field = table.fields.iter()
        .find(|f| f.field_name == field_name)
        .ok_or_else(|| vec![SpannedError { 
            error: TransActError::ParseError(format!("Field {} not found in table {}", field_name, table_name)), 
            span: None 
        }])?
        .clone();

    Ok(AssignmentStatement { 
        table, 
        pk_field: Rc::new(pk_field),
        pk_expr, 
        field: Rc::new(field),
        rhs 
    })
}

fn parse_if_statement(pair: Pair<Rule>, table_map: &HashMap<String, Rc<TableDeclaration>>) -> Result<IfStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let condition = parse_expression(inner.next().unwrap())?;
    
    let then_block = parse_block(inner.next().unwrap(), table_map)?;
    
    let else_branch = if let Some(else_block) = inner.next() {
        Some(parse_block(else_block, table_map)?)
    } else {
        None
    };
    
    Ok(IfStatement { condition, then_branch: then_block, else_branch })
}

fn parse_return_statement(pair: Pair<Rule>) -> Result<ReturnStatement, Vec<SpannedError>> {
    let mut inner = pair.into_inner();
    let value = if let Some(expr_pair) = inner.next() {
        Some(parse_expression(expr_pair)?)
    } else {
        None
    };
    
    Ok(ReturnStatement { value })
}

fn parse_abort_statement(_pair: Pair<Rule>) -> Result<AbortStatement, Vec<SpannedError>> {
    Ok(AbortStatement {})
}

fn parse_expression(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    match pair.as_rule() {
        Rule::expression => {
            let inner = pair.into_inner().next().unwrap();
            parse_expression(inner)
        }
        Rule::logic_or => parse_logic_or(pair),
        Rule::logic_and => parse_logic_and(pair),
        Rule::equality => parse_equality(pair),
        Rule::comparison => parse_comparison(pair),
        Rule::addition => parse_addition(pair),
        Rule::multiplication => parse_multiplication(pair),
        Rule::unary => parse_unary(pair),
        Rule::primary => parse_primary(pair),
        _ => {
            let span = Span::from_pest(pair.as_span());
            Err(vec![SpannedError {
                error: TransActError::ParseError(format!("Unknown expression rule: {:?}", pair.as_rule())),
                span: Some(span),
            }])
        }
    }
}

fn parse_logic_or(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, BinaryOp::Or)
}

fn parse_logic_and(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    parse_binary_expr(pair, BinaryOp::And)
}

fn parse_equality(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let mut left = parse_expression(inner.next().unwrap())?;
    
    while let Some(op_pair) = inner.next() {
        let right = parse_expression(inner.next().unwrap())?;
        let op = match op_pair.as_rule() {
            Rule::equality_op => {
                match op_pair.as_str() {
                    "==" => BinaryOp::Eq,
                    "!=" => BinaryOp::Neq,
                    _ => return Err(vec![SpannedError { 
                        error: TransActError::ParseError(format!("Unknown equality op: {}", op_pair.as_str())),
                        span: Some(span)
                    }]),
                }
            }
            _ => return Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Expected equality operator")),
                span: Some(span)
            }]),
        };
        
        left = Spanned {
            node: ExpressionKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span: span.clone(),
        };
    }
    
    Ok(left)
}

fn parse_comparison(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let mut left = parse_expression(inner.next().unwrap())?;
    
    while let Some(op_pair) = inner.next() {
        let right = parse_expression(inner.next().unwrap())?;
        let op = match op_pair.as_rule() {
            Rule::comparison_op => {
                match op_pair.as_str() {
                    "<" => BinaryOp::Lt,
                    "<=" => BinaryOp::Lte,
                    ">" => BinaryOp::Gt,
                    ">=" => BinaryOp::Gte,
                    _ => return Err(vec![SpannedError { 
                        error: TransActError::ParseError(format!("Unknown comparison op: {}", op_pair.as_str())),
                        span: Some(span)
                    }]),
                }
            }
            _ => return Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Expected comparison operator")),
                span: Some(span)
            }]),
        };
        
        left = Spanned {
            node: ExpressionKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span: span.clone(),
        };
    }
    
    Ok(left)
}

fn parse_addition(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let mut left = parse_expression(inner.next().unwrap())?;
    
    while let Some(op_pair) = inner.next() {
        let right = parse_expression(inner.next().unwrap())?;
        let op = match op_pair.as_rule() {
            Rule::addition_op => {
                match op_pair.as_str() {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    _ => return Err(vec![SpannedError { 
                        error: TransActError::ParseError(format!("Unknown addition op: {}", op_pair.as_str())),
                        span: Some(span)
                    }]),
                }
            }
            _ => return Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Expected addition operator")),
                span: Some(span)
            }]),
        };
        
        left = Spanned {
            node: ExpressionKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span: span.clone(),
        };
    }
    
    Ok(left)
}

fn parse_multiplication(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let mut left = parse_expression(inner.next().unwrap())?;
    
    while let Some(op_pair) = inner.next() {
        let right = parse_expression(inner.next().unwrap())?;
        let op = match op_pair.as_rule() {
            Rule::multiplication_op => {
                match op_pair.as_str() {
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    _ => return Err(vec![SpannedError { 
                        error: TransActError::ParseError(format!("Unknown multiplication op: {}", op_pair.as_str())),
                        span: Some(span)
                    }]),
                }
            }
            _ => return Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Expected multiplication operator")),
                span: Some(span)
            }]),
        };
        
        left = Spanned {
            node: ExpressionKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span: span.clone(),
        };
    }
    
    Ok(left)
}

fn parse_unary(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    
    if first.as_rule() == Rule::unary_op {
        let op_str = first.as_str();
        let operand = parse_expression(inner.next().unwrap())?;
        let op = match op_str {
            "!" => UnaryOp::Not,
            "-" => UnaryOp::Neg,
            _ => return Err(vec![SpannedError { 
                error: TransActError::ParseError(format!("Unknown unary op: {}", op_str)),
                span: Some(span)
            }]),
        };
        Ok(Spanned {
            node: ExpressionKind::UnaryOp { op, expr: Box::new(operand) },
            span,
        })
    } else {
        parse_expression(first)
    }
}

fn parse_primary(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::bool_literal => parse_bool_literal(inner),
        Rule::table_field_access => parse_table_field_access(inner),
        Rule::float_literal => parse_float_literal(inner),
        Rule::integer_literal => parse_integer_literal(inner),
        Rule::string_literal => parse_string_literal(inner),
        Rule::identifier => parse_identifier_expr(inner),
        Rule::expression => parse_expression(inner),
        _ => {
            Err(vec![SpannedError {
                error: TransActError::ParseError(format!("Unknown primary expression: {:?}", inner.as_rule())),
                span: Some(span),
            }])
        }
    }
}

fn parse_bool_literal(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let value = pair.as_str() == "true";
    Ok(Spanned {
        node: ExpressionKind::BoolLit(value),
        span,
    })
}

fn parse_table_field_access(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let table_name = parse_identifier(inner.next().unwrap())?;
    let pk_column = parse_identifier(inner.next().unwrap())?;
    let pk_expr = Box::new(parse_expression(inner.next().unwrap())?);
    let field_name = parse_identifier(inner.next().unwrap())?;
    
    // Create dummy references - will be resolved in semantic analysis
    let dummy_table = Rc::new(TableDeclaration {
        name: table_name.clone(),
        node: Rc::new(NodeDef { name: "unknown".to_string(), span: Span::default() }),
        fields: Vec::new(),
        primary_key: Rc::new(FieldDeclaration {
            field_type: TypeName::Int,
            field_name: "dummy".to_string(),
            is_primary: true,
            span: Span::default(),
        }),
        span: Span::default(),
    });
    
    let dummy_pk_field = Rc::new(FieldDeclaration {
        field_type: TypeName::Int,
        field_name: pk_column,
        is_primary: true,
        span: Span::default(),
    });
    
    let dummy_field = Rc::new(FieldDeclaration {
        field_type: TypeName::Int,
        field_name: field_name,
        is_primary: false,
        span: Span::default(),
    });
    
    Ok(Spanned {
        node: ExpressionKind::TableFieldAccess {
            table: dummy_table,
            pk_field: dummy_pk_field,
            pk_expr,
            field: dummy_field,
        },
        span,
    })
}

fn parse_float_literal(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let value = pair.as_str().parse().map_err(|_| vec![SpannedError {
        error: TransActError::ParseError(format!("Invalid float: {}", pair.as_str())),
        span: Some(span.clone()),
    }])?;
    Ok(Spanned {
        node: ExpressionKind::FloatLit(value),
        span,
    })
}

fn parse_integer_literal(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let value = pair.as_str().parse().map_err(|_| vec![SpannedError {
        error: TransActError::ParseError(format!("Invalid integer: {}", pair.as_str())),
        span: Some(span.clone()),
    }])?;
    Ok(Spanned {
        node: ExpressionKind::IntLit(value),
        span,
    })
}

fn parse_string_literal(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let s = pair.as_str();
    let content = if s.len() >= 2 {
        s[1..s.len()-1].to_string()
    } else {
        String::new()
    };
    Ok(Spanned {
        node: ExpressionKind::StringLit(content),
        span,
    })
}

fn parse_identifier_expr(pair: Pair<Rule>) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let name = parse_identifier(pair)?;
    Ok(Spanned {
        node: ExpressionKind::Ident(name),
        span,
    })
}

// Helper functions for basic types

fn parse_identifier(pair: Pair<Rule>) -> Result<String, Vec<SpannedError>> {
    Ok(pair.as_str().to_string())
}

fn parse_type_name(pair: Pair<Rule>) -> Result<TypeName, Vec<SpannedError>> {
    match pair.as_str() {
        "int" => Ok(TypeName::Int),
        "float" => Ok(TypeName::Float),
        "string" => Ok(TypeName::String),
        "bool" => Ok(TypeName::Bool),
        _ => Err(vec![SpannedError { 
            error: TransActError::ParseError(format!("Unknown type: {}", pair.as_str())), 
            span: Some(Span::from_pest(pair.as_span())) 
        }])
    }
}

fn parse_ret_type(pair: Pair<Rule>) -> Result<ReturnType, Vec<SpannedError>> {
    match pair.as_str() {
        "void" => Ok(ReturnType::Void),
        _ => Ok(ReturnType::Type(parse_type_name(pair)?)),
    }
}

// Helper function for binary expressions with simple operators
fn parse_binary_expr(pair: Pair<Rule>, default_op: BinaryOp) -> Result<Expression, Vec<SpannedError>> {
    let span = Span::from_pest(pair.as_span());
    let mut inner = pair.into_inner();
    
    let mut left = parse_expression(inner.next().unwrap())?;
    
    while inner.peek().is_some() {
        let _op_pair = inner.next().unwrap(); // Skip operator for now, use default
        let right = parse_expression(inner.next().unwrap())?;
        
        left = Spanned {
            node: ExpressionKind::BinaryOp {
                left: Box::new(left),
                op: default_op.clone(),
                right: Box::new(right),
            },
            span: span.clone(),
        };
    }
    
    Ok(left)
}
