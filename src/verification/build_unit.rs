use std::rc::Rc;
use std::collections::HashSet;
use crate::ast::*;
use crate::graph::*;
use crate::verification::VerificationUnit;

pub fn build_verification_unit(
    program: &Program,
    scgraph: &SCGraph,
    edge: &Edge,
) -> Option<VerificationUnit> {
    // Get the hops involved in this C edge
    let hop1 = &scgraph.hops[edge.v1];
    let hop2 = &scgraph.hops[edge.v2];

    // Check if they are on the same node (conflict condition)
    if !Rc::ptr_eq(&hop1.node, &hop2.node) {
        return None; // Not a conflict
    }

    // Find the functions containing these hops
    let mut func1_opt = None;
    let mut func2_opt = None;

    for function in &program.functions {
        for (hop_idx, hop) in function.hops.iter().enumerate() {
            if Rc::ptr_eq(hop, &scgraph.hops[edge.v1]) {
                func1_opt = Some((function.clone(), hop_idx));
            }
            if Rc::ptr_eq(hop, &scgraph.hops[edge.v2]) {
                func2_opt = Some((function.clone(), hop_idx));
            }
        }
    }

    let (func1, idx1) = func1_opt?;
    let (func2, idx2) = func2_opt?;

    // Get relevant hops up to and including the conflict
    let hops1: Vec<Rc<HopBlock>> = func1.hops.iter()
        .take(idx1 + 1)
        .cloned()
        .collect();
    
    let hops2: Vec<Rc<HopBlock>> = func2.hops.iter()
        .take(idx2 + 1)
        .cloned()
        .collect();

    // Collect relevant tables
    let mut relevant_tables = HashSet::new();
    
    for hop in &hops1 {
        collect_tables_in_hop(hop, &mut relevant_tables);
    }
    
    for hop in &hops2 {
        collect_tables_in_hop(hop, &mut relevant_tables);
    }

    Some(VerificationUnit {
        node: hop1.node.clone(),
        func1,
        hops1,
        func2,
        hops2,
        idx1,
        idx2,
        relevant_tables,
    })
}

fn collect_tables_in_hop(hop: &HopBlock, set: &mut HashSet<Rc<TableDeclaration>>) {
    for stmt in &hop.statements {
        collect_tables_in_statement(stmt, set);
    }
}

fn collect_tables_in_statement(stmt: &Statement, set: &mut HashSet<Rc<TableDeclaration>>) {
    match &stmt.node {
        StatementKind::Assignment(a) => {
            set.insert(a.table.clone());
        }
        StatementKind::VarAssignment(var_assign) => {
            collect_tables_in_expression(&var_assign.rhs, set);
        }
        StatementKind::IfStmt(ifstmt) => {
            collect_tables_in_expression(&ifstmt.condition, set);
            for stmt in &ifstmt.then_branch {
                collect_tables_in_statement(stmt, set);
            }
            if let Some(else_branch) = &ifstmt.else_branch {
                for stmt in else_branch {
                    collect_tables_in_statement(stmt, set);
                }
            }
        }
        StatementKind::VarDecl(var_decl) => {
            collect_tables_in_expression(&var_decl.init_value, set);
        }
        StatementKind::Return(ret_stmt) => {
            if let Some(expr) = &ret_stmt.value {
                collect_tables_in_expression(expr, set);
            }
        }
        StatementKind::Abort(_) => {}
        StatementKind::Empty => {}
    }
}

fn collect_tables_in_expression(expr: &Expression, set: &mut HashSet<Rc<TableDeclaration>>) {
    match &expr.node {
        ExpressionKind::TableFieldAccess { table, pk_expr, .. } => {
            set.insert(table.clone());
            collect_tables_in_expression(pk_expr, set);
        }
        ExpressionKind::UnaryOp { expr, .. } => {
            collect_tables_in_expression(expr, set);
        }
        ExpressionKind::BinaryOp { left, right, .. } => {
            collect_tables_in_expression(left, set);
            collect_tables_in_expression(right, set);
        }
        _ => {} // Literals and identifiers don't contain tables
    }
}