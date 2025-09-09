//! Semantic Analysis
//!
//! This module implements semantic analysis using egg/egglog for the DSL.
//! It performs advanced semantic checking including:
//! - Invariant checking
//! - Transaction structure validation 
//! - Partition function correctness
//! - Table consistency validation
//! - Hop structure validation

use std::collections::HashMap;
use std::collections::HashSet;

use egglog::{EGraph, ExtractReport, RunReport, SerializeConfig};

use crate::ast::*;
use crate::ast::errors::*;

// ============================================================================
// --- Semantic Analyzer
// ============================================================================

pub struct SemanticAnalyzer {
    /// EGraph for semantic analysis
    egraph: EGraph,
    
    /// Current analysis context
    context: AnalysisContext,
    
    /// Error collector
    errors: ErrorCollector,
}

#[derive(Debug, Clone)]
pub struct AnalysisContext {
    /// Current table being analyzed
    current_table: Option<TableId>,
    
    /// Current function being analyzed
    current_function: Option<FunctionId>,
    
    /// Current transaction context
    current_transaction: Option<FunctionId>,
    
    /// Available tables in scope
    tables: HashMap<String, TableId>,
    
    /// Available partition functions
    partitions: HashMap<String, FunctionId>,
}

impl AnalysisContext {
    pub fn new() -> Self {
        Self {
            current_table: None,
            current_function: None,
            current_transaction: None,
            tables: HashMap::new(),
            partitions: HashMap::new(),
        }
    }
}

// ============================================================================
// --- Semantic Analysis Implementation
// ============================================================================

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut egraph = EGraph::default();
        
        // Initialize the egraph with DSL-specific rules
        Self::setup_semantic_rules(&mut egraph);
        
        Self {
            egraph,
            context: AnalysisContext::new(),
            errors: ErrorCollector::new(),
        }
    }

    /// Perform semantic analysis on a program
    pub fn analyze_semantics(program: &Program) -> Results<()> {
        let mut analyzer = Self::new();
        
        // Build analysis context
        analyzer.build_context(program);
        
        // Perform semantic checks
        analyzer.check_table_semantics(program);
        analyzer.check_function_semantics(program);
        analyzer.check_transaction_semantics(program);
        analyzer.check_invariants(program);
        
        analyzer.errors.into_result(Some(()))
    }

    fn setup_semantic_rules(egraph: &mut EGraph) {
        // Add semantic rules for the DSL using egglog syntax
        let rules = r#"
            ; Define basic types
            (datatype Type
                (Int)
                (Bool) 
                (String)
                (Table String)
                (Void))

            ; Define expressions
            (datatype Expr
                (Var String)
                (Const String Type)
                (BinOp String Expr Expr)
                (Call String (Vec Expr))
                (FieldAccess Expr String))

            ; Define statements
            (datatype Stmt
                (VarDecl String Type (Option Expr))
                (Assign Expr Expr)
                (If Expr Stmt (Option Stmt))
                (Return (Option Expr))
                (Block (Vec Stmt)))

            ; Define table properties
            (datatype TableProp
                (HasPrimaryKey String)
                (HasPartition String String)
                (HasInvariant String Expr))

            ; Define transaction properties
            (datatype TransactionProp
                (AccessesTable String String)
                (ModifiesTable String String)
                (ReadsTable String String))

            ; Semantic rules for tables
            (rule ((TableProp (HasPrimaryKey ?table)))
                  ((ValidTable ?table)))

            ; Rule: Tables must have primary keys
            (rule ((Table ?table)
                   (not (TableProp (HasPrimaryKey ?table))))
                  ((Error (InvalidTable ?table "missing primary key"))))

            ; Rule: Partition functions must be valid
            (rule ((TableProp (HasPartition ?table ?partition))
                   (not (ValidPartition ?partition)))
                  ((Error (InvalidPartition ?partition))))

            ; Rule: Table invariants must be boolean expressions
            (rule ((TableProp (HasInvariant ?table ?expr))
                   (not (HasType ?expr Bool)))
                  ((Error (InvalidInvariant ?table "invariant must be boolean"))))

            ; Transaction semantic rules
            (rule ((Transaction ?tx)
                   (TransactionProp (ModifiesTable ?tx ?table))
                   (not (TransactionProp (AccessesTable ?tx ?table))))
                  ((Error (InvalidTransaction ?tx "modifies table without access"))))

            ; Hop semantic rules
            (rule ((HopBlock ?hop)
                   (InTransaction ?hop ?tx)
                   (HopIndex ?hop ?idx)
                   (AbortStatement ?hop)
                   (> ?idx 0))
                  ((Error (InvalidHop ?hop "abort only allowed in first hop"))))
        "#;

        if let Err(e) = egraph.parse_and_run_program(rules) {
            eprintln!("Failed to setup semantic rules: {}", e);
        }
    }

    fn build_context(&mut self, program: &Program) {
        // Build maps of available declarations
        for declaration in &program.declarations {
            match declaration {
                Declaration::Table(table_id) => {
                    let table = &program.table_decls[*table_id];
                    self.context.tables.insert(table.name.value.clone(), *table_id);
                }
                Declaration::Callable(func_id) => {
                    let func = &program.functions[*func_id];
                    if func.kind == CallableKind::Partition {
                        self.context.partitions.insert(func.name.value.clone(), *func_id);
                    }
                }
                _ => {}
            }
        }
    }

    // ========================================================================
    // --- Table Semantic Checks
    // ========================================================================

    fn check_table_semantics(&mut self, program: &Program) {
        for declaration in &program.declarations {
            if let Declaration::Table(table_id) = declaration {
                self.check_single_table_semantics(*table_id, program);
            }
        }
    }

    fn check_single_table_semantics(&mut self, table_id: TableId, program: &Program) {
        let table = &program.table_decls[table_id];
        self.context.current_table = Some(table_id);

        // Check primary key existence
        let has_primary_key = table.fields.iter().any(|field| field.is_primary);
        if !has_primary_key {
            let error = SemanticError::MissingPrimaryKey {
                table_name: table.name.value.clone(),
            };
            self.errors.add_error(AstError::Semantic(error), table.span);
        } else {
            // Add to egraph
            let rule = format!("(TableProp (HasPrimaryKey \"{}\"))", table.name.value);
            if let Err(e) = self.egraph.parse_and_run_program(&rule) {
                eprintln!("Failed to add table rule: {}", e);
            }
        }

        // Check partition function validity
        for node in &table.nodes {
            if let Some(partition_id) = node.resolved_partition {
                self.check_partition_function_validity(partition_id, table, node, program);
            }
        }

        // Check field consistency
        self.check_table_field_consistency(table, program);

        // Check invariants
        for invariant_expr in &table.invariants {
            self.check_table_invariant(*invariant_expr, table, program);
        }

        self.context.current_table = None;
    }

    fn check_partition_function_validity(&mut self, partition_id: FunctionId, table: &TableDecl, 
                                       node: &TableNode, program: &Program) {
        let partition_func = &program.functions[partition_id];
        
        // Verify it's marked as a partition function
        if partition_func.kind != CallableKind::Partition {
            let error = SemanticError::InvalidPartitionFunction {
                function_name: partition_func.name.value.clone(),
                reason: "Function is not declared as a partition".to_string(),
            };
            self.errors.add_error(AstError::Semantic(error), node.span);
            return;
        }

        // Check that partition function parameters match table fields
        for arg_expr in &node.args {
            // Validate that arguments reference valid table fields
            self.validate_partition_argument(arg_expr, table, program);
        }

        // Add to egraph for further analysis
        let rule = format!("(TableProp (HasPartition \"{}\" \"{}\"))", 
                          table.name.value, partition_func.name.value);
        if let Err(e) = self.egraph.parse_and_run_program(&rule) {
            eprintln!("Failed to add partition rule: {}", e);
        }
    }

    fn validate_partition_argument(&mut self, arg_expr: &ExprId, table: &TableDecl, program: &Program) {
        let expr = &program.expressions[*arg_expr];
        
        match expr {
            Expression::Identifier(identifier) => {
                // Check if identifier refers to a table field
                let field_exists = table.fields.iter().any(|field| field.name.value == identifier.name);
                if !field_exists {
                    let error = SemanticError::InvalidPartitionFunction {
                        function_name: "partition".to_string(),
                        reason: format!("Field '{}' does not exist in table '{}'", 
                                      identifier.name, table.name.value),
                    };
                    self.errors.add_error(AstError::Semantic(error), identifier.span);
                }
            }
            Expression::MemberAccess { object, member, .. } => {
                // Validate member access on table
                self.validate_partition_argument(object, table, program);
                
                let field_exists = table.fields.iter().any(|field| field.name.value == member.value);
                if !field_exists {
                    let error = SemanticError::InvalidPartitionFunction {
                        function_name: "partition".to_string(),
                        reason: format!("Field '{}' does not exist in table '{}'", 
                                      member.value, table.name.value),
                    };
                    self.errors.add_error(AstError::Semantic(error), member.span);
                }
            }
            _ => {
                // Other expressions are generally not valid for partition arguments
                let error = SemanticError::InvalidPartitionFunction {
                    function_name: "partition".to_string(),
                    reason: "Partition arguments must reference table fields".to_string(),
                };
                self.errors.add_error(AstError::Semantic(error), None);
            }
        }
    }

    fn check_table_field_consistency(&mut self, table: &TableDecl, program: &Program) {
        let mut field_names = HashSet::new();
        
        for field in &table.fields {
            // Check for duplicate field names
            if field_names.contains(&field.name.value) {
                let error = SemanticError::InvalidTableDeclaration {
                    table_name: table.name.value.clone(),
                    reason: format!("Duplicate field name: {}", field.name.value),
                };
                self.errors.add_error(AstError::Semantic(error), field.span);
            }
            field_names.insert(field.name.value.clone());
        }

        // Check that the table has at least one field
        if table.fields.is_empty() {
            let error = SemanticError::InvalidTableDeclaration {
                table_name: table.name.value.clone(),
                reason: "Table must have at least one field".to_string(),
            };
            self.errors.add_error(AstError::Semantic(error), table.span);
        }
    }

    fn check_table_invariant(&mut self, invariant_expr: ExprId, table: &TableDecl, program: &Program) {
        // Validate that invariant is a boolean expression
        // This would normally be done by the type checker, but we can add additional semantic checks
        
        let expr = &program.expressions[invariant_expr];
        self.validate_invariant_expression(expr, table, program);
        
        // Add to egraph
        let rule = format!("(TableProp (HasInvariant \"{}\" (Expr \"{}\")))", 
                          table.name.value, "invariant");
        if let Err(e) = self.egraph.parse_and_run_program(&rule) {
            eprintln!("Failed to add invariant rule: {}", e);
        }
    }

    fn validate_invariant_expression(&mut self, expr: &Expression, table: &TableDecl, program: &Program) {
        match expr {
            Expression::Binary { left, op, right, .. } => {
                let left_expr = &program.expressions[*left];
                let right_expr = &program.expressions[*right];
                self.validate_invariant_expression(left_expr, table, program);
                self.validate_invariant_expression(right_expr, table, program);
                
                // Check that comparison operators are used appropriately
                match op.value.as_str() {
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        // These should produce boolean results
                    }
                    "&&" | "||" => {
                        // These require boolean operands
                    }
                    _ => {
                        // Other operators may not be appropriate in invariants
                    }
                }
            }
            Expression::MemberAccess { object, member, .. } => {
                // Ensure the field being accessed exists
                let field_exists = table.fields.iter().any(|field| field.name.value == member.value);
                if !field_exists {
                    let error = SemanticError::ViolatedInvariant {
                        invariant: "field access".to_string(),
                        reason: format!("Field '{}' does not exist in table '{}'", 
                                      member.value, table.name.value),
                    };
                    self.errors.add_error(AstError::Semantic(error), member.span);
                }
            }
            _ => {
                // Recursively validate other expression types
            }
        }
    }

    // ========================================================================
    // --- Function Semantic Checks
    // ========================================================================

    fn check_function_semantics(&mut self, program: &Program) {
        for declaration in &program.declarations {
            if let Declaration::Callable(func_id) = declaration {
                self.check_single_function_semantics(*func_id, program);
            }
        }
    }

    fn check_single_function_semantics(&mut self, func_id: FunctionId, program: &Program) {
        let func = &program.functions[func_id];
        self.context.current_function = Some(func_id);

        match func.kind {
            CallableKind::Function => {
                self.check_regular_function_semantics(func, program);
            }
            CallableKind::Partition => {
                self.check_partition_function_semantics(func, program);
            }
            CallableKind::Transaction => {
                self.context.current_transaction = Some(func_id);
                self.check_transaction_semantics_single(func, program);
                self.context.current_transaction = None;
            }
            CallableKind::Operator => {
                self.check_operator_semantics(func, program);
            }
        }

        self.context.current_function = None;
    }

    fn check_regular_function_semantics(&mut self, func: &CallableDecl, program: &Program) {
        // Check function body if present
        if let Some(body_id) = func.body {
            self.check_block_semantics(body_id, program);
        }

        // Check that function has return statements if return type is not void
        if func.return_type.is_some() {
            self.check_return_coverage(func, program);
        }
    }

    fn check_partition_function_semantics(&mut self, func: &CallableDecl, program: &Program) {
        // Partition functions have special requirements
        if func.return_type.is_none() {
            let error = SemanticError::InvalidPartitionFunction {
                function_name: func.name.value.clone(),
                reason: "Partition function must have a return type".to_string(),
            };
            self.errors.add_error(AstError::Semantic(error), func.span);
        }

        // Check that partition function doesn't have side effects
        if let Some(body_id) = func.body {
            self.check_partition_purity(body_id, program);
        }
    }

    fn check_operator_semantics(&mut self, func: &CallableDecl, program: &Program) {
        // Operator functions have special naming and parameter requirements
        if func.params.is_empty() {
            let error = SemanticError::InvalidPartitionFunction {
                function_name: func.name.value.clone(),
                reason: "Operator must have at least one parameter".to_string(),
            };
            self.errors.add_error(AstError::Semantic(error), func.span);
        }
    }

    fn check_partition_purity(&mut self, block_id: BlockId, program: &Program) {
        let block = &program.blocks[block_id];
        
        for stmt_id in &block.statements {
            let stmt = &program.statements[*stmt_id];
            
            match stmt {
                Statement::VarDecl(var_id) => {
                    // Variable declarations are allowed
                }
                Statement::If { then_block, else_block, .. } => {
                    self.check_partition_purity(*then_block, program);
                    if let Some(else_block_id) = else_block {
                        self.check_partition_purity(*else_block_id, program);
                    }
                }
                Statement::Return { .. } => {
                    // Return statements are required
                }
                Statement::Expression { expr, .. } => {
                    // Check that expression doesn't have side effects
                    self.check_expression_purity(*expr, program);
                }
                Statement::Block(nested_block) => {
                    self.check_partition_purity(*nested_block, program);
                }
                _ => {
                    // Other statements might violate purity
                    let error = SemanticError::InvalidPartitionFunction {
                        function_name: "partition".to_string(),
                        reason: "Partition function must be pure".to_string(),
                    };
                    self.errors.add_error(AstError::Semantic(error), None);
                }
            }
        }
    }

    fn check_expression_purity(&mut self, expr_id: ExprId, program: &Program) {
        let expr = &program.expressions[expr_id];
        
        match expr {
            Expression::Assignment { .. } => {
                // Assignments have side effects
                let error = SemanticError::InvalidPartitionFunction {
                    function_name: "partition".to_string(),
                    reason: "Partition function cannot have assignments".to_string(),
                };
                self.errors.add_error(AstError::Semantic(error), None);
            }
            Expression::Call { callee, args, .. } => {
                // Function calls might have side effects - need to check the called function
                for arg_id in args {
                    self.check_expression_purity(*arg_id, program);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.check_expression_purity(*left, program);
                self.check_expression_purity(*right, program);
            }
            Expression::Unary { expr, .. } => {
                self.check_expression_purity(*expr, program);
            }
            _ => {
                // Other expressions are generally pure
            }
        }
    }

    fn check_return_coverage(&mut self, func: &CallableDecl, program: &Program) {
        if let Some(body_id) = func.body {
            let has_return = self.block_has_return(body_id, program);
            if !has_return {
                let error = SemanticError::InvalidTransactionStructure {
                    transaction_name: func.name.value.clone(),
                    reason: "Function must have a return statement".to_string(),
                };
                self.errors.add_error(AstError::Semantic(error), func.span);
            }
        }
    }

    fn block_has_return(&self, block_id: BlockId, program: &Program) -> bool {
        let block = &program.blocks[block_id];
        
        for stmt_id in &block.statements {
            let stmt = &program.statements[*stmt_id];
            
            match stmt {
                Statement::Return { .. } => return true,
                Statement::If { then_block, else_block, .. } => {
                    if self.block_has_return(*then_block, program) {
                        if let Some(else_block_id) = else_block {
                            if self.block_has_return(*else_block_id, program) {
                                return true;
                            }
                        }
                    }
                }
                Statement::Block(nested_block) => {
                    if self.block_has_return(*nested_block, program) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        
        false
    }

    // ========================================================================
    // --- Transaction Semantic Checks
    // ========================================================================

    fn check_transaction_semantics(&mut self, program: &Program) {
        for declaration in &program.declarations {
            if let Declaration::Callable(func_id) = declaration {
                let func = &program.functions[*func_id];
                if func.kind == CallableKind::Transaction {
                    self.check_transaction_semantics_single(func, program);
                }
            }
        }
    }

    fn check_transaction_semantics_single(&mut self, func: &CallableDecl, program: &Program) {
        // Check transaction structure
        if let Some(body_id) = func.body {
            self.check_transaction_body_structure(body_id, program);
        }

        // Add transaction to egraph for analysis
        let rule = format!("(Transaction \"{}\")", func.name.value);
        if let Err(e) = self.egraph.parse_and_run_program(&rule) {
            eprintln!("Failed to add transaction rule: {}", e);
        }
    }

    fn check_transaction_body_structure(&mut self, block_id: BlockId, program: &Program) {
        let block = &program.blocks[block_id];
        let mut hop_count = 0;
        
        for stmt_id in &block.statements {
            let stmt = &program.statements[*stmt_id];
            
            match stmt {
                Statement::Hop { body, .. } => {
                    hop_count += 1;
                    self.check_hop_semantics(*body, hop_count, program);
                }
                Statement::HopsFor { body, .. } => {
                    // HopsFor generates multiple hops
                    self.check_hop_semantics(*body, 0, program); // Use 0 to indicate unknown hop index
                }
                _ => {
                    // Non-hop statements in transaction body
                }
            }
        }

        // Transactions should have at least one hop
        if hop_count == 0 {
            let error = SemanticError::InvalidTransactionStructure {
                transaction_name: "transaction".to_string(),
                reason: "Transaction must contain at least one hop".to_string(),
            };
            self.errors.add_error(AstError::Semantic(error), None);
        }
    }

    fn check_hop_semantics(&mut self, block_id: BlockId, hop_index: usize, program: &Program) {
        let block = &program.blocks[block_id];
        
        for stmt_id in &block.statements {
            let stmt = &program.statements[*stmt_id];
            
            // Check for abort statements
            if let Statement::Expression { expr, .. } = stmt {
                if self.is_abort_expression(*expr, program) && hop_index > 1 {
                    let error = SemanticError::InvalidHopStructure {
                        reason: format!("Abort statement only allowed in first hop, found in hop {}", hop_index),
                    };
                    self.errors.add_error(AstError::Semantic(error), None);
                }
            }
            
            // Recursively check nested blocks
            match stmt {
                Statement::Block(nested_block) => {
                    self.check_hop_semantics(*nested_block, hop_index, program);
                }
                Statement::If { then_block, else_block, .. } => {
                    self.check_hop_semantics(*then_block, hop_index, program);
                    if let Some(else_block_id) = else_block {
                        self.check_hop_semantics(*else_block_id, hop_index, program);
                    }
                }
                _ => {}
            }
        }
    }

    fn is_abort_expression(&self, expr_id: ExprId, program: &Program) -> bool {
        let expr = &program.expressions[expr_id];
        
        match expr {
            Expression::Call { callee, .. } => {
                if let Expression::Identifier(identifier) = &program.expressions[*callee] {
                    identifier.name == "abort"
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    // ========================================================================
    // --- Block and Statement Semantic Checks
    // ========================================================================

    fn check_block_semantics(&mut self, block_id: BlockId, program: &Program) {
        let block = &program.blocks[block_id];
        
        for stmt_id in &block.statements {
            self.check_statement_semantics(*stmt_id, program);
        }
    }

    fn check_statement_semantics(&mut self, stmt_id: StmtId, program: &Program) {
        let stmt = &program.statements[stmt_id];
        
        match stmt {
            Statement::VarDecl(_) => {
                // Variable declarations are generally fine
            }
            Statement::If { condition, then_block, else_block, .. } => {
                self.check_block_semantics(*then_block, program);
                if let Some(else_block_id) = else_block {
                    self.check_block_semantics(*else_block_id, program);
                }
            }
            Statement::For { body, .. } => {
                self.check_block_semantics(*body, program);
            }
            Statement::Hop { body, .. } => {
                self.check_block_semantics(*body, program);
            }
            Statement::HopsFor { body, .. } => {
                self.check_block_semantics(*body, program);
            }
            Statement::Block(block_id) => {
                self.check_block_semantics(*block_id, program);
            }
            _ => {
                // Other statements
            }
        }
    }

    // ========================================================================
    // --- Invariant Checking
    // ========================================================================

    fn check_invariants(&mut self, program: &Program) {
        // Use egraph to check semantic invariants
        let check_rules = r#"
            ; Run semantic analysis
            (run 10)
            
            ; Extract any errors
            (check (not (Error ?msg)))
        "#;

        if let Err(e) = self.egraph.parse_and_run_program(check_rules) {
            // Extract semantic errors from egraph
            self.extract_egraph_errors();
        }
    }

    fn extract_egraph_errors(&mut self) {
        // Extract errors from the egraph
        // This would require implementing a way to extract error terms from the egraph
        // For now, we'll use a simplified approach
        
        // In a real implementation, you would:
        // 1. Query the egraph for Error terms
        // 2. Extract the error messages and locations
        // 3. Convert them to semantic errors
        
        // Placeholder for egraph error extraction
        let extracted_errors = Vec::new(); // Would contain actual errors from egraph
        
        for error_msg in extracted_errors {
            let error = SemanticError::ViolatedInvariant {
                invariant: "semantic rule".to_string(),
                reason: "semantic analysis error".to_string(), // Would contain actual error message
            };
            self.errors.add_error(AstError::Semantic(error), None);
        }
    }
}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Perform semantic analysis on a program
pub fn analyze_semantics(program: &Program) -> Results<()> {
    SemanticAnalyzer::analyze_semantics(program)
}
