//! Constant Expression Evaluation and Checking
//!
//! This module implements constant expression evaluation and compile-time checking.
//! It performs:
//! - Constant expression evaluation
//! - Compile-time arithmetic
//! - Constant folding optimizations
//! - Circular dependency detection in constants
//! - Range and overflow checking

use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::*;
use crate::ast::errors::*;

// ============================================================================
// --- Constant Values
// ============================================================================

/// Represents a compile-time constant value
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    List(Vec<ConstantValue>),
    Row(HashMap<String, ConstantValue>),
}

impl ConstantValue {
    /// Convert a constant value to a string representation
    pub fn to_string(&self) -> String {
        match self {
            ConstantValue::Integer(i) => i.to_string(),
            ConstantValue::Float(f) => f.to_string(),
            ConstantValue::String(s) => format!("\"{}\"", s),
            ConstantValue::Bool(b) => b.to_string(),
            ConstantValue::List(elements) => {
                let element_strs: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                format!("[{}]", element_strs.join(", "))
            }
            ConstantValue::Row(fields) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.to_string()))
                    .collect();
                format!("{{{}}}", field_strs.join(", "))
            }
        }
    }

    /// Get the type name of this constant value
    pub fn type_name(&self) -> &'static str {
        match self {
            ConstantValue::Integer(_) => "int",
            ConstantValue::Float(_) => "float",
            ConstantValue::String(_) => "string",
            ConstantValue::Bool(_) => "bool",
            ConstantValue::List(_) => "list",
            ConstantValue::Row(_) => "row",
        }
    }
}

// ============================================================================
// --- Constant Checker
// ============================================================================

pub struct ConstantChecker {
    /// Cache of evaluated constants
    constant_cache: HashMap<ConstId, ConstantValue>,
    
    /// Set of constants currently being evaluated (for cycle detection)
    evaluating: HashSet<ConstId>,
    
    /// Error collector
    errors: ErrorCollector,
}

/// Context for constant evaluation
#[derive(Debug, Clone)]
pub struct ConstantContext {
    /// Available constant bindings
    pub constants: HashMap<String, ConstantValue>,
    
    /// Whether we allow non-constant expressions
    pub allow_non_constant: bool,
}

impl ConstantContext {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            allow_non_constant: false,
        }
    }

    pub fn with_constants(constants: HashMap<String, ConstantValue>) -> Self {
        Self {
            constants,
            allow_non_constant: false,
        }
    }
}

// ============================================================================
// --- Constant Checker Implementation
// ============================================================================

impl ConstantChecker {
    pub fn new() -> Self {
        Self {
            constant_cache: HashMap::new(),
            evaluating: HashSet::new(),
            errors: ErrorCollector::new(),
        }
    }

    /// Check and evaluate all constants in a program
    pub fn check_constants(program: &Program) -> Results<HashMap<ConstId, ConstantValue>> {
        let mut checker = Self::new();
        
        // Evaluate all constant declarations
        for declaration in &program.declarations {
            if let Declaration::Const(const_id) = declaration {
                checker.evaluate_constant(*const_id, program);
            }
        }
        
        checker.errors.into_result(Some(checker.constant_cache))
    }

    /// Evaluate a single constant expression
    pub fn evaluate_constant_expression(expr_id: ExprId, program: &Program, 
                                       context: &ConstantContext) -> Results<ConstantValue> {
        let mut checker = Self::new();
        
        // Add context constants to our cache
        for (name, value) in &context.constants {
            // Would need to map names to const IDs
        }
        
        if let Some(value) = checker.evaluate_expression(expr_id, program, context) {
            Results::success(value)
        } else {
            Results::failure(checker.errors.errors)
        }
    }

    fn evaluate_constant(&mut self, const_id: ConstId, program: &Program) -> Option<ConstantValue> {
        // Check if already evaluated
        if let Some(cached_value) = self.constant_cache.get(&const_id) {
            return Some(cached_value.clone());
        }

        // Check for circular dependency
        if self.evaluating.contains(&const_id) {
            let const_decl = &program.const_decls[const_id];
            let error = ConstantError::CircularConstantDependency {
                cycle: vec![const_decl.name.value.clone()],
            };
            self.errors.add_error(AstError::ConstantEvaluation(error), const_decl.span);
            return None;
        }

        self.evaluating.insert(const_id);

        let const_decl = &program.const_decls[const_id];
        
        // Create context with previously evaluated constants
        let mut context = ConstantContext::new();
        for (other_const_id, value) in &self.constant_cache {
            let other_const = &program.const_decls[*other_const_id];
            context.constants.insert(other_const.name.value.clone(), value.clone());
        }

        let result = self.evaluate_expression(const_decl.value, program, &context);
        
        self.evaluating.remove(&const_id);

        if let Some(value) = result {
            self.constant_cache.insert(const_id, value.clone());
            Some(value)
        } else {
            None
        }
    }

    // ========================================================================
    // --- Expression Evaluation
    // ========================================================================

    fn evaluate_expression(&mut self, expr_id: ExprId, program: &Program, 
                          context: &ConstantContext) -> Option<ConstantValue> {
        let expr = &program.expressions[expr_id];
        
        match expr {
            Expression::Literal { value, .. } => {
                self.evaluate_literal(value)
            }
            
            Expression::Identifier(identifier) => {
                self.evaluate_identifier(identifier, program, context)
            }
            
            Expression::Binary { left, op, right, .. } => {
                let left_value = self.evaluate_expression(*left, program, context)?;
                let right_value = self.evaluate_expression(*right, program, context)?;
                self.evaluate_binary_operation(&op.value, left_value, right_value, op.span)
            }
            
            Expression::Unary { op, expr, .. } => {
                let operand_value = self.evaluate_expression(*expr, program, context)?;
                self.evaluate_unary_operation(&op.value, operand_value, op.span)
            }
            
            Expression::Call { callee, args, .. } => {
                // Function calls in constant context - only allow certain built-in functions
                self.evaluate_function_call(callee, args, program, context, None)
            }
            
            Expression::Grouped { expr, .. } => {
                self.evaluate_expression(*expr, program, context)
            }
            
            _ => {
                // Other expressions are not constant
                if !context.allow_non_constant {
                    let error = ConstantError::NonConstantExpression {
                        expression: "complex expression".to_string(),
                    };
                    self.errors.add_error(AstError::ConstantEvaluation(error), None);
                }
                None
            }
        }
    }

    fn evaluate_literal(&self, literal: &Literal) -> Option<ConstantValue> {
        match literal {
            Literal::Integer(s) => {
                match s.parse::<i64>() {
                    Ok(i) => Some(ConstantValue::Integer(i)),
                    Err(_) => {
                        let error = ConstantError::IntegerOverflow {
                            operation: "parsing".to_string(),
                            value: s.clone(),
                        };
                        // Can't report error here as we don't have mutable access
                        None
                    }
                }
            }
            
            Literal::Float(s) => {
                match s.parse::<f64>() {
                    Ok(f) => Some(ConstantValue::Float(f)),
                    Err(_) => None,
                }
            }
            
            Literal::String(s) => {
                // Remove quotes if present
                let cleaned = if s.starts_with('"') && s.ends_with('"') {
                    s[1..s.len()-1].to_string()
                } else {
                    s.clone()
                };
                Some(ConstantValue::String(cleaned))
            }
            
            Literal::Bool(b) => Some(ConstantValue::Bool(*b)),
            
            Literal::List(elements) => {
                let mut const_elements = Vec::new();
                for element_id in elements {
                    // Would need to evaluate each element
                    // For now, return None for non-trivial lists
                }
                None // Simplified for now
            }
            
            Literal::RowLiteral(key_values) => {
                let mut const_fields = HashMap::new();
                for key_value in key_values {
                    // Would need to evaluate each field value
                    // For now, return None for non-trivial rows
                }
                None // Simplified for now
            }
        }
    }

    fn evaluate_identifier(&mut self, identifier: &Identifier, program: &Program, 
                          context: &ConstantContext) -> Option<ConstantValue> {
        // Check if it's a constant in the context
        if let Some(value) = context.constants.get(&identifier.name) {
            return Some(value.clone());
        }

        // Check if it resolves to a constant declaration
        if let Some(DeclRef::Const(const_id)) = identifier.resolved {
            return self.evaluate_constant(const_id, program);
        }

        // Not a constant
        if !context.allow_non_constant {
            let error = ConstantError::UndefinedConstant {
                name: identifier.name.clone(),
            };
            self.errors.add_error(AstError::ConstantEvaluation(error), identifier.span);
        }
        None
    }

    // ========================================================================
    // --- Operation Evaluation
    // ========================================================================

    fn evaluate_binary_operation(&mut self, op: &str, left: ConstantValue, right: ConstantValue, 
                                span: Option<Span>) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                self.evaluate_integer_binary_op(op, a, b, span)
            }
            
            (ConstantValue::Float(a), ConstantValue::Float(b)) => {
                self.evaluate_float_binary_op(op, a, b, span)
            }
            
            (ConstantValue::Integer(a), ConstantValue::Float(b)) => {
                self.evaluate_float_binary_op(op, a as f64, b, span)
            }
            
            (ConstantValue::Float(a), ConstantValue::Integer(b)) => {
                self.evaluate_float_binary_op(op, a, b as f64, span)
            }
            
            (ConstantValue::Bool(a), ConstantValue::Bool(b)) => {
                self.evaluate_bool_binary_op(op, a, b, span)
            }
            
            (ConstantValue::String(a), ConstantValue::String(b)) => {
                self.evaluate_string_binary_op(op, a, b, span)
            }
            
            (left, right) => {
                let error = ConstantError::InvalidTypeOperation {
                    operation: op.to_string(),
                    operand_type: format!("{} and {}", left.type_name(), right.type_name()),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
                None
            }
        }
    }

    fn evaluate_integer_binary_op(&mut self, op: &str, a: i64, b: i64, span: Option<Span>) -> Option<ConstantValue> {
        match op {
            "+" => {
                match a.checked_add(b) {
                    Some(result) => Some(ConstantValue::Integer(result)),
                    None => {
                        let error = ConstantError::IntegerOverflow {
                            operation: "addition".to_string(),
                            value: format!("{} + {}", a, b),
                        };
                        self.errors.add_error(AstError::ConstantEvaluation(error), span);
                        None
                    }
                }
            }
            
            "-" => {
                match a.checked_sub(b) {
                    Some(result) => Some(ConstantValue::Integer(result)),
                    None => {
                        let error = ConstantError::IntegerOverflow {
                            operation: "subtraction".to_string(),
                            value: format!("{} - {}", a, b),
                        };
                        self.errors.add_error(AstError::ConstantEvaluation(error), span);
                        None
                    }
                }
            }
            
            "*" => {
                match a.checked_mul(b) {
                    Some(result) => Some(ConstantValue::Integer(result)),
                    None => {
                        let error = ConstantError::IntegerOverflow {
                            operation: "multiplication".to_string(),
                            value: format!("{} * {}", a, b),
                        };
                        self.errors.add_error(AstError::ConstantEvaluation(error), span);
                        None
                    }
                }
            }
            
            "/" => {
                if b == 0 {
                    let error = ConstantError::DivisionByZero;
                    self.errors.add_error(AstError::ConstantEvaluation(error), span);
                    None
                } else {
                    match a.checked_div(b) {
                        Some(result) => Some(ConstantValue::Integer(result)),
                        None => {
                            let error = ConstantError::IntegerOverflow {
                                operation: "division".to_string(),
                                value: format!("{} / {}", a, b),
                            };
                            self.errors.add_error(AstError::ConstantEvaluation(error), span);
                            None
                        }
                    }
                }
            }
            
            "%" => {
                if b == 0 {
                    let error = ConstantError::DivisionByZero;
                    self.errors.add_error(AstError::ConstantEvaluation(error), span);
                    None
                } else {
                    Some(ConstantValue::Integer(a % b))
                }
            }
            
            "==" => Some(ConstantValue::Bool(a == b)),
            "!=" => Some(ConstantValue::Bool(a != b)),
            "<" => Some(ConstantValue::Bool(a < b)),
            ">" => Some(ConstantValue::Bool(a > b)),
            "<=" => Some(ConstantValue::Bool(a <= b)),
            ">=" => Some(ConstantValue::Bool(a >= b)),
            
            _ => {
                let error = ConstantError::InvalidTypeOperation {
                    operation: op.to_string(),
                    operand_type: "integer".to_string(),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
                None
            }
        }
    }

    fn evaluate_float_binary_op(&mut self, op: &str, a: f64, b: f64, span: Option<Span>) -> Option<ConstantValue> {
        match op {
            "+" => Some(ConstantValue::Float(a + b)),
            "-" => Some(ConstantValue::Float(a - b)),
            "*" => Some(ConstantValue::Float(a * b)),
            "/" => {
                if b == 0.0 {
                    let error = ConstantError::DivisionByZero;
                    self.errors.add_error(AstError::ConstantEvaluation(error), span);
                    None
                } else {
                    Some(ConstantValue::Float(a / b))
                }
            }
            "%" => Some(ConstantValue::Float(a % b)),
            "==" => Some(ConstantValue::Bool((a - b).abs() < f64::EPSILON)),
            "!=" => Some(ConstantValue::Bool((a - b).abs() >= f64::EPSILON)),
            "<" => Some(ConstantValue::Bool(a < b)),
            ">" => Some(ConstantValue::Bool(a > b)),
            "<=" => Some(ConstantValue::Bool(a <= b)),
            ">=" => Some(ConstantValue::Bool(a >= b)),
            
            _ => {
                let error = ConstantError::InvalidTypeOperation {
                    operation: op.to_string(),
                    operand_type: "float".to_string(),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
                None
            }
        }
    }

    fn evaluate_bool_binary_op(&mut self, op: &str, a: bool, b: bool, span: Option<Span>) -> Option<ConstantValue> {
        match op {
            "&&" => Some(ConstantValue::Bool(a && b)),
            "||" => Some(ConstantValue::Bool(a || b)),
            "==" => Some(ConstantValue::Bool(a == b)),
            "!=" => Some(ConstantValue::Bool(a != b)),
            
            _ => {
                let error = ConstantError::InvalidTypeOperation {
                    operation: op.to_string(),
                    operand_type: "boolean".to_string(),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
                None
            }
        }
    }

    fn evaluate_string_binary_op(&mut self, op: &str, a: String, b: String, span: Option<Span>) -> Option<ConstantValue> {
        match op {
            "+" => Some(ConstantValue::String(a + &b)),
            "==" => Some(ConstantValue::Bool(a == b)),
            "!=" => Some(ConstantValue::Bool(a != b)),
            
            _ => {
                let error = ConstantError::InvalidTypeOperation {
                    operation: op.to_string(),
                    operand_type: "string".to_string(),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
                None
            }
        }
    }

    fn evaluate_unary_operation(&mut self, op: &str, operand: ConstantValue, span: Option<Span>) -> Option<ConstantValue> {
        match (op, operand) {
            ("-", ConstantValue::Integer(i)) => {
                match i.checked_neg() {
                    Some(result) => Some(ConstantValue::Integer(result)),
                    None => {
                        let error = ConstantError::IntegerOverflow {
                            operation: "negation".to_string(),
                            value: i.to_string(),
                        };
                        self.errors.add_error(AstError::ConstantEvaluation(error), span);
                        None
                    }
                }
            }
            
            ("-", ConstantValue::Float(f)) => Some(ConstantValue::Float(-f)),
            
            ("!", ConstantValue::Bool(b)) => Some(ConstantValue::Bool(!b)),
            
            (op, operand) => {
                let error = ConstantError::InvalidTypeOperation {
                    operation: op.to_string(),
                    operand_type: operand.type_name().to_string(),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
                None
            }
        }
    }

    // ========================================================================
    // --- Function Call Evaluation
    // ========================================================================

    fn evaluate_function_call(&mut self, callee: &ExprId, args: &[ExprId], program: &Program, 
                             context: &ConstantContext, span: Option<Span>) -> Option<ConstantValue> {
        let callee_expr = &program.expressions[*callee];
        
        if let Expression::Identifier(identifier) = callee_expr {
            match identifier.name.as_str() {
                // Built-in constant functions
                "abs" => {
                    if args.len() != 1 {
                        return None;
                    }
                    let arg_value = self.evaluate_expression(args[0], program, context)?;
                    self.evaluate_abs_function(arg_value)
                }
                
                "min" => {
                    if args.len() != 2 {
                        return None;
                    }
                    let arg1 = self.evaluate_expression(args[0], program, context)?;
                    let arg2 = self.evaluate_expression(args[1], program, context)?;
                    self.evaluate_min_function(arg1, arg2)
                }
                
                "max" => {
                    if args.len() != 2 {
                        return None;
                    }
                    let arg1 = self.evaluate_expression(args[0], program, context)?;
                    let arg2 = self.evaluate_expression(args[1], program, context)?;
                    self.evaluate_max_function(arg1, arg2)
                }
                
                _ => {
                    // Not a constant function
                    if !context.allow_non_constant {
                        let error = ConstantError::NonConstantExpression {
                            expression: format!("function call to {}", identifier.name),
                        };
                        self.errors.add_error(AstError::ConstantEvaluation(error), identifier.span);
                    }
                    None
                }
            }
        } else {
            // Complex callee expression - not constant
            if !context.allow_non_constant {
                let error = ConstantError::NonConstantExpression {
                    expression: "complex function call".to_string(),
                };
                self.errors.add_error(AstError::ConstantEvaluation(error), span);
            }
            None
        }
    }

    fn evaluate_abs_function(&self, arg: ConstantValue) -> Option<ConstantValue> {
        match arg {
            ConstantValue::Integer(i) => Some(ConstantValue::Integer(i.abs())),
            ConstantValue::Float(f) => Some(ConstantValue::Float(f.abs())),
            _ => None,
        }
    }

    fn evaluate_min_function(&self, arg1: ConstantValue, arg2: ConstantValue) -> Option<ConstantValue> {
        match (arg1, arg2) {
            (ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Integer(a.min(b)))
            }
            (ConstantValue::Float(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float(a.min(b)))
            }
            (ConstantValue::Integer(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float((a as f64).min(b)))
            }
            (ConstantValue::Float(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Float(a.min(b as f64)))
            }
            _ => None,
        }
    }

    fn evaluate_max_function(&self, arg1: ConstantValue, arg2: ConstantValue) -> Option<ConstantValue> {
        match (arg1, arg2) {
            (ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Integer(a.max(b)))
            }
            (ConstantValue::Float(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float(a.max(b)))
            }
            (ConstantValue::Integer(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float((a as f64).max(b)))
            }
            (ConstantValue::Float(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Float(a.max(b as f64)))
            }
            _ => None,
        }
    }
}

// ============================================================================
// --- Constant Folding Optimization
// ============================================================================

/// Performs constant folding optimization on expressions
pub struct ConstantFolder {
    constant_values: HashMap<ConstId, ConstantValue>,
}

impl ConstantFolder {
    pub fn new(constant_values: HashMap<ConstId, ConstantValue>) -> Self {
        Self { constant_values }
    }

    /// Fold constants in an expression, returning a new expression if changes were made
    pub fn fold_expression(&self, expr_id: ExprId, program: &mut Program) -> Option<ExprId> {
        let expr = &program.expressions[expr_id];
        
        match expr {
            Expression::Binary { left, op, right, span } => {
                // Try to evaluate as constant
                let context = ConstantContext::new();
                let mut checker = ConstantChecker::new();
                
                if let Some(const_value) = checker.evaluate_expression(expr_id, program, &context) {
                    // Replace with literal
                    let literal = self.constant_value_to_literal(const_value);
                    let new_expr = Expression::Literal { value: literal, span: *span };
                    let new_expr_id = program.expressions.alloc(new_expr);
                    return Some(new_expr_id);
                }
            }
            
            Expression::Unary { op, expr, span } => {
                // Similar constant folding for unary expressions
                let context = ConstantContext::new();
                let mut checker = ConstantChecker::new();
                
                if let Some(const_value) = checker.evaluate_expression(expr_id, program, &context) {
                    let literal = self.constant_value_to_literal(const_value);
                    let new_expr = Expression::Literal { value: literal, span: *span };
                    let new_expr_id = program.expressions.alloc(new_expr);
                    return Some(new_expr_id);
                }
            }
            
            _ => {}
        }
        
        None
    }

    fn constant_value_to_literal(&self, value: ConstantValue) -> Literal {
        match value {
            ConstantValue::Integer(i) => Literal::Integer(i.to_string()),
            ConstantValue::Float(f) => Literal::Float(f.to_string()),
            ConstantValue::String(s) => Literal::String(format!("\"{}\"", s)),
            ConstantValue::Bool(b) => Literal::Bool(b),
            ConstantValue::List(_) => {
                // Would need to convert back to expression list
                Literal::List(Vec::new()) // Simplified
            }
            ConstantValue::Row(_) => {
                // Would need to convert back to key-value pairs
                Literal::RowLiteral(Vec::new()) // Simplified
            }
        }
    }
}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Check and evaluate all constants in a program
pub fn check_constants(program: &Program) -> Results<HashMap<ConstId, ConstantValue>> {
    ConstantChecker::check_constants(program)
}

/// Evaluate a single constant expression
pub fn evaluate_constant_expression(expr_id: ExprId, program: &Program) -> Results<ConstantValue> {
    let context = ConstantContext::new();
    ConstantChecker::evaluate_constant_expression(expr_id, program, &context)
}

/// Perform constant folding optimization on a program
pub fn fold_constants(program: &mut Program, constant_values: HashMap<ConstId, ConstantValue>) {
    let folder = ConstantFolder::new(constant_values);
    
    // Fold constants in all expressions
    // This would require more sophisticated traversal in a real implementation
}
