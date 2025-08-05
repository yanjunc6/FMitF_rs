//! Constant Expression Checker
//!
//! This module provides compile-time constant evaluation and validation for expressions.
//! It determines whether expressions can be evaluated at compile time, which is required
//! for certain language constructs like array sizes and hop loop bounds.
//!
//! # Features
//!
//! - Validates expressions containing only literals and const-declared variables
//! - Supports basic arithmetic operations on constant values
//! - Caches results for performance
//!
//! # Examples
//!
//! ```rust
//! // These expressions are considered constant:
//! // - Literals: 5, 3.14, "hello", true
//! // - Const variables: const int C = 4; then C
//! // - Arithmetic with constants: 5 * C, C + 10
//! //
//! // These are NOT constant:
//! // - Regular variables: int x = 5; then x
//! // - Complex expressions involving non-constants: x * 7
//! ```

use crate::ast::*;
use std::collections::HashMap;

/// A compile-time constant value that can be evaluated
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

impl ConstantValue {
    /// Get the type of this constant value
    pub fn type_name(&self) -> TypeName {
        match self {
            ConstantValue::Int(_) => TypeName::Int,
            ConstantValue::Float(_) => TypeName::Float,
            ConstantValue::String(_) => TypeName::String,
            ConstantValue::Bool(_) => TypeName::Bool,
        }
    }

    /// Try to get this constant as an integer (useful for array sizes, loop bounds)
    pub fn as_int(&self) -> Option<i64> {
        match self {
            ConstantValue::Int(n) => Some(*n),
            _ => None,
        }
    }

    /// Try to get this constant as a float
    pub fn as_float(&self) -> Option<f64> {
        match self {
            ConstantValue::Float(f) => Some(*f),
            ConstantValue::Int(i) => Some(*i as f64),
            _ => None,
        }
    }
}

/// Checker for constant expressions and compile-time evaluation
pub struct ConstantChecker<'p> {
    program: &'p Program,
    /// Cache of known constant variables (name -> constant_value)
    const_values: HashMap<String, ConstantValue>,
    /// Cache of evaluated constant expressions (expr_id -> result)
    expression_cache: HashMap<ExpressionId, Option<ConstantValue>>,
}

impl<'p> ConstantChecker<'p> {
    /// Create a new constant checker for the given program
    pub fn new(program: &'p Program) -> Self {
        let mut checker = Self {
            program,
            const_values: HashMap::new(),
            expression_cache: HashMap::new(),
        };

        // Pre-analyze all const variable declarations
        checker.analyze_const_declarations();

        checker
    }

    /// Check if an expression is a compile-time constant
    pub fn is_constant_expression(&mut self, expr_id: ExpressionId) -> bool {
        self.evaluate_constant(expr_id).is_some()
    }

    /// Evaluate a constant expression to its value
    /// Returns None if the expression is not constant
    pub fn evaluate_constant(&mut self, expr_id: ExpressionId) -> Option<ConstantValue> {
        // Check cache first
        if let Some(cached) = self.expression_cache.get(&expr_id) {
            return cached.clone();
        }

        let result = self.evaluate_expression_uncached(expr_id);
        self.expression_cache.insert(expr_id, result.clone());
        result
    }

    /// Require that an expression is constant, returning an error context if not
    pub fn require_constant(
        &mut self,
        expr_id: ExpressionId,
        context: &str,
    ) -> Result<ConstantValue, String> {
        self.evaluate_constant(expr_id)
            .ok_or_else(|| format!("Expression must be constant for {}", context))
    }

    /// Internal evaluation without caching
    fn evaluate_expression_uncached(&mut self, expr_id: ExpressionId) -> Option<ConstantValue> {
        let expr = &self.program.expressions[expr_id];

        match &expr.node {
            // Literal constants are always constant
            ExpressionKind::IntLit(value) => Some(ConstantValue::Int(*value)),
            ExpressionKind::FloatLit(value) => Some(ConstantValue::Float(*value)),
            ExpressionKind::StringLit(value) => Some(ConstantValue::String(value.clone())),
            ExpressionKind::BoolLit(value) => Some(ConstantValue::Bool(*value)),

            // Variables are constant only if they're declared as const
            ExpressionKind::Ident(name) => {
                // Check if this is a const declaration by name
                self.const_values.get(name).cloned()
            }

            // Unary operations on constants
            ExpressionKind::UnaryOp {
                op,
                expr: inner_expr,
                ..
            } => {
                let operand = self.evaluate_constant(*inner_expr)?;
                self.evaluate_unary_operation(op, &operand)
            }

            // Binary operations on constants
            ExpressionKind::BinaryOp {
                left, op, right, ..
            } => {
                let left_val = self.evaluate_constant(*left)?;
                let right_val = self.evaluate_constant(*right)?;
                self.evaluate_binary_operation(op, &left_val, &right_val)
            }

            // These expression types are not constant
            ExpressionKind::TableFieldAccess { .. }
            | ExpressionKind::FieldAccess { .. }
            | ExpressionKind::TableAccess { .. }
            | ExpressionKind::ArrayAccess { .. }
            | ExpressionKind::ArrayLiteral { .. }
            | ExpressionKind::RecordLiteral { .. } => None,
        }
    }

    /// Evaluate a unary operation on constant values
    fn evaluate_unary_operation(
        &self,
        op: &UnaryOp,
        operand: &ConstantValue,
    ) -> Option<ConstantValue> {
        match (op, operand) {
            (UnaryOp::Neg, ConstantValue::Int(n)) => Some(ConstantValue::Int(-n)),
            (UnaryOp::Neg, ConstantValue::Float(f)) => Some(ConstantValue::Float(-f)),
            (UnaryOp::Not, ConstantValue::Bool(b)) => Some(ConstantValue::Bool(!b)),
            _ => None, // Invalid operation
        }
    }

    /// Evaluate a binary operation on constant values
    fn evaluate_binary_operation(
        &self,
        op: &BinaryOp,
        left: &ConstantValue,
        right: &ConstantValue,
    ) -> Option<ConstantValue> {
        match (op, left, right) {
            // Arithmetic operations
            (BinaryOp::Add, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Int(a + b))
            }
            (BinaryOp::Sub, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Int(a - b))
            }
            (BinaryOp::Mul, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Int(a * b))
            }
            (BinaryOp::Div, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                if *b != 0 {
                    Some(ConstantValue::Int(a / b))
                } else {
                    None
                }
            }

            // Mixed int/float arithmetic
            (BinaryOp::Add, a, b) => {
                let af = a.as_float()?;
                let bf = b.as_float()?;
                Some(ConstantValue::Float(af + bf))
            }
            (BinaryOp::Sub, a, b) => {
                let af = a.as_float()?;
                let bf = b.as_float()?;
                Some(ConstantValue::Float(af - bf))
            }
            (BinaryOp::Mul, a, b) => {
                let af = a.as_float()?;
                let bf = b.as_float()?;
                Some(ConstantValue::Float(af * bf))
            }
            (BinaryOp::Div, a, b) => {
                let af = a.as_float()?;
                let bf = b.as_float()?;
                if bf != 0.0 {
                    Some(ConstantValue::Float(af / bf))
                } else {
                    None
                }
            }

            // Comparison operations
            (BinaryOp::Eq, a, b) => Some(ConstantValue::Bool(self.values_equal(a, b))),
            (BinaryOp::Neq, a, b) => Some(ConstantValue::Bool(!self.values_equal(a, b))),

            (BinaryOp::Lt, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a < b))
            }
            (BinaryOp::Lte, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a <= b))
            }
            (BinaryOp::Gt, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a > b))
            }
            (BinaryOp::Gte, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a >= b))
            }

            // Logical operations
            (BinaryOp::And, ConstantValue::Bool(a), ConstantValue::Bool(b)) => {
                Some(ConstantValue::Bool(*a && *b))
            }
            (BinaryOp::Or, ConstantValue::Bool(a), ConstantValue::Bool(b)) => {
                Some(ConstantValue::Bool(*a || *b))
            }

            _ => None, // Unsupported operation
        }
    }

    /// Check if two constant values are equal
    fn values_equal(&self, a: &ConstantValue, b: &ConstantValue) -> bool {
        match (a, b) {
            (ConstantValue::Int(a), ConstantValue::Int(b)) => a == b,
            (ConstantValue::Float(a), ConstantValue::Float(b)) => (a - b).abs() < f64::EPSILON,
            (ConstantValue::String(a), ConstantValue::String(b)) => a == b,
            (ConstantValue::Bool(a), ConstantValue::Bool(b)) => a == b,
            _ => false,
        }
    }

    /// Pre-analyze all const declarations to build the constants map
    fn analyze_const_declarations(&mut self) {
        // Iterate through all constants in the program
        for const_id in &self.program.root_constants {
            if let Some(var_decl) = self.program.variables.get(*const_id) {
                // Find the corresponding constant declaration
                for (_, const_decl) in self.program.constants.iter() {
                    if const_decl.name == var_decl.name {
                        // Evaluate the constant value
                        if let Some(value) = self.evaluate_constant_uncached(const_decl.value) {
                            self.const_values.insert(const_decl.name.clone(), value);
                        }
                        break;
                    }
                }
            }
        }
    }

    /// Internal evaluation without caching - used for initial constant evaluation
    fn evaluate_constant_uncached(&self, expr_id: ExpressionId) -> Option<ConstantValue> {
        let expr = &self.program.expressions[expr_id];

        match &expr.node {
            // Literal constants are always constant
            ExpressionKind::IntLit(value) => Some(ConstantValue::Int(*value)),
            ExpressionKind::FloatLit(value) => Some(ConstantValue::Float(*value)),
            ExpressionKind::StringLit(value) => Some(ConstantValue::String(value.clone())),
            ExpressionKind::BoolLit(value) => Some(ConstantValue::Bool(*value)),

            // For initial evaluation, don't allow references to other constants
            // This avoids circular dependencies during initialization
            ExpressionKind::Ident(_) => None,

            // Unary operations on constants
            ExpressionKind::UnaryOp {
                op,
                expr: inner_expr,
                ..
            } => {
                let operand = self.evaluate_constant_uncached(*inner_expr)?;
                self.evaluate_unary_operation(op, &operand)
            }

            // Binary operations on constants
            ExpressionKind::BinaryOp {
                left, op, right, ..
            } => {
                let left_val = self.evaluate_constant_uncached(*left)?;
                let right_val = self.evaluate_constant_uncached(*right)?;
                self.evaluate_binary_operation(op, &left_val, &right_val)
            }

            // These expression types are not constant
            ExpressionKind::TableFieldAccess { .. }
            | ExpressionKind::FieldAccess { .. }
            | ExpressionKind::TableAccess { .. }
            | ExpressionKind::ArrayAccess { .. }
            | ExpressionKind::ArrayLiteral { .. }
            | ExpressionKind::RecordLiteral { .. } => None,
        }
    }
}

/// Public interface for checking if an expression is constant
pub fn is_constant_expression(program: &Program, expr_id: ExpressionId) -> bool {
    let mut checker = ConstantChecker::new(program);
    checker.is_constant_expression(expr_id)
}

/// Public interface for evaluating a constant expression
pub fn evaluate_constant_expression(
    program: &Program,
    expr_id: ExpressionId,
) -> Option<ConstantValue> {
    let mut checker = ConstantChecker::new(program);
    checker.evaluate_constant(expr_id)
}
