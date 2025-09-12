//! Core utilities for error reporting and diagnostics
//!
//! This module provides a unified error system for consistent,
//! beautiful error reporting across all compiler stages.

use ariadne::{Color, ReportKind};
use std::fmt;

// ============================================================================
// --- Unified Span Type
// ============================================================================

/// Source location information - unified across all modules
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub filename: &'static str,
}

impl Span {
    pub fn new(start: usize, end: usize, filename: &'static str) -> Self {
        Self {
            start,
            end,
            filename,
        }
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

// ============================================================================
// --- Error Severity
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

impl Severity {
    pub fn color(&self) -> Color {
        match self {
            Severity::Error => Color::Red,
            Severity::Warning => Color::Yellow,
            Severity::Info => Color::Blue,
        }
    }

    pub fn report_kind(&self) -> ReportKind<'static> {
        match self {
            Severity::Error => ReportKind::Error,
            Severity::Warning => ReportKind::Warning,
            Severity::Info => ReportKind::Advice,
        }
    }
}

// ============================================================================
// --- Unified Error Type
// ============================================================================

/// Core trait for all compiler errors with unified span support
pub trait CompilerErrorKind: fmt::Display + fmt::Debug + Send + Sync {
    /// Error code for categorization
    fn code(&self) -> &'static str;

    /// Severity level
    fn severity(&self) -> Severity {
        Severity::Error
    }

    /// Optional help message
    fn help(&self) -> Option<&str> {
        None
    }
}

/// Wraps any value with its source location.

pub struct CompilerError {
    pub value: Box<dyn CompilerErrorKind>,
    pub span: Span,
}

impl CompilerError {
    pub fn new(error: impl CompilerErrorKind + 'static, span: Span) -> Self {
        Self {
            value: Box::new(error),
            span,
        }
    }
}

pub mod diagnostic_reporter;
pub use diagnostic_reporter::DiagnosticReporter;
