//! Core utilities for error reporting and diagnostics
//!
//! This module provides a unified error system for consistent,
//! beautiful error reporting across all compiler stages.
use ariadne::{Color, ReportKind};
use serde::{Deserialize, Deserializer, Serialize};
use std::fmt;

// ============================================================================
// --- Unified Span Type
// ============================================================================

/// Source location information - unified across all modules
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
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

// The proxy struct from Step 1
#[derive(Deserialize)]
struct SpanForDe {
    start: usize,
    end: usize,
    filename: String,
}

// Manual `Deserialize` implementation for your real `Span`
impl<'de> Deserialize<'de> for Span {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // 1. Deserialize into the temporary struct that can handle a String
        let span_for_de = SpanForDe::deserialize(deserializer)?;

        // 2. Convert the owned String into a Box<str>, then leak it.
        //    Box::leak() consumes the Box and returns a mutable reference
        //    with a 'static lifetime. This is the key to the whole solution.
        let static_filename: &'static str = Box::leak(span_for_de.filename.into_boxed_str());

        // 3. Create your actual Span struct with the now-'static filename
        Ok(Span {
            start: span_for_de.start,
            end: span_for_de.end,
            filename: static_filename,
        })
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
