//! Core utilities for error reporting and diagnostics
//!
//! This module provides a unified error system for consistent,
//! beautiful error reporting across all compiler stages.

use ariadne::{Color, Label, Report, ReportKind, Source};
use std::collections::HashMap;
use std::fmt;

// ============================================================================
// --- Unified Span Type
// ============================================================================

/// Source location information - unified across all modules
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file_name: &'static str,
}

impl Span {
    pub fn new(start: usize, end: usize, file_name: &'static str) -> Self {
        Self {
            start,
            end,
            file_name,
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
pub trait CompilerError: fmt::Display + fmt::Debug + Send + Sync {
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

    /// Source location span
    fn span(&self) -> Option<Span> {
        None
    }

    /// Set the span for this error
    fn with_span(self, span: Span) -> Self
    where
        Self: Sized;
}

// ============================================================================
// --- Diagnostic Reporter
// ============================================================================

/// High-performance error reporter using ariadne
pub struct DiagnosticReporter {
    sources: HashMap<String, String>,
}

impl DiagnosticReporter {
    pub fn new() -> Self {
        Self {
            sources: HashMap::new(),
        }
    }

    /// Add source file for error reporting
    pub fn add_source(&mut self, filename: &str, content: &str) {
        self.sources
            .insert(filename.to_string(), content.to_string());
    }

    /// Report a single error
    pub fn report<E: CompilerError>(&self, error: &E, filename: &str) {
        let severity = error.severity();
        let span_range = error.span().map(|s| s.range()).unwrap_or(0..0);

        let mut report = Report::build(severity.report_kind(), (filename, span_range.clone()))
            .with_code(error.code())
            .with_message(error.to_string());

        if let Some(span) = error.span() {
            report = report.with_label(
                Label::new((filename, span.range()))
                    .with_message(error.code())
                    .with_color(severity.color()),
            );
        }

        if let Some(help) = error.help() {
            report = report.with_help(help);
        }

        let source = self
            .sources
            .get(filename)
            .map(|s| Source::from(s.as_str()))
            .unwrap_or_else(|| Source::from(""));

        report.finish().eprint((filename, source)).ok();
    }

    /// Report multiple errors
    pub fn report_all<E: CompilerError>(&self, errors: &[E], filename: &str) {
        for error in errors {
            self.report(error, filename);
        }
    }
}

impl Default for DiagnosticReporter {
    fn default() -> Self {
        Self::new()
    }
}
