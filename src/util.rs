//! Utility module for error reporting and common functionality
//!
//! This module provides a unified error reporting system using ariadne
//! for beautiful terminal error displays.

use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use std::collections::HashMap;
use std::fmt;

/// Universal error trait that all module errors should implement
/// to enable unified ariadne-based error reporting
pub trait DiagnosticError: fmt::Display + fmt::Debug {
    /// Get the error kind/type for display
    fn error_type(&self) -> &'static str;

    /// Get the main error message
    fn message(&self) -> String {
        self.to_string()
    }

    /// Get optional help text
    fn help(&self) -> Option<String> {
        None
    }

    /// Get optional note text
    fn note(&self) -> Option<String> {
        None
    }

    /// Get the severity level
    fn severity(&self) -> Severity {
        Severity::Error
    }
}

/// Error severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Note,
}

impl Severity {
    pub fn to_report_kind(self) -> ReportKind<'static> {
        match self {
            Severity::Error => ReportKind::Error,
            Severity::Warning => ReportKind::Warning,
            Severity::Info => ReportKind::Advice,
            Severity::Note => ReportKind::Advice,
        }
    }

    pub fn to_color(self) -> Color {
        match self {
            Severity::Error => Color::Red,
            Severity::Warning => Color::Yellow,
            Severity::Info => Color::Blue,
            Severity::Note => Color::Cyan,
        }
    }
}

/// Span information for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl From<crate::ast::Span> for Span {
    fn from(ast_span: crate::ast::Span) -> Self {
        Self {
            start: ast_span.start,
            end: ast_span.end,
            line: ast_span.line,
            column: ast_span.column,
        }
    }
}

/// A spanned error with diagnostic information
#[derive(Debug, Clone)]
pub struct SpannedDiagnostic<E: DiagnosticError> {
    pub error: E,
    pub span: Option<Span>,
    pub file_name: Option<String>,
}

impl<E: DiagnosticError> SpannedDiagnostic<E> {
    pub fn new(error: E, span: Option<Span>) -> Self {
        Self {
            error,
            span,
            file_name: None,
        }
    }

    pub fn with_file_name(mut self, file_name: String) -> Self {
        self.file_name = Some(file_name);
        self
    }
}

/// Error reporter using ariadne for beautiful terminal output
pub struct ErrorReporter {
    color_gen: ColorGenerator,
    sources: HashMap<String, String>,
}

impl ErrorReporter {
    pub fn new() -> Self {
        Self {
            color_gen: ColorGenerator::new(),
            sources: HashMap::new(),
        }
    }

    /// Register a source file for error reporting
    pub fn add_source(&mut self, file_name: String, content: String) {
        self.sources.insert(file_name, content);
    }

    /// Report a single diagnostic error
    pub fn report_error<E: DiagnosticError>(&mut self, diagnostic: &SpannedDiagnostic<E>) {
        let file_name = diagnostic.file_name.as_deref().unwrap_or("<input>");
        let severity = diagnostic.error.severity();
        let color = severity.to_color();

        // Use a default span if none provided
        let span = diagnostic.span.map(|s| s.start..s.end).unwrap_or(0..0);

        let mut report = Report::build(severity.to_report_kind(), (file_name, span.clone()))
            .with_code(diagnostic.error.error_type())
            .with_message(diagnostic.error.message());

        // Add span information if available
        if let Some(error_span) = diagnostic.span {
            report = report.with_label(
                Label::new((file_name, error_span.start..error_span.end))
                    .with_message(diagnostic.error.error_type())
                    .with_color(color),
            );
        }

        // Add help and note if available
        if let Some(help) = diagnostic.error.help() {
            report = report.with_help(help);
        }

        if let Some(note) = diagnostic.error.note() {
            report = report.with_note(note);
        }

        // Get source for the file
        let source = self
            .sources
            .get(file_name)
            .map(|s| Source::from(s.as_str()))
            .unwrap_or_else(|| Source::from(""));

        report
            .finish()
            .eprint((file_name, source))
            .unwrap_or_else(|e| eprintln!("Failed to print error report: {}", e));
    }

    /// Report multiple diagnostic errors
    pub fn report_errors<E: DiagnosticError>(&mut self, diagnostics: &[SpannedDiagnostic<E>]) {
        for diagnostic in diagnostics {
            self.report_error(diagnostic);
        }
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
    }
}
