//! Core utilities for error reporting and diagnostics
//!
//! This module provides a unified error system for consistent,
//! beautiful error reporting across all compiler stages.

use ariadne::{Cache, Color, Label, Report, ReportKind, Source};
use std::collections::HashMap;
use std::fmt::Debug;
use std::{fmt, io};

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
    fn span(&self) -> Span;
}

// ============================================================================
// --- Diagnostic Reporter
// ============================================================================

/// High-performance error reporter using ariadne.
///
/// This struct holds all source files and acts as a cache for the reporting
/// infrastructure, allowing for efficient reporting across multiple files.
pub struct DiagnosticReporter {
    sources: HashMap<String, Source<String>>, // Changed to String for both key and value
}

impl DiagnosticReporter {
    /// Creates a new `DiagnosticReporter`.
    pub fn new() -> Self {
        Self {
            sources: HashMap::new(),
        }
    }

    /// Adds a source file to the reporter's cache.
    ///
    /// The `filename` and `source_code` are now owned Strings to avoid lifetime issues.
    pub fn add_source(&mut self, filename: impl Into<String>, source_code: impl Into<String>) {
        let filename = filename.into();
        let source_code = source_code.into();
        self.sources.insert(filename, Source::from(source_code));
    }

    /// Reports a diagnostic (error, warning, or info) to `stderr`.
    ///
    /// This method takes any error that implements the `CompilerError` trait,
    /// builds a report using `ariadne`, and prints it.
    pub fn report<E: CompilerError>(&mut self, error: &E) -> io::Result<()> {
        // Changed to &mut self
        let severity = error.severity();
        let message = error.to_string();
        let code = error.code();
        let span = error.span();

        let mut report_builder =
            Report::build(severity.report_kind(), (span.filename, span.range()))
                .with_code(code)
                .with_message(&message);

        // Add the primary label to the report if a span is available
        report_builder = report_builder
            .with_label(Label::new((span.filename, span.range())).with_color(severity.color()));

        // Add a help message if the error provides one
        if let Some(help) = error.help() {
            report_builder = report_builder.with_help(help);
        }

        let report = report_builder.finish();

        // Print the report to stderr.
        // We use the `Cache` implementation on `DiagnosticReporter` to provide
        // the necessary source file content to `ariadne`.
        report.eprint(self) // Now works with &mut self
    }
}

/// Implementation of ariadne's `Cache` trait for our reporter.
///
/// This allows `ariadne` to fetch source file content by its filename,
/// which is crucial for printing the code snippets in the error report.
impl Cache<&str> for DiagnosticReporter {
    // Changed from &'static str to &str
    type Storage = String;

    fn fetch(&mut self, id: &&str) -> Result<&Source<<Self as Cache<&str>>::Storage>, impl Debug> {
        self.sources.get(*id).ok_or_else(|| {
            Box::new(format!("Failed to fetch source: {}", id)) as Box<dyn fmt::Debug>
        })
    }

    fn display<'a>(&self, id: &'a &str) -> Option<impl std::fmt::Display + 'a> {
        Some(Box::new(*id))
    }
}
