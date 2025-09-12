use crate::util::CompilerError;
use ariadne::{Cache, Label, Report, Source};
use std::collections::HashMap;
use std::fmt::Debug;
use std::{fmt, io};

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
    pub fn report(&mut self, error: &CompilerError) -> io::Result<()> {
        // Changed to &mut self
        let severity = error.value.severity();
        let message = error.value.to_string();
        let code = error.value.code();
        let span = error.span;

        let mut report_builder =
            Report::build(severity.report_kind(), (span.filename, span.range()))
                .with_code(code)
                .with_message(&message);

        // Add the primary label to the report if a span is available
        report_builder = report_builder
            .with_label(Label::new((span.filename, span.range())).with_color(severity.color()));

        // Add a help message if the error provides one
        if let Some(help) = error.value.help() {
            report_builder = report_builder.with_help(help);
        }

        let report = report_builder.finish();

        // Print the report to stderr.
        // We use the `Cache` implementation on `DiagnosticReporter` to provide
        // the necessary source file content to `ariadne`.
        report.eprint(self) // Now works with &mut self
    }

    pub fn report_all(&mut self, errors: &[CompilerError]) -> io::Result<()> {
        for error in errors {
            self.report(error)?;
        }
        Ok(())
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
