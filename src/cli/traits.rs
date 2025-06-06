// src/cli/traits.rs
use std::io::Write;
use std::path::PathBuf;

/// Core trait for all pipeline stages
pub trait PipelineStage {
    type Input;
    type Output;
    type Error;

    /// Execute this stage
    fn execute(&mut self, input: Self::Input) -> Result<Self::Output, Self::Error>;

    /// Get the name of this stage for logging
    fn name(&self) -> &'static str;

    /// Get stage number for progress reporting
    fn stage_number(&self) -> usize;
}

/// Trait for stages that can output to a file
pub trait FileOutput {
    type Data;

    /// Write the stage's data to a writer
    fn write_output(
        &self,
        data: &Self::Data,
        writer: &mut dyn Write,
        cli: &super::Cli,
    ) -> Result<(), String>;
}

/// Trait for stages that can output to a directory (like verification with multiple Boogie files)
pub trait DirectoryOutput {
    type Data;

    /// Write the stage's data to a directory
    fn write_to_directory(
        &self,
        data: &Self::Data,
        dir: &PathBuf,
        cli: &super::Cli,
    ) -> Result<(), String>;
}

/// Trait for stages that have summary statistics
pub trait StageSummary {
    type Data;

    /// Get summary statistics for this stage
    fn get_summary(&self, data: &Self::Data) -> String;
}

/// Stage execution context
#[derive(Debug)]
pub struct StageContext<'a> {
    pub cli: &'a super::Cli,
    pub source_code: Option<&'a str>,
}

impl<'a> StageContext<'a> {
    pub fn new(cli: &'a super::Cli) -> Self {
        Self {
            cli,
            source_code: None,
        }
    }

    pub fn with_source(mut self, source: &'a str) -> Self {
        self.source_code = Some(source);
        self
    }
}
