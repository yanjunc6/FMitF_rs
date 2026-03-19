use std::fs;
use std::io::Write;
use std::path::PathBuf;

#[derive(Default)]
pub struct Logger {
    file: Option<fs::File>,
}

impl Logger {
    pub fn new() -> Self {
        Self { file: None }
    }

    pub fn init(&mut self, output_dir: &PathBuf) -> std::io::Result<()> {
        fs::create_dir_all(output_dir)?;
        let file = fs::File::create(output_dir.join("compiler.log"))?;
        self.file = Some(file);
        Ok(())
    }

    pub fn line<S: AsRef<str>>(&mut self, s: S) -> std::io::Result<()> {
        if let Some(f) = self.file.as_mut() {
            writeln!(f, "{}", s.as_ref())?;
        }
        Ok(())
    }

    pub fn block<S: AsRef<str>>(&mut self, header: &str, body: S) -> std::io::Result<()> {
        if let Some(f) = self.file.as_mut() {
            writeln!(f, "{}", header)?;
            writeln!(f, "{}", body.as_ref())?;
        }
        Ok(())
    }

    pub fn line_and_stdout<S: AsRef<str>>(&mut self, s: S) -> std::io::Result<()> {
        let msg = s.as_ref();
        self.line(msg)?;
        println!("{}", msg);
        Ok(())
    }
}
