use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CacheOptions {
    pub enabled: bool,
    pub dir: PathBuf,
    pub max_size_mb: u64,
    pub clear: bool,
}

#[derive(Debug, Clone)]
pub struct SplitOptions {
    pub enabled: bool,
    pub max_depth: u32,
    pub strategy: String,
}

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub instances: usize,
    pub loop_unroll: u32,
    pub timeout_secs: u32,
    pub enable_optimization: bool,
    pub enable_verification: bool,
    pub cache: CacheOptions,
    pub split: SplitOptions,
}
