use crate::cfg;
use crate::verification::Boogie::BoogieProgram;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone)]
pub struct CacheRuntimeOptions {
    pub enabled: bool,
    pub dir: PathBuf,
    pub max_size_mb: u64,
    pub clear: bool,
}

#[derive(Default, Debug, Clone)]
pub struct CacheStatsSnapshot {
    pub hits: usize,
    pub misses: usize,
    pub stores: usize,
    pub evictions: usize,
    pub invalidations: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CacheResult {
    Pass,
    Error,
    Timeout,
    CompilationError,
}

#[derive(Debug, Clone)]
struct CacheIndexEntry {
    path: PathBuf,
    size: u64,
    accessed_at: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheEntry {
    key: String,
    result: CacheResult,
    compiler_version: String,
    solver_version: String,
    created_at: u64,
    accessed_at: u64,
    access_count: u64,
}

#[derive(Default)]
struct CacheStatsAtomic {
    hits: AtomicUsize,
    misses: AtomicUsize,
    stores: AtomicUsize,
    evictions: AtomicUsize,
    invalidations: AtomicUsize,
}

impl CacheStatsAtomic {
    fn snapshot(&self) -> CacheStatsSnapshot {
        CacheStatsSnapshot {
            hits: self.hits.load(Ordering::Relaxed),
            misses: self.misses.load(Ordering::Relaxed),
            stores: self.stores.load(Ordering::Relaxed),
            evictions: self.evictions.load(Ordering::Relaxed),
            invalidations: self.invalidations.load(Ordering::Relaxed),
        }
    }
}

struct VerificationCache {
    enabled: bool,
    root_dir: PathBuf,
    max_size_bytes: u64,
    compiler_version: String,
    solver_version: String,
    index: RwLock<HashMap<String, CacheIndexEntry>>,
    stats: CacheStatsAtomic,
}

impl VerificationCache {
    fn now_secs() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs()
    }

    fn key_path(root: &Path, key: &str) -> PathBuf {
        let shard = &key[..2.min(key.len())];
        root.join(shard).join(format!("{}.json", key))
    }

    fn new(
        options: &CacheRuntimeOptions,
        compiler_version: String,
        solver_version: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let cache = Self {
            enabled: options.enabled,
            root_dir: options.dir.clone(),
            max_size_bytes: options.max_size_mb * 1024 * 1024,
            compiler_version,
            solver_version,
            index: RwLock::new(HashMap::new()),
            stats: CacheStatsAtomic::default(),
        };

        if !cache.enabled {
            return Ok(cache);
        }

        if options.clear && cache.root_dir.exists() {
            fs::remove_dir_all(&cache.root_dir)?;
        }
        fs::create_dir_all(&cache.root_dir)?;
        cache.rebuild_index()?;
        Ok(cache)
    }

    fn rebuild_index(&self) -> Result<(), Box<dyn std::error::Error>> {
        if !self.enabled {
            return Ok(());
        }

        let mut fresh = HashMap::new();
        for shard_entry in fs::read_dir(&self.root_dir)? {
            let shard_entry = shard_entry?;
            let shard_path = shard_entry.path();
            if !shard_path.is_dir() {
                continue;
            }
            for entry in fs::read_dir(shard_path)? {
                let entry = entry?;
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) != Some("json") {
                    continue;
                }
                let bytes = match fs::read(&path) {
                    Ok(v) => v,
                    Err(_) => {
                        let _ = fs::remove_file(&path);
                        continue;
                    }
                };
                let cache_entry: CacheEntry = match serde_json::from_slice(&bytes) {
                    Ok(v) => v,
                    Err(_) => {
                        let _ = fs::remove_file(&path);
                        continue;
                    }
                };
                if cache_entry.compiler_version != self.compiler_version
                    || cache_entry.solver_version != self.solver_version
                {
                    self.stats.invalidations.fetch_add(1, Ordering::Relaxed);
                    let _ = fs::remove_file(&path);
                    continue;
                }

                fresh.insert(
                    cache_entry.key.clone(),
                    CacheIndexEntry {
                        path,
                        size: bytes.len() as u64,
                        accessed_at: cache_entry.accessed_at,
                    },
                );
            }
        }

        let mut idx = self.index.write().unwrap();
        *idx = fresh;
        Ok(())
    }

    fn get(&self, key: &str) -> Option<CacheResult> {
        if !self.enabled {
            return None;
        }

        let index_hit = {
            let idx = self.index.read().unwrap();
            idx.get(key).cloned()
        };

        let mut index_entry = match index_hit {
            Some(v) => v,
            None => {
                self.stats.misses.fetch_add(1, Ordering::Relaxed);
                return None;
            }
        };

        let bytes = match fs::read(&index_entry.path) {
            Ok(v) => v,
            Err(_) => {
                self.stats.misses.fetch_add(1, Ordering::Relaxed);
                return None;
            }
        };

        let mut cache_entry: CacheEntry = match serde_json::from_slice(&bytes) {
            Ok(v) => v,
            Err(_) => {
                self.stats.misses.fetch_add(1, Ordering::Relaxed);
                return None;
            }
        };

        let now = Self::now_secs();
        cache_entry.accessed_at = now;
        cache_entry.access_count = cache_entry.access_count.saturating_add(1);
        if self
            .write_cache_entry_atomically(&index_entry.path, &cache_entry)
            .is_ok()
        {
            index_entry.accessed_at = now;
            let mut idx = self.index.write().unwrap();
            idx.insert(key.to_string(), index_entry);
        }

        self.stats.hits.fetch_add(1, Ordering::Relaxed);
        Some(cache_entry.result)
    }

    fn write_cache_entry_atomically(
        &self,
        path: &Path,
        entry: &CacheEntry,
    ) -> Result<u64, Box<dyn std::error::Error>> {
        let parent = path.parent().ok_or("missing cache parent directory")?;
        fs::create_dir_all(parent)?;

        let tmp_path = path.with_extension(format!("{}.tmp", std::process::id()));
        let payload = serde_json::to_vec_pretty(entry)?;
        {
            let mut f = fs::File::create(&tmp_path)?;
            f.write_all(&payload)?;
            f.flush()?;
        }
        fs::rename(&tmp_path, path)?;
        Ok(payload.len() as u64)
    }

    fn store(&self, key: &str, result: CacheResult) -> Result<(), Box<dyn std::error::Error>> {
        if !self.enabled {
            return Ok(());
        }

        let now = Self::now_secs();
        let entry = CacheEntry {
            key: key.to_string(),
            result,
            compiler_version: self.compiler_version.clone(),
            solver_version: self.solver_version.clone(),
            created_at: now,
            accessed_at: now,
            access_count: 1,
        };

        let path = Self::key_path(&self.root_dir, key);
        let size = self.write_cache_entry_atomically(&path, &entry)?;
        {
            let mut idx = self.index.write().unwrap();
            idx.insert(
                key.to_string(),
                CacheIndexEntry {
                    path,
                    size,
                    accessed_at: now,
                },
            );
        }
        self.stats.stores.fetch_add(1, Ordering::Relaxed);
        self.evict_if_needed()?;
        Ok(())
    }

    fn evict_if_needed(&self) -> Result<(), Box<dyn std::error::Error>> {
        if !self.enabled {
            return Ok(());
        }

        let mut idx = self.index.write().unwrap();
        let mut total_size: u64 = idx.values().map(|v| v.size).sum();
        if total_size <= self.max_size_bytes {
            return Ok(());
        }

        let mut keys: Vec<(String, u64)> = idx
            .iter()
            .map(|(k, v)| (k.clone(), v.accessed_at))
            .collect();
        keys.sort_by_key(|(_, ts)| *ts);

        for (key, _) in keys {
            if total_size <= self.max_size_bytes {
                break;
            }
            if let Some(v) = idx.remove(&key) {
                total_size = total_size.saturating_sub(v.size);
                let _ = fs::remove_file(v.path);
                self.stats.evictions.fetch_add(1, Ordering::Relaxed);
            }
        }

        Ok(())
    }

    fn snapshot(&self) -> CacheStatsSnapshot {
        self.stats.snapshot()
    }

    fn hit_count(&self) -> usize {
        self.stats.hits.load(Ordering::Relaxed)
    }
}

pub struct CacheManager {
    inner: VerificationCache,
}

impl CacheManager {
    pub fn new(options: &CacheRuntimeOptions) -> Result<Self, Box<dyn std::error::Error>> {
        let solver_version = detect_boogie_solver_version();
        let cache = VerificationCache::new(
            options,
            env!("CARGO_PKG_VERSION").to_string(),
            solver_version,
        )?;
        Ok(Self { inner: cache })
    }

    pub fn key_for_program(
        &self,
        cfg_program: &cfg::Program,
        boogie_program: &BoogieProgram,
        loop_unroll: u32,
        timeout_secs: u32,
    ) -> String {
        format!(
            "top:f{}:h{}:b{}:{}:u{}:t{}:{}",
            cfg_program.functions.len(),
            cfg_program.hops.len(),
            cfg_program.basic_blocks.len(),
            boogie_program.name,
            loop_unroll,
            timeout_secs,
            boogie_fingerprint(boogie_program)
        )
    }

    pub fn lookup(&self, key: &str) -> Option<CacheResult> {
        self.inner.get(key)
    }

    pub fn store(&self, key: &str, result: CacheResult) {
        let _ = self.inner.store(key, result);
    }

    pub fn stats(&self, enabled: bool) -> Option<CacheStatsSnapshot> {
        if enabled {
            Some(self.inner.snapshot())
        } else {
            None
        }
    }

    pub fn hit_count(&self) -> usize {
        self.inner.hit_count()
    }
}

fn deterministic_boogie_repr(program: &BoogieProgram) -> String {
    let mut out = String::new();
    out.push_str(&format!("program:{}\n", program.name));

    let mut gvars: Vec<_> = program.global_vars.iter().collect();
    gvars.sort_by(|a, b| a.0.cmp(b.0));
    for (name, decl) in gvars {
        out.push_str(&format!(
            "g:{}:{:?}:{}\n",
            name, decl.var_type, decl.is_const
        ));
    }

    let mut others = program.other_declarations.clone();
    others.sort();
    for d in others {
        out.push_str(&format!("decl:{}\n", d));
    }

    let mut glits: Vec<_> = program.global_string_literals.iter().collect();
    glits.sort_by(|a, b| a.0.cmp(b.0));
    for (lit, decl) in glits {
        out.push_str(&format!(
            "str:{}:{}:{:?}\n",
            lit, decl.var_name, decl.var_type
        ));
    }

    let mut procedures = program.procedures.clone();
    procedures.sort_by(|a, b| a.name.cmp(&b.name));
    for proc in procedures {
        out.push_str(&format!("proc:{}\n", proc.name));
        for p in proc.params {
            out.push_str(&format!("param:{}:{:?}\n", p.var_name, p.var_type));
        }
        for l in proc.local_vars {
            out.push_str(&format!("local:{}:{:?}\n", l.var_name, l.var_type));
        }
        let mut modifies = proc.modifies.clone();
        modifies.sort();
        for m in modifies {
            out.push_str(&format!("mod:{}\n", m));
        }
        for line in proc.lines {
            out.push_str(&format!("line:{:?}\n", line));
        }
    }

    out
}

fn normalize_identifiers(s: &str) -> String {
    let mut map: HashMap<String, usize> = HashMap::new();
    let keywords: HashSet<&'static str> = [
        "var",
        "const",
        "procedure",
        "modifies",
        "assert",
        "assume",
        "havoc",
        "goto",
        "if",
        "else",
        "call",
        "type",
        "function",
        "axiom",
        "int",
        "real",
        "bool",
        "true",
        "false",
    ]
    .into_iter()
    .collect();

    let mut out = String::with_capacity(s.len());
    let mut token = String::new();
    let flush_token = |tok: &mut String, out_s: &mut String, m: &mut HashMap<String, usize>| {
        if tok.is_empty() {
            return;
        }
        if keywords.contains(tok.as_str()) || tok.chars().all(|c| c.is_ascii_digit()) {
            out_s.push_str(tok);
        } else {
            let id = if let Some(v) = m.get(tok) {
                *v
            } else {
                let v = m.len();
                m.insert(tok.clone(), v);
                v
            };
            out_s.push_str(&format!("id{}", id));
        }
        tok.clear();
    };

    for ch in s.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            token.push(ch);
        } else {
            flush_token(&mut token, &mut out, &mut map);
            out.push(ch);
        }
    }
    flush_token(&mut token, &mut out, &mut map);
    out
}

fn boogie_fingerprint(program: &BoogieProgram) -> String {
    let deterministic = deterministic_boogie_repr(program);
    let normalized = normalize_identifiers(&deterministic);
    let mut hasher = Sha256::new();
    hasher.update(normalized.as_bytes());
    format!("{:x}", hasher.finalize())
}

fn detect_boogie_solver_version() -> String {
    let output = Command::new("boogie").arg("/version").output();
    match output {
        Ok(out) => {
            let text = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if text.is_empty() {
                "unknown".to_string()
            } else {
                text
            }
        }
        Err(_) => "unknown".to_string(),
    }
}
