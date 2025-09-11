# FMitF CLI Design Document

## Overview

FMitF (Formal Methods in Transaction Framework) is a compiler for distributed transaction verification. The CLI interface processes `.transact` files through a 5-stage compilation pipeline to verify serializability in distributed "chopped" transactions.

## Design Philosophy

### Simplicity Over Flexibility
- **Single Purpose**: The tool has one job - compile and verify transaction serializability
- **No Mode Selection**: Remove complex `--mode` flags; always run the complete pipeline
- **Sensible Defaults**: Minimize required flags while providing comprehensive output options

### Flexible Output System
- **Multiple Output Formats**: Support console, file, and directory output for the same information
- **Output Selection**: Users choose what information they want (logs, results, artifacts)
- **Structured Results**: Organize compiler artifacts in predictable, explorable formats

### Comprehensive Logging
- **Detailed Debug Logs**: Capture every step of the compilation process
- **Structured Information**: Separate concerns between results, artifacts, and debug information
- **Persistent Records**: Save compilation details for later analysis

## Compilation Pipeline

The tool processes input through these fixed stages:

1. **AST** - Parse and analyze source code (grammar parsing, name resolution, semantics)
2. **CFG** - Build Control Flow Graphs with hop-based execution model
3. **Optimization** - Apply dataflow optimizations (constant propagation, dead code elimination, CSE)
4. **SC-Graph** - Build Serializability Conflict Graph with S-edges and C-edges
5. **Verification** - Generate and execute Boogie proofs for C-edge commutativity

## Command Line Interface

### Basic Usage

```bash
# Compile and verify a transaction file
fmitf input.transact

# Save results to a directory
fmitf input.transact --output results/

# Generate specific artifacts
fmitf input.transact --boogie --dot --output-dir artifacts/

# Verbose logging with debug information
fmitf input.transact --verbose --log-file debug.log
```

### Core Arguments

**Positional Arguments:**
- `<INPUT>` - Input `.transact` file (required)

**Output Control:**
- `--output <PATH>` - Output destination (file or directory)
- `--output-dir <DIR>` - Force directory output mode
- `--console` - Output summary to console (default when no output specified)

**Artifact Generation:**
- `--boogie` - Generate Boogie verification files (`.bpl`)
- `--dot` - Generate GraphViz files (`.dot`) for CFG and SC-Graph
- `--summary` - Generate compilation summary (`.md`)
- `--all-artifacts` - Generate all available artifacts

**Logging and Verbosity:**
- `--verbose, -v` - Detailed progress information
- `--quiet, -q` - Minimal output, errors only
- `--log-file <FILE>` - Save detailed debug log to file
- `--no-log` - Disable debug logging entirely

**Verification Options:**
- `--timeout <SECONDS>` - Boogie verification timeout (default: 30)
- `--no-optimize` - Skip optimization passes
- `--keep-temps` - Keep temporary verification files

**Display Options:**
- `--no-color` - Disable colored output
- `--json` - Output results in JSON format
- `--plain` - Plain text output (no formatting)

### Output Modes

#### Console Mode (Default)
When no output is specified, display a concise summary:

```
✓ Compilation successful
  AST     : 15 functions, 8 tables, 3 partitions
  CFG     : 45 basic blocks, 12 optimization passes applied
  SC-Graph: 23 nodes, 34 S-edges, 8 C-edges  
  Verify  : 8/8 C-edges proven commutative (100%)
  Result  : No mixed cycles - system is serializable
```

#### File Mode
When `--output <file>` is specified, save the primary result to a single file:

```bash
# Save SC-Graph as DOT file
fmitf input.transact --dot --output graph.dot

# Save verification summary as markdown
fmitf input.transact --summary --output report.md

# Save compilation log
fmitf input.transact --verbose --output log.txt
```

#### Directory Mode  
When `--output-dir <dir>` is specified, create a structured output directory:

```
results/
├── input.transact          # Original source file (copied)
├── compilation.log         # Detailed debug log
├── summary.md             # Human-readable compilation report
├── ast/
│   └── program.txt        # AST representation
├── cfg/
│   ├── program.txt        # CFG text representation
│   └── program.dot        # CFG GraphViz (if --dot)
├── scgraph/
│   ├── graph.txt          # SC-Graph text representation  
│   └── graph.dot          # SC-Graph GraphViz (if --dot)
├── verification/
│   ├── results.json       # Verification results
│   ├── boogie/           # Boogie files (if --boogie)
│   │   ├── edge_1_2.bpl
│   │   └── edge_3_4.bpl
│   └── failed/           # Failed verification details
└── artifacts/            # Additional artifacts
    └── metadata.json     # Compilation metadata
```

### Logging System

#### Log Levels
- **Error**: Critical failures that stop compilation
- **Warning**: Non-fatal issues that may affect results
- **Info**: Major compilation stages and results
- **Debug**: Detailed step-by-step process information
- **Trace**: Internal data structures and transformations

#### Log Output
- **Console**: Respects `--verbose`/`--quiet` flags
- **File**: Always captures full debug information when `--log-file` specified
- **Directory**: Automatically saves `compilation.log` in directory mode

#### Log Format
```
[2024-08-04 15:23:45] INFO  Stage 1/5: Frontend Analysis
[2024-08-04 15:23:45] DEBUG   Parsing input.transact (1,234 bytes)
[2024-08-04 15:23:45] DEBUG   Found 15 function definitions
[2024-08-04 15:23:45] DEBUG   Found 8 table definitions  
[2024-08-04 15:23:45] DEBUG   Found 3 partition functions
[2024-08-04 15:23:45] DEBUG   Running name resolution...
[2024-08-04 15:23:45] DEBUG   Running semantic analysis...
[2024-08-04 15:23:45] INFO  ✓ Frontend Analysis complete (142ms)
```

## Error Handling

### Compilation Errors
Show contextual error information with source locations:

```
ERROR: Undefined variable 'account_id' in function 'transfer'
  --> input.transact:15:12
   |
15 |     balance = account_id.balance + amount;
   |               ^^^^^^^^^^
   |
help: Did you mean 'account.id'?
```

### Verification Failures
Provide actionable information about failed proofs:

```
VERIFICATION FAILED: Edge transfer_hop1 → balance_check_hop2
  Reason: Non-commutative operations detected
  Details: Write-write conflict on table 'Account' field 'balance'
  
  Boogie output saved to: verification/failed/edge_1_2.bpl
  
  Suggestion: Consider adding explicit ordering constraints
```

### Runtime Errors
Handle infrastructure failures gracefully:

```
ERROR: Boogie verifier not found
  Expected: boogie.exe in PATH or /usr/local/bin/boogie
  
  Install Boogie from: https://github.com/boogie-org/boogie
  Or set BOOGIE_PATH environment variable
```

## Flag Design Principles

### Consistency
- Use standard UNIX conventions (`-v`/`--verbose`, `-q`/`--quiet`)
- Follow GNU long option style (`--output-dir`, not `--outputdir`)
- Prefer explicit over implicit behavior

### Composability
```bash
# These combinations should work intuitively
fmitf input.transact --verbose --dot --boogie --output-dir results/
fmitf input.transact --quiet --json --output summary.json
fmitf input.transact --log-file debug.log --no-color --timeout 60
```

### Future-Proofing
- Reserve short flags for most common options
- Use consistent naming patterns for related features
- Design flags to be additive, not conflicting

### Validation
```bash
# These should produce helpful error messages
fmitf input.transact --output file.txt --output-dir dir/  # Conflicting output modes
fmitf input.transact --boogie                           # Missing output destination  
fmitf nonexistent.transact                              # File not found
```

## Implementation Plan

### Phase 1: Core Interface
1. Remove pipeline modes and `Mode` enum
2. Implement new argument parsing with clap
3. Create flexible output system with console/file/directory modes
4. Implement comprehensive logging with file output

### Phase 2: Output Formats
1. Implement structured directory output
2. Add JSON output format for machine consumption
3. Create markdown summary generator
4. Enhance DOT file generation

### Phase 3: Error Handling
1. Improve error messages with source context
2. Add verification failure analysis
3. Implement helpful suggestions and recovery

### Phase 4: Quality of Life
1. Add configuration file support
2. Implement shell completion
3. Add progress indicators for long operations
4. Create comprehensive documentation and examples

## Configuration

### Environment Variables
- `FMITF_BOOGIE_PATH` - Path to Boogie verifier executable
- `FMITF_LOG_LEVEL` - Default logging level (error/warn/info/debug/trace)
- `FMITF_NO_COLOR` - Disable colored output (same as `--no-color`)
- `FMITF_TIMEOUT` - Default verification timeout

### Configuration File
Support `.fmitf.toml` for project-specific settings:

```toml
[verification]
timeout = 60
keep_temps = true

[output]
default_artifacts = ["boogie", "dot", "summary"]
log_level = "debug"

[optimization]
enable_passes = ["constant_prop", "dead_code", "cse"]
```

## Examples

### Basic Usage
```bash
# Quick verification
fmitf examples/tpcc_distributed.transact

# Development workflow with debugging
fmitf src/transfer.transact --verbose --log-file debug.log --output-dir build/

# CI/CD integration  
fmitf *.transact --quiet --json --output results.json
```

### Advanced Usage
```bash
# Full artifact generation
fmitf complex.transact --all-artifacts --output-dir analysis/ --verbose

# Performance analysis
fmitf slow.transact --no-optimize --timeout 120 --log-file perf.log

# Research/debugging
fmitf test.transact --boogie --keep-temps --dot --output-dir debug/ -v
```

### Integration Examples
```bash
# Shell scripting
for file in tests/*.transact; do
    echo "Verifying $file..."
    fmitf "$file" --quiet || echo "FAILED: $file"
done

# Build systems (Makefile)
%.verified: %.transact
    fmitf $< --output-dir build/$(basename $<)/ --summary --log-file build/$(basename $<).log

# CI Pipeline
fmitf src/**/*.transact --json --output results.json --log-file build.log
```

This design creates a focused, predictable tool that does one thing well while providing flexible output options for different use cases, from quick development feedback to comprehensive analysis and CI integration.
