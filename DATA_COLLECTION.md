# Data Collection System - Implementation Summary

## Overview
Implemented a comprehensive data collection and benchmarking system that tracks detailed verification metrics including execution time, memory usage, and Boogie outputs. All data is exported to `data.json` for external analysis and visualization.

## Architecture & Design Patterns

### 1. **Separation of Concerns**
- **`src/cli/data.rs`**: New module handling all data collection, storage, and export
- **`src/cli/compiler.rs`**: Refactored to use data collector, focusing on compilation orchestration
- **`src/pretty/sc_graph_dot_printer.rs`**: Updated to consume the new data structures

### 2. **Builder Pattern**
The `DataCollector` class uses a builder-like pattern for incremental data collection:
```rust
let mut data_collector = DataCollector::new(input_file_name, instances);
data_collector.set_config(loop_unroll, timeout_secs);
data_collector.set_program_stats(func_count, hop_count);
data_collector.set_sc_stats(sc_c_edges, simplified_sc_c_edges);
data_collector.add_c_edge_verification(edge_data);
```

### 3. **Strategy Pattern**
Verification result handling uses an enum strategy:
```rust
pub enum VerificationResult {
    Pass,       // UNSAT - no counterexample
    Error,      // SAT - counterexample found
    Timeout,    // Verification timed out
    CompilationError, // Boogie compile failed
}
```

### 4. **Data Transfer Objects (DTOs)**
Clear separation between internal tracking and output format:
- `CEdgeVerificationData`: Complete per-edge data with all metrics
- `BenchmarkSummary`: Aggregated statistics
- `BenchmarkData`: Complete output structure for JSON export
- `MemoryStats`: Structured memory/resource metrics

## Key Features Implemented

### 1. Memory Statistics Collection
Uses `/usr/bin/time -v` to track:
- Maximum resident set size (KB)
- Page faults (minor/major)
- Context switches (voluntary/involuntary)
- CPU usage percentage
- User/system time
- Elapsed wall-clock time

### 2. Complete Verification Data
Each C-edge verification records:
- Source/target node IDs (function, instance, hop)
- Wall-clock duration (milliseconds)
- Verification result (Pass/Error/Timeout/CompilationError)
- Elimination status (removed by verification or retained)
- Full memory statistics
- Complete Boogie stdout/stderr
- Boogie filename

### 3. Robust Error Handling
- **Critical**: Ignores stderr and exit codes from `/usr/bin/time`
- Boogie errors detected only from stdout content
- Proper parsing of `/usr/bin/time -v` output from stderr
- Graceful handling of missing data fields

### 4. JSON Export
Complete data exported to `data.json` with structure:
```json
{
  "summary": { /* aggregated statistics */ },
  "c_edge_verifications": [ /* per-edge data */ ],
  "timestamp": "ISO-8601 timestamp",
  "input_file": "source filename"
}
```

## Code Organization

### File Structure
```
src/cli/
├── compiler.rs      (refactored - orchestration only)
├── data.rs          (new - data collection & export)
├── log.rs           (existing - logging)
├── stage.rs         (existing - stage execution)
└── mod.rs           (updated - exports data module)
```

### Key Methods in `compiler.rs`

#### `run_verification_stage()`
Separated verification logic for clarity:
1. Generate Boogie programs
2. Run verifications in parallel
3. Process results
4. Collect data
5. Mark eliminated edges
6. Return simplified SC-graph

#### `run_boogie_verifications_parallel()`
Handles parallel execution with `/usr/bin/time`:
- Uses `rayon` for parallel processing
- Progress bar with `indicatif`
- Returns structured `BoogieRunResult` objects
- Parses memory stats from stderr

#### `process_boogie_result()`
Determines verification outcome:
- Checks stdout for compilation errors
- Detects timeouts
- Parses S-expressions for verification errors
- Returns `VerificationResult` enum

### Key Methods in `data.rs`

#### `parse_time_output()`
Parses `/usr/bin/time -v` output:
- Handles various field formats
- Parses elapsed time strings (h:mm:ss or m:ss)
- Returns `MemoryStats` structure
- Gracefully handles missing fields

#### `DataCollector` API
Public methods following clear naming convention:
- `set_*()`: Set configuration or statistics
- `add_*()`: Add individual data points
- `mark_*()`: Update existing data
- `write_to_file()`: Export to JSON
- `plot_*()`: Generate visualizations
- `format_*()`: Display formatting

## Testing & Validation

### Test Results (social.transact)
- ✅ 128 verifications tracked
- ✅ All memory statistics captured
- ✅ All Boogie outputs recorded
- ✅ Eliminated edge status correct (10 eliminated, 118 remaining)
- ✅ JSON export successful (223 KB)
- ✅ Histogram and plotted SC-graph generated

### Data Completeness
- All verifications have memory stats
- All verifications have Boogie output
- Timestamps in ISO-8601 format
- Proper serialization/deserialization

## Usage

### Running with Data Collection
```bash
cargo run ./examples/social.transact
```

### Output Files
- `tmp/data.json` - Complete benchmark data
- `tmp/verification_time_histogram.png` - Timing histogram
- `tmp/sc_graph_plotted.dot` - SC-graph with timing colors
- `tmp/compiler.log` - Detailed logs

### Analyzing Data
```python
import json

data = json.load(open('tmp/data.json'))

# Access summary
print(f"Total verifications: {data['summary']['verification_total']}")
print(f"Pass rate: {data['summary']['verification_pass'] / data['summary']['verification_total']:.1%}")

# Analyze per-edge data
for v in data['c_edge_verifications']:
    if v['result'] == 'Timeout':
        print(f"Timeout: {v['boogie_file']}")
        
# Memory analysis
max_memory = max(v['memory_stats']['max_rss_kb'] for v in data['c_edge_verifications'])
print(f"Peak memory: {max_memory / 1024:.1f} MB")
```

## Future Enhancements

### Potential Improvements
1. **Additional Metrics**: Track Boogie SMT solver time separately
2. **Aggregations**: Pre-compute percentiles, averages in summary
3. **Compression**: Optional gzip compression for large datasets
4. **Incremental**: Support appending to existing data files
5. **Filtering**: Export filtered subsets of verifications
6. **Comparison**: Compare multiple runs side-by-side

### External Analysis Tools
The JSON format enables:
- Jupyter notebooks for analysis
- Custom plotting with matplotlib/seaborn
- Statistical analysis with pandas
- Integration with CI/CD pipelines
- Long-term performance tracking

## Design Principles Applied

1. **Single Responsibility**: Each module has one clear purpose
2. **Open/Closed**: Extensible without modifying existing code
3. **Dependency Inversion**: Compiler depends on abstractions (DataCollector interface)
4. **Don't Repeat Yourself**: Parsing and formatting logic centralized
5. **Fail-Safe Defaults**: Missing data doesn't prevent export

## Conclusion

The new data collection system provides comprehensive benchmarking capabilities while maintaining clean architecture and code organization. The use of `/usr/bin/time -v` ensures accurate memory tracking, and the JSON export enables powerful external analysis workflows.
