# FMitF Copilot Instructions

## Project Overview

FMitF (Formal Methods in Transaction Framework) is a Rust-based framework for verifying serializability in distributed "chopped" transactions. Key characteristics:

- **Domain**: Distributed transaction verification using formal methods
- **Architecture**: Multi-stage compilation pipeline: AST → CFG → SC-Graph (Optimization currently disabled, Verification removed)
- **DSL**: Custom transaction language (.transact files) parsed via Pest grammar
- **Core Goal**: Analyze concurrent chopped transactions for conflict serializability with **multiple transaction instances**

## Architecture & Data Flow

### 4-Stage Compilation Pipeline (src/cli/compiler.rs)
1. **AST Stage**: Parse .transact files using Pest grammar (src/ast/grammar.pest)
2. **CFG Stage**: Convert AST to Control Flow Graphs with Hop-based execution model
3. **Optimize Stage**: Apply dataflow optimizations (currently skipped - not yet implemented)
4. **SC-Graph Stage**: Build Serializability Conflict Graph with S-edges (sequential) and C-edges (conflict) for **multiple transaction instances**

**Note**: Verification stage has been removed from the current implementation

### Key Data Structures
- **CfgProgram**: Never clone - use Arena<T> for memory-efficient storage
- **HopId/FunctionId/TableId**: ID-based references via id-arena crate
- **SCGraph**: Contains directed S-edges (sequential within transaction instances) and undirected C-edges (conflicts between transaction instances)
- **SCGraphNodeId**: Uniquely identifies nodes with `{function_id, hop_id, instance}` triple
- **Transaction Instances**: Each transaction function can have multiple concurrent instances with different parameters

## DSL Language (.transact files)

### Transaction Functions vs Partitions
```rust
// Partition function (always returns int, used for table placement)
partition node_selector(int x, int y) = 10 * x + y;

// Transaction function (main business logic)
void transfer(int from_id, int to_id, int amount) {
    hop { /* Hop 1: atomically executed on one node */ }
    hop { /* Hop 2: atomically executed on another node */ }
}
```

### Tables with Node Placement
```rust
table Account {
    primary int id;        // Primary key
    node node_selector(id, 0);  // Partition assignment
    int balance;
    string owner;
}
```

## Development Patterns

### Code Organization & Module Structure
- **mod.rs files are header-like**: Contains ONLY type definitions, structs, enums, and trait declarations
- **NEVER put impl blocks in mod.rs** - all implementations go in separate files
- Example: `src/cfg/mod.rs` defines `CfgProgram`, `HopId`, etc. but `CfgBuilder` impl is in `cfg_builder.rs`
- Each module's mod.rs acts like a C header file - pure interface definitions

### Function Design Philosophy
- **Avoid trivial constructor functions**: Don't create `new()`, `create()`, `is_valid()` patterns for simple cases
- **Avoid simple predicate functions**: Don't create `is_empty()`, `is_ready()` if it's just a field comparison
- **Inline simple checks**: `if node.status == NodeStatus::Ready` instead of `if node.is_ready()`
- **Name complex functions descriptively**: Use algorithm names or descriptive names that indicate computational cost
- **Minimize parameter passing**: Don't pass 5+ arguments to avoid "elegant" constructors

### Arena-Based Memory Management
- Use `id_arena::Arena<T>` for large collections (tables, functions, variables)
- Pass around IDs (`TableId`, `HopId`) instead of references
- Never clone `CfgProgram` or `FunctionCfg` - they contain Arenas

### CFG Construction (src/cfg/cfg_builder.rs)
- Convert AST transactions into basic blocks
- Each hop becomes a sequence of basic blocks
- Use `ControlFlowEdge` with `EdgeType` for all control flow
- If any place says it is SSA, delete this, stop working on the task, start check whether anything is wrong.

### Dataflow Analysis (src/dataflow/)
- Hop-level analysis for table modification/reference tracking
- Liveness analysis for variable dependencies
- Results stored in `DataflowResults` with entry/exit states per basic block

### SC-Graph Building (src/sc_graph/)
- **Multiple Transaction Instances**: Each transaction function creates multiple concurrent instances (configurable via `--instances` CLI flag)
- **Node Structure**: `SCGraphNodeId {function_id, hop_id, instance}` uniquely identifies each hop instance
- **S-edges**: Connect sequential hops within the **same transaction instance** only
- **C-edges**: Connect hops from **any transaction instances** that access same tables (RW or WW conflicts)
- **Instance Handling**: 
  - S-edges only within same instance: `transfer_inst1_hop1 → transfer_inst1_hop2`
  - C-edges between any conflicting instances: `transfer_inst1_hop1 ↔ payment_inst2_hop1`
- **Edge Deduplication**: C-edges are undirected, so only one edge per pair is created using `node1_id < node2_id` ordering
- **Builder Pattern**: Use `SCGraphBuilder::new(num_instances).build(cfg_program)` with configurable instance count

### Testing

Create testing case, tmp file and all the output files in the `tmp/` directory.

## CLI & Output System

### Enhanced CLI Interface
```bash
cargo run -- input.transact                          # Default: creates output directory with input filename, 2 instances
cargo run -- input.transact my_output                # Custom output directory
cargo run -- input.transact --instances 3           # Create 3 instances of each transaction
cargo run -- input.transact --instances 1           # Traditional single-instance behavior
cargo run -- input.transact --no-optimize            # Skip optimization passes (currently no-op)
cargo run -- input.transact --no-color               # Disable colored terminal output
```

### CLI Flags
- `--instances N`: Number of instances to create for each transaction function (default: 2)
- `--no-optimize`: Skip optimization passes for debugging
- `--no-color`: Disable colored terminal output

### Output Structure
- **Always runs full pipeline**: No mode selection - processes through all stages
- **Directory output**: Creates structured output directory with:
  - `summary.md`: Compilation report in Markdown format
  - `compilation.log`: Detailed compilation log
  - `scgraph.dot`: GraphViz DOT file for SC-Graph visualization with instance labels
  - `scgraph_pretty.txt`: Human-readable SC-Graph with instance information
  - Console summary displayed during compilation

### Instance Visualization
- **DOT files**: Nodes labeled as `function_name #N` (e.g., `transfer #1`, `transfer #2`)
- **Pretty print**: Shows instance numbers in node and edge descriptions
- **Node naming**: `hop_X_Y_instZ` format for unique identification

## Compilation Process (src/cli/)

### Compiler Architecture
- **Compiler**: Main orchestration struct that runs the 4-stage pipeline
- **Logger**: Simple console output with stage progress tracking
- **OutputManager**: Stream-based output system for console, file, and directory output
- **CompilationResult**: Contains all intermediate products (AST, CFG, SC-Graph) and statistics

### Output Management
- Flexible stream-based approach - can write to any `Write` destination
- Always generates structured directory output with logs and summaries
- Console progress display with colored output (unless `--no-color`)

### File Structure
- No pipeline.rs - compilation is handled directly in compiler.rs
- Removed verification module dependencies
- Simplified CLI argument processing

## Testing & Examples

### Example Files (examples/)
- `test_minimal.transact`: Simple two-function example for testing instances
- `test_1.transact`: Basic transaction example
- `tpcc_distributed.transact`: Complex TPC-C workload with multi-hop transactions

### Testing Instance Functionality
```bash
# Test with different instance counts
cargo run -- examples/test_minimal.transact /tmp/test_1 --instances 1    # 4 nodes, 2 S-edges, 2 C-edges
cargo run -- examples/test_minimal.transact /tmp/test_2 --instances 2    # 8 nodes, 4 S-edges, 12 C-edges
cargo run -- examples/test_minimal.transact /tmp/test_3 --instances 3    # 12 nodes, 6 S-edges, 30 C-edges
```

### Current Status Notes
- AST parsing is stable and compatible with example files
- SC-Graph instance functionality is fully implemented and tested
- CLI redesign is complete with instance configuration
- All output formats support instance visualization

### Runtime REPL (commented out in current version)
- Interactive mode for testing transaction execution
- Table state inspection and function testing

## Common Pitfalls

1. **Don't clone CFG structures** - use ID references
2. **Dataflow analysis order**: Must run after CFG predecessor building
3. **SC-Graph edge direction**: S-edges are directed, C-edges are undirected but stored with consistent ordering
4. **Instance edge creation**: S-edges only within same instance, C-edges between any conflicting instances
5. **C-edge deduplication**: Use `node1_id < node2_id` condition to avoid duplicate undirected edges
6. **Output directory creation**: Always creates structured output with logs and summaries
7. **Transaction vs Partition**: Transactions have hops, partitions are placement functions
8. **Instance numbering**: Display instances starting from 1 (`#1`, `#2`) but store starting from 0

## Key Dependencies
- `pest`: Grammar parsing (see src/ast/grammar.pest)
- `id-arena`: Memory-efficient ID-based storage
- `petgraph`: For potential cycle detection in SC-graphs
- `colored`: Terminal color output (can be disabled with --no-color)
- `clap`: CLI argument parsing with derive features
