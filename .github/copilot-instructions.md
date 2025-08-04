# FMitF Copilot Instructions

## Project Overview

FMitF (Formal Methods in Transaction Framework) is a Rust-based framework for verifying serializability in distributed "chopped" transactions. Key characteristics:

- **Domain**: Distributed transaction verification using formal methods (Boogie)
- **Architecture**: Multi-stage pipeline: AST → CFG → Optimization → SC-Graph → Verification
- **DSL**: Custom transaction language (.transact files) parsed via Pest grammar
- **Core Goal**: Prove that concurrent chopped transactions maintain conflict serializability

## Architecture & Data Flow

### 5-Stage Pipeline (src/cli/pipeline.rs)
1. **AST Stage**: Parse .transact files using Pest grammar (src/ast/grammar.pest)
2. **CFG Stage**: Convert AST to Control Flow Graphs with Hop-based execution model
3. **Optimize Stage**: Apply dataflow optimizations (constant prop, dead code elimination, CSE)
4. **SC-Graph Stage**: Build Serializability Conflict Graph with S-edges (sequential) and C-edges (conflict)
5. **Verification Stage**: Generate Boogie code for C-edges to verify commutativity and prune graph, and do final semantic checks

### Key Data Structures
- **CfgProgram**: Never clone - use Arena<T> for memory-efficient storage
- **HopId/FunctionId/TableId**: ID-based references via id-arena crate
- **SCGraph**: Contains directed S-edges (sequential within transactions) and undirected C-edges (conflicts between transactions)

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
- Convert AST transactions into SSA-form basic blocks
- Each hop becomes a sequence of basic blocks
- Use `ControlFlowEdge` with `EdgeType` for all control flow

### Dataflow Analysis (src/dataflow/)
- Hop-level analysis for table modification/reference tracking
- Liveness analysis for variable dependencies
- Results stored in `DataflowResults` with entry/exit states per basic block

### SC-Graph Building (src/sc_graph/)
- S-edges: Connect sequential hops within same transaction
- C-edges: Connect hops from different transactions that access same tables (RW or WW conflicts)
- Use `SCGraphBuilder::build()` - never manually construct

## CLI & Output Modes

### Mode Hierarchy (each includes previous stages)
```bash
cargo run -- input.transact --mode ast      # Parse only
cargo run -- input.transact --mode cfg      # + CFG building  
cargo run -- input.transact --mode optimize # + Optimizations
cargo run -- input.transact --mode scgraph  # + Conflict analysis
cargo run -- input.transact --mode verify   # + Boogie verification (default)
```

### Key Flags
- `--dot`: Generate GraphViz output for CFG/SC-graph visualization
- `--output-dir`: For verify mode - saves generated Boogie files
- `--verbose`: Detailed analysis output
- `--no-optimize`: Skip optimization passes for debugging

## Verification Process (src/verification/)

### Commutativity Checking
1. For each C-edge, create `VerificationUnit` with hop prefixes
2. Generate Boogie code proving hop commutativity
3. Execute Boogie verifier with timeout
4. Remove successful C-edges from SC-graph
5. Final mixed cycles indicate serializability violations

### File Naming Convention
- Boogie files: `edge_{source}_{target}.bpl`
- Temporary files cleaned up automatically
- Use `BoogieFileManager` for file operations

## Testing & Examples

### Example Files (examples/)
- `tpcc_distributed.transact`: Complex TPC-C workload with multi-hop transactions

### Runtime REPL (commented out in current version)
- Interactive mode for testing transaction execution
- Table state inspection and function testing

## Common Pitfalls

1. **Don't clone CFG structures** - use ID references
2. **Dataflow analysis order**: Must run after CFG predecessor building
3. **SC-Graph edge direction**: S-edges are directed, C-edges are undirected but stored with consistent ordering
4. **Boogie output**: Only generated in verify mode with `--output-dir`
5. **Transaction vs Partition**: Transactions have hops, partitions are placement functions

## Key Dependencies
- `pest`: Grammar parsing (see src/ast/grammar.pest)
- `id-arena`: Memory-efficient ID-based storage
- `petgraph`: For potential cycle detection in SC-graphs
- External: Boogie verifier (required for verification mode)
