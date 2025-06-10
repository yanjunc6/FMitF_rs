# FMitF: Formal Methods in Transaction Framework

A comprehensive framework for verifying *serializability* in distributed transaction systems where transactions are "chopped" into multiple *hops* across different nodes. This tool provides formal verification capabilities using Boogie to ensure that chopped transactions maintain ACID properties under concurrent execution.

## Overview

This project addresses the challenge of verifying serializability in distributed systems where:
- **Transactions are decomposed** into sequential hops executing on different nodes
- **Each hop runs atomically** on its designated node under a lock-based concurrency control
- **Multiple transactions** can execute concurrently, with hops potentially interleaving

The framework includes a domain-specific language (DSL) for expressing chopped transactions, static analysis tools for conflict detection, and formal verification using the Boogie verification engine.

## Key Features

- 🔍 **Multi-stage Analysis Pipeline**: AST parsing → CFG construction → Optimization → Conflict analysis <-> Formal verification
- 🌐 **Distributed Transaction Modeling**: Support for multi-node, hop-based transaction definitions
- 🔒 **Concurrency Analysis**: Automated detection of read-write, write-write conflicts between transaction hops
- ✅ **Formal Verification**: Translation to Boogie for automated serializability proofs
- 📊 **Visualization**: DOT format output for conflict graphs and control flow visualization
- 🛠️ **Developer Tools**: Comprehensive CLI with multiple output formats and debugging capabilities

## Concurrency Model

### Hop-Level Locking
- Each hop acquires an exclusive lock on its target node
- Locks are held for the duration of the hop execution
- Locks are released upon hop completion before proceeding to the next hop

### Transaction Execution Model
- Transactions consist of ordered sequences of hops: `Hop₁ → Node₁, Hop₂ → Node₂, ...`
- Node locks operate independently (no global coordination)
- Concurrent transactions can interleave at hop boundaries

### Verification Guarantees
Our verification proves **conflict serializability**: for any concurrent execution of chopped transactions, the final database state is equivalent to some serial execution of the same transactions.

## Installation

### Prerequisites

- **Rust** (1.70 or later): [Install Rust](https://rustup.rs/)
- **Boogie** (for verification): [Install Boogie](https://github.com/boogie-org/boogie)
  ```bash
  # Example installation via .NET
  dotnet tool install --global Boogie
  ```

### Building from Source

1. **Clone the repository:**
   ```bash
   git clone https://github.com/yanjunc6/FMitF_rs.git
   cd FMitF_rs
   ```

2. **Build the project:**
   ```bash
   cargo build
   ```

3. **Install system-wide (optional):**
   ```bash
   cargo install --path .
   ```

### Verify Installation

```bash
# Run with cargo
cargo run -- --help

# Or if installed system-wide
fmitf --help
```

## DSL Language Reference

The framework uses a custom DSL for defining chopped transactions. Here's the basic syntax:

### Node and Table Definitions

```rust
// Define distributed nodes
nodes {
    NodeA, NodeB, NodeC
}

// Define tables with schemas
table Account on NodeA {
    primary int id;      // Primary key (one primary key only)
    int balance;
    string owner;
}

table AuditLog on NodeB {
    primary int transaction_id;
    string description;
    int amount;
}
```

### Transaction Functions

```rust
// Transaction function with multiple hops
void transfer(int from_id, int to_id, int amount) {
    // Hop 1: Debit source account
    hop on NodeA {
        int current_balance = Account[id: from_id].balance;
        Account[id: from_id].balance = current_balance - amount;
    }
    
    // Hop 2: Log transaction
    hop on NodeB {
        AuditLog[transaction_id: from_id].amount = amount;
        AuditLog[transaction_id: from_id].description = "transfer";
    }
    
    // Hop 3: Credit destination account
    hop on NodeA {
        int current_balance = Account[id: to_id].balance;
        Account[id: to_id].balance = current_balance + amount;
    }
}
```

### Supported Data Types

- `int`: Integer values
- `float`: Floating-point numbers
- `string`: Text strings
- `bool`: Boolean values (`true`/`false`)
- `void`: Function return type (no return value)

### Control Flow

```rust
void conditional_update(int account_id, int threshold) {
    hop on NodeA {
        int balance = Account[id: account_id].balance;
        
        if (balance > threshold) {
            Account[id: account_id].balance = balance * 2;
        }
        
        while (balance > 0) {
            balance = balance - 1;
            // Some operation
        }
    }
}
```

## Usage

### Command Line Interface

The tool is invoked via the command-line interface:

```bash
# Basic syntax
cargo run -- <input_file> [options]

# Or if installed
fmitf <input_file> [options]
```

### Processing Modes

The tool supports multiple analysis modes, each building on the previous stages:

#### 1. AST Mode
Parse source code and display the Abstract Syntax Tree:

```bash
# Parse and show AST
cargo run -- examples/number_commute.transact --mode ast

# Save AST to file
cargo run -- examples/bank.transact --mode ast --output ast_output.txt

# Include source code spans
cargo run -- examples/transfer.transact --mode ast --show-spans
```

#### 2. CFG Mode
Build Control Flow Graphs for transaction functions:

```bash
# Generate CFG
cargo run -- examples/transfer.transact --mode cfg

# Export CFG as DOT format for visualization
cargo run -- examples/bank.transact --mode cfg --dot --output cfg.dot

# Visualize with Graphviz
dot -Tpng cfg.dot -o cfg.png
```

#### 3. Optimize Mode
Apply optimization passes to the control flow graphs:

```bash
# Run optimization passes
cargo run -- examples/warehouse.transact --mode optimize

# Skip optimizations
cargo run -- examples/simple.transact --mode optimize --no-optimize
```

#### 4. SCGraph Mode
Build and analyze Serializability Conflict Graphs:

```bash
# Generate conflict graph analysis
cargo run -- examples/conflict.transact --mode scgraph

# Export as DOT for visualization
cargo run -- examples/number_conflict.transact --mode scgraph --dot --output conflict.dot

# Verbose output with detailed conflict information
cargo run -- examples/bank.transact --mode scgraph --verbose
```

#### 5. Verify Mode (Default)
Run formal verification using Boogie:

```bash
# Run verification
cargo run -- examples/number_commute.transact --mode verify

# Save generated Boogie files for inspection
cargo run -- examples/transfer.transact --mode verify --output-dir ./boogie_files/

# Verbose verification with detailed output
cargo run -- examples/bank.transact --mode verify --verbose --output-dir ./verification/

# Custom verification timeout
cargo run -- examples/complex.transact --mode verify --timeout 60
```

### Common Options

- `-v, --verbose`: Enable detailed output and debugging information
- `-q, --quiet`: Suppress non-essential output
- `-o, --output <PATH>`: Specify output file or directory
- `--output-dir <DIR>`: Directory for Boogie files (verify mode only)
- `--dot`: Generate DOT format output for graph visualization
- `--timeout <SECONDS>`: Verification timeout (default: 30 seconds)
- `--no-optimize`: Skip optimization passes
- `--show-spans`: Include source code location information

### Example Workflows

#### Basic Verification Workflow

```bash
cargo run -- examples/transfer.transact --mode verify --output-dir ./results/

# Examine generated Boogie files (if needed)
ls ./results/
cat ./results/edge_*.bpl
```

#### Visualization Workflow

```bash
# Generate all visualizations
mkdir -p visualizations

# CFG visualization
cargo run -- examples/bank.transact --mode cfg --dot --output visualizations/cfg.dot
dot -Tpng visualizations/cfg.dot -o visualizations/cfg.png

# Conflict SC-graph visualization  
cargo run -- examples/bank.transact --mode scgraph --dot --output visualizations/conflicts.dot
dot -Tpng visualizations/conflicts.dot -o visualizations/conflicts.png

# View results
open visualizations/*.png  # macOS
xdg-open visualizations/*.png  # Linux
```

## Examples

The `examples/` directory contains several transaction scenarios:

- **`number_commute.transact`**: Simple commutative operations (increment counters)
- **`number_conflict.transact`**: Non-commutative operations showing conflicts
- **`bank.transact`**: Banking operations with account transfers
- **`transfer.transact`**: Multi-hop money transfer with audit logging
- **`warehouse.transact`**: Inventory management transactions
- **`simple.transact`**: Basic single-hop transactions
- **`conflict.transact`**: Example demonstrating conflict detection

## Output Formats

### AST Output
Human-readable program structure showing parsed syntax tree.

### CFG Output
Control flow information for each transaction function.

### SCGraph Output
Conflict analysis showing:
- **S-edges**: Sequential dependencies within transactions
- **C-edges**: Conflict dependencies between transactions
- **Mixed cycles**: Potential serializability violations

### Verification Output
- Verification results (pass/fail)
- Performance metrics
- Generated Boogie files (`.bpl` format)
- Detailed conflict resolution information

## Development

### Project Structure

```
src/
├── ast/           # Abstract Syntax Tree and parsing
├── cfg/           # Control Flow Graph construction
├── cli/           # Command-line interface
├── dataflow/      # Dataflow analysis algorithms
├── optimization/  # CFG optimization passes
├── pretty/        # Output formatting and printing
├── sc_graph/      # Serializability Conflict Graph
└── verification/  # Boogie code generation and verification
```