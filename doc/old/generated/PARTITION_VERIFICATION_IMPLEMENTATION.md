# Partition Verification Implementation

## Overview
This document describes the implementation of the Partition Verification Manager in `src/verification/partition.rs`. The implementation follows the formal verification strategy outlined in `doc/verification.md` section 2 (Property P-1: Correct Node Placement).

## Architecture

### Core Components

#### `PartitionVerificationManager`
Main orchestration struct that handles partition consistency verification for transaction functions.

**Key Methods:**
- `new()` - Creates a new manager instance
- `generate_partition_verification(cfg_program)` - Main entry point that generates Boogie programs for all transaction functions
- `generate_function_partition_verification(cfg_program, function_id)` - Generates verification for a specific function

### Data Structures

#### `PartitionCall`
Represents a table access that uses a partition function:
```rust
struct PartitionCall {
    hop_id: HopId,           // Which hop contains this call
    table_id: TableId,       // Which table is being accessed
    partition_args: Vec<Operand>, // Arguments passed to partition function
}
```

## Implementation Strategy

### 1. CFG Simulation
Translates each transaction function's CFG to pure Boogie code:
- `generate_cfg_simulation()` - Main CFG traversal
- `generate_basic_block_simulation()` - Per-block translation
- `generate_block_control_flow()` - Control flow edge generation

Uses `gen_Boogie.rs` functions extensively:
- `convert_statement()` for statement translation
- `gen_block_label()` for label generation
- `gen_function_start_label()` / `gen_function_end_label()` for function boundaries

### 2. Partition-Call Catalogue
Identifies all table accesses within transaction functions:
- `collect_partition_calls()` - Traverses all hops and basic blocks
- Records both LValue (table writes) and RValue (table reads) accesses
- Groups calls by `(hop_id, table_id)` pairs

### 3. Verification Condition Generation
Implements the consistency check from `verification.md` section 2.2:
- `generate_partition_consistency_checks()` - Main consistency logic
- `generate_consistency_assertion()` - Per-group assertion generation
- `generate_args_equality_condition()` - Argument comparison logic

**Key Insight:** The verification condition `assert !(arg₁ == arg₁' && ... && argₖ == argₖ');` ensures that if the same partition function is called multiple times within a hop, it cannot be called with identical arguments (which would mean the same node is selected multiple times, violating the single-node execution constraint).

## Utilization of gen_Boogie.rs

The implementation leverages `gen_Boogie.rs` extensively rather than generating Boogie code directly:

### Core Infrastructure
- `BoogieProgramGenerator::gen_base_program()` - Base program setup
- `gen_string_axioms()` - String handling axioms
- `gen_global_constants()` / `gen_table_variables()` - Global setup

### Statement Translation  
- `convert_statement()` - CFG statements to Boogie
- `convert_operand()` - Operand conversion with prefix support
- `convert_rvalue()` / `convert_assignment()` - Expression handling

### Naming & Labeling
- `gen_var_name()` - Variable name generation with prefixes
- `gen_block_label()` - Basic block labels
- `gen_table_field_var_name()` - Table field variable names
- `gen_procedure_params()` - Procedure parameter lists

### Utility Functions
- `add_comment()` / `add_assertion()` - Code generation helpers
- `boogie_type_to_string()` - Type representation
- `collect_modified_globals()` - Global variable tracking

## Output Structure

Each transaction function generates one `BoogieProgram` containing:

1. **Global Declarations** (via `gen_base_program`):
   - String axioms and functions
   - Global constants 
   - Table field map variables

2. **Verification Procedure**:
   - CFG simulation (executable Boogie code mirroring the transaction)
   - Partition consistency assertions
   - Proper procedure signature with parameters and modifies clauses

## Key Features

### Structural Design
- Uses helper functions for modularity
- Leverages `gen_Boogie.rs` functions wherever possible
- Follows the architecture patterns from the verification document

### Error Handling
- Returns `Results<T>` for comprehensive error reporting
- Propagates errors from underlying `gen_Boogie.rs` calls
- Provides meaningful error messages for assertion failures

### Consistency with FMitF Architecture
- Works with the 4-stage compilation pipeline
- Uses ID-based references (never clones CFG structures)
- Integrates with existing error handling system

## Example Usage

```rust
let partition_manager = PartitionVerificationManager::new();
let programs = partition_manager.generate_partition_verification(&cfg_program)?;

for program in programs {
    // Each program contains verification for one transaction function
    println!("Generated {} procedures", program.procedures.len());
}
```

## Future Enhancements

1. **Control Flow Refinement**: The current `generate_block_control_flow()` is simplified. Could be enhanced to handle more complex control flow patterns.

2. **Optimization Integration**: Could be integrated with the optimization pipeline to verify optimized code.

3. **Error Message Enhancement**: Could provide more detailed error messages with source location information.

4. **Performance Optimization**: For large functions, could optimize the partition call collection and grouping algorithms.
