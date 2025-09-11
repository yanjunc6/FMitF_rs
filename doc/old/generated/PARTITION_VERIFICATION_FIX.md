# Partition Verification Fix: Same Partition Function Tracking

## Problem Identified
The original implementation was incorrectly tracking partition consistency by `(hop_id, table_id)`, which would only verify consistency within the same table. However, **the correct verification requirement is to check consistency for the same partition function across different tables**.

## Root Issue
If two different tables use the same partition function, they should be verified against each other because they represent the same logical partitioning strategy. The original approach missed this cross-table verification.

## Key Changes Made

### 1. Changed Tracking Key Structure
```rust
// BEFORE: Track by table
HashMap<(HopId, TableId), Vec<Operand>>

// AFTER: Track by partition function  
HashMap<(HopId, FunctionId), Vec<Operand>>
```

### 2. Proper Partition Function Resolution
```rust
// Get the partition function for this table
let table = &cfg_program.tables[table_id];
let partition_function_id = table.partition_function;
let key = (hop_id, partition_function_id);
```

### 3. Correct Argument Mapping
The crucial fix was properly mapping table primary key values to partition function parameters:

```rust
// Map table primary key values to partition function arguments
// The table.partition_fields tells us which table fields correspond to which partition parameters
let mut partition_args = Vec::new();
for &partition_field_id in &table.partition_fields {
    // Find the index of this field in the table's primary keys
    if let Some(pk_index) = table.primary_keys.iter().position(|&pk_id| pk_id == partition_field_id) {
        if pk_index < pk_values.len() {
            partition_args.push(pk_values[pk_index].clone());
        }
    }
}
```

### 4. Updated Method Signatures
All related methods now use `FunctionId` instead of `TableId` for the tracking key:
- `check_and_insert_partition_assertion()`
- `handle_table_access()`
- `insert_partition_consistency_assertion()`

## Verification Logic Now Correctly Handles

### Scenario 1: Same Table, Multiple Accesses
```rust
// Transaction function
void transfer(int from_id, int to_id, int amount) {
    hop {
        Account[from_id].balance -= amount;  // First access to partition function node_selector(from_id, 0)
        Account[from_id].owner = "locked";   // Second access to partition function node_selector(from_id, 0)
        // ✅ Assertion: !(from_id == from_id) - this will correctly fail if same args
    }
}
```

### Scenario 2: Different Tables, Same Partition Function  
```rust
table Account {
    primary int id;
    node node_selector(id, 0);
    int balance;
}

table History {
    primary int account_id;  
    node node_selector(account_id, 0);  // Same partition function!
    string action;
}

// Transaction function
void log_transfer(int acc_id, int amount) {
    hop {
        Account[acc_id].balance -= amount;      // First access: node_selector(acc_id, 0)
        History[acc_id].action = "transfer";   // Second access: node_selector(acc_id, 0) 
        // ✅ Assertion: !(acc_id == acc_id) - correctly detects same partition function call
    }
}
```

## Benefits of the Fix

1. **Cross-Table Verification**: Now correctly verifies consistency across different tables using the same partition function

2. **Proper Argument Mapping**: Uses `table.partition_fields` to correctly map table primary keys to partition function parameters

3. **Logical Correctness**: Aligns with the formal verification requirement that the same partition function should not be called with identical arguments in the same hop

4. **Maintains Single-Node Constraint**: Ensures that if multiple table accesses in the same hop use the same partition function with the same arguments, it correctly identifies this as a violation of the single-node execution constraint

## Example Generated Assertion
```boogie
// Partition consistency check for function 'node_selector'
assert {:msg "Partition function 'node_selector' called with same arguments in the same hop, violating single-node constraint"} 
       !(previous_acc_id == current_acc_id);
```

The verification now correctly handles the semantic requirement that **same partition function + same arguments = same node**, regardless of which tables are being accessed.
