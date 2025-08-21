# Partition Verification Logic Correction

## Problem Identified
The assertion logic was **completely backwards**. The verification was asserting that arguments should be **different**, when the correct requirement is that arguments should be **the same**.

## Conceptual Error
### ❌ **Previous Incorrect Logic**
```rust
// Assert !(args are equal) - if same args, this should fail
let not_args_equal = BoogieExpr {
    kind: BoogieExprKind::UnOp(BoogieUnOp::Not, Box::new(args_equal)),
};

let error_msg = ErrorMessage {
    msg: "Partition function called with same arguments in same hop, violating single-node constraint".to_string(),
};

BoogieProgramGenerator::add_assertion(lines, not_args_equal, error_msg);
```

This was **wrong** because:
- It asserted `!(arg1 == arg2)` - meaning it would fail if arguments were the same
- It treated "same arguments" as a **violation** rather than a **requirement**

### ✅ **Corrected Logic**
```rust
// Assert (args are equal) - all accesses to same partition function in same hop must have same args
let error_msg = ErrorMessage {
    msg: "Partition function called with different arguments in same hop, violating single-node constraint".to_string(),
};

BoogieProgramGenerator::add_assertion(lines, args_equal, error_msg);
```

This is **correct** because:
- It asserts `arg1 == arg2` - meaning it fails if arguments are **different**
- It treats "different arguments" as the **violation**

## Correct Verification Requirement

### **Single-Node Constraint Logic**
In the same hop, if a partition function is called multiple times:
- **Required**: All calls must have **identical arguments**
- **Reason**: Same partition function + same arguments = same node
- **Goal**: Ensure all table accesses in a hop execute on the **same node**

### **Example Scenarios**

#### ✅ **Valid Case** (should pass verification)
```rust
void transfer(int account_id, int amount) {
    hop {
        Account[account_id].balance -= amount;    // node_selector(account_id, 0)
        History[account_id].action = "transfer"; // node_selector(account_id, 0)
        // ✅ Both calls have same args (account_id) → same node → VALID
    }
}
```

#### ❌ **Invalid Case** (should fail verification)
```rust
void transfer(int from_id, int to_id, int amount) {
    hop {
        Account[from_id].balance -= amount;  // node_selector(from_id, 0)
        Account[to_id].balance += amount;    // node_selector(to_id, 0)  
        // ❌ Different args (from_id ≠ to_id) → different nodes → INVALID
    }
}
```

## Generated Boogie Assertions

### **Corrected Assertion**
```boogie
// Partition consistency check for function 'node_selector': all calls in same hop must have same args
assert {:msg "Partition function 'node_selector' called with different arguments in the same hop, violating single-node constraint"} 
       (previous_account_id == current_account_id);
```

### **Verification Behavior**
- **Passes** when: `previous_account_id == current_account_id` (same node)
- **Fails** when: `previous_account_id != current_account_id` (different nodes - violation!)

## Impact of the Fix

1. **Logical Correctness**: Now enforces the actual single-node constraint requirement
2. **Proper Error Detection**: Catches actual violations (different arguments) instead of correct behavior (same arguments)  
3. **Meaningful Error Messages**: Error message now correctly describes the violation
4. **Verification Soundness**: The verification now actually verifies what it's supposed to verify

## Before vs After Summary

| Aspect | Before (❌ Wrong) | After (✅ Correct) |
|--------|------------------|-------------------|
| **Assertion** | `!(arg1 == arg2)` | `arg1 == arg2` |
| **Fails When** | Arguments are same | Arguments are different |
| **Treats as Violation** | Same arguments | Different arguments |
| **Error Message** | "same arguments violate constraint" | "different arguments violate constraint" |
| **Verification Logic** | Backwards/Incorrect | Correct |

The fix ensures that the verification actually enforces the single-node execution constraint as intended by the formal verification specification.
