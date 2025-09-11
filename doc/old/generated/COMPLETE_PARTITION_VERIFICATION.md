 Complete Partition Verification Implementation

## Updated Summary of gen_Boogie.rs Capabilities

### **Core Infrastructure Functions**
1. **Program Creation & Setup**
   - `new(name: String)` - Creates generator with empty Boogie program
   - `with_program(program: BoogieProgram)` - Creates generator with existing program  
   - `gen_base_program(cfg_program)` - Creates complete base program with globals, axioms, tables

2. **Global Element Generation**
   - `gen_string_axioms()` - Adds string type definitions, functions, and axioms (Concat, IntToString, etc.)
   - `gen_global_constants(cfg_program)` - Converts CFG global constants to Boogie global variables
   - `gen_table_variables(cfg_program)` - Generates nested map variables for each table field
   - `add_string_literal(literal)` - Manages global string literal constants with sanitized names

### **Type System & Conversion**
3. **Type Conversion & Handling**
   - `convert_type(TypeName)` - Maps CFG types to Boogie types (handles Int, Real, Bool, String, nested Maps)
   - `flatten_array_type(TypeName)` - Recursively flattens nested array types to map dimensions
   - `boogie_type_to_string(BoogieType)` - Converts Boogie types to their string representation
   - `infer_operand_type(cfg_program, operand)` - Type inference for operands in expressions

### **Variable & Identifier Generation**
4. **Naming & Labeling Utilities**
   - `gen_var_name(cfg_program, var_id, prefix)` - Variable names with optional prefixes (for P-2 verification)
   - `gen_table_field_var_name(table_name, field_name)` - Table field variable names (`table_field`)
   - `gen_procedure_params(cfg_program, function, prefix)` - Procedure parameter lists with prefix support
   - `gen_block_label(function_name, hop_index, block_index, prefix)` - Basic block labels
   - `gen_function_start_label(function_name)` / `gen_function_end_label(function_name)` - Function boundary labels
   - `gen_basic_block_label(cfg_program, block_id, function_name)` - Block ID to label resolution

### **Expression & Statement Translation**
5. **CFG to Boogie Expression Conversion**
   - `convert_operand(cfg_program, operand, prefix)` - Converts variables and constants to Boogie expressions
   - `convert_constant(constant)` - Handles all constant types (int, float, bool, string, arrays)
   - `convert_rvalue(cfg_program, rvalue, prefix)` - Converts right-hand values (table access, array access, operations)
   - `convert_assignment(cfg_program, lvalue, rvalue, prefix)` - Complete assignment statement conversion
   - `convert_statement(cfg_program, stmt, prefix)` - Top-level statement conversion to Boogie lines
   - `convert_operand_to_string(cfg_program, operand, prefix)` - Automatic type conversion for string operations

6. **Operator Translation**
   - `convert_binary_op(BinaryOp)` - Maps CFG binary operations to Boogie operators
   - `convert_unary_op(UnaryOp)` - Maps CFG unary operations to Boogie operators  
   - Special handling for string concatenation via function calls

### **Control Flow Generation**
7. **Control Flow & Block Processing**
   - `gen_basic_block_control_flow(lines, cfg_program, block_id, function_name, hop_index)` - **NEW PUBLIC METHOD**
   - `gen_basic_block_edges(...)` - Private method handling all EdgeType variants
   - Handles: Unconditional, ConditionalTrue/False, Return, Abort, HopExit transitions
   - Generates appropriate goto statements and conditional jumps

### **Boolean Logic & Utilities**
8. **NEW: General Boolean Operations**
   - `gen_conjunction(expressions: Vec<BoogieExpr>)` - **NEW** Creates AND of multiple expressions
   - `gen_disjunction(expressions: Vec<BoogieExpr>)` - **NEW** Creates OR of multiple expressions
   - Handles empty lists appropriately (true for AND, false for OR)
   - Single expression optimization

9. **Code Generation Utilities**
   - `add_comment(lines, comment)` - Adds comment lines to procedures
   - `add_assertion(lines, condition, error_msg)` - Adds assert statements with error messages

### **Dataflow Integration**
10. **NEW: Dataflow Analysis Integration**
   - `collect_modified_globals_from_dataflow(cfg_program, function_id)` - **NEW** 
   - Uses hop-level table_mod_ref analysis to determine which global variables are modified
   - Integrates with the existing dataflow analysis framework
   - Returns list of table field variable names that have write accesses

---

## Completely Rewritten Partition Verification Implementation

### **Architecture Overview**

The new implementation follows the requested design principles:

1. **Inline Assertion Generation** - Assertions are inserted immediately when statements contain table accesses
2. **Simplified Control Flow** - Uses existing `gen_basic_block_control_flow()` from gen_Boogie.rs  
3. **Simplified Consistency Checking** - Only compares parameter equality between consecutive accesses
4. **Dataflow Integration** - Uses table_mod_ref analysis for modified globals
5. **General Boolean Operations** - Leverages new conjunction/disjunction functions

### **Key Components**

#### **PartitionVerificationManager**
Main orchestration class with simplified, focused methods:

**Public Interface:**
- `new()` - Creates manager instance
- `generate_partition_verification(cfg_program)` - Generates programs for all transaction functions

**Core Implementation Methods:**
- `generate_function_partition_verification()` - Per-function program generation
- `generate_verification_procedure()` - Main procedure generation with inline simulation
- `check_and_insert_partition_assertion()` - Statement-level checking and assertion insertion
- `handle_table_access()` - Table access handling with consistency tracking
- `insert_partition_consistency_assertion()` - Immediate assertion insertion

### **Implementation Strategy**

#### **1. Unified CFG Simulation with Inline Assertions**
Instead of separating CFG simulation and assertion generation:

- **Single Pass Processing**: Traverses hops → basic blocks → statements
- **Immediate Translation**: Each statement converted to Boogie using `generator.convert_statement()`
- **Immediate Checking**: Each statement checked for table accesses  
- **Immediate Assertion**: If table access found and previous call exists, assertion inserted immediately

#### **2. Simplified Partition Call Tracking**
Uses simple HashMap tracking:
```rust
HashMap<(HopId, TableId), Vec<Operand>>
```
- **Key**: `(hop_id, table_id)` - identifies unique partition function call site
- **Value**: `Vec<Operand>` - arguments from the first call
- **Logic**: On second+ call, compare with first call and insert assertion

#### **3. Single Assertion per Access Pattern**
As requested, simplified to just compare one access against the previous:
- No complex pairwise comparisons
- No separate collection/grouping phases  
- Just: "If we've seen this partition function called before in this hop, assert args differ"

#### **4. Leveraged gen_Boogie.rs Functionality**

**Core Infrastructure:**
- `gen_base_program()` - Base program setup
- `convert_statement()` - Statement translation
- `gen_basic_block_control_flow()` - Control flow generation
- `collect_modified_globals_from_dataflow()` - Modified globals via dataflow analysis

**Boolean Operations:**
- `gen_conjunction()` - Combines multiple argument equality checks
- `add_assertion()` - Assertion insertion

**Utilities:**
- `add_comment()` - Documentation comments
- `gen_block_label()` / `gen_function_start_label()` / `gen_function_end_label()` - Labels
- `gen_procedure_params()` - Procedure signatures

### **Verification Logic**

The core verification condition implements the strategy from `verification.md` section 2.2:

1. **Track First Call**: When a partition function is called for the first time in a hop, store the arguments
2. **Check Subsequent Calls**: When the same partition function is called again in the same hop:
   - Generate equality check for each argument: `arg1_prev == arg1_curr && arg2_prev == arg2_curr && ...`
   - Use `gen_conjunction()` to combine multiple equality conditions
   - Assert `!(args_equal)` - if arguments are equal, the assertion fails
   - This catches the violation: same partition function called with identical arguments = same node selected multiple times

**Error Message**: 
```
"Partition function 'X' called with same arguments in the same hop, violating single-node constraint"
```

### **Integration Benefits**

#### **Dataflow Analysis Integration**
- `collect_modified_globals_from_dataflow()` uses the existing table_mod_ref analysis
- Hop-level analysis automatically aggregates all table modifications across the function
- Returns precise list of only the global variables that are actually modified

#### **Clean Architecture**  
- No duplicate code - everything leverages gen_Boogie.rs
- Consistent naming and code generation patterns
- Simplified control flow using existing proven methods

#### **Maintainability**
- Single responsibility methods
- Clear separation of concerns
- Leverages existing tested infrastructure

### **Example Generated Boogie Structure**

```boogie
procedure verify_partition_transfer(param_from_id: int, param_to_id: int, param_amount: int)
  modifies Account_balance;  // From dataflow analysis
{
  // Partition verification for function: transfer
  
  transfer_start:
  
  // --- Hop 0 ---
  transfer_hop0_block0:
    // Translated CFG statement: Account[from_id].balance := Account[from_id].balance - amount
    Account_balance := Account_balance[param_from_id := Account_balance[param_from_id] - param_amount];
    
    // Partition consistency check for function 'node_selector' on table 'Account'  
    // (Only appears if there's a second access to Account with node_selector in this hop)
    assert {:msg "Partition function 'node_selector' called with same arguments in the same hop, violating single-node constraint"} 
           !(param_from_id == param_to_id);
    
    goto transfer_hop0_block1;
    
  // ... rest of CFG simulation ...
  
  transfer_end:
}
```

The implementation is now complete, simplified, and fully integrated with the existing FMitF architecture while leveraging the enhanced gen_Boogie.rs functionality.
