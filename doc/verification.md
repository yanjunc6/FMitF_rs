Absolutely! Here’s a detailed and systematic summary of the **verification pattern** for your concurrency/serializability checking, suitable for implementation—in particular, for automatic code generation from your Rust AST and SC-graph.

---

# **Summary of the Serializability Verification Pattern**

---

## **I. Construction of the Verification Unit**

**Given:**
- Program AST (tables, globals, functions/hops)
- SC-Graph (nodes, hops, edges)
- Specific **C-edge** between two hops (from different transactions/functions)

**Build:**
1. **Identify the conflict node:** Node where the C-edge hops both occur.
2. **For each transaction/function (e.g., F, G):**
    - Collect the **prefix sequence of hops** *on the conflict node* required by the C-edge, including the conflicting hop.
3. **Verification Unit:** Contains
    - The node in question
    - 2 functions and their hops’ sequence up to the C-edge
    - All relevant tables (those read or written by the above hops)
    - All global variables read or written (in all these hops)
    - (Optionally: all function parameters for both functions)

---

## **II. Enumeration of Interleavings**

**Goal:**  
List all possible interleavings of the two hop sequences, **preserving the order of each sequence**.

**Algorithm:**
- Given `A = [a1, a2, ..., an]`, `B = [b1, b2, ..., bm]`
- Output all possible lists containing all hops of `A` and `B`, with `A` hops in original order and `B` hops in original order.

**Implementation:**  
Standard recursive merge/interleavings algorithm.

---

## **III. Modeling Tables and Globals**

**For Each Table:**
- Declare in Boogie: For each column (non-primary)  
  ```boogie
  var <TableName>_<Field>: [<PKType>]<FieldType>;
  ```
- Use primary key as map index.

**For Each Global:**
- Declare as  
  ```boogie
  var <Name>: <Type>;
  ```

---

## **IV. Initial State "Havoc"**

**At the start of verification:**
- All relevant tables and global variables are **havoced** (unconstrained).
    - This means all initial values are arbitrary (modeling unknown database state and variables).
- Also, **havoc** input parameters for the functions involved.

---

## **V. Modeling Each Hop**

**Each hop is modeled as a Boogie procedure/block**:
- Takes necessary arguments (including all needed parameters/locals/globals)
- Modifies state as described in AST

---

## **VI. Main Harness Procedure Generation**

**In the Boogie `main`:**

1. **For each interleaving:**
    - Reset all relevant state (tables, globals, inputs) to the same havoced initial state.
    - Execute the corresponding sequence of Boogie calls for hops.
    - At the end, **save the state** of all relevant tables and globals, e.g., as `Users_balance_i1`, `balance_i1`.

2. **Serial executions (references):**
    - Do the same for the two serial orders (all F hops then all G hops, all G hops then all F hops), saving as `..._serial1`, `..._serial2`.

3. **Assertions:**
    - For each interleaving, assert that **the final state (tables + globals) matches at least one of the serial executions**:
      ```boogie
      assert (forall k: PKType :: Table_i[k] == Table_serial1[k] || Table_i[k] == Table_serial2[k]);
      assert Global_i == Global_serial1 || Global_i == Global_serial2;
      ```
    - If you have multiple globals/tables, repeat for each.

---

## **VII. Summary Pseudocode**

```pseudocode
for each relevant table and global:
    declare a Boogie variable

for each function parameter/global relevant:
    havoc (assign arbitrarily)
save initial copies of each relevant table/global

for each SERIAL order in {FG, GF}:
    reset state to saved initial
    execute all hops for that serial
    save serial final state

for each INTERLEAVING order:
    reset state to saved initial
    execute all hops for that interleaving
    save interleaving final state
    for all tables/globals:
        assert state matches at least one serial execution
```

---

## **VIII. Implementation Considerations**

- **Locals vs Globals:**  
  Track any local variables that are assigned in one hop and used in another. For verification, treat persistent variables as “globals” unless they are re-declared.
- **Return values and aliasing:**  
  Make sure each assignment and result is mapped correctly to its Boogie variable; rename variables per interleaving/serial as needed.
- **Multiple Tables/Globals:**  
  Extend the snapshot and assert mechanism to cover any table/global touched by the relevant hops (directly or indirectly).
- **Transactions with more hops:**  
  The above pattern generalizes to longer hop sequences or more complicated C-edges.

---

## **IX. What does it verify?**

It checks that, no matter how the relevant hops are interleaved while respecting their program order, **the outcome for all relevant shared state (tables, globals) is indistinguishable from some serial execution**.  
If this is true, the C-edge can be deleted (the operations commute with respect to the observed state).

---

**This is the pattern you want to implement and automate in Rust, driven by your AST and SC-graph.**  
If you’d like, I can help provide skeleton code for code generation, or help with interpreting your AST to generate Boogie!