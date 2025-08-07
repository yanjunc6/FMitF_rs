### FMitF Verification Documentation (Expanded)

---

## **What We Do**

FMitF (Formal Methods in Transaction Framework) is a Rust-based framework designed to verify the **serializability** of distributed "chopped" transactions. The verification module ensures two key properties:

1. **Partition-Soundness (P-1)**: Ensures that all table accesses within a single `hop { ... }` are routed to the same cluster partition.
2. **Commutativity (P-2)**: Verifies that all order-preserving interleavings of two user-selected hop slices are observationally equivalent.

---

## **Why We Need This**

Distributed systems often execute transactions across multiple nodes, leading to potential conflicts and inconsistencies. FMitF addresses these challenges by:

- **Guaranteeing Safety**: Ensures correctness of distributed transactions through formal verification.
- **Optimizing Performance**: By eliminating unnecessary conflict edges (C-edges), the system reduces runtime contention.
- **Providing Debugging Insights**: Offers detailed logs and counterexamples for failed proofs.

---

## **How We Do That (Techniques)**

### **1. Verification Techniques**
- **Symbolic Execution**: Reads, writes, and table accesses are symbolically executed to generate I/O summaries. This involves modeling tables as Boogie `map` variables and tracking all operations performed on them.
- **Havocing**: All live-in variables and table maps are assigned arbitrary values before proofs to ensure generality. This ensures that the verification holds for all possible initial states.
- **Run-Snapshot-Compare**: Executes candidate interleavings, captures snapshots of the database state and output variables, and asserts equivalence between snapshots for all interleavings.

### **2. SC-Graph Construction**
- **S-Edges**: Directed edges representing sequential order within a transaction. These are derived from the program order of hops within a transaction.
- **C-Edges**: Undirected edges representing conflicts between transactions accessing the same table. These are added when two hops access the same table, and at least one access is a write.

### **3. Boogie Harness**
- **Boogie IVL**: Translates verification tasks into Boogie procedures. Each procedure models a specific interleaving of hops.
- **Assertions**: Ensures equivalence of snapshots for all interleavings. For example, `assert DB1 == DB2` ensures that the database state is identical after executing two interleavings.

---

## **UI Design**

### **CLI Interface**
- **Commands**:
  - `--instances N`: Configures the number of transaction instances. Internally, this determines how many concurrent instances of each transaction are created in the SC-Graph.
  - `--no-optimize`: Skips optimization passes. This is useful for debugging.
  - `--no-color`: Disables colored output for better compatibility with plain text environments.
- **Validation**:
  - Ensures input files exist and have the `.transact` extension.
  - Automatically generates output directories if not specified.

### **Console Output**
- **Color-Coded Results**:
  - ✔ for successful proofs.
  - ✗ for failed proofs.
- **Detailed Logs**:
  - Displays failed hops and counterexamples, including the specific interleaving that caused the failure.

---

## **IO Process**

### **Input**
- `.transact` files containing transaction definitions and partition functions.

### **Output**
- **Directory Structure**:
  - `boogie/`: Contains `.bpl` files for Boogie verification.
  - `scgraph_simplified.dot`: GraphViz DOT file for SC-Graph visualization.
  - `scgraph_simplified.txt`: Human-readable SC-Graph summary.
  - `compilation.log`: Detailed logs of the verification process, every boogie file output is included.

### **File Generation**
- **Boogie Files**: Generated for each verification task. These files contain the Boogie procedures and assertions for the verification.
- **SC-Graph Outputs**: Includes both DOT and text formats for visualization and debugging.

---

## **Implementation Details**

### **1. Verification Module**
- **Partition Verification**:
  - Validates that all table accesses within a hop are routed to the same partition.
  - Uses Boogie assertions to enforce partition soundness. For example, `forall k1, k2 :: Accessed(k1) && Accessed(k2) ==> p(k1) == p(k2)` ensures that all accessed keys are mapped to the same partition.
- **Commutativity Verification**:
  - Generates all order-preserving interleavings of two hop slices.
  - Eliminates unnecessary C-edges using SC-Graph analysis. For example, if two hops do not conflict, the corresponding C-edge is removed.

### **2. CLI Module**
- **VerificationCli**:
  - High-level interface for running verification tasks.
  - Manages Boogie file generation and execution.
- **Logger**:
  - Provides detailed logs for debugging.
  - Records Boogie results and SC-Graph outputs.

### **3. Boogie Integration**
- **BoogieFileResult**:
  - Captures the success, errors, and runtime of each Boogie verification.
- **Boogie Execution**:
  - Runs `.bpl` files with a 30-second timeout.
  - Parses results to identify failed assertions and syntax errors.

### **4. SC-Graph Simplification**
- **DotPrinter**:
  - Generates DOT files for SC-Graph visualization. These files can be used with GraphViz to create graphical representations of the SC-Graph.
- **Pretty Printer**:
  - Outputs a simplified text representation of the SC-Graph, including the nodes and edges.

### **5. Key Points**
- **Static Facts**:
  - Read/Write sets, live-in/live-out variables, and SC-Graph edges are precomputed during CFG and SC-Graph construction.
- **Edge Deduplication**:
  - Ensures no duplicate C-edges using consistent node ordering. This reduces redundancy in the SC-Graph.
- **Error Handling**:
  - Logs warnings for Boogie execution failures, such as syntax errors or assertion failures.
  - Provides user-friendly error messages for invalid inputs, such as missing `.transact` files or invalid CLI arguments.

---

This document consolidates the implementation details, techniques, and design principles of FMitF's verification framework, ensuring clarity and maintainability for future development.