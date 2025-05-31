# README: Chopped Transaction Verification Project

## Overview
This project aims to verify *serializability* in a system where each transaction is “chopped” into multiple *hops*. Each hop executes on a designated node under a lock, releasing the lock at the end of the hop before moving on. By encoding these multi-hop transactions in a minimal domain-specific language (DSL) and then translating them into a verification tool like Boogie, we can check whether all possible interleavings of these hops preserve serializability.

## Concurrency Model and Assumptions
1. **Hop-Level Locking**:  
   - Each hop acquires a lock on its target node for the duration of the hop.  
   - No other hops can access that node while the lock is held.  
   - The lock is released upon completion of the hop.

2. **Transaction Flow**:  
   - A transaction is an ordered sequence of hops (e.g., Hop1 → Node1, Hop2 → Node2, …).  
   - Each node’s lock operates independently of locks on other nodes.  
   - After one hop finishes (and the lock is released), the transaction proceeds to the next hop on the next node, and so on.

3. **Distributed System Simplifications**:
   - We do not address network failures, latency, or advanced distributed consensus.  
   - We assume that if a node is reachable, its lock works correctly.  
   - Liveness concerns (e.g., ensuring forward progress) are out of scope; we only verify serializability for all valid schedules.

4. **No Complex Control Flow**:  
   - For verification simplicity, each hop has a bounded set of read/write operations (e.g., read row X, write row Y).  
   - Unbounded loops or recursion are avoided to keep the state space tractable.

## What We Want to Prove
Our primary objective is to prove **serializability**:  
> For every possible interleaving of chopped transactions in this system, the resulting final database state is equivalent to some serial (one-at-a-time) ordering of those same transactions.

To achieve this, our verification process will check that no interleaving of hops violates transaction isolation or produces anomalies (e.g., lost updates, dirty reads).

## DSL and Verification Flow
1.  **Domain-Specific Language (DSL)**:
    *   We define a language to specify transactions, the nodes they run on, and the table schemas. Key constructs include:
        *   Node definitions (e.g., `nodes { node1, node2 }`).
        *   Table schemas with typed fields (e.g., `table TableX on node1 { int fieldA; string fieldB; }`).
        *   Transactions (defined as functions) composed of sequential hops, each targeting a specific node.
        *   Basic data types (`int`, `float`, `string`, `bool`) and operations.
        *   Statements within hops for variable declaration, assignment, table access (read/write), and basic control flow (`if`, `while`, `break`, `continue`).
    *   Example transaction snippet (based on the grammar):
        ```fmitf
        // Define nodes
        nodes { n1, n2 }

        // Define tables
        table Account on n1 {
            int balance;
            string owner;
        }

        table Ledger on n2 {
            int amount;
            string description;
        }

        // Transaction to transfer funds
        void transfer(int from_id, int to_id, int transfer_amount) {
            hop on n1 {
                // Read current balance from Account table
                int current_from_balance = Account[id:from_id].balance;
                int new_from_balance = current_from_balance - transfer_amount;
                Account[id:from_id].balance = new_from_balance; // Write updated balance
            }
            hop on n2 {
                // Log the transfer in Ledger table
                Ledger[tx_id:from_id].amount = transfer_amount; // Example write
                // Assume some unique tx_id generation or passing
            }
            hop on n1 {
                 // Read current balance for the recipient
                int current_to_balance = Account[id:to_id].balance;
                int new_to_balance = current_to_balance + transfer_amount;
                Account[id:to_id].balance = new_to_balance; // Write updated balance
            }
        }
        ```
    *   Each hop's operations are considered atomic with respect to its designated node.

2.  **Intermediate Representation & Analysis**:
    *   The DSL source code is first parsed into an Abstract Syntax Tree (AST).
    *   Semantic analysis is performed on the AST to check for type errors, scope issues, and other language-specific rules.
    *   A key intermediate representation is the **Serializability Conflict Graph (SCGraph)**. This graph captures:
        *   Vertices: Individual hops from all transactions.
        *   Sequential (S) Edges: Representing the order of hops within the same transaction.
        *   Conflict (C) Edges: Representing potential conflicts between hops of different transactions (e.g., read-write, write-write conflicts on the same data item at the same node).
    *   The tool can output the SCGraph, including a DOT format for visualization, to help understand potential conflicts.

3.  **Verification (using a Model Checker like Boogie)**:
    *   The transaction specifications and the SCGraph information are translated into a formal model suitable for a verification engine like Boogie.
    *   This involves encoding:
        *   The state of the database (abstractly).
        *   The operational semantics of read and write operations.
        *   Node-level locking: A hop acquires a lock on its node, performs its operations, and releases the lock.
        *   The possible interleavings of hops from different transactions, constrained by the locks and sequential dependencies.
    *   The `verify` mode in the tool automates parts of this process. It attempts to prove that certain C-edges in the SCGraph do not actually lead to serializability violations under the locking protocol. Verified C-edges can then be "pruned" from the SCGraph.
    *   The verification engine explores reachable states to check if any execution could lead to a non-serializable outcome (e.g., by detecting mixed S/C cycles in the SCGraph that cannot be resolved).

4.  **Constraints and Outputs**:
    *   The verification can be performed under certain assumptions or constraints (e.g., "at most `n` concurrent instances of TxA can run").
    *   **Output**:
        *   If all potential conflicts (mixed S/C cycles) are resolved or proven safe, the system is deemed serializable under the given conditions.
        *   If the verification identifies irresolvable conflicts (persistent mixed S/C cycles after pruning), it indicates a potential serializability violation. The tool will report these remaining cycles.
        *   Boogie files generated during the `verify` mode can be saved for inspection.
    *   Based on the output, developers may need to refine the transaction logic, the chopping strategy, or concurrency control mechanisms.

## Usage
The tool is invoked via the `fmitf` command-line interface.

```bash
fmitf <input_file> [options]
```

**Key Arguments & Options:**

*   `<input_file>`: Path to the input source file containing transaction definitions in the DSL.
*   `-m, --mode <MODE>`: Specifies the operation mode.
    *   `ast`: Parses the input and prints the Abstract Syntax Tree.
    *   `scgraph`: (Default) Parses, analyzes, and prints the Serializability Conflict Graph.
        *   Use `--dot` to output the graph in DOT format (e.g., for Graphviz).
    *   `verify`: Parses, analyzes, builds the SCGraph, and then attempts to verify and prune conflict edges using Boogie.
        *   If an output path is provided with `-o`, Boogie (`.bpl`) files generated during verification will be saved to that directory.
*   `-o, --output <PATH>`: Specifies the output file or directory.
    *   For `ast` and `scgraph` (non-DOT) modes: path to the output file. If not provided, output goes to stdout.
    *   For `scgraph --dot` mode: path to the output `.dot` file. If not provided, output goes to stdout.
    *   For `verify` mode: path to a directory where Boogie (`.bpl`) files will be saved. Textual summary still goes to stdout.
*   `-v, --verbose`: Enables verbose output, providing more detailed information during processing (e.g., detailed SCGraph structure, initial/final states in verify mode).
*   `-q, --quiet`: Suppresses non-essential output, showing only critical messages or final results.
*   `--show-spans`: (For `ast` mode) Includes source code span information in the AST output.

**Examples:**

1.  **Parse a file and show the AST:**
    ```bash
    fmitf examples/my_transactions.fmitf --mode ast
    ```

2.  **Analyze SCGraph and print summary:**
    ```bash
    fmitf examples/my_transactions.fmitf --mode scgraph
    ```

3.  **Generate SCGraph in DOT format and save to a file:**
    ```bash
    fmitf examples/my_transactions.fmitf --mode scgraph --dot --output graphs/scgraph.dot
    ```
    You can then visualize `scgraph.dot` using Graphviz: `dot -Tpng graphs/scgraph.dot -o graphs/scgraph.png`

4.  **Run verification and save Boogie files:**
    ```bash
    fmitf examples/my_transactions.fmitf --mode verify --output boogie_files/ --verbose
    ```
    This will print a summary to stdout and save `.bpl` files in the `boogie_files/` directory
