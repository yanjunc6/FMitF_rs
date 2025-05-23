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
1. **DSL** (TODO):
   - We define a minimal language with constructs like:  
     ```
     transaction TxA {
       hop node1 {
         read(TableX);
         write(TableY, newValue);
       }
       hop node2 {
         read(TableZ);
         write(TableX, anotherValue);
       }
     }
     ```  
   - Each hop is atomic with respect to its node.

2. **Intermediate Representation**:  
   - We parse each transaction specification to record which nodes are accessed, the set of read/writes, and the sequence of hops.

3. **Boogie/Model Checker**:  
   - We translate the DSL into Boogie (or another suitable verification framework).  
   - We encode the lock acquisition, read/write sets, and allowable interleavings.  
   - The verification engine checks all valid interleavings (within some bounded scope, if necessary) to confirm serializability.

4. **Constraints/Outputs**:
   - We may also encode additional constraints (e.g., “at most `n` concurrent instances of TxA can run”) to reflect design or performance considerations.  
   - If the verification fails for a particular concurrency constraint, we refine either the transaction design or the concurrency policy.
