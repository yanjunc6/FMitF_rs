# **Revised Concurrency Checking: "Commutativity" Approach**

Below is a concise description of the verification strategy for checking whether two hops—in separate functions/transactions—commute.

---

## **1. Goal**

We have two functions (transactions), each with a sequence of hops:
- **A** = [A₁, A₂, …, Aₐ]  
- **B** = [B₁, B₂, …, B_b]

We focus on checking whether a particular pair of hops (Aₘ, Bₖ) commutes. Specifically, we look at the partial sequences:
- **PrefixA** = [A₁, …, Aₘ],  
- **PrefixB** = [B₁, …, Bₖ].

If, for *all* ways of interleaving those prefixes while preserving the internal order of each list, running (Aₘ, Bₖ) in either order **always** yields the same final state, we say they commute. We can then safely remove their conflict edge (C-edge).

---

## **2. Constructing the Verification Unit**

1. **Relevant State**  
   Gather the tables, columns, and global variables accessed by (A₁..Aₘ) and (B₁..Bₖ).

2. **Partial AST**  
   Only keep the hops up to Aₘ and Bₖ.

3. **Parameters/Locals**  
   Include any function parameters or local variables that may be read or written by these hops.

---

## **3. Enumerating Prefix Interleavings**

1. **PrefixA** = [A₁, …, Aₘ₋₁], **PrefixB** = [B₁, …, Bₖ₋₁].  
2. List **all merges** of PrefixA and PrefixB that preserve the original order within each.  
   - Example: If PrefixA = [A₁, A₂] and PrefixB = [B₁], the merges are:  
     1. [A₁, A₂, B₁]  
     2. [A₁, B₁, A₂]  
     3. [B₁, A₁, A₂]

---

## **4. Appending the Final Pair**

For each prefix interleaving **P**, we then append either:
1. `[Aₘ, Bₖ]`, or  
2. `[Bₖ, Aₘ]`.

We want to see if these two final orders always produce the same final state when starting from the same initial state.

---

## **5. Commutativity Check**

For each prefix interleaving **P**:

1. **Compute Final State for “Aₘ, Bₖ”**  
   - Start at the same (havoced) initial state.  
   - Execute **P**.  
   - Then run **Aₘ** followed by **Bₖ**.  
   - Call the result (tables/globals) **State_AB**.

2. **Compute Final State for “Bₖ, Aₘ”**  
   - Reset to the same initial havoced state.  
   - Execute **P** (identical prefix as above).  
   - Then run **Bₖ** followed by **Aₘ**.  
   - Call the result (tables/globals) **State_BA**.

3. **Compare**  
   - Check that **State_AB** = **State_BA** for all tables/globals of interest.  

If **State_AB** = **State_BA** for *every* prefix interleaving **P**, then (Aₘ, Bₖ) commute and we can delete that edge.

---

## **6. Example**

### **Transactions A and B**

- **A**: [A₁, A₂]  
- **B**: [B₁, B₂]

We want to verify whether **(A₂, B₂)** commute:

- **PrefixA** = [A₁, A₂]  
- **PrefixB** = [B₁, B₂]

But for the *interleaving*, we only use [A₁] and [B₁] as the prefix (since A₂ and B₂ are the final hops). So:

- **PrefixA** = [A₁],  
- **PrefixB** = [B₁].

**Possible merges** of (A₁) and (B₁):  
1. [A₁, B₁]  
2. [B₁, A₁]

Then we **append** either `[A₂, B₂]` or `[B₂, A₂]`. For each prefix, we:

1. Execute the prefix, then [A₂, B₂], record **State_AB**.  
2. Reset to the initial state, execute the same prefix, then [B₂, A₂], record **State_BA**.  
3. Check **State_AB** = **State_BA**.

If equal for both prefix merges, then (A₂, B₂) commute.

---

## **7. Boogie Implementation Sketch**

1. **Declare** all relevant tables/globals as maps or variables.  
2. **Havoc** them (and any parameters) to represent an arbitrary initial state.  
3. **Translate** each hop (Aᵢ, Bⱼ) into a Boogie procedure that updates the tables/globals.  
4. **For each prefix interleaving**:  
   - (a) Reset to the initial (havoced) state.  
   - (b) Execute the prefix hops.  
   - (c) Execute [Aₘ, Bₖ], record State_AB.  
   - (d) Reset to the initial (havoced) state.  
   - (e) Execute the prefix hops again.  
   - (f) Execute [Bₖ, Aₘ], record State_BA.  
   - (g) **Assert** State_AB = State_BA.

---

## **8. Conclusion**

To test whether two hops (Aₘ, Bₖ) commute:

1. **Take prefixes** [A₁..Aₘ], [B₁..Bₖ].  
2. **Interleave** [A₁..Aₘ₋₁] and [B₁..Bₖ₋₁].  
3. **Append** (Aₘ, Bₖ) or (Bₖ, Aₘ).  
4. **Compare** final states for each interleaving + final-order combination.  

If these match in all cases, the hops commute; otherwise, they do not. This framework—particularly when encoded in Boogie or any formal verification tool—lets you systematically check commutativity and prune redundant concurrency edges.
