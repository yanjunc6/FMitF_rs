# `gen_transact.py` — Synthetic `.transact` Workload Generator

Emits parseable FMitF programs (partition decls + table decls + transactions)
using a tunable random workload model. Intended for stress-testing the
commutativity / SC-graph / Boogie verification pipeline.

All emitted values are `int` so the program is type-trivially well-formed
without a type checker on the generator side.

---

## How to run

The generator is a single-file Python script at the workspace root, stdlib only.

```bash
# Print to stdout with all defaults
python3 gen_transact.py

# Write 50 transactions to a file
python3 gen_transact.py --num-transactions 50 --out workload.transact

# Reproducible larger sweep
python3 gen_transact.py --num-transactions 200 --seed 42 --out big.transact

# See every flag
python3 gen_transact.py --help
```

To then feed the generated file through FMitF (parse only — skip Boogie):

```bash
cd FMitF_rs
./target/release/FMitF_rs --no-verify ../workload.transact
```

Notes:
- Every run is **deterministic** given `--seed`.
- The resolved configuration is embedded as a header comment in the output
  file, so a workload is reproducible from its own header.
- `--num-tables` is auto-derived from `--num-transactions` when omitted
  (see below); pass it explicitly to override.

---

## Default parameters

### Schema

| Parameter           | CLI flag                | Default        | Notes |
|---------------------|-------------------------|----------------|-------|
| `num_tables`        | `--num-tables`          | *auto-derived* | `max(tables_min, ceil(sqrt(num_transactions)))` when omitted; pass an int to override. |
| `tables_min`        | `--tables-min`          | `2`            | Floor used by the auto-derivation. |
| `fields_per_table`  | `--fields-per-table`    | `10`           | Constant int per table (PK columns are separate from this count). |
| `pk_arity_min`      | `--pk-arity-min`        | `1`            | Per-table PK count is uniform in `[min, max]`. |
| `pk_arity_max`      | `--pk-arity-max`        | `2`            | |

### Workload sizing — **MAIN KNOB**

| Parameter          | CLI flag             | Default | Notes |
|--------------------|----------------------|---------|-------|
| `num_transactions` | `--num-transactions` | `20`    | Number of `transaction TxN(...) { ... }` blocks emitted. |

### Hops per transaction

| Parameter           | CLI flag              | Default | Notes |
|---------------------|-----------------------|---------|-------|
| `hops_min`          | `--hops-min`          | `1`     | Hop count is sampled in `[min, max]`. |
| `hops_max`          | `--hops-max`          | `10`    | |
| `hops_dist`         | `--hops-dist`         | `zipf`  | One of `zipf`, `reverse_zipf`, `uniform`, `poisson`. |
| `hops_zipf_s`       | `--hops-zipf-s`       | `2.0`   | Tail-weight for the (reverse-)Zipf variants. |
| `hops_poisson_mean` | `--hops-poisson-mean` | `7.0`   | Only used when `hops_dist == "poisson"`. |

### Operations per hop

| Parameter     | CLI flag        | Default | Notes |
|---------------|-----------------|---------|-------|
| `ops_min`     | `--ops-min`     | `1`     | Per-hop op count, Zipf-distributed (head-heavy). |
| `ops_max`     | `--ops-max`     | `5`     | |
| `ops_zipf_s`  | `--ops-zipf-s`  | `1.5`   | Higher → more concentration on small counts. |

### Operation mix (auto-normalized)

| Parameter        | CLI flag           | Default | Op kind |
|------------------|--------------------|---------|---------|
| `op_read_p`      | `--op-read-p`      | `0.50`  | `var v: int = T[k: ...].field;` |
| `op_write_p`     | `--op-write-p`     | `0.20`  | Row-literal insert OR `T[k: ...].field = expr;` |
| `op_inc_dec_p`   | `--op-inc-dec-p`   | `0.10`  | `T[k: ...].field = T[k: ...].field +/- N;` |
| `op_logical_p`   | `--op-logical-p`   | `0.20`  | `var v: int = <expr>;` (no DB touch) |

The four are auto-normalized in `__post_init__`; they don't have to sum to
exactly `1.0` on the command line.

### For-loop within a hop

| Parameter             | CLI flag                | Default | Notes |
|-----------------------|-------------------------|---------|-------|
| `for_loop_prob`       | `--for-loop-prob`       | `0.05`  | Per-hop chance of wrapping the op sequence in `for (...) { ... }`. |
| `for_loop_iters_min`  | `--for-loop-iters-min`  | `5`     | Iteration bound is uniform in `[min, max]`. |
| `for_loop_iters_max`  | `--for-loop-iters-max`  | `10`    | |

### If-statement within a hop

| Parameter           | CLI flag              | Default | Notes |
|---------------------|-----------------------|---------|-------|
| `if_prob`           | `--if-prob`           | `0.25`  | Per-hop chance of splicing in `if cond { ... }`. |
| `if_else_prob`      | `--if-else-prob`      | `0.5`   | Conditional probability of an `else` branch given the `if` exists. |
| `if_body_min_ops`   | `--if-body-min-ops`   | `1`     | Op count per branch is uniform in `[min, max]`. |
| `if_body_max_ops`   | `--if-body-max-ops`   | `2`     | |

Each branch (`if` body, `else` body, `for` body) opens its own block scope:
vars declared inside die at the end of the block and never become eligible
for cross-hop continuation.

### Cross-hop continuation

| Parameter            | CLI flag              | Default | Notes |
|----------------------|-----------------------|---------|-------|
| `continuation_prob`  | `--continuation-prob` | `0.30`  | Per-hop-boundary chance of promoting one hop-level var into the cross-hop reference pool (à la auctionmark's `buyer_id` flowing from hop 1 to hop 2). |

### Reproducibility / I/O

| Parameter | CLI flag  | Default | Notes |
|-----------|-----------|---------|-------|
| `seed`    | `--seed`  | `42`    | Single source of randomness. |
| —         | `--out`   | *stdout*| Output path. Stdout if omitted. |

---

## Auto-derived `num_tables`

When `--num-tables` is omitted, the table count uses square-root scaling.
Sub-linear by construction, so the table-to-txn ratio shrinks as the
workload grows (more sharing → more SC-graph C-edges per generated file):

| `--num-transactions` | derived `num_tables` |
|----------------------|----------------------|
| 5                    | 3                    |
| 10                   | 4                    |
| 20                   | 5                    |
| 50                   | 8                    |
| 100                  | 10                   |
| 200                  | 15                   |
| 500                  | 23                   |
| 1000                 | 32                   |

Override with `--num-tables N` for any explicit value.

---

## Output structure

Each generated file is a single self-contained `.transact` program in this order:

1. **Header comment** — every resolved config value (so the workload is
   reproducible from its own header).
2. **Partition functions** — one per distinct PK arity used by the schema
   (`p1(x: int) -> int { return x; }`, `p2(...)`, ...).
3. **Table declarations** — `num_tables` tables, each with PKs, a `node pK(...)`
   placement clause, and `fields_per_table` int fields.
4. **Transactions** — `num_transactions` blocks of the form
   `transaction TxN(<pk params>) { hop { ... } hop { ... } ... }`.
