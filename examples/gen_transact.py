#!/usr/bin/env python3
"""
gen_transact.py — Synthetic .transact file generator for FMitF.

Emits a parseable FMitF program (partition decls + table decls + transactions)
using a tunable random workload model. Intended for stress-testing the
commutativity / SC-graph / Boogie verification pipeline.

All generated values are int (no float / string mix) so the emitted program
is type-trivially well-formed without a type checker on the generator side.

Usage:
    python gen_transact.py --num-transactions 50 --out workload.transact
    python gen_transact.py --help

Reproducibility: every run is deterministic given --seed.  The chosen config
is also embedded as a header comment in the output file.
"""

from __future__ import annotations

import argparse
import math
import random
from dataclasses import dataclass, field, asdict
from pathlib import Path


# ============================================================================
# Configuration
# ============================================================================

@dataclass
class GenConfig:
    # ---- Schema --------------------------------------------------------
    # ``num_tables`` is auto-derived from ``num_transactions`` as
    # ``max(tables_min, ceil(sqrt(num_transactions)))`` when left as None.
    # Pass an explicit int to override.  Square-root scaling is sub-linear
    # (so the table-to-txn ratio shrinks as the workload grows) and matches
    # the rough shape of real OLTP suites without any extra knobs to tune.
    num_tables: int | None = None
    tables_min: int = 2              # floor when auto-deriving
    fields_per_table: int = 10        # constant int; "fixed to a rate"
    pk_arity_min: int = 1
    pk_arity_max: int = 2            # uniform in [min, max] per table

    # ---- Workload sizing (MAIN KNOB: num_transactions) -----------------
    num_transactions: int = 20

    # ---- Hops per transaction ------------------------------------------
    hops_min: int = 1
    hops_max: int = 10
    # one of: zipf, reverse_zipf, uniform, poisson
    hops_dist: str = "zipf"
    hops_zipf_s: float = 2.0         # tail-weight for (reverse_)zipf
    hops_poisson_mean: float = 7.0   # only used when hops_dist == "poisson"

    # ---- Operations per hop (Zipf, head-heavy) -------------------------
    ops_min: int = 1
    ops_max: int = 5
    ops_zipf_s: float = 1.5

    # ---- Operation mix (auto-normalized in __post_init__) --------------
    op_read_p: float = 0.50
    op_write_p: float = 0.20
    op_inc_dec_p: float = 0.10
    op_logical_p: float = 0.20

    # ---- For-loop within hop -------------------------------------------
    for_loop_prob: float = 0.05
    for_loop_iters_min: int = 5
    for_loop_iters_max: int = 10

    # ---- If-statement within hop ---------------------------------------
    # Per-hop chance of inserting an ``if cond { ... } [else { ... }]`` block
    # somewhere in the op sequence.  Body branches each have their own block
    # scope (vars declared inside die at the end of the branch).
    if_prob: float = 0.25
    if_else_prob: float = 0.5         # P(else branch | if exists)
    if_body_min_ops: int = 1
    if_body_max_ops: int = 2

    # ---- Cross-hop continuation variable -------------------------------
    # Probability that a hop-level var declared in hop N is promoted into the
    # set of names eligible for reference in hop N+1, N+2, ...
    # Loop-body locals are NEVER eligible — see Scope.begin_loop / end_loop.
    continuation_prob: float = 0.30

    # ---- Reproducibility -----------------------------------------------
    seed: int = 42

    def __post_init__(self) -> None:
        # Validate basics that the table-derivation depends on
        assert self.num_transactions >= 0
        assert self.tables_min >= 1

        # Auto-derive num_tables when not supplied explicitly: sqrt scaling.
        if self.num_tables is None:
            self.num_tables = max(
                self.tables_min,
                math.ceil(math.sqrt(self.num_transactions)),
            )

        # Remaining range checks
        assert self.hops_min >= 1, "hops_min must be >= 1 (zero-hop txns are degenerate)"
        assert self.hops_max >= self.hops_min
        assert self.ops_min >= 1 and self.ops_max >= self.ops_min
        assert self.pk_arity_min >= 1 and self.pk_arity_max >= self.pk_arity_min
        assert self.num_tables >= 1
        assert 0.0 <= self.for_loop_prob <= 1.0
        assert 0.0 <= self.if_prob <= 1.0
        assert 0.0 <= self.if_else_prob <= 1.0
        assert 1 <= self.if_body_min_ops <= self.if_body_max_ops
        assert 0.0 <= self.continuation_prob <= 1.0
        assert self.hops_dist in {"zipf", "reverse_zipf", "uniform", "poisson"}

        # Normalize op probabilities
        total = (self.op_read_p + self.op_write_p
                 + self.op_inc_dec_p + self.op_logical_p)
        if total <= 0:
            raise ValueError("Operation probabilities must sum to a positive value.")
        self.op_read_p /= total
        self.op_write_p /= total
        self.op_inc_dec_p /= total
        self.op_logical_p /= total


# ============================================================================
# Distributions
# ============================================================================

def zipf_sample(low: int, high: int, s: float, rng: random.Random) -> int:
    """Bounded Zipf: weight(rank_i) ∝ 1/(i+1)^s.  Mass concentrated at `low`."""
    ks = list(range(low, high + 1))
    weights = [1.0 / ((i + 1) ** s) for i in range(len(ks))]
    return rng.choices(ks, weights=weights, k=1)[0]


def reverse_zipf_sample(low: int, high: int, s: float, rng: random.Random) -> int:
    """Reverse-Zipf: weight(rank_i) ∝ (i+1)^s.  Mass concentrated at `high`."""
    ks = list(range(low, high + 1))
    weights = [((i + 1) ** s) for i in range(len(ks))]
    return rng.choices(ks, weights=weights, k=1)[0]


def poisson_sample(mean: float, low: int, high: int, rng: random.Random) -> int:
    """Knuth's algorithm; clamped to [low, high]."""
    L = math.exp(-mean)
    k = 0
    p = 1.0
    while True:
        k += 1
        p *= rng.random()
        if p <= L:
            return max(low, min(high, k - 1))


def sample_hops(cfg: GenConfig, rng: random.Random) -> int:
    if cfg.hops_dist == "zipf":
        return zipf_sample(cfg.hops_min, cfg.hops_max, cfg.hops_zipf_s, rng)
    if cfg.hops_dist == "reverse_zipf":
        return reverse_zipf_sample(cfg.hops_min, cfg.hops_max, cfg.hops_zipf_s, rng)
    if cfg.hops_dist == "uniform":
        return rng.randint(cfg.hops_min, cfg.hops_max)
    if cfg.hops_dist == "poisson":
        return poisson_sample(cfg.hops_poisson_mean, cfg.hops_min, cfg.hops_max, rng)
    raise ValueError(f"Unknown hops_dist: {cfg.hops_dist}")


# ============================================================================
# Schema model
# ============================================================================

@dataclass
class Field:
    name: str
    # type is always "int" in this generator


@dataclass
class Table:
    name: str
    pks: list[Field]
    fields: list[Field]

    @property
    def arity(self) -> int:
        return len(self.pks)


def gen_schema(cfg: GenConfig, rng: random.Random) -> list[Table]:
    """Generate `num_tables` tables with int PKs + int fields."""
    tables: list[Table] = []
    for i in range(cfg.num_tables):
        tname = f"T{i}"
        pk_arity = rng.randint(cfg.pk_arity_min, cfg.pk_arity_max)
        pks = [Field(name=f"{tname.lower()}_pk{j}") for j in range(pk_arity)]
        fields = [Field(name=f"{tname.lower()}_f{j}")
                  for j in range(cfg.fields_per_table)]
        tables.append(Table(name=tname, pks=pks, fields=fields))
    return tables


# ============================================================================
# Scope tracking
# ============================================================================

@dataclass
class Var:
    name: str


class Scope:
    """Tracks int-typed names visible to op emitters in a transaction.

    Three tiers of visibility:

    * ``txn_params``   — always in scope (transaction parameters).
    * ``prior_hop_vars`` — hop-level vars from earlier hops that were promoted
      via ``end_hop`` based on ``continuation_prob``.  Only TOP-LEVEL hop vars
      are eligible: block-local vars (loop bodies, if/else branches) never
      enter this set, mirroring FMitF's block scoping rules.
    * ``hop_vars`` — current-hop locals declared at the hop's top level.
    * ``_block_stack`` — a stack of dicts holding vars declared inside nested
      block constructs (``for`` body, ``if`` body, ``else`` body, ...).  The
      top frame is the innermost open block.  Each block frame is discarded
      by its corresponding ``end_block`` call.
    """

    def __init__(self) -> None:
        self.txn_params: dict[str, Var] = {}
        self.hop_vars: dict[str, Var] = {}
        self.prior_hop_vars: dict[str, Var] = {}
        self._block_stack: list[dict[str, Var]] = []
        self._counter = 0

    def fresh(self, prefix: str = "v") -> str:
        n = f"{prefix}{self._counter}"
        self._counter += 1
        return n

    def add_param(self, name: str) -> None:
        if name not in self.txn_params:
            self.txn_params[name] = Var(name=name)

    def add_hop_var(self, var: Var) -> None:
        """Register a newly declared var.  Routes to the innermost block
        frame when any is open, otherwise to the hop top-level dict."""
        target = self._block_stack[-1] if self._block_stack else self.hop_vars
        target[var.name] = var

    def begin_block(self) -> None:
        """Open a new block-local sub-scope (for a ``for`` body, ``if`` body,
        ``else`` body, ...).  Vars added while it is the innermost open frame
        will be discarded by the matching ``end_block`` and will NEVER become
        cross-hop continuation candidates."""
        self._block_stack.append({})

    def end_block(self) -> None:
        """Close the innermost block sub-scope, discarding its locals."""
        assert self._block_stack, "end_block without matching begin_block"
        self._block_stack.pop()

    def end_hop(self, continuation_prob: float, rng: random.Random) -> None:
        """At hop boundary: optionally promote one hop-level var into
        ``prior_hop_vars`` so later hops can reference it (cross-hop
        continuation, à la auctionmark's ``buyer_id``).  Block-local vars are
        intentionally excluded — they're not in ``hop_vars`` to begin with."""
        assert not self._block_stack, "end_hop called inside an open block frame"
        candidates = list(self.hop_vars.values())
        if candidates and rng.random() < continuation_prob:
            chosen = rng.choice(candidates)
            self.prior_hop_vars[chosen.name] = chosen
        self.hop_vars = {}

    def all_int_vars(self) -> list[Var]:
        """Every int-typed name visible at the current emission point."""
        out: list[Var] = []
        out.extend(self.txn_params.values())
        out.extend(self.prior_hop_vars.values())
        out.extend(self.hop_vars.values())
        for frame in self._block_stack:
            out.extend(frame.values())
        return out


# ============================================================================
# Operation emitters
# ============================================================================

def _pick_int_value(scope: Scope, rng: random.Random,
                    prefer_var_prob: float = 0.7) -> str:
    """Return an int-typed expression: an in-scope var name or a literal."""
    int_vars = scope.all_int_vars()
    if int_vars and rng.random() < prefer_var_prob:
        return rng.choice(int_vars).name
    return str(rng.randint(0, 1000))


def _pk_args(table: Table, scope: Scope, rng: random.Random) -> str:
    """Build the ``pk0: expr, pk1: expr, ...`` clause for a row access."""
    parts = [f"{pk.name}: {_pick_int_value(scope, rng)}" for pk in table.pks]
    return ", ".join(parts)


def emit_read(table: Table, scope: Scope, rng: random.Random) -> list[str]:
    fld = rng.choice(table.fields)
    var_name = scope.fresh()
    line = f"var {var_name}: int = {table.name}[{_pk_args(table, scope, rng)}].{fld.name};"
    scope.add_hop_var(Var(name=var_name))
    return [line]


def emit_write(table: Table, scope: Scope, rng: random.Random) -> list[str]:
    """Row-literal overwrite (INSERT-like) or direct single-field write."""
    if rng.random() < 0.5:
        keys = _pk_args(table, scope, rng)
        assigns = ", ".join(
            f"{f.name}: {_pick_int_value(scope, rng, prefer_var_prob=0.4)}"
            for f in table.fields
        )
        return [f"{table.name}[{keys}] = {{ {assigns} }};"]

    # Direct single-field write: T[keys].field = expr;
    keys = _pk_args(table, scope, rng)
    fld = rng.choice(table.fields)
    new_val = _pick_int_value(scope, rng)
    return [f"{table.name}[{keys}].{fld.name} = {new_val};"]


def emit_inc_dec(table: Table, scope: Scope, rng: random.Random) -> list[str]:
    """Direct in-place increment/decrement: T[keys].field = T[keys].field +/- N;"""
    keys = _pk_args(table, scope, rng)
    fld = rng.choice(table.fields)
    op = rng.choice(["+", "-"])
    delta = rng.randint(1, 5)
    return [
        f"{table.name}[{keys}].{fld.name} = "
        f"{table.name}[{keys}].{fld.name} {op} {delta};"
    ]


def emit_logical(scope: Scope, rng: random.Random) -> list[str]:
    """Pure computation; touches no table."""
    var_name = scope.fresh()
    int_vars = scope.all_int_vars()
    if int_vars and rng.random() < 0.7:
        lhs = rng.choice(int_vars).name
        op = rng.choice(["+", "-", "*"])
        rhs = (rng.choice(int_vars).name
               if rng.random() < 0.5 else str(rng.randint(0, 10)))
        expr = f"{lhs} {op} {rhs}"
    else:
        expr = str(rng.randint(0, 100))
    scope.add_hop_var(Var(name=var_name))
    return [f"var {var_name}: int = {expr};"]


# ============================================================================
# Hop / transaction assembly
# ============================================================================

OP_LABELS = ("read", "write", "inc_dec", "logical")
COND_OPS = ("<", ">", "<=", ">=", "==", "!=")


def _emit_one_op(table: Table, weights: list[float],
                 scope: Scope, rng: random.Random) -> list[str]:
    """Emit a single op (one statement) chosen by the weighted mix."""
    op = rng.choices(OP_LABELS, weights=weights, k=1)[0]
    if op == "read":
        return emit_read(table, scope, rng)
    if op == "write":
        return emit_write(table, scope, rng)
    if op == "inc_dec":
        return emit_inc_dec(table, scope, rng)
    return emit_logical(scope, rng)


def _emit_op_sequence(table: Table, n_ops: int, weights: list[float],
                      scope: Scope, rng: random.Random) -> list[str]:
    """Emit ``n_ops`` operations against ``table`` per the weighted op mix."""
    out: list[str] = []
    for _ in range(n_ops):
        out.extend(_emit_one_op(table, weights, scope, rng))
    return out


def _gen_condition(scope: Scope, rng: random.Random) -> str:
    """Generate a simple boolean expression: <int_expr> <op> <int_expr>.

    Biased to use in-scope variables on the LHS so the condition exercises
    real dataflow rather than constant-folding away.
    """
    lhs = _pick_int_value(scope, rng, prefer_var_prob=0.85)
    rhs = _pick_int_value(scope, rng, prefer_var_prob=0.4)
    return f"{lhs} {rng.choice(COND_OPS)} {rhs}"


def _emit_if_block(table: Table, cfg: GenConfig, weights: list[float],
                   scope: Scope, rng: random.Random) -> list[str]:
    """Emit ``if cond { ... } [else { ... }]`` with proper block scoping.

    Each branch opens its own block frame on ``scope``, so vars declared
    inside the branch die when the branch ends and never become eligible
    for cross-hop continuation.
    """
    cond = _gen_condition(scope, rng)

    scope.begin_block()
    try:
        if_body = _emit_op_sequence(
            table,
            rng.randint(cfg.if_body_min_ops, cfg.if_body_max_ops),
            weights, scope, rng,
        )
    finally:
        scope.end_block()

    out = [f"if {cond} {{"]
    out.extend("    " + ln for ln in if_body)

    if rng.random() < cfg.if_else_prob:
        scope.begin_block()
        try:
            else_body = _emit_op_sequence(
                table,
                rng.randint(cfg.if_body_min_ops, cfg.if_body_max_ops),
                weights, scope, rng,
            )
        finally:
            scope.end_block()
        out.append("} else {")
        out.extend("    " + ln for ln in else_body)

    out.append("}")
    return out


def _emit_hop_statements(table: Table, n_ops: int, weights: list[float],
                         cfg: GenConfig, scope: Scope,
                         rng: random.Random) -> list[str]:
    """Build one hop's statement sequence in textual order.

    With probability ``cfg.if_prob`` an ``if``-block is spliced in at a
    randomly chosen position ``[0, n_ops]``.  Crucially, the position is
    chosen FIRST and statements are then emitted in order, so the if-block's
    condition and body only see variables that have actually been declared
    before it in the output.
    """
    if_pos = rng.randint(0, n_ops) if rng.random() < cfg.if_prob else None

    statements: list[list[str]] = []
    for i in range(n_ops):
        if i == if_pos:
            statements.append(_emit_if_block(table, cfg, weights, scope, rng))
        statements.append(_emit_one_op(table, weights, scope, rng))
    if if_pos == n_ops:
        statements.append(_emit_if_block(table, cfg, weights, scope, rng))

    return [line for stmt in statements for line in stmt]


def gen_hop_body(table: Table, cfg: GenConfig, scope: Scope,
                 rng: random.Random) -> list[str]:
    """Generate the lines that go inside a single hop block.

    With probability ``cfg.for_loop_prob`` the hop's entire statement
    sequence is wrapped in a ``for`` loop.  An ``if``-block may also be
    spliced in (see ``cfg.if_prob``); when both fire, the if lives inside
    the for body, with each construct getting its own block frame on
    ``scope`` so block-local vars never leak.
    """
    n_ops = zipf_sample(cfg.ops_min, cfg.ops_max, cfg.ops_zipf_s, rng)
    weights = [cfg.op_read_p, cfg.op_write_p,
               cfg.op_inc_dec_p, cfg.op_logical_p]

    if rng.random() < cfg.for_loop_prob:
        iters = rng.randint(cfg.for_loop_iters_min, cfg.for_loop_iters_max)
        loop_var = scope.fresh("i")
        scope.begin_block()
        try:
            scope.add_hop_var(Var(name=loop_var))  # loop var visible in body
            inner = _emit_hop_statements(table, n_ops, weights, cfg, scope, rng)
        finally:
            scope.end_block()
        body = ["    " + ln for ln in inner]
        return [
            f"for (var {loop_var}: int = 0; "
            f"{loop_var} < {iters}; "
            f"{loop_var} = {loop_var} + 1) {{",
            *body,
            "}",
        ]

    return _emit_hop_statements(table, n_ops, weights, cfg, scope, rng)


def gen_transaction(idx: int, cfg: GenConfig, tables: list[Table],
                    rng: random.Random) -> str:
    """Build one ``transaction TxN(...) { hop {...} ... }`` block."""
    scope = Scope()
    n_hops = sample_hops(cfg, rng)

    # Pick a primary table per hop. Multi-hop transactions prefer distinct
    # tables so different hops naturally land on different partitions
    # (which is what makes chopping verification non-trivial).
    if n_hops <= len(tables):
        hop_tables = rng.sample(tables, n_hops)
    else:
        hop_tables = [rng.choice(tables) for _ in range(n_hops)]

    # Hoist all PKs of every touched table into the txn parameter list.
    param_order: list[str] = []
    for t in hop_tables:
        for pk in t.pks:
            if pk.name not in scope.txn_params:
                scope.add_param(pk.name)
                param_order.append(pk.name)

    hop_blocks: list[str] = []
    for t in hop_tables:
        body_lines = gen_hop_body(t, cfg, scope, rng)
        body_text = "\n".join("        " + ln for ln in body_lines)
        hop_blocks.append("    hop {\n" + body_text + "\n    }")
        scope.end_hop(cfg.continuation_prob, rng)

    param_str = ", ".join(f"{p}: int" for p in param_order)
    header = f"transaction Tx{idx}({param_str}) {{"
    return header + "\n" + "\n".join(hop_blocks) + "\n}"


# ============================================================================
# Schema emit
# ============================================================================

def emit_partitions(arities: set[int]) -> str:
    """Emit one partition function per distinct PK arity in use."""
    out: list[str] = []
    for a in sorted(arities):
        params = ", ".join(f"x{i}: int" for i in range(a))
        if a == 1:
            body = "return x0;"
        else:
            body = "return " + " + ".join(f"x{i}" for i in range(a)) + ";"
        out.append(f"partition p{a}({params}) -> int {{ {body} }}")
    return "\n".join(out)


def emit_table(t: Table) -> str:
    lines = [f"table {t.name} {{"]
    for pk in t.pks:
        lines.append(f"    primary {pk.name}: int;")
    pk_args = ", ".join(pk.name for pk in t.pks)
    lines.append(f"    node p{t.arity}({pk_args});")
    for f in t.fields:
        lines.append(f"    {f.name}: int;")
    lines.append("}")
    return "\n".join(lines)


def emit_header(cfg: GenConfig) -> str:
    bar = "// " + "=" * 72
    lines = [bar,
             "// Synthetic .transact file generated by gen_transact.py",
             "//",
             "// Configuration:"]
    for k, v in asdict(cfg).items():
        lines.append(f"//   {k} = {v}")
    lines.append(bar)
    return "\n".join(lines)


# ============================================================================
# Top-level generator
# ============================================================================

def generate(cfg: GenConfig) -> str:
    rng = random.Random(cfg.seed)
    tables = gen_schema(cfg, rng)
    arities = {t.arity for t in tables}

    parts: list[str] = [emit_header(cfg), emit_partitions(arities)]
    parts.extend(emit_table(t) for t in tables)
    parts.extend(gen_transaction(i, cfg, tables, rng)
                 for i in range(cfg.num_transactions))
    return "\n\n".join(parts) + "\n"


# ============================================================================
# CLI
# ============================================================================

def _build_parser() -> argparse.ArgumentParser:
    """Mirror every GenConfig field as a --kebab-case CLI flag."""
    defaults = GenConfig()
    p = argparse.ArgumentParser(
        description="Synthetic FMitF .transact file generator.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )

    # Schema
    p.add_argument("--num-tables", type=int, default=None,
                   help="Explicit table count.  When omitted (default), the "
                        "value is auto-derived as "
                        "max(tables_min, ceil(sqrt(num_transactions))).")
    p.add_argument("--tables-min", type=int, default=defaults.tables_min,
                   help="Floor for the auto-derived table count.")
    p.add_argument("--fields-per-table", type=int,
                   default=defaults.fields_per_table)
    p.add_argument("--pk-arity-min", type=int, default=defaults.pk_arity_min)
    p.add_argument("--pk-arity-max", type=int, default=defaults.pk_arity_max)

    # Workload size
    p.add_argument("--num-transactions", type=int,
                   default=defaults.num_transactions,
                   help="MAIN KNOB: number of transactions to emit.")

    # Hops per txn
    p.add_argument("--hops-min", type=int, default=defaults.hops_min)
    p.add_argument("--hops-max", type=int, default=defaults.hops_max)
    p.add_argument("--hops-dist",
                   choices=["zipf", "reverse_zipf", "uniform", "poisson"],
                   default=defaults.hops_dist)
    p.add_argument("--hops-zipf-s", type=float, default=defaults.hops_zipf_s)
    p.add_argument("--hops-poisson-mean", type=float,
                   default=defaults.hops_poisson_mean)

    # Ops per hop
    p.add_argument("--ops-min", type=int, default=defaults.ops_min)
    p.add_argument("--ops-max", type=int, default=defaults.ops_max)
    p.add_argument("--ops-zipf-s", type=float, default=defaults.ops_zipf_s)

    # Op mix
    p.add_argument("--op-read-p", type=float, default=defaults.op_read_p)
    p.add_argument("--op-write-p", type=float, default=defaults.op_write_p)
    p.add_argument("--op-inc-dec-p", type=float,
                   default=defaults.op_inc_dec_p)
    p.add_argument("--op-logical-p", type=float,
                   default=defaults.op_logical_p)

    # For-loop
    p.add_argument("--for-loop-prob", type=float,
                   default=defaults.for_loop_prob)
    p.add_argument("--for-loop-iters-min", type=int,
                   default=defaults.for_loop_iters_min)
    p.add_argument("--for-loop-iters-max", type=int,
                   default=defaults.for_loop_iters_max)

    # If-statement
    p.add_argument("--if-prob", type=float, default=defaults.if_prob,
                   help="Per-hop chance of splicing in an if-block.")
    p.add_argument("--if-else-prob", type=float, default=defaults.if_else_prob,
                   help="Conditional probability of an else branch given the "
                        "if-block is present.")
    p.add_argument("--if-body-min-ops", type=int,
                   default=defaults.if_body_min_ops)
    p.add_argument("--if-body-max-ops", type=int,
                   default=defaults.if_body_max_ops)

    # Continuation
    p.add_argument("--continuation-prob", type=float,
                   default=defaults.continuation_prob,
                   help="Per-hop-boundary chance of promoting one hop-level "
                        "var into the cross-hop reference pool.")

    # Seed + I/O
    p.add_argument("--seed", type=int, default=defaults.seed)
    p.add_argument("--out", type=Path, default=None,
                   help="Output path (writes to stdout if omitted).")
    return p


def main() -> None:
    args = _build_parser().parse_args()
    out_path: Path | None = args.out
    cfg_kwargs = {k: v for k, v in vars(args).items() if k != "out"}
    cfg = GenConfig(**cfg_kwargs)

    text = generate(cfg)

    if out_path is None:
        print(text)
    else:
        out_path.write_text(text)
        print(f"Wrote {out_path}  ({len(text):,} bytes, "
              f"{cfg.num_transactions} transactions)")


if __name__ == "__main__":
    main()
