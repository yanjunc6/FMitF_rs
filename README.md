# FMitF (Formal Methods in Transaction Framework)

FMitF is a Rust implementation of a compiler and verification toolchain for a domain-specific language that models distributed transactions. It parses `.transact` programs, builds intermediate representations (AST, CFG, and serialization-conflict graphs), optionally optimizes them, and generates Boogie verification conditions to check commutativity and serializability guarantees.

The repository contains:

- A command-line compiler (`fmitf`) that drives the full compilation and verification pipeline.
- Core libraries for syntax/semantic analysis, control-flow graph construction, optimization, and serialization-conflict graph generation.
- Boogie code generation and result processing utilities for automated formal verification.
- Sample transaction specifications under `examples/` and generated artifacts in `tmp/` for inspection.
- A prototype database that implements distributed IC3 protocol under `db_engine` folder. It is used for evaluating the generated code.

## Installation

### Prerequisites

- **Rust toolchain** with `cargo`.
- **Boogie** command-line verifier (requires .NET 6+).

### Install Rust and Cargo

If you do not already have Rust installed, use `rustup`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"
rustup default stable
```

The compiler and CLI are built with stable Rust; `cargo` is bundled with the toolchain.

### Install Boogie

Boogie is distributed as a .NET global tool. Ensure you have the .NET SDK installed, then run:

```bash
dotnet tool install --global Boogie
```

Verify that Boogie is on your `PATH`:

```bash
boogie /help
```

If you already have Boogie, update it with `dotnet tool update --global Boogie`.

### Build the compiler

Clone the repository and build the binary:

```bash
git clone https://github.com/yanjunc6/FMitF_rs.git
cd FMitF_rs
cargo build --release
```

This produces the optimized `fmitf` binary at `target/release/fmitf`. You can also run the tool directly with `cargo run` (see below).

## Usage

The `fmitf` CLI accepts a `.transact` source file and optionally an output directory and verification parameters. Example:

```bash
cargo run --release -- examples/tpcc.transact
```

Key options (all can be passed after `--` when using `cargo run`):

| Flag | Description | Default |
| --- | --- | --- |
| `--output-dir <path>` | Directory where AST/CFG/graph dumps and Boogie files are written. | `tmp/` |
| `--instances <n>` | Number of transaction instances used when building the serialization-conflict graph. | `2` |
| `--loop-unroll <n>` | Loop unrolling bound forwarded to Boogie (must be ≥ 1). | `12` |
| `--timeout <sec>` | Per-program Boogie time limit (must be ≥ 1). | `30` |
| `--no-optimize` | Skip control-flow optimizations before verification. | disabled |
| `--no-verify` | Skip Boogie verification (still emits intermediate artifacts). | disabled |
| `--no-color` | Disable ANSI coloring in terminal output. | disabled |

During a run, the compiler writes:

- `tmp/ast_pretty.txt`: formatted AST of the input program.
- `tmp/cfg_pretty.txt` (and `cfg_opt_pretty.txt` when optimizations are enabled): graph-friendly CFG dumps.
- `tmp/sc_graph*.dot` (and `simplified_sc_graph*.dot` when verif are enabled): serialization-conflict graph visualizations.
- `tmp/sc_graph_plotted.dot`: a colored serialization-conflict graph visualizations combining two versions.
- `tmp/Boogie/*.bpl`: generated Boogie verification conditions. Logs and Boogie output are stored in `tmp/compiler.log`.

To inspect verification failures, open the associated `.bpl` files or review the detailed log.

## Evaluation

A script is provided to compile three benchmark:
```
bash ./run_benchmark.sh
```

Please note that this will take a few hours. A quick benchmark can be achieved by commenting out first two tpcc passes in script.

To run them on our prototype database, please refer [database engine guide](db_engine/README.md).