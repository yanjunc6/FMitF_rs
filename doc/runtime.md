# Runtime Mode

The Runtime mode provides an interactive REPL (Read-Eval-Print Loop) environment for testing and experimenting with TransAct programs. It allows you to execute functions and inspect table data in real-time.

## Quick Start

1. **Start the runtime**: `fmitf --mode runtime examples/bank.transact`
2. **List functions**: Type `functions` to see available functions
3. **Call a function**: Use `call <function> <args>` (e.g., `call deposit 1 1000`)
4. **View tables**: Use `table <name>` to inspect data
5. **Exit**: Type `exit` or press `Ctrl+C`

## Usage

```bash
# Start runtime with a TransAct file
fmitf --mode runtime input.transact

# Equivalent short form
fmitf -m runtime input.transact
```

## Features

- **Interactive REPL**: Command-line interface with auto-completion
- **Function Execution**: Call any function defined in your TransAct program
- **Table Inspection**: View table contents and structure
- **Tab Completion**: Auto-complete commands, function names, and table names
- **Clean State Management**: Clear all data to reset the database

## Available Commands

| Command | Description | Example |
|---------|-------------|---------|
| `call <function> [args...]` | Execute a function with arguments | `call transfer 1 2 100` |
| `table <name>` | Display table contents | `table Account` |
| `functions` | List all available functions | `functions` |
| `tables` | List all available tables | `tables` |
| `clear` | Clear all table data (reset database) | `clear` |
| `exit` | Exit the REPL | `exit` |

## Key Points

- **Clean Start**: The database starts empty - use functions to populate data
- **Optimized CFG**: Runs on the optimized Control Flow Graph for better performance
- **Testing Focus**: Designed for quick testing and experimentation, not production use
- **Tab Completion**: Press TAB for command and parameter auto-completion
- **Exit Options**: Use `exit`, `Ctrl+C`, or `Ctrl+D` to quit

## Example Session

```
🚀 TransAct Interactive Runtime
Loaded optimized CFG with 3 functions and 2 tables
> functions
Available functions:
  - transfer
  - deposit
  - withdraw

> tables
Available tables:
  - Account
  - Transaction

> call deposit 1 1000
Executing: deposit ["1", "1000"]

> table Account
Table: Account
+----+---------+
| id | balance |
+----+---------+
| 1  | 1000    |
+----+---------+

> exit
👋 Goodbye!
```
