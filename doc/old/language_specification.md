# FMitF Language Specification

## Table of Contents

1. [Introduction](#introduction)
2. [Lexical Structure](#lexical-structure)
3. [Types](#types)
4. [Declarations](#declarations)
5. [Statements](#statements)
6. [Expressions](#expressions)
7. [Distributed Computing Constructs](#distributed-computing-constructs)
8. [Built-in Types and Operations](#built-in-types-and-operations)
9. [Examples](#examples)

## Introduction

FMitF (Formal Methods in Transaction Framework) is a domain-specific language for defining and analyzing distributed transactions with formal verification capabilities. The language provides constructs for defining distributed data structures (tables), transaction logic with atomic hops, and partition functions for data placement.

### Key Features

- **Tables**: Distributed data structures with partitioning and invariants
- **Transactions**: Multi-hop distributed transactions with atomic execution
- **Partitions**: Functions defining data placement across nodes
- **Operators**: Custom operators with configurable precedence
- **Formal Verification**: Built-in constructs for assumptions and assertions

## Lexical Structure

### Keywords

The following identifiers are reserved and cannot be used as variable names:

```
assert  assume  const   else    false   for     function
hop     hops    if      in      invariant node   operator
partition primary return table  transaction true  type
var     void    to
```

### Identifiers

Identifiers start with a letter or underscore, followed by letters, digits, or underscores:

```
identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

### Operators

Custom operators can be defined using any combination of these characters:
```
* / % + - : < > = ! & ^ | $ @ ~
```

Operator precedence is determined by the first character:
1. `!`, `-` (prefix operators) - highest precedence
2. `*`, `/`, `%` (left-associative)
3. `+`, `-` (infix, left-associative)
4. `:` (right-associative)
5. `<`, `>`, `=`, `!` (comparison, left-associative)
6. `@`, `~` (right-associative)
7. `&` (left-associative)
8. `^` (right-associative)
9. `$`, `|` (left-associative)
10. `=` (assignment, right-associative) - lowest precedence

### Comments

Single-line comments start with `//` and continue to the end of the line.

## Types

### Primitive Types

Built-in primitive types:
- `int`: Integer numbers
- `bool`: Boolean values (`true`, `false`)
- `string`: Text strings
- `float`: Floating-point numbers
- `void`: Absence of a value

### Generic Types

Types can be parameterized with type arguments:
```
List<int>
Map<string, int>
Result<T, E>
```

### Function Types

Function types specify parameter and return types:
```
(int, string) -> bool
() -> void
(T) -> T
```

### User-Defined Types

New types can be declared:
```
type UserId;
type AccountId;
type Balance;
```

## Declarations

### Function Declarations

Functions define reusable computation:

```
function add(x: int, y: int) -> int {
    return x + y;
}

// Forward declaration
function complex_calculation(data: DataSet) -> Result;
```

### Operator Declarations

Custom operators with specified precedence:

```
@infix operator +(int, int) -> int;
@prefix operator -(int) -> int;
```

Decorators specify operator associativity:
- `@infix`: Binary infix operator (default)
- `@prefix`: Unary prefix operator

### Partition Functions

Partition functions determine data placement across nodes:

```
partition by_user_id(user_id: int) -> int {
    return user_id % 100;
}

partition by_account(account_id: string, region: string) -> int {
    return hash(account_id) + region_offset(region);
}
```

### Transaction Functions

Transactions define distributed business logic:

```
transaction transfer(from: AccountId, to: AccountId, amount: int) {
    hop {
        // First hop: validate source account
        assert(accounts[from].balance >= amount);
        accounts[from].balance = accounts[from].balance - amount;
    }
    
    hop {
        // Second hop: credit destination account
        accounts[to].balance = accounts[to].balance + amount;
    }
}
```

### Constant Declarations

Constants define immutable values:

```
const MAX_TRANSFER_AMOUNT: int = 1000000;
const DEFAULT_TIMEOUT: float = 30.0;
```

### Table Declarations

Tables define distributed data structures:

```
table Account {
    primary id: AccountId;
    node by_user_id(id);
    balance: int;
    owner: string;
    invariant balance >= 0;
}
```

Table fields can be:
- **Primary key**: `primary field_name: type;`
- **Regular field**: `field_name: type;`
- **Node placement**: `node partition_func(args);`
- **Invariant**: `invariant boolean_expression;`

### Type Declarations

Define new type names:

```
type AccountId;
type UserId;
type Balance;

// Generic types
type Result<T, E>;
type List<T>;
```

## Statements

### Variable Declarations

Variables can be declared with optional type annotation and initialization:

```
var count: int;
var name: string = "default";
var computed = calculate_value();
```

### Control Flow

#### If Statements
```
if (balance >= amount) {
    process_transfer();
} else {
    reject_transfer();
}
```

#### For Loops
```
for (var i: int = 0; i < count; i = i + 1) {
    process_item(i);
}
```

#### Return Statements
```
return result;
return;  // void return
```

### Assertions

Verify conditions at runtime:
```
assert(balance >= 0);
assert(user_id != null);
```

### Expression Statements

Any expression can be used as a statement:
```
process_data();
x = y + z;
```

## Expressions

### Literals

#### Numbers
```
42          // integer
3.14159     // float
```

#### Strings
```
"Hello, world!"
"Escaped \"quotes\" and \\backslashes"
```

#### Booleans
```
true
false
```

#### Lists
```
[1, 2, 3, 4]
[]  // empty list
```

#### Row Literals
```
{id: 123, name: "Alice", balance: 1000}
{}  // empty row
```

### Binary Operations

Operations follow operator precedence rules:
```
a + b * c       // equivalent to a + (b * c)
x < y && y < z  // equivalent to (x < y) && (y < z)
```

### Function Calls

```
add(x, y)
process_data(input, options)
```

### Member Access

Access fields of tables or objects:
```
account.balance
user.name
```

### Table Row Access

Access specific rows in tables:
```
accounts[{id: user_id}]
users[{email: "user@example.com"}]
```

### Assignment

Assign values to variables or table fields:
```
x = 42;
account.balance = new_balance;
```

## Distributed Computing Constructs

### Hops

Hops define atomic operations that execute on a single node:

```
hop {
    // All operations in this block execute atomically
    // on the same node
    account.balance = account.balance - amount;
    log_transaction(account.id, amount);
}
```

### Hops For Loops

Distribute loop iterations across nodes:

```
hops for user: UserId = start_user to end_user {
    // Each iteration executes on the node containing user
    update_user_statistics(user);
}
```

<!-- ### Decorators on Hops

Hops can have decorators for optimization hints:

```
@parallel hop {
    // This hop can potentially run in parallel with others
    compute_statistics();
}

@critical hop {
    // This hop requires strong consistency
    update_critical_data();
} 
```-->

## Built-in Types and Operations

### Arithmetic Operators

For `int` and `float` types:
- `+`, `-`, `*`, `/`: Basic arithmetic
- `%`: Modulo (integers only)
- `-`: Unary negation

### Comparison Operators

For all types:
- `==`, `!=`: Equality comparison
- `<`, `<=`, `>`, `>=`: Ordering (numbers and strings)

### Logical Operators

For `bool` type:
- `&&`: Logical AND
- `||`: Logical OR
- `!`: Logical NOT

### String Operators

- `+`: String concatenation

### Built-in Functions

- `print(s: string)`: Output string
- `len(s: string) -> int`: String length
- `min(a: int, b: int) -> int`: Minimum value
- `max(a: int, b: int) -> int`: Maximum value
- `abs(x: int) -> int`: Absolute value

## Examples

### Bank Transfer System

```
// Data types
type AccountId;
type TransactionId;

// Partition function
partition by_account(id: AccountId) -> int {
    return hash(id) % 100;
}

// Account table
table Account {
    primary id: AccountId;
    node by_account(id);
    balance: int;
    owner: string;
    invariant balance >= 0;
}

// Transaction table
table Transaction {
    primary id: TransactionId;
    node by_account(from_account);
    from_account: AccountId;
    to_account: AccountId;
    amount: int;
    status: string;
}

// Transfer transaction
transaction transfer(from: AccountId, to: AccountId, amount: int)
    assume amount > 0; 
{
    hop {
        // Validate and debit source account
        assert(Account[id: from].balance >= amount);
        Account[id: from].balance = Account[id: from].balance - amount;
    }
    
    hop {
        // Credit destination account
        Account[id: to].balance = Account[id: to].balance + amount;
    }
    
    hop {
        // Record transaction
        var tx_id: TransactionId = generate_id();
        Transaction[id: tx_id] = {
            id: tx_id,
            from_account: from,
            to_account: to,
            amount: amount,
            status: "completed"
        };
    }
}
```

### Custom Operators

```
// Define matrix multiplication operator
@infix operator **(Matrix, Matrix) -> Matrix;

// Define pipe operator for function composition
@infix operator |>(T, (T) -> U) -> U;

// Usage
var result: Matrix = matrix_a ** matrix_b;
var processed: string = input |> clean |> validate |> format;
```

### Distributed Data Processing

```
type UserId;
type EventId;

partition by_user(user: UserId) -> int {
    return hash(user) % 50;
}

table UserEvent {
    primary id: EventId;
    node by_user(user_id);
    user_id: UserId;
    event_type: string;
    timestamp: int;
    data: string;
}

transaction process_user_events(start_time: int, end_time: int) {
    hops for user: UserId = min_user() to max_user() {
        var events: List<UserEvent> = get_user_events(user, start_time, end_time);
        for (var i: int = 0; i < len(events); i = i + 1) {
            process_event(events[i]);
        }
    }
}
```
