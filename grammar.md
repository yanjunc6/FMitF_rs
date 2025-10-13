# FMitF language grammar

This document describes the FMitF (Formal Methods in Transaction Framework) language grammar. This file summarizes the grammar, highlights key design and parsing decisions, and provides an EBNF-like rendering of the same grammar for easier reading and reference.

## Overview

FMitF is a small, statically-typed domain-specific language for specifying distributed transactions, partitioning, and verification-friendly constructs. The compiler pipeline parses `.transact` programs into an AST, builds control-flow graphs (CFGs), generates serialization-conflict graphs (SC-graphs), and emits verification conditions for Boogie.

Key language features:

- Top-level declarations: functions, transactions, partitions, type aliases, constants, and table definitions.
- Table and node declarations used to describe data layout and deployment topology.
- `hop` blocks and `hops for` loops for expressing localized transaction fragments (pieces/hops).
- First-class expressions with function calls, method-style invocation, row/list literals, and lambda expressions.
- User-definable operator symbols (multi-character operators are allowed) with a precedence hierarchy encoded in the grammar.

## Lexical structure

- Whitespace and comments
	- `WHITESPACE` matches spaces, tabs and newlines; comments use `//` to end of line.
- Identifiers
	- An identifier is an alpha or underscore followed by alphanumeric/underscore characters.
	- Keywords (e.g., `function`, `transaction`, `hop`, `table`, `var`) are reserved and cannot be used as identifiers.
- Operators
	- Operator symbols are built from a fixed set of characters and may be multi-character (e.g. `==`, `=>`, `++-`).
	- The grammar uses a precedence-based rule set where the operator's first character is used to select precedence level.

## Types

Types include primitives (named types), generic types (e.g. `List<T>`), and function types (e.g. `(int, string) -> bool`). Generic parameter lists and type lists are supported.

## Top-level declarations

Programs are sequences of declarations. Declarations include:

- `callable_declaration`: `function`, `partition`, `transaction`, or `operator` definitions with optional decorator annotations and optional bodies.
- `type` declarations for aliases.
- `const` declarations for compile-time constants.
- `table` declarations for describing tables with fields, node declarations, and invariants.

Callable declarations allow forward declarations (a trailing `;`) or a body block. `assume` statements may appear on callable declarations as a lightweight contract mechanism.

## Statements & control flow

Statements include variable declarations (`var`), `if`, `for` (C-style), `return`, `assert`, `hop` blocks, `hops for` loops, and nested blocks. The `hops for` construct defines a ranged loop over `hop` indices with an explicit type annotation.

`hop` blocks are the fundamental unit used by the compiler to split transactions into pieces; each `hop` represents a transaction fragment executed on a single node/partition.

## Expressions and operator precedence

Expressions are ordered by precedence from lowest to highest using a series of levels:

- Assignment (`=`) — right-associative (lowest precedence).
- Level 10: `$` and `|` operators — left-assoc.
- Level 9: `^` — right-assoc.
- Level 8: `&` — left-assoc.
- Level 7: `~` — right-assoc.
- Level 6: comparison-like operators `<`, `>`, `!`, `==`, `=>`, etc. — left-assoc. (note: single `=` as assignment is handled separately)
- Level 5: `:` — right-assoc.
- Level 4: `+`, `-` (infix) — left-assoc.
- Level 3: `*`, `/`, `%` — left-assoc.
- Level 2: prefix operators `!` and unary `-` — highest prefix precedence.
- Level 1: Postfix operators — function calls, method calls, field access, and table row access (highest overall precedence).

The grammar allows defining custom operator symbols; the lexer and parser choose precedence using the operator's first character (see `operator_char`).

## Primary expressions and literals

Primary expressions include identifiers, grouped expressions, lambda expressions, and literals. Literal forms include integers, floats, strings, booleans, list literals (`[...]`), and row literals (`{ k: v, ... }`). Row literals are used to construct or update table rows.

Lambda expressions use the syntax `(params?) -> type block` and are first-class values in the language.

## Notes and implementation hints

- `grammar.pest` is the source of truth. This document is descriptive and intended for humans.
- The parser intentionally treats method calls (`a.b(c)`) as `b(a, c)` style calls when lowering; members and field accesses are kept where relevant for semantics.
- The grammar uses Pest's positive lookahead (`&`) to inspect the first character of operator tokens to resolve precedence categories.

If you change precedences or add new operator characters, update both the `operator_char` rule and the precedence levels accordingly.

---

## EBNF (for reference)


```
program         ::=  { declaration }

declaration     ::=  callable_declaration
                 |   type_declaration
                 |   const_declaration
                 |   table_declaration

callable_declaration ::= { decorator } (callable_keyword identifier | 'operator' operator_symbol) [ generic_param_list ] '(' [ parameter_list ] ')' [ '->' type_rule ] { assume_statement } ( block | ';' )
callable_keyword ::= 'function' | 'partition' | 'transaction'

type_declaration ::= { decorator } 'type' identifier [ generic_param_list ] ';'
const_declaration ::= 'const' identifier ':' type_rule '=' expression ';'

table_declaration ::= 'table' identifier '{' { table_field } '}'
table_field    ::= field_declaration | node_declaration | invariant_declaration
field_declaration ::= [ 'primary' ] identifier ':' type_rule ';'
node_declaration ::= 'node' identifier '(' [ expression_list ] ')' ';'
invariant_declaration ::= 'invariant' expression ';'

generic_param_list ::= '<' identifier { ',' identifier } '>'
parameter_list  ::= parameter { ',' parameter }
parameter       ::= identifier ':' type_rule
assume_statement ::= 'assume' expression ';'

block           ::= '{' { statement } '}'
statement       ::= var_declaration
                  | if_statement
                  | for_statement
                  | return_statement
                  | assert_statement
                  | hop_block
                  | hops_for_loop
                  | block
                  | expression_statement

var_declaration ::= 'var' identifier [ ':' type_rule ] [ '=' expression ] ';'
if_statement    ::= 'if' expression block [ 'else' block ]
for_statement   ::= 'for' '(' (var_declaration | expression_statement | ';') ( expression? ';' ) ( expression? ) ')' block
return_statement ::= 'return' [ expression ] ';'
assert_statement ::= 'assert' expression ';'
expression_statement ::= expression ';'

hop_block       ::= { decorator } 'hop' block
hops_for_loop   ::= { decorator } 'hops' 'for' identifier ':' type_rule '=' expression 'to' expression block

-- Types and parameters
type_rule       ::= generic_type | function_type | primitive_type
primitive_type  ::= identifier
generic_type    ::= identifier '<' type_list '>'
function_type   ::= '(' [ type_list ] ')' '->' type_rule
type_list       ::= type_rule { ',' type_rule }

-- Expressions (precedence summarized; each "level" is a nonterminal delegating to the next)
expression      ::= assignment
assignment      ::= level10_expr [ '=' assignment ]
level10_expr    ::= level9_expr { op10 level9_expr }
level9_expr     ::= level8_expr [ op9 level9_expr ]
level8_expr     ::= level7_expr { op8 level7_expr }
level7_expr     ::= level6_expr [ op7 level6_expr ]
level6_expr     ::= level5_expr { op6 level5_expr }
level5_expr     ::= level4_expr [ op5 level4_expr ]
level4_expr     ::= level3_expr { op4 level3_expr }
level3_expr     ::= prefix { op3 prefix }
prefix          ::= [ prefix_op prefix ] | postfix
postfix         ::= primary { call | method_call | field_access | table_row_access }

call            ::= '(' [ expression_list ] ')'
method_call     ::= '.' identifier '(' [ expression_list ] ')'
field_access    ::= '.' identifier
table_row_access ::= '[' [ key_value_pair { ',' key_value_pair } ] ']'

primary         ::= literal | lambda_expression | identifier | '(' expression ')'
lambda_expression ::= '(' [ parameter_list ] ')' '->' type_rule block

literal         ::= primitive_literal | list_literal | row_literal
primitive_literal ::= integer | float | string | bool
integer         ::= digit { digit }
float           ::= digit { digit } '.' digit { digit }
string          ::= '"' { character } '"'
bool            ::= 'true' | 'false'
list_literal    ::= '[' [ expression_list ] ']'
row_literal     ::= '{' [ key_value_pair { ',' key_value_pair } ] '}'
key_value_pair  ::= identifier ':' expression
expression_list ::= expression { ',' expression }

-- Operator token categories (dependent on first character in the concrete grammar)
op10 ::= operator_symbol  -- ($ and | category)
op9  ::= operator_symbol  -- (^ category)
op8  ::= operator_symbol  -- (& category)
op7  ::= operator_symbol  -- (~ category)
op6  ::= operator_symbol  -- (<, >, !, ==, => etc.)
op5  ::= operator_symbol  -- (: category)
op4  ::= operator_symbol  -- (+, - infix)
op3  ::= operator_symbol  -- (*, /, %)
prefix_op ::= operator_symbol -- (!, - unary)

decorator       ::= '@' identifier
identifier      ::= letter { letter | digit | '_' }

WHITESPACE and COMMENT are lexical tokens: skip as usual in a parser.

``` 

If you want the canonical Pest grammar, see `src/frontend/grammar.pest`. Keep this EBNF in sync with that file when you make changes to the language.
