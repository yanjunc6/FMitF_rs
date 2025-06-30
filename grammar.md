```
(* ------------------------------------------------- *)
(* Lexical tokens *)
(* ------------------------------------------------- *)
letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z" | "_" ;
digit  = "0" | "1" | ... | "9" ;
letterOrDigitOrUnderscore = letter | digit | "_" ;

Identifier =
    letter,
    { letterOrDigitOrUnderscore }
;

IntegerLiteral =
    digit,
    { digit }
;

FloatLiteral =
    digit,
    { digit },
    ".",
    digit,
    { digit }
;

StringLiteral =
    '"',
    { characterExceptDoubleQuote },
    '"'
;

BooleanLiteral = "true" | "false" ;

Comment          = "//", { ANY_CHARACTER_EXCEPT_NEWLINE } ;
Whitespace       = { " " | "\t" | "\r" | "\n" } ;

(* ------------------------------------------------- *)
(* Program structure *)
(* ------------------------------------------------- *)
Program =
    NodesBlock,
    { TableDeclaration },
    { FunctionDeclaration }
;

(* ------------------------------------------------- *)
(* Nodes *)
(* ------------------------------------------------- *)
NodesBlock =
    "nodes",
    "{",
        NodeList,
    "}"
;

NodeList =
    Identifier,
    { ",", Identifier }
;

(* ------------------------------------------------- *)
(* Tables *)
(* ------------------------------------------------- *)
TableDeclaration =
    "table",
    Identifier,
    "on",
    Identifier,
    "{",
        { FieldDeclaration },
    "}"
;

FieldDeclaration =
    [ "primary" ],
    Type,
    Identifier,
    ";"
;

Type = "int" | "float" | "string" | "bool" ;

(* ------------------------------------------------- *)
(* Functions *)
(* ------------------------------------------------- *)
FunctionDeclaration =
    ReturnType,
    Identifier,
    "(",
        [ ParameterList ],
    ")",
    "{",
        { FunctionBodyItem },
    "}"
;

ReturnType = Type | "void" ;

ParameterList =
    ParameterDecl,
    { ",", ParameterDecl }
;

ParameterDecl =
    Type,
    Identifier
;

FunctionBodyItem =
    HopBlock
;

(* ------------------------------------------------- *)
(* Hops *)
(* ------------------------------------------------- *)
HopBlock =
    "hop",
    "on",
    Identifier,
    "{",
        { Statement },
    "}"
;

(* ------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------- *)
Statement =
    VarDeclStatement
  | VarAssignmentStatement
  | AssignmentStatement
  | IfStatement
  | WhileStatement
  | ReturnStatement
  | AbortStatement
  | BreakStatement
  | ContinueStatement
  | EmptyStatement
;

VarDeclStatement =
    Type,
    Identifier,
    "=",
    Expression,
    ";"
;

VarAssignmentStatement =
    Identifier,
    "=",
    Expression,
    ";"
;

AssignmentStatement =
    Identifier,
    "[",
        PrimaryKeyList,
    "]",
    ".",
    Identifier,
    "=",
    Expression,
    ";"
;

PrimaryKeyList =
    PrimaryKeyPair,
    { ",", PrimaryKeyPair }
;

PrimaryKeyPair =
    Identifier,
    ":",
    Expression
;

IfStatement =
    "if",
    "(",
        Expression,
    ")",
    Block,
    [ "else", Block ]
;

WhileStatement =
    "while",
    "(",
        Expression,
    ")",
    Block
;

ReturnStatement =
    "return",
    [ Expression ],
    ";"
;

AbortStatement =
    "abort",
    ";"
;

BreakStatement =
    "break",
    ";"
;

ContinueStatement =
    "continue",
    ";"
;

EmptyStatement = ";" ;

Block =
    "{",
        { Statement },
    "}"
;

(* ------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------- *)
(* Expressions follow operator precedence from lowest to highest *)

Expression = LogicOr ;

LogicOr =
    LogicAnd,
    { "||", LogicAnd }
;

LogicAnd =
    Equality,
    { "&&", Equality }
;

Equality =
    Comparison,
    { ( "==" | "!=" ), Comparison }
;

Comparison =
    Addition,
    { ( "<=" | ">=" | "<" | ">" ), Addition }
;

Addition =
    Multiplication,
    { ( "+" | "-" ), Multiplication }
;

Multiplication =
    Unary,
    { ( "*" | "/" ), Unary }
;

Unary =
    Primary
  | ( "!" | "-" ), Unary
;

Primary =
    BooleanLiteral
  | TableFieldAccess
  | FloatLiteral
  | IntegerLiteral
  | StringLiteral
  | Identifier
  | "(", Expression, ")"
;

TableFieldAccess =
    Identifier,
    "[",
        PrimaryKeyList,
    "]",
    ".",
    Identifier
;
```