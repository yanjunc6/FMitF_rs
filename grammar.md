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
    { PartitionDeclaration },
    { TableDeclaration },
    { ConstDeclaration },
    { FunctionDeclaration }
;

(* ------------------------------------------------- *)
(* Partition Functions *)
(* ------------------------------------------------- *)
PartitionDeclaration =
    "partition",
    Identifier,
    "(",
        [ ParameterList ],
    ")",
    [ "=", Expression ],
    ";"
;

(* ------------------------------------------------- *)
(* Constants *)
(* ------------------------------------------------- *)
ConstDeclaration =
    "const",
    Type,
    Identifier,
    "=",
    Expression,
    ";"
;

(* ------------------------------------------------- *)
(* Tables *)
(* ------------------------------------------------- *)
TableDeclaration =
    "table",
    Identifier,
    "{",
        { FieldDeclaration },
    "}"
;

FieldDeclaration =
    ( [ "primary" ], Type, Identifier, ";" )
  | ( "node", Identifier, "(", [ ExpressionList ], ")", ";" )
;

Type = 
    BasicType
  | ArrayType
;

BasicType = "int" | "float" | "string" | "bool" ;

ArrayType =
    BasicType,
    "[",
    [ IntegerLiteral ],
    "]"
;

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
  | HopsForLoop
;

(* ------------------------------------------------- *)
(* Hops *)
(* ------------------------------------------------- *)
HopBlock =
    "hop",
    "{",
        { Statement },
    "}"
;

HopsForLoop =
    "hops",
    "for",
    "(",
        Type,
        Identifier,
        "=",
        Expression,
        ";",
        Expression,
        ";",
        Expression,
    ")",
    "{",
        { Statement },
    "}"
;

(* ------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------- *)
Statement =
    VarDeclStatement
  | AssignmentStatement
  | IfStatement
  | ForStatement
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

AssignmentStatement =
    LValue,
    AssignmentOperator,
    Expression,
    ";"
;

AssignmentOperator =
    "="
  | "+="
  | "-="
  | "*="
  | "/="
;

LValue =
    Identifier
  | TableFieldAccess
  | TableAccess
  | ArrayAccess
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

RecordLiteral =
    "{",
        [ FieldAssignmentList ],
    "}"
;

FieldAssignmentList =
    FieldAssignment,
    { ",", FieldAssignment }
;

FieldAssignment =
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

ForStatement =
    "for",
    "(",
        Type,
        Identifier,
        "=",
        Expression,
        ";",
        Expression,
        ";",
        Expression,
    ")",
    Block
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

ExpressionList =
    Expression,
    { ",", Expression }
;

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
  | TableAccess
  | ArrayAccess
  | RecordLiteral
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

TableAccess =
    Identifier,
    "[",
        PrimaryKeyList,
    "]"
;

ArrayAccess =
    Identifier,
    "[",
        Expression,
    "]"
;
```