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
    "}",
    ";"
;

FieldDeclaration =
    Type,
    Identifier,
    ";"
;

Type = "int" | "float" | "string" ;

(* ------------------------------------------------- *)
(* Functions *)
(* ------------------------------------------------- *)
FunctionDeclaration =
    "void",
    Identifier,
    "(",
        [ ParameterList ],
    ")",
    "{",
        { FunctionBodyItem },
    "}"
;

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
  | IfStatement
  | ";"
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
    AssignmentStatement
  | IfStatement
  | ";"
;

AssignmentStatement =
    Identifier,
    "[",
        Identifier, 
        ":",
        Expression,
    "]",
    ".",
    Identifier,
    "=",
    Expression,
    ";"
;

IfStatement =
    "if",
    "(",
        BooleanExpression,
    ")",
    "{",
        { Statement },
    "}",
    [ "else", "{", { Statement }, "}" ]
;

(* ------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------- *)
Expression =
    Identifier
  | IntegerLiteral
  | FloatLiteral
  | StringLiteral
  | "(", Expression, ")"
  | Expression, BinaryOp, Expression
;

BooleanExpression = Expression ;

BinaryOp =
      "+"
    | "-"
    | "*"
    | "/"
    | "=="
    | "!="
    | "<"
    | "<="
    | ">"
    | ">="
    | "&&"
    | "||"
;
```