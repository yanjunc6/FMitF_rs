/**************************************************************************
 *  Learn Boogie IVL for Verification & CFG in Y Minutes
 *
 *  This file is intentionally small but feature-rich.  Every item marked
 *  “[★]” demonstrates one of the requested topics:
 *
 *    [★1]  Basic variable & constant declarations
 *    [★2]  Built-in primitive types: int, real, bool
 *    [★3]  How to MODEL strings (Boogie has no native string literal type)
 *    [★4]  Arrays & maps  —  read and write
 *    [★5]  Nested maps    —  read and write
 *    [★6]  Labels + goto  (explicit CFG)
 *    [★7]  Operators for different types (+, -, *, /, ==, != …)
 *            – including “addition” for our modeled String type
 *    [★8]  assert / assume syntax
 *
 *  Save-and-run tips:
 *     boogie /noVerify learn-boogie.bpl   # parse only
 *     boogie learn-boogie.bpl             # run the verifier
 **************************************************************************/

/* ----------------------------------------------------------
 * [★1]  Declaring basic (global) variables & constants
 * -------------------------------------------------------- */
var gInt   : int;     // mutable global integer
var gReal  : real;    // mutable global real
const UNIQUE_ZERO : int;  // immutable global constant

/* ----------------------------------------------------------
 * [★3]  Modeling Strings
 * ----------------------------------------------------------
 * Many verification targets (e.g. Dafny, Java) compile to Boogie,
 * but Boogie itself currently has **no built-in concrete string
 * literals**.  The most common, lightweight approach is:
 *   • declare an **uninterpreted type**  `type String;`
 *   • declare an **uninterpreted function**  `Concat`
 *   • optionally add axioms you care about (associativity, length …)
 */
type String;                                 // uninterpreted
function Concat(s1:String, s2:String) : String;   // String “+”
function StrLen(s:String)              : int;      // (example)

/* Two symbolic string constants so we can write concrete-looking code */
const Hello : String;
const World : String;
axiom Hello != World;          // easy example axiom

/* ----------------------------------------------------------
 * [★4]  Arrays / Maps
 * ----------------------------------------------------------
 * A Boogie “map type” is a pure functional array:
 *          [DomainType]RangeType
 *
 * Read    :  A[i]          (“select”)
 * Write   :  A[i := v]     (“store” returns a NEW map value)
 *            → you normally put that on the left of ":=" to mutate a var
 */
var IntArray       : [int]int;          // int → int   (1-D array)
var RealMatrixRow  : [int]real;         // int → real  (seen below)
var StrTable       : [int]String;       // int → String

/* ----------------------------------------------------------
 * [★5]  Nested maps
 * ----------------------------------------------------------
 * A map’s **range** may itself be another map.  Example:
 *          [int][int]real      // a 2-D real matrix
 *
 * For nested writes you combine stores:
 *     M        :=   M[i := (M[i])[j := 3.14]];
 */
var Matrix : [int][int]real;

/* ----------------------------------------------------------
 * Procedure with CFG, operators, assertions
 * -------------------------------------------------------- */
procedure Main()
modifies IntArray, StrTable, Matrix;  // indicate modified globals
{
  /* [★1] Local variable declarations */
  var i   : int;
  var r   : real;
  var s   : String;        // “string” variable
  var tmp : [int]real;     // helper for nested map write

  /***************** Control-flow graph begins here *****************/
  entry:                              // [★6] label
    // Initialize locals
    i := 0;                           // int assignment
    r := 1.5;                         // real assignment
    s := Concat(Hello, World);        // [★7] “string +” via Concat
    goto arrayOps;

  arrayOps:
    //-------------------------------------------------------
    // [★4] Reading and writing 1-D maps
    //-------------------------------------------------------
    IntArray      := IntArray[2  := i + 3];   // store
    StrTable      := StrTable[5  := s];       // store
    assert IntArray[2] == 3;                  // [★8] assertion

    //-------------------------------------------------------
    // [★5] A single element write in a nested map `Matrix`
    //       Matrix[i][j] := 3.14
    //-------------------------------------------------------
    tmp     := Matrix[0];            // read outer slice (row 0)
    tmp     := tmp[1 := 3.14];       // update column 1
    Matrix  := Matrix[0 := tmp];     // write row back
    assert Matrix[0][1] == 3.14;

    Matrix := Matrix[3 := (Matrix[3])[4 := 3.14]];
    assert Matrix[3][4] == 3.14;

    Matrix[7] := Matrix[7][8 := 3.14];
    assert Matrix[7][8] == 3.14;


    goto loop;

  loop:
    //------------------------ simple loop ---------------------------
    i := i + 1;                      // [★7] int +
    r := r * 2.0;                    // [★7] real *
    // Loop guard
    if (i < 5) {                     // uses builtin “<”
      goto loop;                     // back-edge
    }
    goto done;

  done:
    // Some final assertions
    assert {:msg "This i cannot be proved"} i == 5;
    assert r == 1.5 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0;  // just for fun
    // Demonstrate boolean expressions & string properties
    assert StrLen(s) >= 0;           // cannot be proved (axiom needed)
    return;                          // procedure exit
}

/* ----------------------------------------------------------
 *  Additional mini-examples (not executed)
 * -------------------------------------------------------- */
procedure OperatorsDemo(x:int, y:int, p:real, q:real, a:String, b:String)
{
  var z:int;
  var mut_a:String;
  var mut_q:real;
  // Integer arithmetic
  z := x + y * 2 - 3; // division will create real;
  // Real arithmetic
  // WRONG: q is immutable. q := p / 2.5 + 1.0;
    mut_q := p / 2.5 + 1.0;
  // Equality / disequality
  assert x != y ==> !(x == y);
  // “String concatenation”
  mut_a := Concat(a, b);
  assume StrLen(a) == StrLen(b);   // [★8] assume
}

// Boogie procedure can return any number of values by declaring a returns clause with one or more output parameters
procedure MaxMin(x:int, y:int) returns (mx:int, mn:int)
{
  if (x >= y) {
    mx := x; mn := y;
  } else {
    mx := y; mn := x;
  }
}


// How to execute this
// Use \quiet flag, no output is everything is correct
// Use {:msg " "} for assertions and the error output will be such string, so that program can parse this string
// Use /errorTrace:0, then only message lines with no counter example trace will be outputed