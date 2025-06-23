// -------------------------------------------------------------------
// Single procedure "interleaving123" representing:
//    * One chosen prefix interleaving: (T1-NodeA -> T1-NodeB -> T2-NodeB)
//    * Final pair in two orders: [T1-NodeC, T2-NodeC] vs. [T2-NodeC, T1-NodeC]
//    * Ensures EXACT same initial state is used for both final-order runs.
// -------------------------------------------------------------------
procedure interleaving123(
    idA: int,   // T1's parameter
    idB: int,   // T2's parameter
    valueB: int // T2's "value" parameter
)
{

    // -------------------------------------------------------------------
    // 1) Table declarations: each table is a map: [primary_key] -> column
    // -------------------------------------------------------------------
    var TableA_value: [int]int;
    var TableB_value: [int]int;
    var TableC_value: [int]int;

    // 1.1) Declare all variables used in the procedure at the top
    var initA: [int]int;
    var initB: [int]int;
    var initC: [int]int;
    var temp: int;
    var x: int;
    var y: int;
    var finalC_AB: [int]int;
    var finalC_BA: [int]int;

    // 2) Havoc all tables to represent unknown initial DB state
    havoc TableA_value;
    havoc TableB_value;
    havoc TableC_value;

    // 3) Save "init" copies of each table so we can restore them
    

    initA := TableA_value;
    initB := TableB_value;
    initC := TableC_value;

    // --------------------------------------------------------------
    // First run: prefix => T1-NodeA -> T1-NodeB -> T2-NodeB
    // and then final [T1-NodeC, T2-NodeC]
    // --------------------------------------------------------------
    
    // T1-NodeA:  int temp = TableA[idA].value
    
    temp := TableA_value[idA];

    // T1-NodeB:  TableB[6].value = temp;  int x = TableB[5].value
    TableB_value[6] := temp;
    
    x := TableB_value[5];

    // T2-NodeB:  TableB[6].value = valueB;  int y = TableB[5].value
    TableB_value[6] := valueB;
    
    y := TableB_value[5];

    // Now the final pair [T1-NodeC, T2-NodeC]
    TableC_value[idA] := x;   // T1-NodeC
    TableC_value[idB] := y;   // T2-NodeC

    
    finalC_AB := TableC_value;

    // --------------------------------------------------------------
    // Restore the EXACT initial state & re-run the prefix
    // --------------------------------------------------------------
    TableA_value := initA;
    TableB_value := initB;
    TableC_value := initC;

    temp := TableA_value[idA];
    TableB_value[6] := temp;
    x := TableB_value[5];

    TableB_value[6] := valueB;
    y := TableB_value[5];

    // --------------------------------------------------------------
    // Final pair in reverse order: [T2-NodeC, T1-NodeC]
    // --------------------------------------------------------------
    TableC_value[idB] := y;   // T2-NodeC
    TableC_value[idA] := x;   // T1-NodeC

    
    finalC_BA := TableC_value;

    // --------------------------------------------------------------
    // Compare final states to ensure commutativity for TableC
    // --------------------------------------------------------------
    assert (forall k: int :: finalC_AB[k] == finalC_BA[k]);
}
