// RowConstructorAxioms.bpl
//
// This file models a Row as an opaque object whose identity is determined
// solely by its primary key.
//
// The construction workflow is as follows:
// 1. Data fields are read directly from global maps.
// 2. A constructor function is called with ALL fields (primary and data).
// 3. The equality axiom, however, ONLY compares the primary key fields
//    to determine if two constructed rows are identical.
//
// This model has no accessor functions (`get_*`) for the Row type.

// #############################################################################
// #  CORE TYPE DEFINITIONS
// #############################################################################

type Row a;
type Table a;
type UUID;
type String;


// #############################################################################
// #  TABLE SCHEMA: Activity
// #############################################################################

type Activity;
const TBL_Activity: Table Activity;

// --- Global Maps for Activity Table ---
// Primary Key: (AID: UUID, UID: int)
var Activity_Ttype: [UUID][int]int;
var Activity_Log:  [UUID][int]String;
var Activity_Time: [UUID][int]int;


// #############################################################################
// #  AXIOMS FOR Row (Table Activity)
// #############################################################################

// --- 1. Constructor Function ---\n// This constructor takes all fields as arguments.
function construct_Row_Activity(aid: UUID, uid: int, ttype: int, log: String, time: int): Row (Table Activity);

// --- 2. Equality Axiom ---\n// This is the crucial axiom. It states that two Activity rows are equal
// if and only if their all fields are identical.
axiom (forall
    aid1: UUID, uid1: int, ttype1: int, log1: String, time1: int,
    aid2: UUID, uid2: int, ttype2: int, log2: String, time2: int
    ::
    construct_Row_Activity(aid1, uid1, ttype1, log1, time1) == construct_Row_Activity(aid2, uid2, ttype2, log2, time2)
    <==>
    (aid1 == aid2 && uid1 == uid2 && ttype1 == ttype2 && log1 == log2 && time1 == time2)
);


// #############################################################################
// #  VERIFICATION AND TEST PROCEDURE
// #############################################################################
// This procedure demonstrates and verifies the specified workflow.

procedure TestRowConstructionAndComparison()
{
    // --- Local Variables ---\n    var row1, row2, row3, row4: Row (Table Activity);
    var aid1, aid2: UUID;
    var uid1, uid2: int;
    var ttype_val: int;
    var log_val, other_log_val: String;
    var time_val: int;
    var looked_up_ttype: int;
    var looked_up_log: String;
    var looked_up_time: int;
    var looked_up_ttype_2: int;
    var looked_up_log_2: String;
    var looked_up_time_2: int;
    var row1, row2, row3, row4: Row (Table Activity);

    // --- Setup: Assume some data exists in the global maps ---
    havoc aid1, aid2, uid1, uid2, ttype_val, log_val, other_log_val, time_val;

    // --- Test Case 1: Equality ---
    // Two rows constructed with the same primary key are equal.

    // Step 1: Access fields from table maps.
    looked_up_ttype := Activity_Ttype[aid1][uid1];
    looked_up_log := Activity_Log[aid1][uid1];
    looked_up_time := Activity_Time[aid1][uid1];

    // Step 2: Construct the rows using all fields.
    row1 := construct_Row_Activity(aid1, uid1, looked_up_ttype, looked_up_log, looked_up_time);
    row2 := construct_Row_Activity(aid1, uid1, looked_up_ttype, looked_up_log, looked_up_time);

    // Step 3: Compare. This assertion must hold.
    assert row1 == row2;


    // --- Test Case 2: Equality Despite Different Data Fields ---
    // This test proves that only the primary key matters for identity.
    // We construct two rows with the SAME primary key but DIFFERENT data fields.
    // According to the axiom, they must still be considered equal.
    assume log_val != other_log_val;

    row1 := construct_Row_Activity(aid1, uid1, ttype_val, log_val, time_val);
    row2 := construct_Row_Activity(aid1, uid1, ttype_val, other_log_val, time_val); // Using different log

    assert row1 != row2;


    // --- Test Case 3: Inequality (Different Primary Key) ---
    // Two rows constructed with different primary keys are not equal.
    assume aid1 != aid2; // Ensure primary keys are different

    // Look up data for the second row (can be anything).
    looked_up_ttype_2 := Activity_Ttype[aid2][uid1];
    looked_up_log_2 := Activity_Log[aid2][uid1];
    looked_up_time_2 := Activity_Time[aid2][uid1];

    // Construct the rows.
    row3 := construct_Row_Activity(aid1, uid1, looked_up_ttype, looked_up_log, looked_up_time);
    row4 := construct_Row_Activity(aid2, uid1, looked_up_ttype_2, looked_up_log_2, looked_up_time_2);

    // Compare. This assertion must hold.
    assert row3 != row4;
}