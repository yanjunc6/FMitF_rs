// --------------------------
// Type and Operators
// --------------------------

type String;

const empty: String;

function Concat(x: String, y: String): String;
function IntToString(i: int): String;
function RealToString(r: real): String;
function BoolToString(b: bool): String;
function int2real(i: int): real;
function real2int(r: real): int;


// --------------------------
// Axioms (with correct triggers)
// --------------------------

// Identity of empty for concat
axiom (forall s: String :: {Concat(empty, s)} Concat(empty, s) == s);
axiom (forall s: String :: {Concat(s, empty)} Concat(s, empty) == s);

// Injectivity of IntToString
axiom (forall i: int, j: int ::
{ IntToString(i), IntToString(j) }
IntToString(i) == IntToString(j) ==> i == j);

// Injectivity of RealToString
axiom (forall x: real, y: real ::
{ RealToString(x), RealToString(y) }
RealToString(x) == RealToString(y) ==> x == y);

// Injectivity of BoolToString
axiom (forall x: bool, y: bool ::
{ BoolToString(x), BoolToString(y) }
BoolToString(x) == BoolToString(y) ==> x == y);

// Conversions never yield empty
axiom (forall i: int :: {IntToString(i)} IntToString(i) != empty);
axiom (forall r: real :: {RealToString(r)} RealToString(r) != empty);
axiom (forall b: bool :: {BoolToString(b)} BoolToString(b) != empty);
var Customer_C_CREDIT_LIMIT : [int][int][int]real;
var Warehouse_W_CITY : [int]String;
var Stock_S_QUANTITY : [int][int]int;
var District_D_YTD : [int][int]real;
var District_D_STATE : [int][int]String;
var District_D_STREET_2 : [int][int]String;
var Order_O_ALL_LOCAL : [int][int][int]int;
var Stock_S_DATA : [int][int]String;
var Order_Line_OL_AMOUNT : [int][int][int][int]int;
var District_D_CITY : [int][int]String;
var Customer_C_DISCOUNT : [int][int][int]real;
var Customer_C_PHONE : [int][int][int]String;
var Warehouse_W_TAX : [int]real;
const O_OL_CNT : int;
var District_D_STREET_1 : [int][int]String;
var Customer_C_STREET_1 : [int][int][int]String;
var Customer_C_CITY : [int][int][int]String;
var Order_O_C_ID : [int][int][int]int;
var Item_I_DATA : [int][int]String;
var Stock_S_ORDER_CNT : [int][int]int;
var Customer_C_MIDDLE : [int][int][int]String;
var Order_Line_OL_I_ID : [int][int][int][int]int;
var Order_O_CARRIER_ID : [int][int][int]int;
var Customer_C_PAYMENT_CNT : [int][int][int]real;
var Warehouse_W_STATE : [int]String;
var District_D_ZIP : [int][int]String;
var Order_Line_OL_DELIVERY_DATE : [int][int][int][int]int;
var Order_O_OL_CNT : [int][int][int]int;
var District_D_TAX : [int][int]real;
var Customer_C_FIRST : [int][int][int]String;
var Item_I_PRICE : [int][int]real;
var Customer_C_SINCE : [int][int][int]int;
var Order_O_ENTRY_DATE : [int][int][int]int;
var District_D_NEXT_NO_ID : [int][int]int;
var Item_I_IM_ID : [int][int]int;
var Customer_C_LAST : [int][int][int]String;
var Customer_C_YTD_PAYMENT : [int][int][int]real;
var Warehouse_W_NAME : [int]String;
var Customer_C_CREDIT : [int][int][int]String;
var Order_Line_OL_DIST_INTO : [int][int][int][int]String;
var Stock_S_YTD : [int][int]real;
var Order_Line_OL_SUPPLY_W_ID : [int][int][int][int]int;
var Customer_C_DATA : [int][int][int]String;
var Customer_C_STREET_2 : [int][int][int]String;
var Item_I_NAME : [int][int]String;
var Order_Line_OL_QUANTITY : [int][int][int][int]int;
var District_D_NEXT_O_ID : [int][int]int;
var District_D_NAME : [int][int]String;
var Customer_C_ZIP : [int][int][int]String;
var Warehouse_W_YTD : [int]real;
var Customer_C_STATE : [int][int][int]String;
var Warehouse_W_STREET_2 : [int]String;
var Stock_S_REMOTE_CNT : [int][int]int;
var Customer_C_BALANCE : [int][int][int]real;
var Warehouse_W_ZIP : [int]String;
var Warehouse_W_STREET_1 : [int]String;
var Customer_C_DELIVERY_CNT : [int][int][int]real;
procedure Check_SliceCommut_Hop11_vs_Hop13()
modifies District_D_NEXT_NO_ID, Order_O_CARRIER_ID, District_D_YTD, Order_O_OL_CNT, Customer_C_DELIVERY_CNT, Order_Line_OL_AMOUNT, Order_Line_OL_DELIVERY_DATE, Customer_C_BALANCE, Order_O_C_ID;
{
  var A__param_w_id : int;
  var A__param_o_carrier_id : int;
  var A__param_date : int;
  var B__param_c_w_id : int;
  var B__param_w_id : int;
  var B__param_amount : real;
  var B__param_c_d_id : int;
  var B__param_d_id : int;
  var B__param_c_id : int;
  var District_D_NEXT_NO_ID_init : [int][int]int;
  var Order_O_OL_CNT_init : [int][int][int]int;
  var Order_Line_OL_DELIVERY_DATE_init : [int][int][int][int]int;
  var Customer_C_DELIVERY_CNT_init : [int][int][int]real;
  var Customer_C_BALANCE_init : [int][int][int]real;
  var Order_O_CARRIER_ID_init : [int][int][int]int;
  var Order_Line_OL_AMOUNT_init : [int][int][int][int]int;
  var Order_O_C_ID_init : [int][int][int]int;
  var District_D_YTD_init : [int][int]real;
  var A__param_w_id_init : int;
  var A__param_o_carrier_id_init : int;
  var A__param_date_init : int;
  var B__param_c_w_id_init : int;
  var B__param_w_id_init : int;
  var B__param_amount_init : real;
  var B__param_c_d_id_init : int;
  var B__param_d_id_init : int;
  var B__param_c_id_init : int;
  var A_param_w_id : int;
  var A__t258 : int;
  var A_local_no_o_id : int;
  var A__t263 : int;
  var A__t267 : int;
  var A_local_c_id : int;
  var A__t271 : int;
  var A_local_ol_cnt : int;
  var A_param_o_carrier_id : int;
  var A__t280 : bool;
  var _t280 : bool;
  var A_param_date : int;
  var A__t290 : int;
  var A__t299 : real;
  var A__t300 : real;
  var A__t305 : real;
  var A__t306 : real;
  var B_param_w_id : int;
  var B_param_d_id : int;
  var B__t314 : real;
  var B_param_amount : real;
  var B__t315 : real;
  var District_D_YTD_a_then_b : [int][int]real;
  var Customer_C_DELIVERY_CNT_a_then_b : [int][int][int]real;
  var District_D_NEXT_NO_ID_a_then_b : [int][int]int;
  var Order_Line_OL_DELIVERY_DATE_a_then_b : [int][int][int][int]int;
  var Order_O_CARRIER_ID_a_then_b : [int][int][int]int;
  var Customer_C_BALANCE_a_then_b : [int][int][int]real;
  var B__param_w_id_a_then_b : int;
  var B__param_d_id_a_then_b : int;
  var B__param_c_id_a_then_b : int;
  var B__param_c_w_id_a_then_b : int;
  var B__param_c_d_id_a_then_b : int;
  var B__param_amount_a_then_b : real;
  var District_D_NEXT_NO_ID_b_then_a : [int][int]int;
  var District_D_YTD_b_then_a : [int][int]real;
  var Customer_C_DELIVERY_CNT_b_then_a : [int][int][int]real;
  var Customer_C_BALANCE_b_then_a : [int][int][int]real;
  var Order_Line_OL_DELIVERY_DATE_b_then_a : [int][int][int][int]int;
  var Order_O_CARRIER_ID_b_then_a : [int][int][int]int;
  var B__param_w_id_b_then_a : int;
  var B__param_d_id_b_then_a : int;
  var B__param_c_id_b_then_a : int;
  var B__param_c_w_id_b_then_a : int;
  var B__param_c_d_id_b_then_a : int;
  var B__param_amount_b_then_a : real;

  // Slice commutativity verification: hop 11 vs hop 13
  // --- Step 1: Havoc initial state ---
    havoc Order_O_C_ID;
    havoc Customer_C_BALANCE;
    havoc District_D_NEXT_NO_ID;
    havoc Order_O_OL_CNT;
    havoc District_D_YTD;
    havoc Order_Line_OL_AMOUNT;
    havoc Customer_C_DELIVERY_CNT;
    havoc Order_Line_OL_DELIVERY_DATE;
    havoc Order_O_CARRIER_ID;
    havoc A__param_w_id;
    havoc A__param_o_carrier_id;
    havoc A__param_date;
    havoc B__param_c_w_id;
    havoc B__param_w_id;
    havoc B__param_amount;
    havoc B__param_c_d_id;
    havoc B__param_d_id;
    havoc B__param_c_id;
  // --- Step 2: Save initial state ---
    District_D_NEXT_NO_ID_init := District_D_NEXT_NO_ID;
    Order_O_OL_CNT_init := Order_O_OL_CNT;
    Order_Line_OL_DELIVERY_DATE_init := Order_Line_OL_DELIVERY_DATE;
    Customer_C_DELIVERY_CNT_init := Customer_C_DELIVERY_CNT;
    Customer_C_BALANCE_init := Customer_C_BALANCE;
    Order_O_CARRIER_ID_init := Order_O_CARRIER_ID;
    Order_Line_OL_AMOUNT_init := Order_Line_OL_AMOUNT;
    Order_O_C_ID_init := Order_O_C_ID;
    District_D_YTD_init := District_D_YTD;
    A__param_w_id_init := A__param_w_id;
    A__param_o_carrier_id_init := A__param_o_carrier_id;
    A__param_date_init := A__param_date;
    B__param_c_w_id_init := B__param_c_w_id;
    B__param_w_id_init := B__param_w_id;
    B__param_amount_init := B__param_amount;
    B__param_c_d_id_init := B__param_c_d_id;
    B__param_d_id_init := B__param_d_id;
    B__param_c_id_init := B__param_c_id;
  // --- Step 3: Execute special interleavings ---
  // Executing A then B:
  // Executing hop 11
  exec_A_a_then_b_block27:
    A__t258 := District_D_NEXT_NO_ID[A_param_w_id][1];
    A_local_no_o_id := A__t258;
    A__t263 := A__t258 + 1;
    District_D_NEXT_NO_ID := District_D_NEXT_NO_ID[A_param_w_id := District_D_NEXT_NO_ID[A_param_w_id][1 := A__t263]];
    A__t267 := Order_O_C_ID[A_param_w_id][1][A__t258];
    A_local_c_id := A__t267;
    A__t271 := Order_O_OL_CNT[A_param_w_id][1][A__t258];
    A_local_ol_cnt := A__t271;
    Order_O_CARRIER_ID := Order_O_CARRIER_ID[A_param_w_id := Order_O_CARRIER_ID[A_param_w_id][1 := Order_O_CARRIER_ID[A_param_w_id][1][A__t258 := A_param_o_carrier_id]]];
    goto exec_A_a_then_b_block28;
  exec_A_a_then_b_block28:
    A__t280 := 0 < A_local_ol_cnt;
    if (_t280) {
      goto exec_A_a_then_b_block29;
    }
    if (!_t280) {
      goto exec_A_a_then_b_block31;
    }
  exec_A_a_then_b_block29:
    Order_Line_OL_DELIVERY_DATE := Order_Line_OL_DELIVERY_DATE[A_param_w_id := Order_Line_OL_DELIVERY_DATE[A_param_w_id][1 := Order_Line_OL_DELIVERY_DATE[A_param_w_id][1][A_local_no_o_id := Order_Line_OL_DELIVERY_DATE[A_param_w_id][1][A_local_no_o_id][0 := A_param_date]]]];
    A__t290 := Order_Line_OL_AMOUNT[A_param_w_id][1][A_local_no_o_id][0];
    goto exec_A_a_then_b_block30;
  exec_A_a_then_b_block30:
    goto exec_A_a_then_b_block28;
  exec_A_a_then_b_block31:
    A__t299 := Customer_C_BALANCE[A_param_w_id][1][A_local_c_id];
    A__t300 := A__t299 + (int2real(0));
    Customer_C_BALANCE := Customer_C_BALANCE[A_param_w_id := Customer_C_BALANCE[A_param_w_id][1 := Customer_C_BALANCE[A_param_w_id][1][A_local_c_id := A__t300]]];
    A__t305 := Customer_C_DELIVERY_CNT[A_param_w_id][1][A_local_c_id];
    A__t306 := A__t305 + (int2real(1));
    Customer_C_DELIVERY_CNT := Customer_C_DELIVERY_CNT[A_param_w_id := Customer_C_DELIVERY_CNT[A_param_w_id][1 := Customer_C_DELIVERY_CNT[A_param_w_id][1][A_local_c_id := A__t306]]];
  exec_A_a_then_b_functionA_end:
  exec_A_a_then_b_functionA_abort:
  exec_A_a_then_b_functionA_return:
  // Executing hop 13
  exec_B_a_then_b_block33:
    B__t314 := District_D_YTD[B_param_w_id][B_param_d_id];
    B__t315 := B__t314 + B_param_amount;
    District_D_YTD := District_D_YTD[B_param_w_id := District_D_YTD[B_param_w_id][B_param_d_id := B__t315]];
    goto exec_B_a_then_b_functionB_end;
  exec_B_a_then_b_functionB_end:
  exec_B_a_then_b_functionB_abort:
  exec_B_a_then_b_functionB_return:
  // Snapshotting final state for a_then_b
    District_D_YTD_a_then_b := District_D_YTD;
    Customer_C_DELIVERY_CNT_a_then_b := Customer_C_DELIVERY_CNT;
    District_D_NEXT_NO_ID_a_then_b := District_D_NEXT_NO_ID;
    Order_Line_OL_DELIVERY_DATE_a_then_b := Order_Line_OL_DELIVERY_DATE;
    Order_O_CARRIER_ID_a_then_b := Order_O_CARRIER_ID;
    Customer_C_BALANCE_a_then_b := Customer_C_BALANCE;
    B__param_w_id_a_then_b := B__param_w_id;
    B__param_d_id_a_then_b := B__param_d_id;
    B__param_c_id_a_then_b := B__param_c_id;
    B__param_c_w_id_a_then_b := B__param_c_w_id;
    B__param_c_d_id_a_then_b := B__param_c_d_id;
    B__param_amount_a_then_b := B__param_amount;
  // Restoring initial state:
    Customer_C_BALANCE := Customer_C_BALANCE_init;
    District_D_YTD := District_D_YTD_init;
    Order_Line_OL_AMOUNT := Order_Line_OL_AMOUNT_init;
    Customer_C_DELIVERY_CNT := Customer_C_DELIVERY_CNT_init;
    District_D_NEXT_NO_ID := District_D_NEXT_NO_ID_init;
    Order_Line_OL_DELIVERY_DATE := Order_Line_OL_DELIVERY_DATE_init;
    Order_O_CARRIER_ID := Order_O_CARRIER_ID_init;
    Order_O_OL_CNT := Order_O_OL_CNT_init;
    Order_O_C_ID := Order_O_C_ID_init;
    A__param_w_id := A__param_w_id_init;
    A__param_o_carrier_id := A__param_o_carrier_id_init;
    A__param_date := A__param_date_init;
    B__param_c_w_id := B__param_c_w_id_init;
    B__param_w_id := B__param_w_id_init;
    B__param_amount := B__param_amount_init;
    B__param_c_d_id := B__param_c_d_id_init;
    B__param_d_id := B__param_d_id_init;
    B__param_c_id := B__param_c_id_init;
  // Executing B then A:
  // Executing hop 13
  exec_B_b_then_a_block33:
    B__t314 := District_D_YTD[B_param_w_id][B_param_d_id];
    B__t315 := B__t314 + B_param_amount;
    District_D_YTD := District_D_YTD[B_param_w_id := District_D_YTD[B_param_w_id][B_param_d_id := B__t315]];
    goto exec_B_b_then_a_functionB_end;
  exec_B_b_then_a_functionB_end:
  exec_B_b_then_a_functionB_abort:
  exec_B_b_then_a_functionB_return:
  // Executing hop 11
  exec_A_b_then_a_block27:
    A__t258 := District_D_NEXT_NO_ID[A_param_w_id][1];
    A_local_no_o_id := A__t258;
    A__t263 := A__t258 + 1;
    District_D_NEXT_NO_ID := District_D_NEXT_NO_ID[A_param_w_id := District_D_NEXT_NO_ID[A_param_w_id][1 := A__t263]];
    A__t267 := Order_O_C_ID[A_param_w_id][1][A__t258];
    A_local_c_id := A__t267;
    A__t271 := Order_O_OL_CNT[A_param_w_id][1][A__t258];
    A_local_ol_cnt := A__t271;
    Order_O_CARRIER_ID := Order_O_CARRIER_ID[A_param_w_id := Order_O_CARRIER_ID[A_param_w_id][1 := Order_O_CARRIER_ID[A_param_w_id][1][A__t258 := A_param_o_carrier_id]]];
    goto exec_A_b_then_a_block28;
  exec_A_b_then_a_block28:
    A__t280 := 0 < A_local_ol_cnt;
    if (_t280) {
      goto exec_A_b_then_a_block29;
    }
    if (!_t280) {
      goto exec_A_b_then_a_block31;
    }
  exec_A_b_then_a_block29:
    Order_Line_OL_DELIVERY_DATE := Order_Line_OL_DELIVERY_DATE[A_param_w_id := Order_Line_OL_DELIVERY_DATE[A_param_w_id][1 := Order_Line_OL_DELIVERY_DATE[A_param_w_id][1][A_local_no_o_id := Order_Line_OL_DELIVERY_DATE[A_param_w_id][1][A_local_no_o_id][0 := A_param_date]]]];
    A__t290 := Order_Line_OL_AMOUNT[A_param_w_id][1][A_local_no_o_id][0];
    goto exec_A_b_then_a_block30;
  exec_A_b_then_a_block30:
    goto exec_A_b_then_a_block28;
  exec_A_b_then_a_block31:
    A__t299 := Customer_C_BALANCE[A_param_w_id][1][A_local_c_id];
    A__t300 := A__t299 + (int2real(0));
    Customer_C_BALANCE := Customer_C_BALANCE[A_param_w_id := Customer_C_BALANCE[A_param_w_id][1 := Customer_C_BALANCE[A_param_w_id][1][A_local_c_id := A__t300]]];
    A__t305 := Customer_C_DELIVERY_CNT[A_param_w_id][1][A_local_c_id];
    A__t306 := A__t305 + (int2real(1));
    Customer_C_DELIVERY_CNT := Customer_C_DELIVERY_CNT[A_param_w_id := Customer_C_DELIVERY_CNT[A_param_w_id][1 := Customer_C_DELIVERY_CNT[A_param_w_id][1][A_local_c_id := A__t306]]];
  exec_A_b_then_a_functionA_end:
  exec_A_b_then_a_functionA_abort:
  exec_A_b_then_a_functionA_return:
  // Snapshotting final state for b_then_a
    District_D_NEXT_NO_ID_b_then_a := District_D_NEXT_NO_ID;
    District_D_YTD_b_then_a := District_D_YTD;
    Customer_C_DELIVERY_CNT_b_then_a := Customer_C_DELIVERY_CNT;
    Customer_C_BALANCE_b_then_a := Customer_C_BALANCE;
    Order_Line_OL_DELIVERY_DATE_b_then_a := Order_Line_OL_DELIVERY_DATE;
    Order_O_CARRIER_ID_b_then_a := Order_O_CARRIER_ID;
    B__param_w_id_b_then_a := B__param_w_id;
    B__param_d_id_b_then_a := B__param_d_id;
    B__param_c_id_b_then_a := B__param_c_id;
    B__param_c_w_id_b_then_a := B__param_c_w_id;
    B__param_c_d_id_b_then_a := B__param_c_d_id;
    B__param_amount_b_then_a := B__param_amount;
  // --- Step 4: Verify A→B ≡ B→A (Special interleavings equivalence) ---
  // Verifying A->B === B->A equivalence:
    assert (District_D_YTD_a_then_b == District_D_YTD_b_then_a);
    assert (Order_Line_OL_DELIVERY_DATE_a_then_b == Order_Line_OL_DELIVERY_DATE_b_then_a);
    assert (Customer_C_BALANCE_a_then_b == Customer_C_BALANCE_b_then_a);
    assert (Customer_C_DELIVERY_CNT_a_then_b == Customer_C_DELIVERY_CNT_b_then_a);
    assert (Order_O_CARRIER_ID_a_then_b == Order_O_CARRIER_ID_b_then_a);
    assert (District_D_NEXT_NO_ID_a_then_b == District_D_NEXT_NO_ID_b_then_a);
    assert (B__param_w_id_a_then_b == B__param_w_id_b_then_a);
    assert (B__param_d_id_a_then_b == B__param_d_id_b_then_a);
    assert (B__param_c_id_a_then_b == B__param_c_id_b_then_a);
    assert (B__param_c_w_id_a_then_b == B__param_c_w_id_b_then_a);
    assert (B__param_c_d_id_a_then_b == B__param_c_d_id_b_then_a);
    assert (B__param_amount_a_then_b == B__param_amount_b_then_a);
}

