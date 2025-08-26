// Test file to verify correct nested map syntax in Boogie

var BankAccount_balance : [int][int]int;
var BankAccount_status : [int][int]String;

type String;
const L_active : String;

procedure test_nested_map_operations()
modifies BankAccount_balance, BankAccount_status;
{
  var param_acc_id : int;
  var param_br_id : int;
  var param_amount : int;
  var temp_val : int;
  
  param_acc_id := 1;
  param_br_id := 2;
  param_amount := 1000;
  
  // Test 1: Correct nested map store
  BankAccount_balance := BankAccount_balance[param_acc_id := BankAccount_balance[param_acc_id][param_br_id := param_amount]];
  
  // Test 2: Correct nested map select  
  temp_val := BankAccount_balance[param_acc_id][param_br_id];
  
  // Test 3: Correct nested map update (read-modify-write)
  BankAccount_balance := BankAccount_balance[param_acc_id := BankAccount_balance[param_acc_id][param_br_id := BankAccount_balance[param_acc_id][param_br_id] + 100]];
  
  // Verify the operations worked
  assert BankAccount_balance[param_acc_id][param_br_id] == param_amount + 100;
}
