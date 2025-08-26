// Test file to demonstrate WRONG nested map syntax in Boogie

var BankAccount_balance : [int][int]int;

procedure test_wrong_nested_map_operations()
modifies BankAccount_balance;
{
  var param_acc_id : int;
  var param_br_id : int;
  var param_amount : int;
  var temp_val : int;
  
  param_acc_id := 1;
  param_br_id := 2;
  param_amount := 1000;
  
  // WRONG: This should cause "wrong number of arguments" errors
  BankAccount_balance := BankAccount_balance[param_acc_id, param_br_id := param_amount];
  temp_val := BankAccount_balance[param_acc_id, param_br_id];
}
