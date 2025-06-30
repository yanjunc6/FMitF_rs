procedure GotoExample()
{
    // Variable declarations should be before the first label.
    var a: int; // Declare variable a
    var b: int; // Declare variable b
    var res: int; // Declare result variable res
    
  // Start at label "Entry"
  Entry:

    a := 5; // Initialize variable a
    b := 10; // Initialize variable b
    // Unconditional jump to Check
    goto Check;

  Check:
    // Conditional branch:
    // if a == b, go to Equal; else go to NotEqual.
    if (a == b) {
      goto Equal;
    } else {
      goto NotEqual;
    }

  Equal:
    res := 0;
    // Jump to End
    goto End;

  NotEqual:
    res := 1;
    // Another jump to End
    goto End;

  End:
    // Return from this procedure
    return;
}