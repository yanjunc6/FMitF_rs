; --- General Z3 Configuration ---
(set-option :print-success false)
(set-info :smt-lib-version 2.6)
(set-option :smt.mbqi false)
(set-option :model.compact false)
(set-option :model.v2 true)
(set-option :pp.bv_literals false)

; --- Boogie-specific Axioms ---
(declare-fun tickleBool (Bool) Bool)
(assert (and (tickleBool true) (tickleBool false)))

; --- Start of the main verification task ---
(push 1)

; --- Declarations of Functions and Variables ---
(declare-fun ControlFlow (Int Int) Int)
(declare-fun m@0 () (Array Int Int))
(declare-fun m () (Array Int Int))
(declare-fun x () Int)
(declare-fun y () Int)
(declare-fun m@1 () (Array Int Int))
(declare-fun z () Int)

; --- Metadata from Boogie ---
(set-info :boogie-vc-id MapUpdateExample)
(set-option :timeout 0)
(set-option :rlimit 0)

; --- The Main Verification Condition (VC) ---
(assert (not
    (=> (= (ControlFlow 0 0) 3)
        (let ((anon0_correct 
                (=> (= m@0 (store m x y))
                    (=> (and (= m@1 (store m@0 z 100)) (= (ControlFlow 0 2) (- 0 1)))
                        (= (select m@1 x) y)
                    )
                )
             ))
            (let ((PreconditionGeneratedEntry_correct
                    (=> (and (not (= x z)) ; <-- Precondition 'requires x != z;' is here
                             (= (ControlFlow 0 3) 2)
                        )
                        anon0_correct
                    )
                 ))
                 PreconditionGeneratedEntry_correct
            )
        )
    )
))

; --- Solver Commands ---
(check-sat)
(get-info :rlimit)
(pop 1)