letrec funnyLastElt = \a. case a of
                            CONS x xs => IF (< x 0) x FAIL

letrec funnyLastElt = \\a. (\\CONS x xs. IF (< x 0) x FAIL) a | (\\CONS x NIL. x) a | (\\CONS x xs. funnyLastElt xs) a | ERROR
in funnyLastElt (CONS 1 (CONS 2 (CONS 3 NIL)))