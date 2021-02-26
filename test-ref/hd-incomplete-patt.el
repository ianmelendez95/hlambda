letrec hd = \a. case a of
                  CONS x xs => x
in hd (CONS 1 (CONS 2 (CONS 3 NIL)))