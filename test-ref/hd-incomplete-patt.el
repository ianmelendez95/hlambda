letrec hd = \_u1. case _u1 of
                    CONS _u2 _u3 => _u2
                    NIL => FAIL
in hd (CONS 1 (CONS 2 (CONS 3 NIL)))