letrec fst = \_u1. case _u1 of
                     PAIR _u2 _u3 => _u2
in fst (PAIR 1 2)