letrec addPair = \_u1. letrec (PAIR x y) = _u1
                       in + x y
in addPair (PAIR 3 7)