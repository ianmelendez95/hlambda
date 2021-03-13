letrec addPair = \w. letrec (PAIR x y) = w
                     in + x y
in addPair (PAIR 3 7)