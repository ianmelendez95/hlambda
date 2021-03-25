letrec reflect = \_u1. case _u1 of
                         BRANCH _u2 _u3 => BRANCH (reflect _u3) (reflect _u2)
                         LEAF _u2 => LEAF _u2
in reflect (LEAF 1) (LEAF 2)