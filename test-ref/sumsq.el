letrec sumsq = \x. \y. letrec xsq = * x x
                              ysq = * y y
                       in + xsq ysq
in sumsq 2 3