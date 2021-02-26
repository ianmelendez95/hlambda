letrec gcd = \a. \b. letrec diff = - a b
                     in IF (> a b) (gcd diff b) (IF (< a b) (gcd a (- diff)) (IF (= a b) a FAIL))
in gcd 6 9