letrec fact = \_u1. IF (= _u1 0) 1 (* _u1 (fact (- _u1 1)))
in fact 4