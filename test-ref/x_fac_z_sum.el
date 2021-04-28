letrec x = fac z
       fac = \_u1. if (= _u1 0) 1 (* _u1 (fac (- _u1 1)))
       z = 4
       sum = \_u1. \_u2. if (= _u1 0) _u2 (sum (- _u1 1) (+ _u2 1))
in sum x z