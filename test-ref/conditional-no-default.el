letrec gcd = \_u1. \_u2. IF (> _u1 _u2) (gcd (- _u1 _u2) _u2) (IF (< _u1 _u2) (gcd _u1 (- _u2 _u1)) (IF (= _u1 _u2) _u1 FAIL))
in gcd 6 9