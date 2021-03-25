letrec xor = \_u1. \_u2. IF (= _u1 FALSE) _u2 (IF (= _u1 TRUE) (IF (= _u2 FALSE) TRUE (IF (= _u2 TRUE) FALSE FAIL)) FAIL)
in xor TRUE TRUE