letrec xor = \a. \b. (\FALSE. \y. y) a b
                     | (\TRUE. \FALSE. TRUE) a b
                       | (\TRUE. \TRUE. FALSE) a b
                         | ERROR
in xor TRUE TRUE