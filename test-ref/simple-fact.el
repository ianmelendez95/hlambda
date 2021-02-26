letrec fact = \n. IF (= n 0) 1 (* n (fact (- n 1)))
in fact 4