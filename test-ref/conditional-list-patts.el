letrec funnyLastElt = \_u1. case _u1 of
                              CONS _u2 _u3 => IF (< _u2 0) _u2 FAIL
                                              | case _u3 of
                                                  CONS _u4 _u5 => FAIL
                                                  NIL => _u2
                                              | funnyLastElt _u3
                              NIL => FAIL
                                     | FAIL
                                     | FAIL
in funnyLastElt (CONS 1 (CONS 2 (CONS 3 NIL)))