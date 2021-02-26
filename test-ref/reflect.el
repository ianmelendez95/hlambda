letrec reflect = \a. case a of
                       LEAF n => LEAF n
                       BRANCH t1 t2 => BRANCH (reflect t2) (reflect t1)
in reflect (LEAF 1) (LEAF 2)