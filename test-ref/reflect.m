reflect (LEAF n) = LEAF n 
reflect (BRANCH t1 t2) = BRANCH (reflect t2) (reflect t1)
reflect (LEAF 1) (LEAF 2)