-- | Module encapsulating alpha conversions for syntaxes
module Lambda.AlphaConv (AlphaConv (..), unsafeAlphaConvert) where 
  
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

class AlphaConv a where 
  unsafeAlphaConv :: String -> String -> a -> a

instance AlphaConv S.Exp where 
  unsafeAlphaConv = unsafeAlphaConvert

instance AlphaConv E.Exp where 
  unsafeAlphaConv = enrConv

enrConv :: String -> String -> E.Exp -> E.Exp 
enrConv old new = go
  where 
    go :: E.Exp -> E.Exp
    go (E.Pure e) = E.Pure (unsafeAlphaConv old new e)
    go (E.Let b e) = E.Let (goBinding b) (go e)
    go (E.Letrec bs e) = E.Letrec (map goBinding bs) (go e)
    go (E.Apply e1 e2) = E.Apply (go e1) (go e2)
    go (E.Lambda p e) = E.Lambda (goPatt p) (go e)
    go (E.FatBar e1 e2) = E.FatBar (go e1) (go e2)
    go (E.Case v cs) = E.Case v (map (E.mapCaseExpr go) cs)

    goVar :: S.Variable -> S.Variable
    goVar v = if v == old then new else v

    goBinding :: E.LetBinding -> E.LetBinding
    goBinding (patt, expr) = (goPatt patt, go expr)

    goPatt :: E.Pattern -> E.Pattern
    goPatt c@(E.PConstant _) = c
    goPatt (E.PVariable v) = E.PVariable $ goVar v
    goPatt (E.PConstructor c patts) = E.PConstructor c $ map goPatt patts

-- | rather unsafely converts instances of the old name with the new name
-- | only scrutinizing on formal parameters (that is, if encounter a lambda with the formal parameter 
-- | matching the old name, it doesn't continue)
-- | 
-- | unsafe in that it makes no discernment for whether it is replacing the name with 
-- | a variable that is also free in the expression
unsafeAlphaConvert :: String -> String -> S.Exp -> S.Exp
unsafeAlphaConvert _ _ l@(S.Let _ _) = error $ "Can't alpha convert let: " ++ show l
unsafeAlphaConvert _ _ l@(S.Letrec _ _) = error $ "Can't alpha convert letrec: " ++ show l
unsafeAlphaConvert old_name new_name (S.Term (S.Variable var)) = 
  S.mkVariable $ S.mapVarName (\n -> if n == old_name then new_name else n) var
unsafeAlphaConvert _ _ t@(S.Term _) = t
unsafeAlphaConvert old_name new_name (S.Apply e1 e2) = 
  S.Apply (unsafeAlphaConvert old_name new_name e1)
          (unsafeAlphaConvert old_name new_name e2)
unsafeAlphaConvert old_name new_name l@(S.Lambda v e)
  | old_name == v = l -- name is already bound, doesn't matter
  | otherwise = S.Lambda v (unsafeAlphaConvert old_name new_name e)

