-- | Module encapsulating alpha conversions for syntaxes
module Lambda.AlphaConv (AlphaConv (..)) where 
  
import qualified Lambda.Reduce as Reduce
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E
import Debug.Trace

class AlphaConv a where 
  unsafeAlphaConv :: String -> String -> a -> a

instance AlphaConv S.Exp where 
  unsafeAlphaConv = Reduce.unsafeAlphaConvert

instance AlphaConv E.Exp where 
  unsafeAlphaConv = enrConv

enrConv :: String -> String -> E.Exp -> E.Exp 
enrConv old new = go
  where 
    go :: E.Exp -> E.Exp
    go (E.Pure e) = E.Pure (unsafeAlphaConv old new e)
    go (E.Let bs e) = E.Let (map goBinding bs) (go e)
    go (E.Letrec bs e) = E.Letrec (map goBinding bs) (go e)
    go (E.Apply e1 e2) = E.Apply (go e1) (go e2)
    go (E.Lambda p e) = E.Lambda (goPatt p) (go e)
    go (E.FatBar e1 e2) = E.FatBar (go e1) (go e2)
    go (E.Case v cs) = E.Case v (map (go <$>) cs)

    goVar :: S.Variable -> S.Variable
    goVar v = if v == old then new else v

    goBinding :: E.LetBinding -> E.LetBinding
    goBinding (patt, expr) = (goPatt patt, go expr)

    goPatt :: E.Pattern -> E.Pattern
    goPatt c@(E.PConstant _) = c
    goPatt (E.PVariable v) = E.PVariable $ goVar v
    goPatt (E.PConstructor c patts) = E.PConstructor c $ map goPatt patts