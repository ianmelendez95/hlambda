module Lambda.LetLetrec where 

import Common.Name (newName)
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S
import qualified Lambda.Constructor as C

data BindingPattern = Constant S.Constant E.Exp
                    | Simple S.Variable E.Exp
                    | Irrefutable C.Product [S.Variable] E.Exp
                    | General     C.Sum     [S.Variable] E.Exp

data IrrefutablePattern = IPVariable S.Variable
                        | IPConstructor C.Product [IrrefutablePattern]

-- | represents the 'cascade' of let bindings (where in letrec it would be flattened)
-- | e.g. [[(x, 1),(y,2)], [(z,3)], [(p,4),(q,5)]]
-- | => let x = 1
-- |        y = 2
-- |     in let z = 3
-- |         in let p = 4
-- |                q = 5
-- |             in _
type LetCascade = [[E.LetBinding]]

type SimpleBinding = (S.Variable, E.Exp)

--------------------------------------------------------------------------------
-- irrefutables

-- | p110
checkIrrefutablePattern :: E.Pattern -> Maybe IrrefutablePattern
checkIrrefutablePattern (E.PVariable v) = Just $ IPVariable v
checkIrrefutablePattern (E.PConstructor c ps) = 
  case C.constrType c of 
    C.Sum _ -> Nothing
    C.Product p -> IPConstructor p <$> traverse checkIrrefutablePattern ps
checkIrrefutablePattern _ = Nothing

irrefutableToEnriched :: IrrefutablePattern -> E.Exp
irrefutableToEnriched = E.fromPattern . irrefutableToPattern

irrefutableToPattern :: IrrefutablePattern -> E.Pattern
irrefutableToPattern (IPVariable v) = E.PVariable v
irrefutableToPattern (IPConstructor p ps) = 
  E.PConstructor (C.tupleForProduct p) (map irrefutableToPattern ps)

--------------------------------------------------------------------------------
-- binding pattern types

mkBindingPattern :: E.LetBinding -> BindingPattern
mkBindingPattern (E.PConstant c, e) = Constant c e 
mkBindingPattern (E.PVariable v, e) = Simple v e
mkBindingPattern (E.PConstructor c ps, e) =
  let patt_vars = concatMap patternVariables ps
   in case C.constrType c of 
        C.Product p -> Irrefutable p patt_vars e
        C.Sum s ->     General     s patt_vars e
      
isIrrefutableFamily :: BindingPattern -> Bool
isIrrefutableFamily Simple{} = True
isIrrefutableFamily Irrefutable{} = True
isIrrefutableFamily _ = False

-- | p117 Definition
patternVariables :: E.Pattern -> [S.Variable]
patternVariables (E.PConstant _) = []
patternVariables (E.PVariable v) = [v]
patternVariables (E.PConstructor _ ps) = concatMap patternVariables ps

--------------------------------------------------------------------------------
-- compile the bindings

compileBindings :: [E.LetBinding] -> [[SimpleBinding]]
compileBindings = concatMap compileBinding

compileBinding :: E.LetBinding -> [[SimpleBinding]]
compileBinding = compileIrrefutableBinding . bindingToIrrefutable
  
bindingToIrrefutable :: E.LetBinding -> (IrrefutablePattern, E.Exp)
bindingToIrrefutable (p, e) = 
  case checkIrrefutablePattern p of 
    Just irr_p -> (irr_p, e)
    Nothing -> coerceToIrrefutable p e

compileIrrefutableBindings :: [(IrrefutablePattern, E.Exp)] -> [[SimpleBinding]]
compileIrrefutableBindings = concatMap compileIrrefutableBinding

compileIrrefutableBinding :: (IrrefutablePattern, E.Exp) -> [[SimpleBinding]]
compileIrrefutableBinding (IPVariable v, e) = [[(v, e)]]
compileIrrefutableBinding (IPConstructor (C.CProduct arty) ps, e) =
      let new_name = newName $ E.freeVariables e
          mkSelectFunc selector = E.Apply (E.Pure $ S.mkVariable selector) 
                                          (E.Pure $ S.mkVariable new_name)
          new_bindings = 
            zipWith (\patt selector -> (patt, mkSelectFunc selector)) 
                    ps
                    (C.selectFunctions' arty)
       in ([(new_name, e)] : compileIrrefutableBindings new_bindings)

coerceToIrrefutable :: E.Pattern -> E.Exp -> (IrrefutablePattern, E.Exp)
coerceToIrrefutable (E.PVariable v) e = (IPVariable v, e)
coerceToIrrefutable (E.PConstant _) e = (IPConstructor (C.CProduct 0) [], e)

{-
[General -> Irrefutable]

let (sum v1 ... vn) = B
in E

=>

let (prod v1 ... vn) = let _u1 = B
                       in (\sum v1 ... vn. prod v1 ... vn) _u1
in E

Where v1 ... vn is the flattened list of variables (e.g. (CONS x (CONS y z))) => v1=x v2=y v3=z
-}
coerceToIrrefutable patt@E.PConstructor{} e = 
  case checkIrrefutablePattern patt of 
    Just irr_patt -> (irr_patt, e)
    Nothing -> 
      let patt_vars = E.boundVarsInPattern patt
          irr_constr = IPConstructor (C.CProduct (length patt_vars)) (map IPVariable patt_vars)
          new_name = newName (patt_vars ++ E.freeVariables e)
       in ( irr_constr
          , E.Let [(E.PVariable new_name, e)]
                  (E.Apply (E.Lambda patt (irrefutableToEnriched irr_constr)) 
                           (E.Pure $ S.mkVariable new_name))
          )

-- | compile the pattern into equivalent simple bindings
-- compilePattern :: BindingPattern -> [(S.Variable, E.Exp)]
-- compilePattern (Simple v expr) = [(v, expr)]
-- compilePattern (Irrefutable prodc vars expr) = 
--   let new_name = newName (vars ++ E.freeVariables expr)
--    in undefined