module Lambda.EnrichedCompiler where 

import Data.Bifunctor (second)

import Common.Name (newName)

import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S

import qualified Miranda.PattMatch as PMC
import qualified Lambda.LetLetrec as LLC

-- | converts all let(rec)s into simple let(rec)s
compileToLambda :: E.Exp -> S.Exp
compileToLambda (E.Pure e) = e

compileToLambda (E.Let bs expr) = 
  let simple_bs :: [[(S.Variable, S.Exp)]]
      simple_bs = (map . map) (second compileToLambda) (LLC.compileBindings bs)
      expr' = compileToLambda expr
   in foldr S.Let expr' simple_bs
compileToLambda (E.Letrec bs expr) =
  let simple_bs = (map . map) (second compileToLambda) (LLC.compileBindings bs)
      expr' = compileToLambda expr
   in S.Letrec (concat simple_bs) expr'

compileToLambda (E.Apply e1 e2) = S.Apply (compileToLambda e1) (compileToLambda e2)

compileToLambda (E.Lambda (E.PVariable v) e) = S.Lambda v (compileToLambda e)
compileToLambda (E.Lambda comp_patt expr) = 
  let enr_func = PMC.patternEquationsToEnriched [([comp_patt], expr)]
   in case enr_func of 
        (E.Lambda (E.PVariable v) e) -> S.Lambda v (compileToLambda e)
        _ -> error $ "Expected pattern match compiler to return single var lambda: " ++ show enr_func

compileToLambda (E.FatBar e1 e2) = 
  let new_name = newName (E.freeVariables e1 ++ E.freeVariables e2)
      new_var_patt = E.PVariable new_name
      new_var_expr = E.Pure $ S.mkVariable new_name
      let_expr = 
        E.Let [(new_var_patt, e1)]
              (E.Lambda new_var_patt 
                (E.mkIf (E.mkApply [E.Pure $ S.mkFunction S.FEq, 
                                    new_var_expr,
                                    E.Pure $ S.mkConstant S.CFail])
                        e2 new_var_expr))
   in compileToLambda let_expr

compileToLambda (E.Case _ _) = error "About to learn how to compile case exprs"