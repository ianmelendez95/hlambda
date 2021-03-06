module Lambda.EnrichedCompiler where 

import Data.Bifunctor (second)

import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S

import qualified Miranda.PattMatch as PMC
import qualified Lambda.LetLetrec as LLC
import Lambda.DepAnalysis (depAnalysis)
import Lambda.CaseCompiler 
  ( CaseExpr (..), 
    compileCase, 
    getLambdaFunction )

compileToLambda :: E.Exp -> S.Exp
compileToLambda (E.Pure e) = e

-- compileToLambda (E.Let bs expr) = 
--   let simple_bs :: [[(S.Variable, S.Exp)]]
--       simple_bs = (map . map) (second compileToLambda) (LLC.compileBindings bs)
--       expr' = compileToLambda expr
--    in foldr S.Let expr' simple_bs
-- compileToLambda (E.Letrec bs expr) =
--   let simple_bs = (map . map) (second compileToLambda) (LLC.compileBindings bs)
--       expr' = compileToLambda expr
--    in S.Letrec (concat simple_bs) expr'

compileToLambda (E.Let b expr) = compileLetLetrec [b] expr
compileToLambda (E.Letrec bs expr) = compileLetLetrec bs expr

compileToLambda (E.Apply e1 e2) = S.Apply (compileToLambda e1) (compileToLambda e2)

compileToLambda (E.Lambda (E.PVariable v) e) = S.Lambda v (compileToLambda e)
compileToLambda (E.Lambda comp_patt expr) = 
  let enr_func = PMC.patternEquationsToEnriched [([comp_patt], expr)]
   in case enr_func of 
        (E.Lambda (E.PVariable v) e) -> S.Lambda v (compileToLambda e)
        _ -> error $ "Expected pattern match compiler to return single var lambda: " ++ show enr_func

-- TODO: this is not covered in tests
compileToLambda (E.FatBar e1 e2) = 
  S.mkApply [S.mkVariable "FATBAR", compileToLambda e1, compileToLambda e2]

compileToLambda (E.Case v cs) = 
  let case_expr@(CaseExpr case_v exprs) = compileCase v cs
   in case exprs of 
        [expr] -> compileToLambda expr
        _ -> S.mkApply ( getLambdaFunction case_expr : 
                         S.mkVariable case_v : 
                         map compileToLambda exprs)

compileLetLetrec :: [E.LetBinding] -> E.Exp -> S.Exp
compileLetLetrec bs expr = 
  let simple_bs :: [[(S.Variable, S.Exp)]]
      simple_bs = (map . map) (second compileToLambda) (LLC.compileBindings bs)

      expr' = compileToLambda expr
   in depAnalysis (concat simple_bs) expr'