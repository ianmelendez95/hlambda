module Lambda.CaseCompiler 
  ( CaseExpr (..),
    compileCase, 
    getLambdaFunction ) 
  where

import Data.List (sortBy)

import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

import Lambda.Constructor 
  ( selectFunctions,
    structTag )

-- | representation of CASE-N lambda expression
-- | as illustrated on p124
data CaseExpr = CaseExpr S.Variable [E.Exp]
              deriving (Show)

getLambdaFunction :: CaseExpr -> S.Exp
getLambdaFunction (CaseExpr _ es) = S.mkVariable ("CASE-" ++ show (length es))

compileCase :: S.Variable -> [E.CaseClause] -> CaseExpr
compileCase v cs = 
  let cs_sorted = sortBy compareStructTag cs
   in CaseExpr v (map (compileClause v) cs_sorted)
  where
    compareStructTag :: E.CaseClause -> E.CaseClause -> Ordering
    compareStructTag (E.Clause c1 _ _) (E.Clause c2 _ _) = 
      compare (structTag c1) (structTag c2)

compileClause :: S.Variable -> E.CaseClause -> E.Exp
compileClause case_var clause@(E.Clause c vs e) = 
  let sel_funcs = selectFunctions c
      case_var_expr = S.mkVariable case_var
      let_binds = zipWith (\clause_v sel_f -> 
                            ( E.PVariable clause_v, 
                              E.mkApply [ E.Pure (S.mkVariable sel_f), 
                                          E.Pure case_var_expr ]))
                          vs sel_funcs
   in if length sel_funcs /= length vs
        then error $ "Malformed case clause. Argument vars differ in number to constructor arity: "
                      ++ show clause
        else E.Let let_binds e