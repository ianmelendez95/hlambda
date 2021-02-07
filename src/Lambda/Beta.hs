module Lambda.Beta (betaReduce) where 

import Lambda.Syntax (Exp (..), Variable (..))

betaReduce :: Exp -> Exp 
betaReduce (Apply e e') = 
  case betaReduce e of 
    (Lambda var lambdaBody) -> betaReduce $ reduceByApply var (betaReduce e') lambdaBody
    reduced -> Apply reduced (betaReduce e') -- TODO check that we only apply to built in functions otherwise
betaReduce (Lambda var expr) = Lambda var (betaReduce expr)
betaReduce expr = expr

reduceByApply :: String -> Exp -> Exp -> Exp
reduceByApply var val = mapExpressions (replaceVarShallow var val)

replaceVarShallow :: String -> Exp -> Exp -> Exp
replaceVarShallow var _ v@(Variable (FreeVar var')) 
  = if var == var' then error $ "Possible name capture: " ++ var else v
replaceVarShallow var expr v@(Variable (BoundVar var')) 
  = if var == var' then expr else v
replaceVarShallow var _ v@(Variable (RawVar var')) 
  = if var == var' then error $ "Haven't identified bound/free vars: " ++ show v else v
replaceVarShallow _ _ expr = expr

mapExpressions :: (Exp -> Exp) -> Exp -> Exp
mapExpressions _ c@(Constant _) = c
mapExpressions _ f@(Function _) = f
mapExpressions _ v@(Variable _) = v
mapExpressions f (Apply e e') = Apply (f (mapExpressions f e)) (f (mapExpressions f e'))
mapExpressions f (Lambda v e) = Lambda v (f (mapExpressions f e))