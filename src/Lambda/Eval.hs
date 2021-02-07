{- | Basically beta reduction, but with evaluation of built in constants
-}
module Lambda.Eval (evalRaw, evalAfterBeta) where 

import Lambda.Beta (betaReduce)
import Lambda.Syntax

data EvalExp = Exp Exp
             | Applied Exp (Exp -> Exp)

evalRaw :: Exp -> Exp
evalRaw = evalAfterBeta . betaReduce

-- | apply any known constants
evalAfterBeta :: Exp -> Exp
evalAfterBeta (Apply (Constant const) expr) = undefined -- lookup constant expression, if available, and apply to expression
evalAfterBeta (Lambda var expr) = (Lambda var (evalAfterBeta expr))
evalAfterBeta expr = expr

applyConstant :: String -> Exp -> EvalExp
applyConstant "+" (Constant const) = undefined
