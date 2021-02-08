{- | Basically beta reduction, but with evaluation of built in constants
-}
module Lambda.Eval (evalRaw, evalAfterBeta) where 

import Lambda.Beta (betaReduce)
import qualified Lambda.Syntax as S

data EvalExp = Exp S.Exp
             | Applicable Applicable
             deriving Show

data Applicable = Appl S.Exp (S.Exp -> EvalExp)

instance Show Applicable where
  show (Appl expr _) = show expr

apply :: Applicable -> S.Exp -> EvalExp
apply (Appl _ f) = f

evalRaw :: S.Exp -> S.Exp
evalRaw = evalAfterBeta . betaReduce

-- | apply any known constants
evalAfterBeta :: S.Exp -> S.Exp
evalAfterBeta expr = case eval expr of 
                       (Exp res) -> res
                       (Applicable (Appl res _)) -> res

eval :: S.Exp -> EvalExp
eval c@(S.Constant _) = Exp c 
eval v@(S.Variable _) = Exp v
eval (S.Function f) = Applicable $ builtinFunc f
eval (S.Apply funcExp argExp) 
  = case eval funcExp of 
      (Applicable appl) -> case eval argExp of 
                             (Exp arg) -> apply appl arg
                             arg -> error $ "Argument evaluated to an applicable: " ++ show arg
      func -> error $ "Expecting applicable function: " ++ show func

-- | identifies the applicable function
builtinFunc :: S.Function -> Applicable
builtinFunc S.FPlus  = makeApplicable2 (S.Function S.FPlus) plus
builtinFunc S.FMinus = makeApplicable2 (S.Function S.FMinus) minus
builtinFunc S.FMult  = makeApplicable2 (S.Function S.FMult) mult
builtinFunc S.FDiv   = makeApplicable2 (S.Function S.FDiv) divE

makeApplicable2 :: S.Exp -> (S.Exp -> S.Exp -> S.Exp) -> Applicable
makeApplicable2 funcExp f2 = 
  Appl funcExp 
    (\e1 -> makeApplicable1 (S.Apply funcExp e1) (f2 e1))

makeApplicable1 :: S.Exp -> (S.Exp -> S.Exp) -> EvalExp
makeApplicable1 funcExp f = Applicable $ Appl funcExp $ \e -> Exp (f e)

plus :: S.Exp -> S.Exp -> S.Exp
plus = onNumbers (+)

minus :: S.Exp -> S.Exp -> S.Exp
minus = onNumbers (-)

mult :: S.Exp -> S.Exp -> S.Exp
mult = onNumbers (*)

divE :: S.Exp -> S.Exp -> S.Exp
divE = onNumbers div

onNumbers :: (Int -> Int -> Int) -> S.Exp -> S.Exp -> S.Exp 
onNumbers f e e' = S.Constant . S.CNat $ f (readNumber e) (readNumber e')

readNumber :: S.Exp -> Int 
readNumber (S.Constant (S.CNat val)) = val
readNumber expr = error $ "Expecting a number, got: " ++ show expr
