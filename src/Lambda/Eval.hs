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
builtinFunc S.FAnd   = makeApplicable2 (S.Function S.FAnd) andE
builtinFunc S.FOr    = makeApplicable2 (S.Function S.FOr) orE
builtinFunc S.FNot   = Appl (S.Function S.FNot) $ \e -> Exp (notE e)
builtinFunc S.FIf    = makeApplicable3 (S.Function S.FIf) ifE 

makeApplicable3 :: S.Exp -> (S.Exp -> S.Exp -> S.Exp -> S.Exp) -> Applicable
makeApplicable3 funcExp f3 = 
  Appl funcExp 
    (\e1 -> Applicable $ makeApplicable2 (S.Apply funcExp e1) (f3 e1))

makeApplicable2 :: S.Exp -> (S.Exp -> S.Exp -> S.Exp) -> Applicable
makeApplicable2 funcExp f2 = 
  Appl funcExp 
    (\e1 -> Applicable $ makeApplicable1 (S.Apply funcExp e1) (f2 e1))

makeApplicable1 :: S.Exp -> (S.Exp -> S.Exp) -> Applicable
makeApplicable1 funcExp f = Appl funcExp $ \e -> Exp (f e)

ifE :: S.Exp -> S.Exp -> S.Exp -> S.Exp
ifE cond tru fals = if readBool cond then tru else fals

plus :: S.Exp -> S.Exp -> S.Exp
plus = onNumbers (+)

minus :: S.Exp -> S.Exp -> S.Exp
minus = onNumbers (-)

mult :: S.Exp -> S.Exp -> S.Exp
mult = onNumbers (*)

divE :: S.Exp -> S.Exp -> S.Exp
divE = onNumbers div

andE :: S.Exp -> S.Exp -> S.Exp
andE = onBools (&&)

orE :: S.Exp -> S.Exp -> S.Exp
orE = onBools (||)

notE :: S.Exp -> S.Exp
notE = toBool . not . readBool

onNumbers :: (Int -> Int -> Int) -> S.Exp -> S.Exp -> S.Exp 
onNumbers f e e' = S.Constant . S.CNat $ f (readNumber e) (readNumber e')

toBool :: Bool -> S.Exp
toBool True = S.Constant S.CTrue
toBool False = S.Constant S.CFalse

onBools :: (Bool -> Bool -> Bool) -> S.Exp -> S.Exp -> S.Exp 
onBools f e e' = S.Constant $ if f (readBool e) (readBool e') then S.CTrue else S.CFalse

readBool :: S.Exp -> Bool
readBool (S.Constant S.CTrue) = True
readBool (S.Constant S.CFalse) = False
readBool expr = error $ "Expecting a boolean, got: " ++ show expr

readNumber :: S.Exp -> Int 
readNumber (S.Constant (S.CNat val)) = val
readNumber expr = error $ "Expecting a number, got: " ++ show expr
