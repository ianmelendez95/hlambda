{- | Basically beta reduction, but with evaluation of built in constants
-}
module Lambda.Eval (evalRaw, evalAfterBeta) where 

import Data.Maybe (fromMaybe)

import Lambda.Beta (betaReduce)
import qualified Lambda.Syntax as S

data EvalExp = Exp S.Exp
             | Cons S.Exp S.Exp
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
evalAfterBeta expr = evalReducing expr
  -- = case eval expr of 
  --     (Exp res) -> res
  --     (Applicable (Appl res _)) -> res
  --     (Cons h t) -> S.Apply (S.Apply (S.Function S.FCons) h) t

evalReducing :: S.Exp -> S.Exp
evalReducing (S.Apply e1 e2) 
  = let re1 = evalReducing e1 
        re2 = evalReducing e2
     in case evalApplyFunc re1 re2 of 
          Nothing -> S.Apply re1 re2
          Just res -> res
evalReducing expr = expr

evalApplyFunc :: S.Exp -> S.Exp -> Maybe S.Exp 
evalApplyFunc (S.Apply (S.Function f) 
                       (S.Constant c1))
                       (S.Constant c2) = evalFuncOnTwoConsts f c1 c2
evalApplyFunc _ _ = Nothing

evalFuncOnTwoConsts :: S.Function -> S.Constant -> S.Constant -> Maybe S.Exp                    
evalFuncOnTwoConsts S.FPlus  (S.CNat x) (S.CNat y) = Just . S.Constant . S.CNat $ (x + y)
evalFuncOnTwoConsts S.FMinus (S.CNat x) (S.CNat y) = Just . S.Constant . S.CNat $ (x - y)
evalFuncOnTwoConsts S.FMult  (S.CNat x) (S.CNat y) = Just . S.Constant . S.CNat $ (x * y)
evalFuncOnTwoConsts S.FDiv   (S.CNat x) (S.CNat y) = Just . S.Constant . S.CNat $ (x `div` y)
evalFuncOnTwoConsts S.FAnd   c1         c2         = onBoolsMaybe (&&) c1 c2
evalFuncOnTwoConsts S.FOr    c1         c2         = onBoolsMaybe (||) c1 c2
evalFuncOnTwoConsts _        _          _          = Nothing

onBoolsMaybe :: (Bool -> Bool -> Bool) -> S.Constant -> S.Constant -> Maybe S.Exp
onBoolsMaybe f b1 b2 = toBool <$> (f <$> readBoolMaybe b1 <*> readBoolMaybe b2)

matchFuncOnTwoConsts :: S.Exp -> Maybe (S.Function, S.Constant, S.Constant)
matchFuncOnTwoConsts (S.Apply (S.Apply (S.Function f) 
                                       (S.Constant c)) 
                                       (S.Constant c')) = Just (f, c, c')
matchFuncOnTwoConsts _ = Nothing

eval :: S.Exp -> EvalExp
eval c@(S.Constant _) = Exp c 
eval v@(S.Variable _) = Exp v
eval (S.Function f) = Applicable $ builtinFunc f
eval (S.Apply funcExp argExp) 
  = case eval funcExp of 
      (Applicable appl) -> case eval argExp of 
                             a@(Applicable _) -> error $ "Argument evaluated to an applicable: " ++ show a
                             (Exp expr) -> apply appl expr
                             (Cons h t) -> apply appl $ consExp h t 
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
builtinFunc S.FCons  = makeApplicable2' (S.Function S.FCons) cons
builtinFunc S.FHead  = Appl (S.Function S.FHead) consHead
builtinFunc S.FTail  = Appl (S.Function S.FTail) consTail

makeApplicable2' :: S.Exp -> (S.Exp -> S.Exp -> EvalExp) -> Applicable
makeApplicable2' funcExp f2 = 
  Appl funcExp 
    (\e1 -> Applicable $ makeApplicable1' (S.Apply funcExp e1) (f2 e1))

makeApplicable1' :: S.Exp -> (S.Exp -> EvalExp) -> Applicable 
makeApplicable1' = Appl

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

cons :: S.Exp -> S.Exp -> EvalExp
cons = Cons

consExp :: S.Exp -> S.Exp -> S.Exp
consExp h = S.Apply (S.Apply (S.Function S.FCons) h)

consHead :: S.Exp -> EvalExp
consHead expr = case eval expr of 
                  (Cons h _) -> Exp h
                  res -> error $ "Invoked head on non-cons: " ++ show res

consTail :: S.Exp -> EvalExp 
consTail expr = case eval expr of
                  (Cons _ t) -> Exp t
                  res -> error $ "Invoked tail on non-cons: " ++ show res

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

readBoolMaybe :: S.Constant -> Maybe Bool
readBoolMaybe S.CTrue = Just True
readBoolMaybe S.CFalse = Just False
readBoolMaybe _ = Nothing

readNumber :: S.Exp -> Int 
readNumber (S.Constant (S.CNat val)) = val
readNumber expr = error $ "Expecting a number, got: " ++ show expr
