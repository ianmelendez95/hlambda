module Lambda.Reduce (reduce) where 

-- new reduction strategy that emulates laziness

import Lambda.Syntax
import Lambda.FreeBound

reduce :: Exp -> Exp 
reduce = reduceAfterMarked . markBoundFree

-- TODO: reduce WHNF
reduceAfterMarked :: Exp -> Exp 
reduceAfterMarked c@(Constant _) = c
reduceAfterMarked f@(Function _) = f
reduceAfterMarked v@(Variable _) = v
reduceAfterMarked l@(Lambda var body) = maybe l reduceAfterMarked $ etaReduceLambda var body
reduceAfterMarked s@(Apply _ _) = reduceApplyChain . parseApplyChain $ s

-------------------
-- Eta Reduction --
-------------------

etaReduceLambda :: String -> Exp -> Maybe Exp
etaReduceLambda var_name (Apply func_exp (Variable var_arg)) 
  = if var_name == varName var_arg && not (varFreeInExp var_name func_exp)
      then Just func_exp
      else Nothing
etaReduceLambda _ _ = Nothing

varFreeInExp :: String -> Exp -> Bool
varFreeInExp _ (Constant _) = False
varFreeInExp _ (Function _) = False
varFreeInExp name (Variable var) = varName var == name
varFreeInExp name (Apply e1 e2)  = varFreeInExp name e1 || varFreeInExp name e2
varFreeInExp name (Lambda v e) = v /= name && varFreeInExp name e

-----------
-- Apply --
-----------

reduceApplyChain :: [Exp] -> Exp
reduceApplyChain (Function func : rest) 
  = case reduceFunctionApplication func rest of 
      Left args -> foldApply $ Function func : args
      Right evaled -> reduceApplyChain evaled
reduceApplyChain (Lambda var body : arg : rest) 
  = reduceApplyChain $ parseApplyChain (reduceLambda var body arg) ++ rest
reduceApplyChain apply = foldApply apply

foldApply :: [Exp] -> Exp
foldApply [] = error "Cannot fold empty apply chain"
foldApply (e:es) = foldl Apply e es

parseApplyChain :: Exp -> [Exp]
parseApplyChain (Apply e1 e2) = parseApplyChain e1 ++ [e2]
parseApplyChain expr = [expr]

-- Function

-- | attempts to reduce the application of the function to the argument expressions 
-- | Left -> contains the argument list without the application of the function 
-- | Right -> contains the resulting expression after successful application
reduceFunctionApplication :: Function -> [Exp] -> Either [Exp] [Exp]
reduceFunctionApplication FPlus  = ((:[]) <$>) . reduceArithmeticApplication (+)
reduceFunctionApplication FMinus = ((:[]) <$>) . reduceArithmeticApplication (-)
reduceFunctionApplication FMult  = ((:[]) <$>) . reduceArithmeticApplication (*)
reduceFunctionApplication FDiv   = ((:[]) <$>) . reduceArithmeticApplication div
reduceFunctionApplication FAnd   = ((:[]) <$>) . reduceLogicApplication (&&)
reduceFunctionApplication FOr    = ((:[]) <$>) . reduceLogicApplication (||)
reduceFunctionApplication FNot   = ((:[]) <$>) . reduceNotApplication
reduceFunctionApplication FIf    = reduceIfApplication
reduceFunctionApplication FCons  = Left -- Cons is lazy, and mark Left so we don't continue evaluating
reduceFunctionApplication FHead  = ((:[]) <$>) . reduceHeadApplication
reduceFunctionApplication FTail  = ((:[]) <$>) . reduceTailApplication

reduceArithmeticApplication :: (Int -> Int -> Int) -> [Exp]  -> Either [Exp] Exp
reduceArithmeticApplication f [arg_exp1, arg_exp2]
  = case reduceAfterMarked arg_exp1 of 
      arg1@(Constant (CNat x)) -> case reduceAfterMarked arg_exp2 of 
                                    Constant (CNat y) -> Right . Constant . CNat $ f x y
                                    arg2 -> Left [arg1, arg2]
      arg1 -> Left [arg1, arg_exp2]
reduceArithmeticApplication _ exps = Left exps

reduceLogicApplication :: (Bool -> Bool -> Bool) -> [Exp]  -> Either [Exp] Exp
reduceLogicApplication f [arg_exp1, arg_exp2] 
  = case reduceAfterMarked arg_exp1 of 
      arg1@(Constant (CBool x)) -> case reduceAfterMarked arg_exp2 of 
                                     Constant (CBool y) -> Right . Constant . CBool $ f x y
                                     arg2 -> Left [arg1, arg2]
      arg1 -> Left [arg1, arg_exp2]
reduceLogicApplication _ exps = Left exps

reduceNotApplication :: [Exp] -> Either [Exp] Exp
reduceNotApplication [arg_exp] = case reduceAfterMarked arg_exp of 
                                   (Constant (CBool p)) -> Right . Constant $ CBool (not p)
                                   arg -> Left [arg]
reduceNotApplication exps = Left exps

reduceIfApplication :: [Exp] -> Either [Exp] [Exp]
reduceIfApplication (case_exp : true_exp : false_exp : rest) 
  = case reduceAfterMarked case_exp of 
      (Constant (CBool bool)) -> Right (reduceAfterMarked (if bool then true_exp else false_exp) : rest)
      casev -> Left (casev : true_exp : false_exp : rest)
reduceIfApplication exps = Left exps

reduceHeadApplication :: [Exp] -> Either [Exp] Exp
reduceHeadApplication (cons_exp : rest)
  = case parseApplyChain $ reduceAfterMarked cons_exp of  -- TODO: rework as WHNF instead of full reduction
      [Function FCons, head_exp, _] -> Right head_exp
      _ -> Left (cons_exp : rest)
reduceHeadApplication exps = Left exps

reduceTailApplication :: [Exp] -> Either [Exp] Exp
reduceTailApplication (cons_exp : rest)
  = case parseApplyChain $ reduceAfterMarked cons_exp of  -- TODO: rework as WHNF instead of full reduction
      [Function FCons, _, tail_exp] -> Right tail_exp
      _ -> Left (cons_exp : rest)
reduceTailApplication exps = Left exps

------------
-- Lambda --
------------

reduceLambda :: String -> Exp -> Exp -> Exp
reduceLambda var body val = replaceVar var val body 

-- showLambdaUpdate :: String -> Exp -> Exp -> String 
-- showLambdaUpdate var body newVal = "(" ++ pShow body ++ ")[(" ++ pShow newVal ++ ")/" ++ var ++ "]"

replaceVar :: String -> Exp -> Exp -> Exp
replaceVar _ _ c@(Constant _) = c
replaceVar _ _ f@(Function _) = f
replaceVar name val v@(Variable var) = if varName var == name then val else v
replaceVar name newExp (Apply e1 e2) = Apply (replaceVar name newExp e1) 
                                                (replaceVar name newExp e2)
replaceVar name newExp l@(Lambda v e) = if v == name then l
                                                     else Lambda v (replaceVar name newExp e)
