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
reduceAfterMarked l@(Lambda _ _) = l
reduceAfterMarked s@(Apply _ _) = reduceApplyChain . parseApplyChain $ s

-----------
-- Apply --
-----------

reduceApplyChain :: [Exp] -> Exp
reduceApplyChain (Function func : rest) 
  = case reduceFunctionApplication func rest of 
      Left args -> foldApply $ Function func : args
      Right evaled -> reduceApplyChain evaled
reduceApplyChain (Lambda var body : arg : rest) 
  = reduceApplyChain (reduceLambda var (reduceAfterMarked arg) body : rest)
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
reduceLambda var val = mapExpressions (replaceVarShallow var val)

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