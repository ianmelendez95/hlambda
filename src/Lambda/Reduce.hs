module Lambda.Reduce (Reducible (..)) where 

-- new reduction strategy that emulates laziness

import Data.List (insert, union)
import Data.Char (isLower)

import Lambda.Syntax
import qualified Lambda.Enriched as E
import qualified Miranda.Syntax as M
import Lambda.FreeBound

class Reducible a where 
  reduce :: a -> Exp

instance Reducible M.Prog where 
  reduce = reduce . toLambda

instance Reducible E.Exp where 
  reduce = reduce . toLambda

instance Reducible Exp where 
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
  = if var_name == varName var_arg && not (varFreeInExp func_exp var_name)
      then Just func_exp
      else Nothing
etaReduceLambda _ _ = Nothing

varFreeInExp :: Exp -> String -> Bool
varFreeInExp expr = (`elem` freeVariables [] expr)

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
reduceFunctionApplication FY     = reduceYCombApplication
reduceFunctionApplication FEq    = ((:[]) <$>) . reduceBinaryNumFuncApplication (==) 

reduceArithmeticApplication :: (Int -> Int -> Int) -> [Exp]  -> Either [Exp] Exp
reduceArithmeticApplication = reduceBinaryNumFuncApplication

reduceBinaryNumFuncApplication :: ToConstant a => (Int -> Int -> a) -> [Exp] -> Either [Exp] Exp
reduceBinaryNumFuncApplication f [arg_exp1, arg_exp2]
  = case reduceAfterMarked arg_exp1 of 
      arg1@(Constant (CNat x)) -> case reduceAfterMarked arg_exp2 of 
                                    Constant (CNat y) -> Right . Constant . toConstant $ f x y
                                    arg2 -> Left [arg1, arg2]
      arg1 -> Left [arg1, arg_exp2]
reduceBinaryNumFuncApplication _ exps = Left exps

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

reduceYCombApplication :: [Exp] -> Either [Exp] [Exp]
reduceYCombApplication [] = Left []
reduceYCombApplication (arg : rest) = Right (arg : Apply (Function FY) arg : rest)

------------
-- Lambda --
------------

-- | reduce lambda, replacing instances of var in body with val
-- | following the rules in 'Figure 2.3 Definition of E[M/x]'
reduceLambda :: String -> Exp -> Exp -> Exp
reduceLambda var body val = replaceVarWithValInBody var val body

-- showLambdaUpdate :: String -> Exp -> Exp -> String 
-- showLambdaUpdate var body newVal = "(" ++ pShow body ++ ")[(" ++ pShow newVal ++ ")/" ++ var ++ "]"

-- | replaceVarWithValInBody is called when we are applying a lambda abstraction to an argument,
-- | replacing instances of the parameter 'name' with the 'new_exp'
replaceVarWithValInBody :: String -> Exp -> Exp -> Exp
replaceVarWithValInBody _ _ c@(Constant _) = c
replaceVarWithValInBody _ _ f@(Function _) = f
replaceVarWithValInBody name val v@(Variable var) = if varName var == name then val else v
replaceVarWithValInBody name newExp (Apply e1 e2) = Apply (replaceVarWithValInBody name newExp e1) 
                                                          (replaceVarWithValInBody name newExp e2)
replaceVarWithValInBody name new_exp l@(Lambda v e)
  | v == name = l
  | (name `elem` freeVariables [] e) && (v `elem` freeVariables [] new_exp)
      = let (new_v, new_e) = alphaConvertRestricted (freeVariables [] new_exp `union` freeVariables [v] e) v e
         in Lambda new_v (replaceVarWithValInBody name new_exp new_e)
  | otherwise = Lambda v (replaceVarWithValInBody name new_exp e)

-- | performs an alpha conversion, where the new name is restricted by the existing 
-- | 'taken' or free variables that would result in a conflict if used
alphaConvertRestricted :: [String] -> String -> Exp -> (String, Exp)
alphaConvertRestricted taken_names var body 
  = let new_name = newName taken_names var 
     in (new_name, alphaConvert var new_name body)

-- | rather unsafely converts instances of the old name with the new name
-- | only scrutinizing on formal parameters (that is, if encounter a lambda with the formal parameter 
-- | matching the old name, it doesn't continue)
-- | 
-- | unsafe in that it makes no discernment for whether it is replacing the name with 
-- | a variable that is also free in the expression
alphaConvert :: String -> String -> Exp -> Exp
alphaConvert _ _ c@(Constant _) = c
alphaConvert _ _ f@(Function _) = f 
alphaConvert old_name new_name (Variable var) = Variable $ mapVarName (\n -> if n == old_name then new_name else n) var
alphaConvert old_name new_name (Apply e1 e2) = Apply (alphaConvert old_name new_name e1)
                                                     (alphaConvert old_name new_name e2)
alphaConvert old_name new_name l@(Lambda v e)
  | old_name == v = l -- name is already bound, doesn't matter
  | otherwise = Lambda v (alphaConvert old_name new_name e)

newName :: [String] -> String -> String 
newName taken name
  | succ_name `elem` taken = newName taken succ_name
  | otherwise = succ_name
  where 
    succ_name = succName name

succName :: String -> String 
succName [] = error "empty name"
succName [c] 
  | not (isLower c) = error $ "variable not lowercase: " ++ [c]
  | c == 'z'        = "z'"
  | otherwise       = [succ c]
succName name = name ++ "'"

freeVariables :: [String] -> Exp -> [String]
freeVariables _ (Constant _) = []
freeVariables _ (Function _) = []
freeVariables bound (Variable var) = [varName var | varName var `notElem` bound]
freeVariables bound (Apply e1 e2)  = freeVariables bound e1 ++ freeVariables bound e2
freeVariables bound (Lambda v e) = freeVariables (insert v bound) e
 