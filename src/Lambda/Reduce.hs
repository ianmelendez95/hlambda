module Lambda.Reduce 
  ( Reducible (..)
  ) where 

-- new reduction strategy that emulates laziness

import Data.List (union)

import Lambda.Name (nextName)
import qualified Miranda.Syntax as M
import Lambda.Syntax
import qualified Lambda.Enriched as E
import Lambda.ToLambda

import Lambda.AlphaConv (unsafeAlphaConvert)

class Reducible a where 
  reduce :: a -> Exp

instance Reducible M.Prog where 
  reduce = reduce . toLambda

instance Reducible E.Exp where 
  reduce = reduce . toLambda

instance Reducible Exp where 
  reduce = reduceExp

reduceExp :: Exp -> Exp 
reduceExp e = 
  case reduceOnce e of 
    Left e' -> e'
    Right e' -> reduceExp e'

-- | reduce with only one (or otherwise minimal) transformation
reduceOnce :: Exp -> Either Exp Exp
reduceOnce l@(Let _ _) = error $ "Can't reduce let expressions: " ++ show l
reduceOnce l@(Letrec _ _) = error $ "Can't reduce letrec expressions: " ++ show l
reduceOnce t@(Term _) = Left t
reduceOnce l@(Lambda var body) = maybe (Left l) Right $ etaReduceLambda var body
reduceOnce s@(Apply _ _) = reduceApplyOnce . parseApplyChain $ s

-------------------
-- Eta Reduction --
-------------------

etaReduceLambda :: String -> Exp -> Maybe Exp
etaReduceLambda var_name (Apply func_exp (Term (Variable var_arg))) 
  = if var_name == varName var_arg && not (varFreeInExp func_exp var_name)
      then Just func_exp
      else Nothing
etaReduceLambda _ _ = Nothing

varFreeInExp :: Exp -> String -> Bool
varFreeInExp expr = (`elem` freeVariables expr)

-----------
-- Apply --
-----------

reduceApplyOnce :: [Exp] -> Either Exp Exp
reduceApplyOnce (Term (Function func) : rest) 
  = case reduceFunctionApplication func rest of 
      Left args -> Left $ mkApply $ mkFunction func : args
      Right evaled -> Right $ mkApply evaled
reduceApplyOnce (Lambda var body : arg : rest) 
  = Right . mkApply $ reduceLambda var body arg : rest
reduceApplyOnce apply = Left . mkApply $ apply

parseApplyChain :: Exp -> [Exp]
parseApplyChain = unApply

-- Function

-- | attempts to reduce the application of the function to the argument expressions 
-- | Left -> contains the argument list without the application of the function 
-- | Right -> contains the resulting expression after successful application
reduceFunctionApplication :: Function -> [Exp] -> Either [Exp] [Exp]
reduceFunctionApplication FPlus      = ((:[]) <$>) . reduceArithmeticApplication (+)
reduceFunctionApplication FMinus     = reduceMinusOrNegation
reduceFunctionApplication FMult      = ((:[]) <$>) . reduceArithmeticApplication (*)
reduceFunctionApplication FDiv       = ((:[]) <$>) . reduceArithmeticApplication div
reduceFunctionApplication FAnd       = ((:[]) <$>) . reduceLogicApplication (&&)
reduceFunctionApplication FOr        = ((:[]) <$>) . reduceLogicApplication (||)
reduceFunctionApplication FNot       = ((:[]) <$>) . reduceNotApplication
reduceFunctionApplication FIf        = reduceIfApplication
reduceFunctionApplication FCons      = Left -- Cons is lazy, and mark Left so we don't continue evaluating
reduceFunctionApplication (FTuple _) = Left
reduceFunctionApplication FHead      = ((:[]) <$>) . reduceHeadApplication
reduceFunctionApplication FTail      = ((:[]) <$>) . reduceTailApplication
reduceFunctionApplication FY         = reduceYCombApplication
reduceFunctionApplication FEq        = reduceEq 
reduceFunctionApplication FNEq       = reduceNEq
reduceFunctionApplication FLt        = ((:[]) <$>) . reduceBinaryNumFuncApplication (<) 
reduceFunctionApplication FGt        = ((:[]) <$>) . reduceBinaryNumFuncApplication (>) 

reduceMinusOrNegation :: [Exp] -> Either [Exp] [Exp]
reduceMinusOrNegation es = 
  case reduceArithmeticApplication (-) es of 
    Right res -> Right [res]
    Left [Term (Constant (CNat x))] -> Right [toConstantExp (-x)] -- can apply as negation
    Left es' -> Left es'

reduceArithmeticApplication :: (Int -> Int -> Int) -> [Exp]  -> Either [Exp] Exp
reduceArithmeticApplication = reduceBinaryNumFuncApplication

reduceEq :: [Exp] -> Either [Exp] [Exp]
reduceEq = reduceTermPredicate (==)

reduceNEq :: [Exp] -> Either [Exp] [Exp]
reduceNEq = reduceTermPredicate (/=) 

reduceTermPredicate :: (Term -> Term -> Bool) -> [Exp] -> Either [Exp] [Exp]
reduceTermPredicate predicate = reduceBinaryApplication (\e1 e2 -> toConstantExp <$> reduceTwoTerms e1 e2) 
  where 
    reduceTwoTerms :: Exp -> Exp -> Either [Exp] Bool
    reduceTwoTerms = withTerms predicate

    withTerms :: (Term -> Term -> a) -> Exp -> Exp -> Either [Exp] a
    withTerms term_f e1 e2 = 
      case reduceExp e1 of 
        e1'@(Term t1) -> 
          case reduceExp e2 of 
            (Term t2) -> Right $ term_f t1 t2
            e2' -> Left [e1', e2']
        e1' -> Left [e1', e2]

reduceBinaryApplication :: (Exp -> Exp -> Either [Exp] Exp) -> [Exp] -> Either [Exp] [Exp]
reduceBinaryApplication f [e1, e2] = 
  case f e1 e2 of 
    Left fail_res -> Left fail_res 
    Right succ_res -> Right [succ_res]
reduceBinaryApplication _ es = Left es

reduceBinaryNumFuncApplication :: ToConstant a => (Int -> Int -> a) -> [Exp] -> Either [Exp] Exp
reduceBinaryNumFuncApplication f [arg_exp1, arg_exp2]
  = case reduceExp arg_exp1 of 
      arg1@(Term (Constant (CNat x))) -> 
        case reduceExp arg_exp2 of 
          Term (Constant (CNat y)) -> Right . toConstantExp $ f x y
          arg2 -> Left [arg1, arg2]
      arg1 -> Left [arg1, arg_exp2]
reduceBinaryNumFuncApplication _ exps = Left exps

reduceLogicApplication :: (Bool -> Bool -> Bool) -> [Exp]  -> Either [Exp] Exp
reduceLogicApplication f [arg_exp1, arg_exp2] 
  = case reduceExp arg_exp1 of 
      arg1@(Term (Constant (CBool x))) -> 
        case reduceExp arg_exp2 of 
          Term (Constant (CBool y)) -> Right . toConstantExp $ f x y
          arg2 -> Left [arg1, arg2]
      arg1 -> Left [arg1, arg_exp2]
reduceLogicApplication _ exps = Left exps

reduceNotApplication :: [Exp] -> Either [Exp] Exp
reduceNotApplication [arg_exp] = 
  case reduceExp arg_exp of 
    (Term (Constant (CBool p))) -> Right . toConstantExp $ not p
    arg -> Left [arg]
reduceNotApplication exps = Left exps

reduceIfApplication :: [Exp] -> Either [Exp] [Exp]
reduceIfApplication (case_exp : true_exp : false_exp : rest) 
  = case reduceExp case_exp of 
      (Term (Constant (CBool bool))) -> Right (reduceExp (if bool then true_exp else false_exp) : rest)
      casev -> Left (casev : true_exp : false_exp : rest)
reduceIfApplication exps = Left exps

reduceHeadApplication :: [Exp] -> Either [Exp] Exp
reduceHeadApplication (cons_exp : rest)
  = case parseApplyChain $ reduceExp cons_exp of  -- TODO: rework as WHNF instead of full reduction
      [Term (Function FCons), head_exp, _] -> Right head_exp
      _ -> Left (cons_exp : rest)
reduceHeadApplication exps = Left exps

reduceTailApplication :: [Exp] -> Either [Exp] Exp
reduceTailApplication (cons_exp : rest)
  = case parseApplyChain $ reduceExp cons_exp of  -- TODO: rework as WHNF instead of full reduction
      [Term (Function FCons), _, tail_exp] -> Right tail_exp
      _ -> Left (cons_exp : rest)
reduceTailApplication exps = Left exps

reduceYCombApplication :: [Exp] -> Either [Exp] [Exp]
reduceYCombApplication [] = Left []
reduceYCombApplication (arg : rest) = Right (arg : Apply (mkFunction FY) arg : rest)

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
replaceVarWithValInBody _ _ l@(Let _ _) = error $ "Can't replace in let: " ++ show l
replaceVarWithValInBody _ _ l@(Letrec _ _) = error $ "Can't replace in letrec: " ++ show l
replaceVarWithValInBody name val v@(Term (Variable var)) = if varName var == name then val else v
replaceVarWithValInBody _ _ t@(Term _) = t
replaceVarWithValInBody name newExp (Apply e1 e2) = Apply (replaceVarWithValInBody name newExp e1) 
                                                          (replaceVarWithValInBody name newExp e2)
replaceVarWithValInBody name new_exp l@(Lambda v e)
  | v == name = l
  | (name `elem` freeVariables e) && (v `elem` freeVariables new_exp)
      = let (new_v, new_e) = unsafeAlphaConvertRestricted (freeVariables new_exp `union` freeVariables' [v] e) v e
         in Lambda new_v (replaceVarWithValInBody name new_exp new_e)
  | otherwise = Lambda v (replaceVarWithValInBody name new_exp e)

-- | performs an alpha conversion, where the new name is restricted by the existing 
-- | 'taken' or free variables that would result in a conflict if used
unsafeAlphaConvertRestricted :: [String] -> String -> Exp -> (String, Exp)
unsafeAlphaConvertRestricted taken_names var body 
  = let new_name = nextName taken_names var 
     in (new_name, unsafeAlphaConvert var new_name body)

-- freeVariables :: [String] -> Exp -> [String]
-- freeVariables _ (Constant _) = []
-- freeVariables _ (Function _) = []
-- freeVariables bound (Variable var) = [varName var | varName var `notElem` bound]
-- freeVariables bound (Apply e1 e2)  = freeVariables bound e1 ++ freeVariables bound e2
-- freeVariables bound (Lambda v e) = freeVariables (insert v bound) e
 