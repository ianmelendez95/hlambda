module Miranda.Syntax 
  ( Prog (..) 
  , Decl (..)
  , AssignDef (..)
  , TypeDef (..)
  , FuncDef (..)
  , PattDef (..)
  , RhsClause (..)
  , GenTypeVar
  , Constr
  , ConstrArg (..)
  , FuncParam (..)
  , Exp (..)
  , funcParamVars
  -- , ToLambda (..)
  -- , showMarked
  ) where 

import Prettyprinter
import Data.List (intersperse, foldl1', foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import qualified Miranda.Token as T
import Lambda.Pretty
    ( PrettyLambda(prettyDoc),
      LambdaAnn(AConstant),
      annStr,
      PrettyParenS,
      LambdaDoc,
      tempState,
      setPrec,
      getParenWrapper,
      mkPrettyDocFromParenS )
import Lambda.Enriched (ToEnriched (..))
import Lambda.Syntax (ToLambda (..))
import Lambda.Reduce (Reducible (..))
import Lambda.Name (newName, nextNames)
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S

----------------------
-- Lambda Expressions --
----------------------

data Prog = Prog [Decl] Exp
          deriving Show

-- p48: Figure 3.3
data Decl = AssignDef AssignDef
          | TypeDef TypeDef
          deriving Show

data AssignDef = FuncDef FuncDef 
               | PattDef PattDef 
               deriving Show

data TypeDef = TDef String [GenTypeVar] [Constr]
             deriving Show

data FuncDef = FDef String [FuncParam] [RhsClause] [AssignDef] -- params, clauses, where defs
             deriving Show

data PattDef = PDef FuncParam [RhsClause] [AssignDef] 
             deriving Show

type GenTypeVar = Int
type Constr = (String, [ConstrArg])

data RhsClause = BaseClause Exp
               | CondClause Exp Exp -- expr, conditional exp  
               deriving Show

data ConstrArg = CAVar String 
               | CAGenTypeVar GenTypeVar
               | CAList [ConstrArg]
               deriving Show

-- subset of Exp that constitutes a valid func param
data FuncParam = FPConstant T.Constant
               | FPVariable String 
               | FPConstructor String 
               | FPApply FuncParam FuncParam
               | FPCons FuncParam FuncParam  -- the only InfixApp that's valid as an fexp
               | FPListLit [FuncParam]
               | FPTuple [FuncParam]
               deriving Show
              
funcParamVars :: FuncParam -> [String]
funcParamVars (FPConstant _) = []
funcParamVars (FPVariable v) = [v]
funcParamVars (FPConstructor _) = []
funcParamVars (FPApply p1 p2) = concatMap funcParamVars [p1, p2]
funcParamVars (FPCons p1 p2) = concatMap funcParamVars [p1, p2]
funcParamVars (FPListLit ps) = concatMap funcParamVars ps
funcParamVars (FPTuple ps) = concatMap funcParamVars ps


data Exp = Constant T.Constant 
         | EGenTypeVar GenTypeVar
         | BuiltinOp ()
         | Variable String 
         | Constructor String
         | Apply Exp Exp 
         | InfixOp T.InfixOp
         | InfixApp T.InfixOp Exp Exp
         | ListLit [Exp]    -- [], [x,y,z], [1,2]
         | Tuple [Exp]      -- (x,y,z), (1,'a',x)
         deriving Show

----------
-- Show --
----------

-- instance Show Prog where 
--   show = pShow

-- instance Show Decl where 
--   show = pShow

-- instance Show Exp where 
--   show = pShow

--------------------------------------------------------------------------------
-- Reducible 

instance Reducible Prog where 
  reduce = reduce . toLambda

---------------------------
-- ToEnriched (ToLambda) --
---------------------------

instance ToLambda Prog where 
  toLambda = toLambda . toEnriched

instance ToEnriched Prog where 
  toEnriched (Prog defs expr) = 
    E.Let (toBindings $ mapMaybe maybeAssignDef defs) (toEnriched expr)

maybeAssignDef :: Decl -> Maybe AssignDef
maybeAssignDef (AssignDef adef) = Just adef
maybeAssignDef _ = Nothing

maybeFuncDef :: AssignDef -> Maybe FuncDef
maybeFuncDef (FuncDef fdef) = Just fdef
maybeFuncDef _ = Nothing

maybePattDef :: AssignDef -> Maybe PattDef
maybePattDef (PattDef pdef) = Just pdef
maybePattDef _ = Nothing

--------------------------------------------------------------------------------
-- Enriching Function Defs

toBindings :: [AssignDef] -> [E.LetBinding]
toBindings defs = 
  let grouped_func_defs = groupFuncDefs (mapMaybe maybeFuncDef defs)
      patt_defs = mapMaybe maybePattDef defs
   in map (uncurry funcToBinding) grouped_func_defs 
        ++ map pattToBinding patt_defs 
      

pattToBinding :: PattDef -> E.LetBinding
pattToBinding (PDef param clauses wdefs) = 
  let binding_exp = toBindingExp [] clauses wdefs
   in (funcParamToPattern param, binding_exp)

funcToBinding :: String -> [FuncSpec] -> E.LetBinding
funcToBinding fname [spec] = 
  if all isVarArg $ fst3 spec
    then (E.PVariable fname, uncurry3 toBindingExp spec)
    else funcToBinding' fname [spec]
  where 
    isVarArg :: FuncParam -> Bool
    isVarArg (FPVariable _) = True
    isVarArg _ = False

funcToBinding fname specs
  | not . allEqual . map (length . fst3) $ specs = error $ "Functions have differing number of arguments: " ++ fname
  | otherwise = funcToBinding' fname specs
  where 
    allEqual :: Eq a => [a] -> Bool
    allEqual [] = True
    allEqual (x:xs) = all (== x) xs 

-- | like funcToBinding, but does not perform preliminary checks, always performs case wrapping
funcToBinding' :: String -> [FuncSpec] -> E.LetBinding
funcToBinding' fname specs = 
  let binding_exprs = map (uncurry3 toBindingExp) specs

      free_vars = concatMap E.freeVariables binding_exprs
      first_arg_name = newName free_vars
      arg_names = take (length . fst3 . head $ specs) $ nextNames free_vars first_arg_name

      lambda_body = foldr (E.FatBar . applyArgsToPatternExpr arg_names)
                          (E.Pure . S.mkConstant $ S.CError) 
                          binding_exprs
      lambda = wrapLambdaArgs arg_names lambda_body

   in (E.PVariable fname, lambda)
  where 
    applyArgsToPatternExpr :: [String] -> E.Exp -> E.Exp
    applyArgsToPatternExpr args expr = 
      foldl' (\apply arg -> E.Apply apply (E.Pure . S.mkVariable $ arg)) expr args

    wrapLambdaArgs :: [String] -> E.Exp -> E.Exp
    wrapLambdaArgs args body = foldr (E.Lambda . E.PVariable) body args

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z


-- | takes the function def components and creates a binding expression
-- | where the def components are 
-- | params -> rhs clauses -> 'where' definitions -> binding expression
toBindingExp :: [FuncParam] -> [RhsClause] -> [AssignDef] -> E.Exp
toBindingExp params clauses [] = wrapLambda params (clausesToExpression clauses)
toBindingExp params clauses wdefs = 
  let wbindings = toBindings wdefs
   in wrapLambda params (E.Let wbindings (clausesToExpression clauses))

clausesToExpression :: [RhsClause]  -> E.Exp
clausesToExpression [] = E.Pure . S.mkConstant $ S.CFail
clausesToExpression cs@(BaseClause expr : rest) = 
  case rest of 
    [] -> toEnriched expr
    _  -> error $ "Base clause expected at end: " ++ show cs
clausesToExpression [CondClause body (Constant (T.CBool True))] = toEnriched body
clausesToExpression (CondClause body cond : rest) = 
  let if_cond = E.Apply (E.Pure . S.mkFunction $ S.FIf) (toEnriched cond)
      if_cond_then_body = E.Apply if_cond (toEnriched body)
      if_cond_then_body_else_rest = E.Apply if_cond_then_body (clausesToExpression rest)
  in if_cond_then_body_else_rest

type FuncSpec = ([FuncParam], [RhsClause], [AssignDef]) -- a 'specification' for a function (everything but the name)
type FuncDefMap = Map.Map String [FuncSpec]

groupFuncDefs :: [FuncDef] -> [(String, [FuncSpec])]
groupFuncDefs = Map.toList . foldl' insertDef Map.empty
  where 
    insertDef :: FuncDefMap -> FuncDef -> FuncDefMap
    insertDef m (FDef name pars rhs wdefs) = Map.insertWith (flip (++)) name [(pars, rhs, wdefs)] m

wrapLambda :: [FuncParam] -> E.Exp -> E.Exp
wrapLambda ps expr = foldr (E.Lambda . funcParamToPattern) expr ps

funcParamToPattern :: FuncParam -> E.Pattern
funcParamToPattern param = 
  case flattenFuncParam param of 
    [FPConstant c]    -> E.PConstant (T.constantToLambda c)
    [FPVariable v]    -> E.PVariable v
    [FPConstructor c] -> constructorToPattern c
    [FPCons p1 p2]    -> E.PConstructor "CONS" (map funcParamToPattern [p1, p2])
    [FPListLit ps]    -> funcListLitToPattern ps
    [FPTuple tuple]   -> tupleToPattern tuple
    (FPConstructor c : rest) -> E.PConstructor c (map funcParamToPattern rest)
    apply@[FPApply _ _] -> error $ "Apply should have been flattened: " ++ show apply
    p -> error $ "Invalid function parameter: " ++ show p

constructorToPattern :: String -> E.Pattern
constructorToPattern "True" = E.PConstant . S.CBool $ True
constructorToPattern "False" = E.PConstant . S.CBool $ False
constructorToPattern constr = E.PConstructor constr []

flattenFuncParam :: FuncParam -> [FuncParam]
flattenFuncParam (FPApply p1 p2) = flattenFuncParam p1 ++ [p2]
flattenFuncParam param = [param]

funcListLitToPattern :: [FuncParam] -> E.Pattern
funcListLitToPattern = foldl' (\cons p -> enrConsPattern (funcParamToPattern p) cons) enrNilPattern
  where
    enrConsPattern :: E.Pattern -> E.Pattern -> E.Pattern
    enrConsPattern p1 p2 = E.PConstructor "CONS" [p1, p2]

    enrNilPattern :: E.Pattern 
    enrNilPattern = E.PConstructor "NIL" []

tupleToPattern :: [FuncParam] -> E.Pattern
tupleToPattern ps = E.PConstructor (tupleToConstructor ps) (map funcParamToPattern ps)
  where 
    tupleToConstructor :: [FuncParam] -> E.Constructor
    tupleToConstructor ps' = 
      case length ps' of 
        0 -> error "0 length tuple"
        1 -> error "1 length tuple"
        2 -> "PAIR"
        3 -> "TRIPLE"
        4 -> "QUADRUPLE"
        n -> "TUPLE-" ++ show n

instance ToEnriched RhsClause where 
  toEnriched (BaseClause expr) = toEnriched expr
  toEnriched clause@(CondClause _ _) = error $ "Can't translate conditional def: " ++ show clause

instance ToEnriched Exp where 
  toEnriched (Constant c)          = toEnriched c
  toEnriched (BuiltinOp _)         = undefined
  toEnriched (Variable x)          = E.Pure (S.mkVariable x)
  toEnriched (Constructor c)       = constructorToEnriched c
  toEnriched (Apply e1 e2)         = E.Apply (toEnriched e1) (toEnriched e2)
  toEnriched (InfixApp op e1 e2)   = E.Apply (E.Apply (toEnriched op) (toEnriched e1)) (toEnriched e2)
  toEnriched (ListLit exps)        = enrichedListLit exps
  toEnriched (Tuple exps)          = enrichedTuple exps
  toEnriched (InfixOp op)          = toEnriched op
  toEnriched (EGenTypeVar v)       = error $ "Can't enrich type variable: " ++ show v

constructorToEnriched :: String -> E.Exp
constructorToEnriched "True" = E.Pure . S.mkConstant . S.CBool $ True
constructorToEnriched "False" = E.Pure . S.mkConstant . S.CBool $ False
constructorToEnriched constr = E.Pure . S.mkVariable $ constr

enrichedListLit :: [Exp] -> E.Exp
enrichedListLit [] = enrNil
enrichedListLit exprs = consEnrichedExprs (map toEnriched exprs ++ [enrNil])

consEnrichedExprs :: [E.Exp] -> E.Exp
consEnrichedExprs = foldr1 (E.Apply . E.Apply enrCons)

enrichedTuple :: [Exp] -> E.Exp
enrichedTuple exps = foldl1' E.Apply (enr_tuple_f : map toEnriched exps) 
  where 
    enr_tuple_f = E.Pure (S.mkFunction (S.FTuple (length exps)))

-- | 'enr'iched nil and cons
enrNil, enrCons :: E.Exp
enrNil  = E.Pure (S.mkConstant S.CNil)
enrCons = E.Pure (S.mkFunction S.FCons)

------------------
-- Pretty Print --
------------------

instance PrettyLambda Prog where 
  prettyDoc (Prog defs expr) = vsep (map prettyDoc defs ++ [prettyDoc expr])

instance PrettyLambda Decl where 
  prettyDoc = mkPrettyDocFromParenS sPrettyDef

instance PrettyLambda FuncParam where 
  prettyDoc = mkPrettyDocFromParenS sPrettyPattern

instance PrettyLambda Exp where 
  prettyDoc = mkPrettyDocFromParenS sPrettyExp

sPrettyDef :: Decl -> PrettyParenS LambdaDoc 
sPrettyDef (AssignDef adef) = sPrettyAssignDef adef
sPrettyDef (TypeDef tdef) = sPrettyTypeDef tdef

sPrettyAssignDef :: AssignDef -> PrettyParenS LambdaDoc
sPrettyAssignDef (FuncDef fdef) = sPrettyFuncDef fdef
sPrettyAssignDef (PattDef pdef) = sPrettyPattDef pdef

sPrettyFuncDef :: FuncDef -> PrettyParenS LambdaDoc
sPrettyFuncDef (FDef func_name vars body wdefs) = 
  do let pname = pretty func_name
         prepend_pvars = if null vars then (mempty <>) else ((hsep . map prettyDoc $ vars) <+>)
         pbody = align . vsep $ map ((pretty "=" <+>) . sPrettyClause) body

     pwdefs <- mapM sPrettyAssignDef wdefs -- pretty where defs

     let pwsection = hang 2 . vsep $ [pretty "where", align . vsep $ pwdefs]

     if null wdefs 
       then pure (pname <+> prepend_pvars pbody)
       else pure (hang 2 . vsep $ [pname <+> prepend_pvars pbody, pwsection])

sPrettyTypeDef :: TypeDef -> PrettyParenS LambdaDoc
sPrettyTypeDef (TDef type_name type_vars constrs) = 
  do pConstrs <- tempState (setPrec 0) (mapM prettyConstructor constrs)
     pure $ prettyLHS type_name type_vars 
              <+> pretty "::=" 
              <+> hsep (intersperse (pretty "|") pConstrs)
  where 
    prettyLHS :: String -> [GenTypeVar] -> LambdaDoc
    prettyLHS name vars = 
      if null vars then pretty name 
                   else pretty name <+> hsep (map prettyTypeVar type_vars)

sPrettyPattDef :: PattDef -> PrettyParenS LambdaDoc
sPrettyPattDef (PDef patt body wdefs) =
  do ppatt <- sPrettyPattern patt
     pwdefs <- mapM sPrettyAssignDef wdefs -- pretty where defs

     let pbody = align . vsep $ map ((pretty "=" <+>) . sPrettyClause) body
         pwsection = hang 2 . vsep $ [pretty "where", align . vsep $ pwdefs]

     if null wdefs 
       then pure (ppatt <+> pbody)
       else pure (hang 2 . vsep $ [ppatt <+> pbody, pwsection])

sPrettyClause :: RhsClause -> LambdaDoc
sPrettyClause (BaseClause expr) = prettyDoc expr
sPrettyClause (CondClause expr cond_expr) = prettyDoc expr <> comma <+> prettyDoc cond_expr

sPrettyPattern :: FuncParam -> PrettyParenS LambdaDoc
sPrettyPattern param = do setPrec 11 
                          sPrettyExp . funcParamToExp $ param

funcParamToExp :: FuncParam -> Exp
funcParamToExp (FPConstant c) = Constant c
funcParamToExp (FPVariable v) = Variable v
funcParamToExp (FPConstructor c) = Constructor c
funcParamToExp (FPApply e1 e2) = Apply (funcParamToExp e1) (funcParamToExp e2)
funcParamToExp (FPCons e1 e2) = InfixApp T.ICons (funcParamToExp e1) (funcParamToExp e2)
funcParamToExp (FPListLit exprs) = ListLit (map funcParamToExp exprs)
funcParamToExp (FPTuple exprs) = Tuple (map funcParamToExp exprs)


prettyConstructor :: Constr -> PrettyParenS LambdaDoc
prettyConstructor (name, vars) = do wrapper <- if null vars then pure id 
                                                            else getParenWrapper 10 
                                    pvars <- tempState (setPrec 11) (mapM prettyConstrArg vars)
                                    pure . wrapper $ hsep (pretty name : pvars)

prettyConstrArg :: ConstrArg -> PrettyParenS LambdaDoc
prettyConstrArg (CAVar v) = pure $ pretty v
prettyConstrArg (CAGenTypeVar gt) = pure $ prettyTypeVar gt
prettyConstrArg (CAList arg_list) = do wrapper <- getParenWrapper 10 
                                       pargs <- mapM prettyConstrArg arg_list
                                       pure . wrapper $ hsep pargs

prettyTypeVar :: GenTypeVar -> LambdaDoc
prettyTypeVar = pretty . (`replicate` '*')

sPrettyExp :: Exp -> PrettyParenS LambdaDoc
sPrettyExp (Constant c)    = pure $ annStr AConstant (show c)
sPrettyExp (BuiltinOp _)   = pure . pretty $ "I don't exist wtf"
sPrettyExp (Variable v)    = pure $ pretty v
sPrettyExp (Constructor c) = pure $ pretty c
sPrettyExp (InfixOp infx)  = pure $ prettyDoc infx
sPrettyExp (EGenTypeVar v) = pure $ pretty $ replicate v '*'
sPrettyExp (Tuple exprs)   = do pexprs <- mapM sPrettyExp exprs
                                pure $ parens $ hcat $ intersperse comma pexprs 
sPrettyExp (ListLit exp_list) = 
  do pexp_list <- mapM sPrettyExp exp_list
     return (lbracket <> hcat (intersperse comma pexp_list) <> rbracket)
sPrettyExp (InfixApp infx e1 e2) = do let infix_prec = infixPrec infx
                                      wrapper <- getParenWrapper infix_prec
                                      ep1 <- tempState (setPrec infix_prec) (sPrettyExp e1)
                                      ep2 <- tempState (setPrec infix_prec) (sPrettyExp e2)
                                      pure $ wrapper $ ep1 <+> prettyDoc infx <+> ep2
sPrettyExp (Apply e1 e2) = do wrapper <- getParenWrapper funcAppPrec
                              ep1 <- tempState (setPrec (funcAppPrec - 1)) (sPrettyExp e1)
                              ep2 <- tempState (setPrec (funcAppPrec + 1)) (sPrettyExp e2)
                              pure $ wrapper $ ep1 <+> ep2 

-- function application precedence: https://wuciawe.github.io/functional%20programming/haskell/2016/07/03/infix-functions-in-haskell.html#:~:text=In%20Haskell%20the%20precedence%20of,the%20application%20in%20right%20order.
funcAppPrec :: Int
funcAppPrec = 10

infixPrec :: T.InfixOp  -> Int 
infixPrec T.IPlus    = 6
infixPrec T.IMinus   = 6
infixPrec T.IMult    = 7
infixPrec T.IDiv     = 7
infixPrec T.ICons    = 5
infixPrec T.IEq      = 4
infixPrec T.ILt      = 4
infixPrec T.IGt      = 4
infixPrec (T.IVar _) = 10