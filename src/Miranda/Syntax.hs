module Miranda.Syntax 
  ( Prog (..) 
  , Decl (..)
  , AssignDef (..)
  , TypeDef (..)
  , FuncDef (..)
  , DefSpec (..)
  , PattDef (..)
  , RhsClause (..)
  , GenTypeVar
  , Constr
  , ConstrArg (..)
  , Pattern (..)
  , Exp (..)

  -- constructors
  , mkApply
  , mkApplyPatt
  , mkFuncDef
  , mkDefSpec
  , mkPattDef

  -- to enriched helpers
  , funcPatternVars
  , funcPatternToPattern

  -- , ToLambda (..)
  -- , showMarked
  ) where 

import Prettyprinter
import Data.List (intersperse, foldl1', foldl', sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Miranda.PattMatch (PattEq, patternEquationsToEnriched)

import qualified Miranda.Token as T
import Lambda.Pretty
    ( PrettyLambda(..),
      LambdaAnn(AConstant),
      annStr,
      PrettyParenS,
      LambdaDoc,
      tempState,
      setPrec,
      getParenWrapper,
      mkPrettyDocFromParenS' )
import Lambda.Enriched (ToEnriched (..))
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S
import qualified Lambda.Constructor as C

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

-- Type Definition

data TypeDef = TDef String [GenTypeVar] [Constr]
             deriving Show

-- Function Definition

data FuncDef = FDef String DefSpec -- params, clauses, where defs
             deriving Show

data DefSpec = DefSpec [Pattern] Rhs -- a 'specification' for a function (everything but the name)
             deriving Show

data Rhs = Rhs [RhsClause] [AssignDef]
         deriving Show

type FuncDefMap = Map.Map String [DefSpec]

-- Pattern Definition

data PattDef = PDef Pattern Rhs
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

-- subset of Exp that constitutes a valid param
data Pattern = PConstant T.Constant
             | PVariable String 
             | PConstructor String 
             | PApply Pattern Pattern
             | PCons Pattern Pattern  -- the only InfixApp that's valid as an fexp
             | PListLit [Pattern]
             | PTuple [Pattern]
             deriving Show
              
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

--------------------------------------------------------------------------------
-- Constructors

mkApply :: [Exp] -> Exp
mkApply = foldl1' Apply

mkApplyPatt :: [Pattern] -> Pattern
mkApplyPatt = foldl1' PApply

mkFuncDef ::  String -> [Pattern] -> [RhsClause] -> [AssignDef] -> FuncDef
mkFuncDef n ps rcs ads = FDef n $ mkDefSpec ps rcs ads

mkPattDef :: Pattern -> [RhsClause] -> [AssignDef] -> PattDef
mkPattDef p rcs ads = PDef p $ Rhs rcs ads

mkDefSpec :: [Pattern] -> [RhsClause] -> [AssignDef] -> DefSpec
mkDefSpec ps cls defs = DefSpec ps (Rhs cls defs)

--------------------------------------------------------------------------------
-- Accessors

funcPatternVars :: Pattern -> [String]
funcPatternVars (PConstant _) = []
funcPatternVars (PVariable v) = [v]
funcPatternVars (PConstructor _) = []
funcPatternVars (PApply p1 p2) = concatMap funcPatternVars [p1, p2]
funcPatternVars (PCons p1 p2) = concatMap funcPatternVars [p1, p2]
funcPatternVars (PListLit ps) = concatMap funcPatternVars ps
funcPatternVars (PTuple ps) = concatMap funcPatternVars ps


----------
-- Show --
----------

-- instance Show Prog where 
--   show = pShow

-- instance Show Decl where 
--   show = pShow

-- instance Show Exp where 
--   show = pShow

---------------------------
-- ToEnriched (ToLambda) --
---------------------------

-- instance ToLambda Prog where 
--   toLambda = toLambda . toEnriched

instance ToEnriched Prog where 
  toEnriched (Prog defs expr) = 
    E.Letrec (toBindings $ mapMaybe maybeAssignDef defs) (toEnriched expr)

instance ToEnriched Rhs where 
  toEnriched (Rhs clauses []) = clausesToExpression clauses
  toEnriched (Rhs clauses wdefs) = 
    E.Letrec (toBindings wdefs) (clausesToExpression clauses)

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
pattToBinding (PDef param rhs) = 
  let binding_exp = toBindingExp (DefSpec [] rhs)
   in (funcPatternToPattern param, binding_exp)

funcToBinding :: String -> [DefSpec] -> E.LetBinding
funcToBinding fname specs = (E.PVariable fname, defSpecsToEnriched specs)

defSpecsToEnriched :: [DefSpec] -> E.Exp
defSpecsToEnriched = patternEquationsToEnriched . map defSpecToPattEq

defSpecToPattEq :: DefSpec -> PattEq
defSpecToPattEq (DefSpec patts rhs) = 
  let enr_patts = map funcPatternToPattern patts
      enr_rhs = E.toEnriched rhs
   in (enr_patts, enr_rhs)

-- | takes the function def components and creates a binding expression
-- | where the def components are 
-- | params -> rhs clauses -> 'where' definitions -> binding expression
toBindingExp :: DefSpec -> E.Exp
toBindingExp (DefSpec params rhs) = wrapLambda params (toEnriched rhs)

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

-- TODO: doesn't preserver order
groupFuncDefs :: [FuncDef] -> [(String, [DefSpec])]
groupFuncDefs defs = map (\n -> (n, lookupDefsByName n)) def_names
  where 
    insertDef :: FuncDefMap -> FuncDef -> FuncDefMap
    insertDef m (FDef name spec) = 
      Map.insertWith (flip (++)) name [spec] m

    mapped_defs :: Map.Map String [DefSpec]
    mapped_defs = foldl' insertDef Map.empty defs

    def_names :: [String]
    def_names = map (\(FDef n _) -> n) defs
    
    lookupDefsByName :: String -> [DefSpec]
    lookupDefsByName n = 
      case Map.lookup n mapped_defs of
        Nothing -> error $ "function definition name not in map: " ++ n
        Just def -> def

wrapLambda :: [Pattern] -> E.Exp -> E.Exp
wrapLambda ps expr = foldr (E.Lambda . funcPatternToPattern) expr ps

funcPatternToPattern :: Pattern -> E.Pattern
funcPatternToPattern param = 
  case flattenPattern param of 
    [PConstant c]    -> E.PConstant (T.constantToLambda c)
    [PVariable v]    -> E.PVariable v
    [PConstructor c] -> constructorToPattern c
    [PCons p1 p2]    -> E.mkPattConstr (C.fromString "CONS") (map funcPatternToPattern [p1, p2])
    [PListLit ps]    -> funcListLitToPattern ps
    [PTuple tuple]   -> tupleToPattern tuple
    (PConstructor c : rest) -> E.mkPattConstr (C.fromString c) (map funcPatternToPattern rest)
    apply@[PApply _ _] -> error $ "Apply should have been flattened: " ++ show apply
    p -> error $ "Invalid function parameter: " ++ show p

constructorToPattern :: String -> E.Pattern
constructorToPattern "True" = E.PConstant . S.CBool $ True
constructorToPattern "False" = E.PConstant . S.CBool $ False
constructorToPattern constr = E.mkPattConstr (C.fromString constr) []

flattenPattern :: Pattern -> [Pattern]
flattenPattern (PApply p1 p2) = flattenPattern p1 ++ [p2]
flattenPattern param = [param]

funcListLitToPattern :: [Pattern] -> E.Pattern
funcListLitToPattern = foldl' (\cons p -> enrConsPattern (funcPatternToPattern p) cons) enrNilPattern
  where
    enrConsPattern :: E.Pattern -> E.Pattern -> E.Pattern
    enrConsPattern p1 p2 = E.mkPattConstr (C.fromString "CONS") [p1, p2]

    enrNilPattern :: E.Pattern 
    enrNilPattern = E.mkPattConstr (C.fromString "NIL") []

tupleToPattern :: [Pattern] -> E.Pattern
tupleToPattern ps = 
  E.mkPattConstr (C.nTuple (length ps)) (map funcPatternToPattern ps)

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
  prettyDoc' _ (Prog defs expr) = vsep (map prettyDoc defs ++ [prettyDoc expr])

instance PrettyLambda Decl where 
  prettyDoc' = mkPrettyDocFromParenS' sPrettyDef

instance PrettyLambda Pattern where 
  prettyDoc' = mkPrettyDocFromParenS' sPrettyPattern

instance PrettyLambda Exp where 
  prettyDoc' = mkPrettyDocFromParenS' sPrettyExp

sPrettyDef :: Decl -> PrettyParenS LambdaDoc 
sPrettyDef (AssignDef adef) = sPrettyAssignDef adef
sPrettyDef (TypeDef tdef) = sPrettyTypeDef tdef

sPrettyAssignDef :: AssignDef -> PrettyParenS LambdaDoc
sPrettyAssignDef (FuncDef fdef) = sPrettyFuncDef fdef
sPrettyAssignDef (PattDef pdef) = sPrettyPattDef pdef

sPrettyFuncDef :: FuncDef -> PrettyParenS LambdaDoc
sPrettyFuncDef (FDef func_name (DefSpec vars (Rhs body wdefs))) = 
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
sPrettyPattDef (PDef patt (Rhs body wdefs)) =
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

sPrettyPattern :: Pattern -> PrettyParenS LambdaDoc
sPrettyPattern param = do setPrec 11 
                          sPrettyExp . funcPatternToExp $ param

funcPatternToExp :: Pattern -> Exp
funcPatternToExp (PConstant c) = Constant c
funcPatternToExp (PVariable v) = Variable v
funcPatternToExp (PConstructor c) = Constructor c
funcPatternToExp (PApply e1 e2) = Apply (funcPatternToExp e1) (funcPatternToExp e2)
funcPatternToExp (PCons e1 e2) = InfixApp T.ICons (funcPatternToExp e1) (funcPatternToExp e2)
funcPatternToExp (PListLit exprs) = ListLit (map funcPatternToExp exprs)
funcPatternToExp (PTuple exprs) = Tuple (map funcPatternToExp exprs)


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