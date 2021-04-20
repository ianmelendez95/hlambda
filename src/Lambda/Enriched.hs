{-# LANGUAGE TupleSections #-}
module Lambda.Enriched where 

import Prettyprinter
import Data.List (foldl', foldl1', insert, nub)

import Lambda.Pretty
import Lambda.Syntax (ToLambda (..))
import qualified Lambda.Syntax as S

import Lambda.Constructor
import qualified Lambda.Constructor as C

import Common.Name (newName)

import Debug.Trace

-- p40: Figure 3.2 - Syntax of Enriched Lambda Expressions

data Exp = Let [LetBinding] Exp
         | Letrec [LetBinding] Exp
         | Pure S.Exp
         | Apply Exp Exp 
         | Lambda Pattern Exp
         | FatBar Exp Exp
         | Case S.Variable [CaseClause]
        
type LetBinding = (Pattern, Exp)
type CaseClause = (Pattern, Exp)

data Pattern = PConstant S.Constant
             | PVariable S.Variable
             | PConstructor Constructor [Pattern]
             deriving Show

-- TODO seems like a case for GADTs (parameterize over whether it's irrefutable)
data IrrefutablePattern = IPVariable S.Variable
                        | IPConstructor Constructor [IrrefutablePattern]

data ConstructorType = CTSum Int Int -- tag arity
                     | CTProduct Int -- arity

--------------------------------------------------------------------------------
-- Constructors

mkPattConstr :: Constructor -> [Pattern] -> Pattern
mkPattConstr = PConstructor

mkLambda :: [Pattern] -> Exp -> Exp
mkLambda ps e = foldr Lambda e ps

mkApply :: [Exp] -> Exp
mkApply = foldl1' Apply

mkVariable :: S.Variable -> Exp
mkVariable = Pure . S.mkVariable

mkConstant :: S.Constant -> Exp
mkConstant = Pure . S.mkConstant

mkIf :: Exp -> Exp -> Exp -> Exp
mkIf cond true_clause false_clause = 
  mkApply [mkFunction S.FIf, cond, true_clause, false_clause]

mkFunction :: S.Function -> Exp
mkFunction = Pure . S.mkFunction

fromPattern :: Pattern -> Exp
fromPattern (PVariable v) = mkVariable v
fromPattern (PConstant c) = mkConstant c
fromPattern (PConstructor c ps) = mkApply (mkVariable (show c) : map fromPattern ps) 

----------------
-- ToEnriched --
----------------

class ToEnriched a where 
  toEnriched :: a -> Exp

--------------
-- ToLambda --
--------------

instance ToLambda Exp where
  toLambda (Letrec bindings body) = letrecToLambda bindings body
  toLambda (Let bindings body) = letToLambda bindings body
  toLambda (Apply e1 e2) = S.Apply (toLambda e1) (toLambda e2)
  toLambda (Lambda pat body) = toLambdaLambda pat body
  toLambda (Pure expr) = expr
  toLambda cas@(Case _ _) = error $ "No support for reducing case: " ++ show cas
  toLambda fb@(FatBar _ _) = error $ "No support for reducing fat bar: " ++ show fb

toLambdaLambda :: Pattern -> Exp -> S.Exp
toLambdaLambda (PVariable v) body = S.Lambda v (toLambda body)
toLambdaLambda (PConstant c) body = 
  let new_name = newName $ freeVariables body
   in S.Lambda new_name (S.mkIf (S.mkApply [S.mkFunction S.FEq, 
                                            S.mkConstant c,
                                            S.mkVariable new_name])
                                (toLambda body)
                                (S.mkConstant S.CFail))
toLambdaLambda (PConstructor c ps) body = toLambdaConstructorLambda c ps body

--------------------------------------------------------------------------------
-- toLambda constructor patterns

toLambdaConstructorLambda :: Constructor -> [Pattern] -> Exp -> S.Exp
toLambdaConstructorLambda c ps body =
  S.mkApply [S.mkVariable (unpackStr c),
             toLambda (mkLambda ps body)]

--------------------------------------------------------------------------------
-- toLambda let bindings

-- | (letrec v = B in E) = (let v = Y (\v. B) in E) - p42
letrecToLambda ::[LetBinding] -> Exp -> S.Exp
letrecToLambda [] body = toLambda body
letrecToLambda [(PConstructor constr ps, val)] body = 
  let (new_name, bindings, body') = letLetrecProductConstructorToBindings constr ps [] body
   in S.Letrec ((new_name, toLambda val) : bindings) body'
letrecToLambda [(var, val)] body = toLambda $ 
  let applyY = Apply (Pure $ S.mkFunction S.FY) 
      new_val = applyY (Lambda var val)
   in Let [(var, new_val)] body  
letrecToLambda bs body = 
  case traverse (\(p, v) -> (,v) <$> patternToIrrefutable p) bs of 
    Nothing -> error $ "letrec: no support for non-irrefutable patterns: " ++ show bs ++ " " ++ show body
    Just irr_bs -> 
      let constr = nTuple $ length irr_bs
          (irr_ps, vs) = foldr (\(p, v) (ps', vs') -> (p : ps', v : vs')) ([],[]) irr_bs
          let_p = PConstructor constr (map irrefutableToPattern irr_ps) 
          let_v = Pure $ S.mkApply (S.mkVariable (show constr) : map toLambda vs)
       in letToLambda [(let_p, Apply (Pure $ S.mkFunction S.FY) (Lambda let_p let_v))] body

-- | p110
patternToIrrefutable :: Pattern -> Maybe IrrefutablePattern
patternToIrrefutable (PVariable v) = Just $ IPVariable v
patternToIrrefutable (PConstructor c ps) = IPConstructor c <$> traverse patternToIrrefutable ps
patternToIrrefutable _ = Nothing

irrefutableToPattern :: IrrefutablePattern -> Pattern
irrefutableToPattern (IPVariable v) = PVariable v
irrefutableToPattern (IPConstructor c ps) = PConstructor c (map irrefutableToPattern ps)

letToLambda :: [LetBinding] -> Exp -> S.Exp
letToLambda [] body = toLambda body
letToLambda ((PVariable var, val):bs) body 
  = let inner = Let bs body
     in S.Apply (S.Lambda var (toLambda inner)) (toLambda val) 
letToLambda ((PConstructor constr ps, val) : bs) body = 
  let (new_name, bindings, body') = letLetrecProductConstructorToBindings constr ps bs body
   in S.Let [(new_name, toLambda val)] (S.Let bindings body')
letToLambda ((pat, _):_) _ = error $ "letToLambda: no support for pattern: " ++ show pat

letLetrecConstructorToBindings :: Constructor 
                               -> [Pattern] 
                               -> [LetBinding] 
                               -> Exp 
                               -> (String, [(String, S.Exp)], S.Exp) -- new_name, arg bindings, let body 
letLetrecConstructorToBindings constr constr_args rest_let_bindings let_body 
  | isProduct constr = letLetrecProductConstructorToBindings constr constr_args rest_let_bindings let_body
  | otherwise =  -- handle sum
    let arg_tuple_constr = C.nTuple (C.arity constr)
        coercion_func = Lambda (PConstructor (C.fromString "CONS") [PVariable "y", PVariable "ys"])
                               (mkApply [Pure $ S.mkVariable $ show arg_tuple_constr, 
                                         Pure $ S.mkVariable "y",
                                         Pure $ S.mkVariable "ys"])
        coercion_appl = FatBar (Apply coercion_func undefined)
     in undefined 

-- | pattern constructor
-- | -> constructor args
-- | -> rest of let bindings
-- | -> let body
-- | -> (new name, arg bindings, transformed let body)
letLetrecProductConstructorToBindings :: Constructor 
                                      -> [Pattern] 
                                      -> [LetBinding] 
                                      -> Exp 
                                      -> (String, [(String, S.Exp)], S.Exp) -- new_name, arg bindings, let body
letLetrecProductConstructorToBindings constr constr_args rest_let_bindings let_body =
  if isSum constr
    then error $ "Only support irrefutable product constructors (is sum constructor): " ++ show constr
    else case maybeAllVarPatterns constr_args of 
           Nothing -> error $ "Only support irrefutable product constructors (args not all variables): " ++ show constr
           Just vs -> 
             let body' = letToLambda rest_let_bindings let_body
                 new_name = newName (S.freeVariables body')
                 bindings = constrArgBindings new_name constr vs
              in (new_name, bindings, body')

constrArgBindings :: String -> Constructor -> [String] -> [(String, S.Exp)]
constrArgBindings new_name constr constr_args = 
  let sel_exprs = 
        map (\sel -> S.Apply (S.mkVariable sel) (S.mkVariable new_name))
            (selectFunctions constr)
   in if length sel_exprs /= length constr_args
        then error $ "Constructor arity does not match number of arguments: " ++ show constr ++ " " ++ show constr_args
        else zip constr_args sel_exprs

maybeAllVarPatterns :: [Pattern] -> Maybe [String]
maybeAllVarPatterns = traverse maybeVarPattern

maybeVarPattern :: Pattern -> Maybe String 
maybeVarPattern (PVariable v) = Just v
maybeVarPattern _ = Nothing

----------------------
-- Enriched -> Pure --
----------------------

enrichedToLambda :: Exp -> S.Exp
enrichedToLambda = toLambda

instance Show Exp where 
  show = pShow

------------------
-- Pretty Print --
------------------

instance PrettyLambda Exp where 
  prettyDoc' = mkPrettyDocFromParenS' sPretty

sPretty :: Exp -> PrettyParenS LambdaDoc
sPretty (Pure expr) = 
  do curPrec <- getPrec
     pure $ prettyDoc' curPrec expr
sPretty (Letrec bindings body) = prettyLet "letrec" bindings body
sPretty (Let bindings body)    = prettyLet "let" bindings body
sPretty (FatBar e1 e2) = do p1 <- sPretty e1
                            p2 <- sPretty e2
                            return $ align . vsep $ [p1, pipe <+> p2]
                            -- return $ p1 <+> 
sPretty (Lambda patt e) = 
  do wrapper <- getParenWrapper 5 
     e_pretty <- tempState (setPrec 0) (sPretty e)
     pure $ wrapper $ backslash
                   <> prettyDoc patt
                   <> dot 
                   <+> e_pretty
sPretty (Apply e e') = do wrapper <- getParenWrapper 10
                          ePretty <- tempState (setPrec 6) (sPretty e)
                          ePretty' <- tempState (setPrec 11) (sPretty e')
                          pure $ wrapper $ ePretty <+> ePretty'
sPretty (Case var clauses) = 
  do wrapper <- getParenWrapper 10
     let pvar = pretty var
         pClause (patt, expr) = 
           do ppatt <- sPrettyPattern patt
              setPrec 10
              pexpr <- sPretty expr
              pure $ ppatt <+> pretty "=>" <+> pexpr
     pclauses <- mapM pClause clauses
     pure . wrapper $ (hang 2 . vsep $ (pretty "case" <+> pvar <+> pretty "of" : pclauses))

prettyLet :: String -> [LetBinding] -> Exp -> PrettyParenS LambdaDoc
prettyLet let_kw bindings body = pure $ 
  align . vsep $ [pretty let_kw <+> (align . vsep $ map prettyBinding bindings), 
                  pretty "in" <+> prettyDoc body]

prettyBinding :: LetBinding -> LambdaDoc
prettyBinding (pat, val) = prettyDoc' 11 pat <+> pretty "=" <+> prettyDoc val

instance PrettyLambda Pattern where 
  prettyDoc' = mkPrettyDocFromParenS' sPrettyPattern

sPrettyPattern :: Pattern -> PrettyParenS LambdaDoc
sPrettyPattern (PConstant c) = pure . prettyDoc $ S.mkConstant c
sPrettyPattern (PVariable v) = pure . pretty $ v
sPrettyPattern (PConstructor c ps) = 
  do wrapper <- getParenWrapper 10
     pretty_args <- mapM sPrettyPattern ps
     return $ wrapper $ hsep (prettyDoc c : pretty_args)

--------------------------------------------------------------------------------
-- Free Variables 

freeVariables :: Exp -> [String]
freeVariables = nub . freeVariables' []

freeVariables' :: [String] -> Exp -> [String]
freeVariables' bound (Let binds expr) = freeVarsInLet bound binds expr
freeVariables' bound (Letrec binds expr) = freeVarsInLet bound binds expr
freeVariables' bound (Pure expr) = S.freeVariables' bound expr
freeVariables' bound (Apply e1 e2) = concatMap (freeVariables' bound) [e1, e2]
freeVariables' bound (FatBar e1 e2) = concatMap (freeVariables' bound) [e1, e2]
freeVariables' bound (Case var clauses) = 
  let inClause (patt, expr) = 
        freeVariables' (insertAll (boundVarsInPattern patt) bound) expr 
      in_clauses = concatMap inClause clauses
   in if var `elem` bound
        then in_clauses
        else var : in_clauses
freeVariables' bound (Lambda patt expr) = 
  freeVariables' (insertAll (boundVarsInPattern patt) bound) expr

insertAll :: [String] -> [String] -> [String]
insertAll ls set = foldl' (flip insert) set ls

boundVarsInPattern :: Pattern -> [String]
boundVarsInPattern (PConstant _) = []
boundVarsInPattern (PVariable v) = [v]
boundVarsInPattern (PConstructor _ args) = concatMap boundVarsInPattern args

freeVarsInLet :: [String] -> [LetBinding] -> Exp -> [String]
freeVarsInLet bound binds expr = 
  let binds_free = foldr (\(bound_patts, e) vars -> 
                             freeVariables' (boundVarsInPattern bound_patts ++ bound) e ++ vars) 
                         [] binds
   in freeVariables' bound expr ++ binds_free