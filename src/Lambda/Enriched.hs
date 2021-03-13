module Lambda.Enriched where 

import Prettyprinter
import Data.List (foldl', foldl1', insert, nub)
import Data.Foldable (foldr')

import Lambda.Pretty
import Lambda.Syntax (ToLambda (..))
import qualified Lambda.Syntax as S

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
type Constructor = String

data Pattern = PConstant S.Constant
             | PVariable S.Variable
             | PConstructor Constructor [Pattern]
             deriving Show

--------------------------------------------------------------------------------
-- Constructors

mkPattConstr :: Constructor -> [Pattern] -> Pattern
mkPattConstr = PConstructor

mkLambda :: [Pattern] -> Exp -> Exp
mkLambda ps e = foldr' Lambda e ps

mkApply :: [Exp] -> Exp
mkApply = foldl1' Apply

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
toLambdaLambda patt body = error $ "No support for reducing pattern lambdas: " ++ show patt ++ ", " ++ show body

-- | (letrec v = B in E) = (let v = Y (\v. B) in E) - p42
letrecToLambda ::[LetBinding] -> Exp -> S.Exp
letrecToLambda [] body = toLambda body
letrecToLambda [(var, val)] body = toLambda $ 
  let applyY = Apply (Pure $ S.mkFunction S.FY) 
      new_val = applyY (Lambda var val)
   in Let [(var, new_val)] body  
letrecToLambda _ _ = error "letrec: no support for multiple bindings (yet)"

letToLambda :: [LetBinding] -> Exp -> S.Exp
letToLambda [] body = toLambda body
letToLambda ((PVariable var, val):bs) body 
  = let inner = Let bs body
     in S.Apply (S.Lambda var (toLambda inner)) (toLambda val) 
letToLambda ((pat, _):_) _ = error $ "letToLambda: no support for pattern: " ++ show pat

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
  prettyDoc = mkPrettyDocFromParenS sPretty

sPretty :: Exp -> PrettyParenS LambdaDoc
sPretty (Pure expr) = pure $ prettyDoc expr
sPretty (Letrec bindings body) = prettyLet "letrec" bindings body
sPretty (Let bindings body)    = prettyLet "let" bindings body
sPretty (FatBar e1 e2) = do p1 <- sPretty e1
                            p2 <- sPretty e2
                            return $ p1 <+> pipe <+> p2
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
  align . vsep $ [pretty let_kw <+> (align . vsep $ map prettyBinding bindings), pretty "in" <+> prettyDoc body]

prettyBinding :: LetBinding -> LambdaDoc
prettyBinding (pat, val) = prettyDoc pat <+> pretty "=" <+> prettyDoc val

instance PrettyLambda Pattern where 
  prettyDoc = mkPrettyDocFromParenS sPrettyPattern

sPrettyPattern :: Pattern -> PrettyParenS LambdaDoc
sPrettyPattern (PConstant c) = pure . prettyDoc $ S.mkConstant c
sPrettyPattern (PVariable v) = pure . pretty $ v
sPrettyPattern (PConstructor c ps) = 
  do setPrec 11
     pretty_args <- mapM sPrettyPattern ps
     return $ hsep (pretty c : pretty_args)

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
  let binds_free = foldr (\(_, e) vars -> freeVariables' bound e ++ vars) [] binds
   in freeVariables' bound expr ++ binds_free