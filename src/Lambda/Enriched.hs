module Lambda.Enriched
 ( Exp (..)
 , LetBinding
 , CaseClause (..)
 , Pattern (..)

 , ToEnriched (..)

 , mkApply
 , mkPattConstr
 , mkLambda
 , mkIf

 , mapCaseExpr
 , fromPattern
 , freeVariables
 , boundVarsInPattern
 ) where 

import Prettyprinter
import Data.List (foldl', foldl1', insert, nub)
import Lambda.Pretty
import qualified Lambda.Syntax as S

import Lambda.Constructor

-- import Debug.Trace

-- p40: Figure 3.2 - Syntax of Enriched Lambda Expressions

data Exp = Let LetBinding Exp
         | Letrec [LetBinding] Exp
         | Pure S.Exp
         | Apply Exp Exp 
         | Lambda Pattern Exp
         | FatBar Exp Exp

         -- TODO: carries various implicit invariants, see Lambda.PattMatch
         | Case S.Variable [CaseClause] 
        
type LetBinding = (Pattern, Exp)
data CaseClause = Clause Constructor [S.Variable] Exp
                deriving Show

data Pattern = PConstant S.Constant
             | PVariable S.Variable
             | PConstructor Constructor [Pattern]
             deriving Show

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

--------------------------------------------------------------------------------
-- Mutators

mapCaseExpr :: (Exp -> Exp) -> CaseClause -> CaseClause
mapCaseExpr f (Clause c ps e) = Clause c ps (f e)

----------------
-- ToEnriched --
----------------

class ToEnriched a where 
  toEnriched :: a -> Exp

----------------------
-- Enriched -> Pure --
----------------------

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
sPretty (Let binding body)    = prettyLet "let" [binding] body
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
         pClause (Clause c vs expr) = 
           do ppatt <- sPrettyPattern (PConstructor c (map PVariable vs))
              setPrec 10
              pexpr <- sPretty expr
              pure $ ppatt <+> pretty "=>" <+> pexpr
     pclauses <- mapM pClause clauses
     pure . wrapper $ (hang 2 . vsep $ (pretty "case" <+> pvar <+> pretty "of" : pclauses))

prettyLet :: String -> [LetBinding] -> Exp -> PrettyParenS LambdaDoc
prettyLet let_kw bindings body = 
  do wrapper <- getParenWrapper 10
     pure $ wrapper $ align . vsep $ [pretty let_kw <+> (align . vsep $ map prettyBinding bindings), 
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
freeVariables' bound (Let bind expr) = freeVarsInLet bound [bind] expr
freeVariables' bound (Letrec binds expr) = freeVarsInLet bound binds expr
freeVariables' bound (Pure expr) = S.freeVariables' bound expr
freeVariables' bound (Apply e1 e2) = concatMap (freeVariables' bound) [e1, e2]
freeVariables' bound (FatBar e1 e2) = concatMap (freeVariables' bound) [e1, e2]
freeVariables' bound (Case var clauses) = 
  let inClause (Clause _ vs expr) = 
        freeVariables' (insertAll vs bound) expr 
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