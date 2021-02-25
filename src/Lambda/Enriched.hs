module Lambda.Enriched where 

import Prettyprinter
import Data.List (foldl', insert, nub)

import Lambda.Pretty
import Lambda.Syntax (ToLambda (..))
import qualified Lambda.Syntax as S
import Lambda.Name (newName)

-- p40: Figure 3.2 - Syntax of Enriched Lambda Expressions

data Exp = Let [LetBinding] Exp
         | Letrec [LetBinding] Exp
         | Pure S.Exp
         | Apply Exp Exp 
         | Lambda Pattern Exp
         | FatBar Exp Exp
        
type LetBinding = (Pattern, Exp)
type Constructor = String

data Pattern = PConstant S.Constant
             | PVariable S.Variable
             | PConstructor Constructor [Pattern]
             deriving Show

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
  -- <e1> | <e2>
  -- (\new_name. IF (/= new_name FAIL) new_name <e2>) <e1>
  toLambda (FatBar e1 e2) = 
    let new_name = newName (nub $ concatMap freeVariables [e1, e2])
        new_var = S.mkVariable new_name

        -- (/= new_name FAIL)
        neq_fail = S.mkApply [S.mkFunction S.FNEq, new_var, S.mkConstant S.CFail]
        
        -- IF (/= new_name FAIL) new_name <e2>
        if_neq_fail_else_e2 = S.mkIf neq_fail new_var (toLambda e2)

        -- (\new_name. IF (/= new_name FAIL) new_name <e2>)
        new_name_lambda = S.Lambda new_name if_neq_fail_else_e2

        -- (\new_name. IF (/= new_name FAIL) new_name <e2>) <e1>
     in S.Apply new_name_lambda (toLambda e1)

toLambdaLambda :: Pattern -> Exp -> S.Exp
toLambdaLambda (PVariable v) body = S.Lambda v (toLambda body)
-- (\k. E) C => (\a. IF (= k a) E FAIL) C
toLambdaLambda (PConstant k) e = 
  let new_name = newName (freeVariables e)
      if_cond = S.mkApply [S.mkFunction S.FEq, S.mkConstant k, S.mkVariable new_name]
   in S.Lambda new_name (S.mkIf if_cond (toLambda e) (S.mkConstant S.CFail)) 
toLambdaLambda c@(PConstructor _ _) _ = error $ "No support for constructors to lambda yet: " ++ show c

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
sPrettyPattern (PConstructor c ps) = do setPrec 11
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
freeVariables' bound (Lambda patt expr) = 
  freeVariables' (insertAll (boundVarsInPattern patt) bound) expr
  where 
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