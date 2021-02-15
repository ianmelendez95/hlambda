module Lambda.Enriched where 

import Prettyprinter

import Lambda.Pretty
import Lambda.Syntax (ToLambda (..))
import qualified Lambda.Syntax as S

data Exp = Let [LetBinding] Exp
         | Letrec [LetBinding] Exp
         | Pure S.Exp
         | Apply Exp Exp 
         | Lambda String Exp
        
type LetBinding = (String, Exp)

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
  toLambda (Lambda var body) = S.Lambda var (toLambda body)
  toLambda (Pure expr) = expr

-- | (letrec v = B in E) = (let v = Y (\v. B) in E) - p42
letrecToLambda ::[LetBinding] -> Exp -> S.Exp
letrecToLambda [] body = toLambda body
letrecToLambda [(var, val)] body = toLambda $ 
  let applyY = Apply (Pure $ S.Function S.FY) 
      new_val = applyY (Lambda var val)
   in Let [(var, new_val)] body  
letrecToLambda _ _ = error "letrec: no support for multiple bindings (yet)"

letToLambda :: [LetBinding] -> Exp -> S.Exp
letToLambda [] body = toLambda body
letToLambda ((var, val):bs) body 
  = let inner = Let bs body
     in S.Apply (S.Lambda var (toLambda inner)) (toLambda val) 

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
sPretty (Lambda var e) = do wrapper <- getParenWrapper 5 
                            ePretty <- tempState (setPrec 0) (sPretty e)
                            pure $ wrapper $ backslash
                                           <> annStr ABoundVar var 
                                           <> dot 
                                           <+> ePretty
sPretty (Apply e e') = do wrapper <- getParenWrapper 10
                          ePretty <- tempState (setPrec 6) (sPretty e)
                          ePretty' <- tempState (setPrec 11) (sPretty e')
                          pure $ wrapper $ ePretty <+> ePretty'

prettyLet :: String -> [LetBinding] -> Exp -> PrettyParenS LambdaDoc
prettyLet let_kw bindings body = pure $ 
  pretty let_kw <+> prettyBindings bindings <+> pretty "in" <+> prettyDoc body

prettyBindings :: [LetBinding] -> LambdaDoc
prettyBindings [] = mempty
prettyBindings ((var, val):bs) = pretty var <+> pretty "=" <+> prettyDoc val <+> prettyBindings bs
