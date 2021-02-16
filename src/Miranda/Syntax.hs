module Miranda.Syntax 
  ( Prog (..) 
  , Def (..)
  , Exp (..)
  -- , ToLambda (..)
  -- , showMarked
  ) where 

import Prettyprinter

import qualified Miranda.Token as T
import Lambda.Pretty
    ( PrettyLambda(prettyDoc, pShow),
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
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S

----------------------
-- Lambda Expressions --
----------------------

data Prog = Prog [Def] Exp

-- p48: Figure 3.3
data Def = FuncDef String [String] Exp
         | VarDef String Exp

data Exp = Constant T.Constant 
         | BuiltinOp ()
         | Variable String 
         | Apply Exp Exp 
         | InfixApp T.InfixOp Exp Exp

----------
-- Show --
----------

instance Show Prog where 
  show = pShow

instance Show Def where 
  show = pShow

instance Show Exp where 
  show = pShow

---------------------------
-- ToEnriched (ToLambda) --
---------------------------

instance ToLambda Prog where 
  toLambda = toLambda . toEnriched

instance ToEnriched Prog where 
  toEnriched (Prog defs expr) = E.Let (map toBinding defs) (toEnriched expr)

toBinding :: Def -> E.LetBinding
toBinding (VarDef var_name var_value) = (var_name, toEnriched var_value)
toBinding (FuncDef func_name var_names body) = (func_name, wrapLambda var_names (toEnriched body))
  where 
    wrapLambda :: [String] -> E.Exp -> E.Exp
    wrapLambda [] expr = expr
    wrapLambda (n:ns) expr = E.Lambda n (wrapLambda ns expr)

instance ToEnriched Exp where 
  toEnriched (Constant c)        = toEnriched c
  toEnriched (BuiltinOp _)       = undefined
  toEnriched (Variable x)        = E.Pure (S.Variable (S.RawVar x))
  toEnriched (Apply e1 e2)       = E.Apply (toEnriched e1) (toEnriched e2)
  toEnriched (InfixApp op e1 e2) = E.Apply (E.Apply (toEnriched op) (toEnriched e1)) (toEnriched e2)

------------------
-- Pretty Print --
------------------

instance PrettyLambda Prog where 
  prettyDoc (Prog defs expr) = vsep (map prettyDoc defs ++ [prettyDoc expr])

instance PrettyLambda Def where 
  prettyDoc = mkPrettyDocFromParenS sPrettyDef

instance PrettyLambda Exp where 
  prettyDoc = mkPrettyDocFromParenS sPrettyExp

sPrettyDef :: Def -> PrettyParenS LambdaDoc 
sPrettyDef (FuncDef func_name vars body) = 
  do let pname = pretty func_name
         pvars = hsep . map pretty $ vars
         pbody = prettyDoc body
     pure $ pname <+> pvars <+> pretty "=" <+> pbody
sPrettyDef (VarDef name value) = 
  do let pname = pretty name 
         pvalue = prettyDoc value
     pure $ pname <+> pretty "=" <+> pvalue

sPrettyExp :: Exp -> PrettyParenS LambdaDoc
sPrettyExp (Constant c) = pure $ annStr AConstant (show c)
sPrettyExp (BuiltinOp _) = pure . pretty $ "I don't exist wtf"
sPrettyExp (Variable v) = pure $ pretty v
sPrettyExp (InfixApp infx e1 e2) = do wrapper <- getParenWrapper 10
                                      ep1 <- tempState (setPrec 6) (sPrettyExp e1)
                                      ep2 <- tempState (setPrec 11) (sPrettyExp e2)
                                      pure $ wrapper $ ep1 <+> prettyDoc infx <+> ep2
sPrettyExp (Apply e1 e2) = do wrapper <- getParenWrapper 10
                              ep1 <- tempState (setPrec 6) (sPrettyExp e1)
                              ep2 <- tempState (setPrec 11) (sPrettyExp e2)
                              pure $ wrapper $ ep1 <+> ep2 
