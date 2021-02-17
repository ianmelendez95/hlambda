module Miranda.Syntax 
  ( Prog (..) 
  , Def (..)
  , GenTypeVar
  , Constr
  , ConstrArg (..)
  , Pattern (..)
  , Exp (..)
  -- , ToLambda (..)
  -- , showMarked
  ) where 

import Prettyprinter
import Data.List (intersperse, foldl', foldl1')

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
data Def = FuncDef String [Pattern] Exp
         | VarDef String Exp
         | TypeDef String [GenTypeVar] [Constr]

type GenTypeVar = Int
type Constr = (String, [ConstrArg])

data ConstrArg = CAVar String 
               | CAGenTypeVar GenTypeVar
               | CAList [ConstrArg]

data Pattern = PVar String 
             | PConstr Constr

data Exp = Constant T.Constant 
         | BuiltinOp ()
         | Variable String 
         | Constructor String
         | Apply Exp Exp 
         | InfixApp T.InfixOp Exp Exp
         | ListLit [Exp]    -- [], [x,y,z], [1,2]
         | ListColon [Exp]  -- (x:y:[]), (x:y:zs)
         | Tuple [Exp]      -- (x,y,z), (1,'a',x)

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
    wrapLambda :: [Pattern] -> E.Exp -> E.Exp
    wrapLambda [] expr = expr
    wrapLambda ((PVar n):ns) expr = E.Lambda n (wrapLambda ns expr)

instance ToEnriched Exp where 
  toEnriched (Constant c)        = toEnriched c
  toEnriched (BuiltinOp _)       = undefined
  toEnriched (Variable x)        = E.Pure (S.Variable (S.RawVar x))
  toEnriched (Constructor c)     = E.Pure (S.Variable (S.RawVar c)) -- TODO: are constructors just vars in the LC?
  toEnriched (Apply e1 e2)       = E.Apply (toEnriched e1) (toEnriched e2)
  toEnriched (InfixApp op e1 e2) = E.Apply (E.Apply (toEnriched op) (toEnriched e1)) (toEnriched e2)
  toEnriched (ListLit exps)      = enrichedListLit exps
  toEnriched (ListColon exps)    = enrichedColonList exps
  toEnriched (Tuple exps)        = enrichedTuple exps

enrichedListLit, enrichedColonList :: [Exp] -> E.Exp
enrichedListLit [] = enrNil
enrichedListLit exprs = consEnrichedExprs (map toEnriched exprs ++ [enrNil])

enrichedColonList exprs = consEnrichedExprs (map toEnriched exprs)

consEnrichedExprs :: [E.Exp] -> E.Exp
consEnrichedExprs = foldr1 (E.Apply . E.Apply enrCons)

enrichedTuple :: [Exp] -> E.Exp
enrichedTuple exps = foldl1' E.Apply (enr_tuple_f : map toEnriched exps) 
  where 
    enr_tuple_f = E.Pure (S.Function (S.FTuple (length exps)))

-- | 'enr'iched nil and cons
enrNil, enrCons :: E.Exp
enrNil  = E.Pure (S.Constant S.CNil)
enrCons = E.Pure (S.Function S.FCons)

------------------
-- Pretty Print --
------------------

instance PrettyLambda Prog where 
  prettyDoc (Prog defs expr) = vsep (map prettyDoc defs ++ [prettyDoc expr])

instance PrettyLambda Def where 
  prettyDoc = mkPrettyDocFromParenS sPrettyDef

instance PrettyLambda Pattern where 
  prettyDoc = mkPrettyDocFromParenS sPrettyPattern

instance PrettyLambda Exp where 
  prettyDoc = mkPrettyDocFromParenS sPrettyExp

sPrettyDef :: Def -> PrettyParenS LambdaDoc 
sPrettyDef (FuncDef func_name vars body) = 
  do let pname = pretty func_name
         pvars = hsep . map prettyDoc $ vars
         pbody = prettyDoc body
     pure $ pname <+> pvars <+> pretty "=" <+> pbody
sPrettyDef (TypeDef type_name type_vars constrs) = 
  do pConstrs <- tempState (setPrec 0) (mapM prettyConstructor constrs)
     pure $ prettyLHS type_name type_vars 
              <+> pretty "::=" 
              <+> hsep (intersperse (pretty "|") pConstrs)
  where 
    prettyLHS :: String -> [GenTypeVar] -> LambdaDoc
    prettyLHS name vars = 
      if null vars then pretty name 
                   else pretty name <+> hsep (map prettyTypeVar type_vars)

sPrettyDef (VarDef name value) = 
  do let pname = pretty name 
         pvalue = prettyDoc value
     pure $ pname <+> pretty "=" <+> pvalue

sPrettyPattern :: Pattern -> PrettyParenS LambdaDoc
sPrettyPattern (PVar v) = pure $ pretty v
sPrettyPattern (PConstr c) = tempState (setPrec 11) (prettyConstructor c)

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
sPrettyExp (ListLit exp_list) = 
  do pexp_list <- mapM sPrettyExp exp_list
     return (lbracket <> hcat (intersperse comma pexp_list) <> rbracket)
sPrettyExp (ListColon exp_list) = 
  do wrapper <- getParenWrapper 10
     pexp_list <- mapM sPrettyExp exp_list
     return $ wrapper (hcat (intersperse colon pexp_list))
sPrettyExp (InfixApp infx e1 e2) = do wrapper <- getParenWrapper 10
                                      ep1 <- tempState (setPrec 6) (sPrettyExp e1)
                                      ep2 <- tempState (setPrec 11) (sPrettyExp e2)
                                      pure $ wrapper $ ep1 <+> prettyDoc infx <+> ep2
sPrettyExp (Apply e1 e2) = do wrapper <- getParenWrapper 10
                              ep1 <- tempState (setPrec 6) (sPrettyExp e1)
                              ep2 <- tempState (setPrec 11) (sPrettyExp e2)
                              pure $ wrapper $ ep1 <+> ep2 
