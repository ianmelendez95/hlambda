module Lambda.Syntax 
  ( Exp (..)
  , Variable
  , Function (..)
  , Constant (..)
  , Term (..)
  , ToConstant (..)

  -- builders
  , mkIf
  , mkApply
  , mkConstant 
  , mkFunction
  , mkVariable
  , mkLambda
  , unApply

  , showMarked
  , varName
  , mapVarName
  , fromConstantToken
  , fromFunctionToken
  , freeVariables
  , freeVariables'
  ) where 

import Prettyprinter ( (<+>), backslash, dot, vsep, align )
import Prettyprinter.Render.Util.SimpleDocTree (treeForm, renderSimplyDecorated)
import Data.Text (Text, unpack, pack)

import qualified Lambda.Token as T
import Lambda.Pretty
import Data.List (foldl1', insert)

-- p13: Figure 2.1 - Syntax of a Lambda Expression

data Exp = Let    [(Variable, Exp)] Exp
         | Letrec [(Variable, Exp)] Exp
         | Term Term 
         | Apply Exp Exp 
         | Lambda String Exp
         deriving Eq

data Term = Constant Constant
          | Function Function
          | Variable Variable
          deriving Eq

type Variable = String

data Function = FPlus
              | FMinus 
              | FMult
              | FDiv
              | FAnd
              | FOr 
              | FNot
              | FIf
              | FCons 
              | FHead 
              | FTail
              | FTuple Int
              | FY   -- The Glorious Y Combinator
              | FEq
              | FNEq
              | FLt
              | FGt
              deriving Eq

data Constant = CNat Int
              | CChar Char
              | CBool Bool 
              | CNil
              | CFail
              | CError
              deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Builders

mkIf :: Exp -> Exp -> Exp -> Exp
mkIf cond true_clause false_clause = 
  mkApply [mkFunction FIf, cond, true_clause, false_clause]

mkVariable :: Variable -> Exp
mkVariable = Term . Variable

mkFunction :: Function -> Exp
mkFunction = Term . Function

mkConstant :: Constant -> Exp
mkConstant = Term . Constant

mkApply :: [Exp] -> Exp
mkApply = foldl1' Apply

unApply :: Exp -> [Exp]
unApply (Apply e1 e2) = unApply e1 ++ [e2]
unApply expr = [expr]

mkLambda :: [String] -> Exp -> Exp
mkLambda vars expr = foldr Lambda expr vars

----------------
-- ToConstant --
----------------

class ToConstant a where 
  toConstant :: a -> Constant
  
  toConstantExp :: a -> Exp
  toConstantExp = mkConstant . toConstant

instance ToConstant Int where 
  toConstant = CNat
instance ToConstant Char where
  toConstant = CChar
instance ToConstant Bool where
  toConstant = CBool

----------------------
-- Token Conversion --
----------------------

fromConstantToken :: T.Constant -> Exp 
fromConstantToken (T.CNat n)   = toConstantExp n
fromConstantToken (T.CChar c)  = toConstantExp c 
fromConstantToken (T.CBool b)  = toConstantExp b

fromFunctionToken :: T.Function -> Exp
fromFunctionToken T.FPlus  = mkFunction FPlus
fromFunctionToken T.FMinus = mkFunction FMinus 
fromFunctionToken T.FMult  = mkFunction FMult
fromFunctionToken T.FDiv   = mkFunction FDiv
fromFunctionToken T.FAnd   = mkFunction FAnd
fromFunctionToken T.FOr    = mkFunction FOr 
fromFunctionToken T.FNot   = mkFunction FNot
fromFunctionToken T.FIf    = mkFunction FIf
fromFunctionToken T.FCons  = mkFunction FCons
fromFunctionToken T.FHead  = mkFunction FHead
fromFunctionToken T.FTail  = mkFunction FTail
fromFunctionToken T.FY     = mkFunction FY

---------
-- Ops --
---------

varName :: String -> String 
varName n = n

mapVarName :: (String -> String) -> Variable -> Variable 
mapVarName = id

--------------
-- Showing --
--------------

-- renderSimplyDecorated
--    :: Monoid out
--    => (Text -> out)       -- ^ Render plain 'Text'
--    -> (ann -> out -> out) -- ^ How to modify an element with an annotation
--    -> SimpleDocTree ann
--    -> out
showMarked :: Exp -> String 
showMarked expr = unpack $ renderSimplyDecorated id renderMarked (treeForm $ prettyStream expr)
  where 
    renderMarked :: LambdaAnn -> Text -> Text 
    renderMarked ABoundVar var = var <> pack ":b"
    renderMarked AFreeVar var = var <> pack ":f"
    renderMarked ARawVar var = var <> pack ":r"
    renderMarked _ var = var

instance Show Exp where 
  show = pShow

instance Show Function where
  show FPlus  = "+"
  show FMinus = "-"
  show FMult  = "*"
  show FDiv   = "/"
  show FAnd   = "AND"
  show FOr    = "OR"
  show FNot   = "NOT"
  show FIf    = "IF"
  show FCons  = "CONS"
  show FHead  = "HEAD"
  show FTail  = "TAIL"
  show FEq    = "="
  show FNEq   = "/="
  show FLt    = "<"
  show FGt    = ">"
  show FY     = "Y"
  show (FTuple 2) = "PAIR"
  show (FTuple 3) = "TRIPLE"
  show (FTuple 4) = "QUADRUPLE"
  show (FTuple n) = "TUPLE-" ++ show n

instance Show Constant where 
  show (CNat n)      = show n
  show (CChar c)     = "\'" ++ [c] ++ "\'"
  show (CBool True)  = "TRUE"
  show (CBool False) = "FALSE"
  show CNil          = "NIL"
  show CError        = "ERROR"
  show CFail         = "FAIL"

------------------
-- Pretty Print --
------------------

instance PrettyLambda Exp where 
  prettyDoc' = mkPrettyDocFromParenS' sPretty

sPretty :: Exp -> PrettyParenS LambdaDoc
sPretty (Term t) = sPrettyTerm t
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

-- let
                        
prettyLet :: String -> [(String, Exp)] -> Exp -> PrettyParenS LambdaDoc
prettyLet let_kw bindings body = pure $ 
  align . vsep $ [pretty let_kw <+> (align . vsep $ map prettyBinding bindings), 
                  pretty "in" <+> prettyDoc body]

prettyBinding :: (String, Exp) -> LambdaDoc
prettyBinding (pat, val) = pretty pat <+> pretty "=" <+> prettyDoc val

-- terms

sPrettyTerm :: Term -> PrettyParenS LambdaDoc
sPrettyTerm (Constant c) = pure $ annStr AConstant (show c)
sPrettyTerm (Function f) = pure $ annStr AFunction (show f)
sPrettyTerm (Variable var) = pure $ prettyVar var

prettyVar :: Variable -> Doc LambdaAnn
prettyVar  = annStr AFreeVar

--------------------------------------------------------------------------------
-- Free Variables

freeVariables :: Exp -> [String]
freeVariables = freeVariables' []

freeVariables' :: [String] -> Exp -> [String]
freeVariables' bound (Let binds expr) = freeVarsInLet bound binds expr
freeVariables' bound (Letrec binds expr) = freeVarsInLet bound binds expr
freeVariables' bound (Term (Variable var)) = [varName var | varName var `notElem` bound]
freeVariables' _ (Term _) = []
freeVariables' bound (Apply e1 e2)  = freeVariables' bound e1 ++ freeVariables' bound e2
freeVariables' bound (Lambda v e) = freeVariables' (insert v bound) e

freeVarsInLet :: [String] -> [(String, Exp)] -> Exp -> [String]
freeVarsInLet bound binds expr = 
  let binds_free = foldr (\(bound_var, e) vars -> 
                             freeVariables' (bound_var : bound) e ++ vars) 
                         [] binds
   in freeVariables' bound expr ++ binds_free