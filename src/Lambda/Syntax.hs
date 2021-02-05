module Lambda.Syntax where 

import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Util

data Exp = Constant String 
         | Variable Variable 
         | Apply Exp Exp 
         | Lambda String Exp
        --  deriving Show

data Variable = RawVar String 
              | FreeVar String 
              | BoundVar String
              -- deriving Show

varName :: Variable -> String 
varName (RawVar n) = n
varName (FreeVar n) = n
varName (BoundVar n) = n

instance Show Exp where 
  showsPrec _ (Constant str) = showString str
  showsPrec _ (Variable var) = showString $ show var
  showsPrec d (Lambda name expr) = showParen (d > lambda_prec) $ 
    showString ("\\" ++ name ++ ". ") . shows expr
    where lambda_prec = 5
  showsPrec d (Apply expr expr') = showParen (d > apply_prec) $
    showsPrec 6 expr . showString " " . showsPrec 11 expr'
    where apply_prec = 10

instance Show Variable where 
  show (RawVar var) = var
  show (FreeVar var) = var
  show (BoundVar var) = var

data LambdaAnn =  ABoundVar
               | AFreeVar
               | ARawVar
               | ANone

prettyExp :: Exp -> Doc LambdaAnn
prettyExp (Constant c) = pretty c
prettyExp (Variable var) = prettyVar var
prettyExp (Apply e e') 
  = parens $ prettyExp e <+> prettyExp e'
prettyExp (Lambda var e) 
  = parens $ backslash <> ann ABoundVar (annStr ABoundVar var) <> dot <+> prettyExp e

prettyVar :: Variable -> Doc LambdaAnn
prettyVar (FreeVar name) = annStr AFreeVar name
prettyVar (BoundVar name) = annStr ABoundVar name
prettyVar (RawVar name) = annStr ARawVar name

ann :: ann -> Doc ann -> Doc ann
ann = annotate

annStr :: LambdaAnn -> String -> Doc LambdaAnn
annStr a = annotate a . pretty