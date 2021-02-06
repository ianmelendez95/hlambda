module Lambda.Syntax where 

import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Util
import Prettyprinter.Render.Terminal

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

data LambdaAnn = ABoundVar
               | AFreeVar
               | ARawVar
               | AConstant
               | AParen AParen

data AParen = AParenYellow
            | AParenMagenta
            | AParenCyan
            deriving (Show)

ansiPrettyExp :: Exp -> SimpleDocStream AnsiStyle
ansiPrettyExp = reAnnotateS ansiStyle . prettyExp
  where
    ansiStyle :: LambdaAnn -> AnsiStyle 
    ansiStyle ABoundVar = color Blue
    ansiStyle AFreeVar = color Green
    ansiStyle ARawVar = color Red
    ansiStyle AConstant = mempty
    ansiStyle (AParen p) = parenStyle p

    parenStyle :: AParen -> AnsiStyle
    parenStyle AParenYellow = color Yellow
    parenStyle AParenMagenta = color Magenta
    parenStyle AParenCyan = color Cyan

prettyExp :: Exp -> SimpleDocStream LambdaAnn
prettyExp = layoutPretty defaultLayoutOptions . prettyExpDoc

prettyExpDoc :: Exp -> Doc LambdaAnn
prettyExpDoc = prettyExpDocPrec AParenMagenta 0

prettyExpDocPrec :: AParen -> Int -> Exp -> Doc LambdaAnn
prettyExpDocPrec _ _ (Constant c) = annStr AConstant c
prettyExpDocPrec _ _ (Variable var) = prettyVar var
prettyExpDocPrec p d (Lambda var e) 
  = let (parens', nextP) = if d > 5 then (annParens (AParen p), nextParenAnn p)
                                    else (id, p)
     in parens' $ backslash 
                <> ann ABoundVar (annStr ABoundVar var) 
                <> dot 
                <+> prettyExpDocPrec nextP 0 e
prettyExpDocPrec p d (Apply e e') 
  = let (parens', nextP) = if d > 10 then (annParens (AParen p), nextParenAnn p)
                                     else (id, p)
     in parens' $ prettyExpDocPrec nextP 6 e <+> prettyExpDocPrec nextP 11 e'

nextParenAnn :: AParen -> AParen
nextParenAnn AParenYellow = AParenMagenta
nextParenAnn AParenMagenta = AParenCyan
nextParenAnn AParenCyan = AParenYellow

annParens :: LambdaAnn -> (Doc LambdaAnn -> Doc LambdaAnn)
annParens a = enclose (annotate a $ pretty "(") (annotate a $ pretty ")")

prettyVar :: Variable -> Doc LambdaAnn
prettyVar (FreeVar name) = annStr AFreeVar name
prettyVar (BoundVar name) = annStr ABoundVar name
prettyVar (RawVar name) = annStr ARawVar name

ann :: ann -> Doc ann -> Doc ann
ann = annotate

annStr :: LambdaAnn -> String -> Doc LambdaAnn
annStr a = annotate a . pretty