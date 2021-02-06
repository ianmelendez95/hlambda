module Lambda.Syntax where 

import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Util
import Prettyprinter.Render.Terminal
import Control.Monad.State.Lazy

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

data ParenState = ParenState {
    parenPrec :: Int,
    parenStyle :: AParen
  }
initParenState :: ParenState
initParenState = ParenState 0 AParenMagenta

type LambdaDoc = Doc LambdaAnn
type PrettyExp = State ParenState
type ParenWrapper = (LambdaDoc -> LambdaDoc)

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
prettyExpDoc expr = evalState (sPretty expr) initParenState 

sPretty :: Exp -> PrettyExp LambdaDoc
sPretty (Constant c) = pure $ annStr AConstant c
sPretty (Variable var) = pure $ prettyVar var
sPretty (Lambda var e) = do wrapper <- getParenWrapper 5 
                            ePretty <- setPrec 0 >> sPretty e
                            pure $ wrapper $ backslash
                                           <> ann ABoundVar (annStr ABoundVar var) 
                                           <> dot 
                                           <+> ePretty
sPretty (Apply e e') = do wrapper <- getParenWrapper 10
                          ePretty <- setPrec 6 >> sPretty e
                          ePretty' <- setPrec 11 >> sPretty e'
                          pure $ wrapper $ ePretty <+> ePretty'

setPrec :: Int -> PrettyExp ()
setPrec prec = modify (\ps -> ps { parenPrec = prec })

-- | main reason for using state, so we can get the paren wrapper and update the paren state
-- | in one swoop (still probably too obfuscated to be worth it)
getParenWrapper :: Int -> PrettyExp ParenWrapper
getParenWrapper prec = do pPrec <- gets parenPrec
                          if pPrec <= prec
                            then pure id 
                            else getWrapper
  where 
    getWrapper :: PrettyExp ParenWrapper
    getWrapper = annParens . AParen <$> (gets parenStyle <* modify updateParenStyle)

-- getParenWrapper' :: PrettyExp ParenWrapper 
-- getParenWrapper' = do pStyle <- gets parenStyle
--                      modify updateParenStyle
--                      pure $ annParens (AParen pStyle)

updateParenStyle :: ParenState -> ParenState
updateParenStyle pState@ParenState { parenStyle = style } 
  = pState { parenStyle = nextParenAnn style }

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