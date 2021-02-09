module Lambda.Syntax 
  ( Exp (..)
  , Variable (..)
  , Function (..)
  , Constant (..)
  , ansiPrettyExp
  , pShow
  , varName
  , fromConstantToken
  , fromFunctionToken
  ) where 

import Prettyprinter
import Control.Monad.State.Lazy
import Prettyprinter.Render.Terminal
    ( color,
      AnsiStyle,
      Color(Cyan, Blue, Green, Red, Yellow, Magenta) )
import qualified Lambda.Token as T

data Exp = Constant Constant 
         | Function Function
         | Variable Variable 
         | Apply Exp Exp 
         | Lambda String Exp
         deriving Show

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

data Constant = CNat Int
              | CChar Char
              | CBool Bool 

data Variable = RawVar String 
              | FreeVar String 
              | BoundVar String
              -- deriving Show

----------------------
-- Token Conversion --
----------------------

fromConstantToken :: T.Constant -> Exp 
fromConstantToken (T.CNat n)   = Constant (CNat n) 
fromConstantToken (T.CChar n)  = Constant (CChar n) 
fromConstantToken (T.CBool b)  = Constant (CBool b) 

fromFunctionToken :: T.Function -> Exp
fromFunctionToken T.FPlus  = Function FPlus
fromFunctionToken T.FMinus = Function FMinus 
fromFunctionToken T.FMult  = Function FMult
fromFunctionToken T.FDiv   = Function FDiv
fromFunctionToken T.FAnd   = Function FAnd
fromFunctionToken T.FOr    = Function FOr 
fromFunctionToken T.FNot   = Function FNot
fromFunctionToken T.FIf    = Function FIf
fromFunctionToken T.FCons  = Function FCons
fromFunctionToken T.FHead  = Function FHead
fromFunctionToken T.FTail  = Function FTail

---------
-- Ops --
---------

varName :: Variable -> String 
varName (RawVar n) = n
varName (FreeVar n) = n
varName (BoundVar n) = n

-- instance Show Exp where 
--   show = show . prettyExpDoc

pShow :: Exp -> String 
pShow = show . prettyExpDoc

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

instance Show Constant where 
  show (CNat n) = show n
  show (CChar c) = "\'" ++ [c] ++ "\'"
  show (CBool True) = "TRUE"
  show (CBool False) = "FALSE"

instance Show Variable where 
  show (RawVar var) = var
  show (FreeVar var) = var
  show (BoundVar var) = var

data LambdaAnn = ABoundVar
               | AFreeVar
               | ARawVar
               | AConstant
               | AFunction
               | AParen AParen

data AParen = AParenYellow
            | AParenMagenta
            | AParenCyan
            deriving (Show)

data ParenState = ParenState {
    parenPrec :: Int,
    parenStyle :: AParen
  }

type LambdaDoc = Doc LambdaAnn
type PrettyExp = State ParenState
type ParenWrapper = (LambdaDoc -> LambdaDoc)

initParenState :: ParenState
initParenState = ParenState 0 AParenMagenta

------------------
-- PRETTY PRINT --
------------------

ansiPrettyExp :: Exp -> SimpleDocStream AnsiStyle
ansiPrettyExp = reAnnotateS ansiStyle . prettyExp
  where
    ansiStyle :: LambdaAnn -> AnsiStyle 
    ansiStyle ABoundVar = color Blue
    ansiStyle AFreeVar = color Green
    ansiStyle ARawVar = color Red
    ansiStyle AConstant = mempty
    ansiStyle AFunction = mempty
    ansiStyle (AParen p) = parenStyle' p

    parenStyle' :: AParen -> AnsiStyle
    parenStyle' AParenYellow = color Yellow
    parenStyle' AParenMagenta = color Magenta
    parenStyle' AParenCyan = color Cyan

prettyExp :: Exp -> SimpleDocStream LambdaAnn
prettyExp = layoutPretty defaultLayoutOptions . prettyExpDoc

prettyExpDoc :: Exp -> Doc LambdaAnn
prettyExpDoc expr = evalState (sPretty expr) initParenState 

sPretty :: Exp -> PrettyExp LambdaDoc
sPretty (Constant c) = pure $ annStr AConstant (show c)
sPretty (Function f) = pure $ annStr AFunction (show f)
sPretty (Variable var) = pure $ prettyVar var
sPretty (Lambda var e) = do wrapper <- getParenWrapper 5 
                            ePretty <- tempState (setPrec 0) (sPretty e)
                            pure $ wrapper $ backslash
                                           <> annotate ABoundVar (annStr ABoundVar var) 
                                           <> dot 
                                           <+> ePretty
sPretty (Apply e e') = do wrapper <- getParenWrapper 10
                          ePretty <- tempState (setPrec 6) (sPretty e)
                          ePretty' <- tempState (setPrec 11) (sPretty e')
                          pure $ wrapper $ ePretty <+> ePretty'

tempState :: PrettyExp () -> PrettyExp a -> PrettyExp a
tempState change pe = do s <- get 
                         change
                         res <- pe
                         put s
                         return res

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

updateParenStyle :: ParenState -> ParenState
updateParenStyle pState@ParenState { parenStyle = style } 
  = pState { parenStyle = nextParenAnn style }

nextParenAnn :: AParen -> AParen
nextParenAnn AParenYellow = AParenMagenta
nextParenAnn AParenMagenta = AParenCyan
nextParenAnn AParenCyan = AParenYellow

annParens :: LambdaAnn -> (Doc LambdaAnn -> Doc LambdaAnn)
annParens a = enclose (annotate a $ pretty "(") (annotate a $ pretty ")")

prettyVar :: Variable -> Doc LambdaAnn
prettyVar (FreeVar name)  = annStr AFreeVar name
prettyVar (BoundVar name) = annStr ABoundVar name
prettyVar (RawVar name)   = annStr ARawVar name

annStr :: LambdaAnn -> String -> Doc LambdaAnn
annStr a = annotate a . pretty