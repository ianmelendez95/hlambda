{-# LANGUAGE FlexibleInstances #-}
module Lambda.Pretty 
  ( PrettyLambda (..)
  , LambdaAnn (..)
  , AParen (..)
  , nextParenAnn
  , annParens
  , annStr
  , pretty

  -- pretty paren util
  , PrettyParenS 
  , LambdaDoc 
  , Doc 
  , mkPrettyDocFromParenS
  , mkPrettyDocFromParenS'
  , getParenWrapper
  , tempState 
  , getPrec
  , setPrec 
  ) where 

import Control.Monad.State.Lazy
    ( gets, modify, evalState, MonadState(put, get), State )
import Prettyprinter
import Prettyprinter.Render.Terminal
    ( color,
      AnsiStyle,
      Color(Cyan, Blue, Green, Red, Yellow, Magenta) )

data LambdaAnn = ABoundVar
               | AFreeVar
               | ARawVar
               | AConstant
               | AFunction
               | AParen AParen
               | None

data AParen = AParenYellow
            | AParenMagenta
            | AParenCyan
            deriving (Show)

-- | class that can be represented as a lambda expression, 
-- | according to how its defined in out pretty print framework
class PrettyLambda a where 
  prettyDoc' :: Int -> a -> Doc LambdaAnn

  prettyDoc :: a -> Doc LambdaAnn
  prettyDoc = prettyDoc' 0

  pShow :: a -> String
  pShow = show . prettyDoc

  prettyStream :: a -> SimpleDocStream LambdaAnn
  prettyStream = layoutPretty defaultLayoutOptions . prettyDoc

  ansiPrettyDoc :: a -> SimpleDocStream AnsiStyle
  ansiPrettyDoc = lambdaToAnsi . prettyStream

instance PrettyLambda String where 
  prettyDoc' _ s = pretty s

----------
-- Ansi --
----------

lambdaToAnsi :: SimpleDocStream LambdaAnn -> SimpleDocStream AnsiStyle
lambdaToAnsi = reAnnotateS ansiStyle
  where
    ansiStyle :: LambdaAnn -> AnsiStyle 
    ansiStyle ABoundVar = color Blue
    ansiStyle AFreeVar = color Green
    ansiStyle ARawVar = color Red
    ansiStyle AConstant = mempty
    ansiStyle AFunction = mempty
    ansiStyle None = mempty
    ansiStyle (AParen p) = parenStyle' p

    parenStyle' :: AParen -> AnsiStyle
    parenStyle' AParenYellow = color Yellow
    parenStyle' AParenMagenta = color Magenta
    parenStyle' AParenCyan = color Cyan

-------------
-- Helpers --
-------------

nextParenAnn :: AParen -> AParen
nextParenAnn AParenYellow = AParenMagenta
nextParenAnn AParenMagenta = AParenCyan
nextParenAnn AParenCyan = AParenYellow

annParens :: LambdaAnn -> (Doc LambdaAnn -> Doc LambdaAnn)
annParens a = enclose (annotate a $ pretty "(") (annotate a $ pretty ")")

annStr :: LambdaAnn -> String -> Doc LambdaAnn
annStr a = annotate a . pretty

-----------------
-- Paren State --
-----------------

data ParenState = ParenState {
    parenPrec :: Int,
    parenStyle :: AParen
  }

type LambdaDoc = Doc LambdaAnn
type PrettyParenS = State ParenState
type ParenWrapper = (LambdaDoc -> LambdaDoc)

-- instance PrettyLambda Exp where 
--   prettyDoc = mkPrettyFromParenS your_prettyparen_func src_value

mkPrettyDocFromParenS :: (a -> PrettyParenS LambdaDoc) -> a -> Doc LambdaAnn
mkPrettyDocFromParenS = flip mkPrettyDocFromParenS' 0

mkPrettyDocFromParenS' :: (a -> PrettyParenS LambdaDoc) -> Int -> a -> Doc LambdaAnn
mkPrettyDocFromParenS' pretty_func init_prec pretty_src = 
  evalState (pretty_func pretty_src) (initParenState init_prec)

initParenState :: Int -> ParenState
initParenState init_prec = ParenState init_prec AParenMagenta

tempState :: PrettyParenS () -> PrettyParenS a -> PrettyParenS a
tempState change pe = do s <- get 
                         change
                         res <- pe
                         put s
                         return res

setPrec :: Int -> PrettyParenS ()
setPrec prec = modify (\ps -> ps { parenPrec = prec })

getPrec :: PrettyParenS Int
getPrec = gets parenPrec

-- | main reason for using state, so we can get the paren wrapper and update the paren state
-- | in one swoop (still probably too obfuscated to be worth it)
getParenWrapper :: Int -> PrettyParenS ParenWrapper
getParenWrapper prec = do pPrec <- gets parenPrec
                          if pPrec <= prec
                            then pure id 
                            else getWrapper
  where 
    getWrapper :: PrettyParenS ParenWrapper
    getWrapper = annParens . AParen <$> (gets parenStyle <* modify updateParenStyle)

updateParenStyle :: ParenState -> ParenState
updateParenStyle pState@ParenState { parenStyle = style } 
  = pState { parenStyle = nextParenAnn style }