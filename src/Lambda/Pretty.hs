module Lambda.Pretty 
  ( PrettyLambda (..)
  , LambdaAnn (..)
  , AParen (..)
  , nextParenAnn
  , annParens
  , annStr
  ) where 

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
  prettyDoc :: a -> Doc LambdaAnn

  pShow :: a -> String
  pShow = show . prettyDoc

  prettyStream :: a -> SimpleDocStream LambdaAnn
  prettyStream = layoutPretty defaultLayoutOptions . prettyDoc

  ansiPrettyDoc :: a -> SimpleDocStream AnsiStyle
  ansiPrettyDoc = lambdaToAnsi . prettyStream

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