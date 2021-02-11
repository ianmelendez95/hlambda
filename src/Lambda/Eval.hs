module Lambda.Eval
  ( Value (..)
  , eval
  ) where 

import Lambda.Reduce (reduce)
import qualified Lambda.Syntax as S
import Lambda.Pretty

data Value = Exp S.Exp
           | Constant S.Constant

class ToValue a where 
  toValue :: a -> Value

instance ToValue S.Exp where 
  toValue (S.Constant c) = Constant c
  toValue expr = Exp expr

instance PrettyLambda Value where 
  prettyDoc (Exp expr) = prettyDoc expr
  prettyDoc (Constant c) = prettyDoc (S.Constant c)

-- | Eval[[ expr ]] = value
eval :: S.Exp -> Value
eval expr = toValue $ reduce expr