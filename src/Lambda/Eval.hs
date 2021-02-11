module Lambda.Eval
  ( Value (..)
  , eval
  , emptyEnvironment
  ) where 

import Lambda.Reduce (reduce)
import qualified Lambda.Syntax as S
import Lambda.Pretty

type Value = S.Exp

-- class ToValue a where 
--   toValue :: a -> Value

-- instance ToValue S.Exp where 
--   toValue (S.Constant c) = Constant c
--   toValue expr = Exp expr

-- instance PrettyLambda Value where 
--   prettyDoc (Exp expr) = prettyDoc expr
--   prettyDoc (Constant c) = prettyDoc (S.Constant c)

type Environment = (S.Variable -> Value)

emptyEnvironment :: Environment 
emptyEnvironment = S.Variable

-- | Eval[[ expr ]] = value
eval :: S.Exp -> Environment -> Value
eval expr _ = reduce expr
eval (S.Variable var) env = env var