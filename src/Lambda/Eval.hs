module Lambda.Eval
  ( Value (..)
  , eval
  , emptyEnvironment
  ) where 

import Lambda.Reduce (reduce)
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S
import Lambda.Pretty

-- | defined in 2.5.1: "abstract mathematical object, such as 'the number 5', or 
-- | 'the function which squares its argument'"
data Value = Constant S.Constant 
           | Function S.Function
           | Bottom

class ToValue a where 
  toValue :: a -> Value

instance ToValue S.Exp where 
  toValue (S.Constant c) = Constant c
  toValue (S.Function f) = Function f
  toValue _ = Bottom

instance PrettyLambda Value where 
  prettyDoc Bottom = annStr None "_|_"
  prettyDoc (Constant c) = prettyDoc (S.Constant c)
  prettyDoc (Function f) = prettyDoc (S.Function f)

type Environment = (S.Variable -> Value)

emptyEnvironment :: Environment 
emptyEnvironment = const Bottom

-- | Eval[[ expr ]] = value
-- |   2.5.1 "in all the situations where we use Eval ... [the environment] plays no significant role"
-- |         and so far it seems there's no unique behavior to eval that reduction doesn't already handle
-- |   
-- |   2.5.2 introduces the idea of bottom, where so far it seems to reflect that if reduction
-- |         cannot result in a value (no normal form), then it is 'bottom'
eval :: E.Exp -> Value
eval = toValue . reduce 

-- eval (S.Variable var) env = env var
-- eval (S.Apply e1 e2) env = (eval e1 env) (eval e2 env)
-- eval expr _ = reduce expr