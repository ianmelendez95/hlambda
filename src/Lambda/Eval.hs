module Lambda.Eval
  ( Value (..)
  , Eval(..)
  ) where 

import Prettyprinter

import Lambda.Reduce (reduce)
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S
import Lambda.Pretty

import Miranda.Syntax (Prog (..))

data Value = Constant S.Constant 
           | Bottom S.Exp

class ToValue a where 
  toValue :: a -> Value

instance ToValue S.Exp where 
  toValue (S.Term (S.Constant c)) = Constant c
  toValue e = Bottom e

class Eval a where 
  eval :: a -> Value

instance Eval Prog where 
  eval = evalE . E.toEnriched

instance Eval E.Exp where 
  eval = evalE

instance PrettyLambda Value where 
  prettyDoc (Bottom e) = annStr None "_|_: " <+> prettyDoc e
  prettyDoc (Constant c) = prettyDoc (S.mkConstant c)

-- | Eval[[ expr ]] = value
-- |   2.5.1 "in all the situations where we use Eval ... [the environment] plays no significant role"
-- |         and so far it seems there's no unique behavior to eval that reduction doesn't already handle
-- |   
-- |   2.5.2 introduces the idea of bottom, where so far it seems to reflect that if reduction
-- |         cannot result in a value (no normal form), then it is 'bottom'
evalE :: E.Exp -> Value
evalE (E.FatBar e1 e2) = 
  case eval e1 of 
    Constant S.CFail -> eval e2
    res -> res
evalE expr = toValue . reduce $ expr