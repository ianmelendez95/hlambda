module Lambda.ToLambda (ToLambda (..)) where 

import qualified Miranda.Syntax as M
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E
import Lambda.EnrichedCompiler (compileToLambda)

class ToLambda a where 
  toLambda :: a -> S.Exp

instance ToLambda M.Prog where 
  toLambda = toLambda . E.toEnriched

instance ToLambda E.Exp where
  toLambda = compileToLambda
