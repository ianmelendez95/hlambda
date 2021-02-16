-- | Primarily for use in GHCI
module SimpleEval where 

import Parse
import Lambda.Reduce (Reducible (..))
import Lambda.Pretty (PrettyLambda (..))
import Miranda.Syntax (Prog)

evalMiranda :: String -> String 
evalMiranda = pShow . reduce . either error id . (parse :: String -> Either String Prog)