-- | Primarily for use in GHCI
module GhciUtil where 

import Parse
import Lambda.Reduce (Reducible (..))
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Enriched (Exp (..))

import Miranda.Lexer (scanTokens)
import Miranda.Syntax (Prog)

evalMiranda :: String -> String 
evalMiranda = pShow . reduce . either error id . (parse :: String -> Either String Prog)

lexMiranda :: String -> String
lexMiranda = show . scanTokens

evalEnriched :: String -> String
evalEnriched = pShow . reduce . either error id . (parse :: String -> Either String Exp)
