module Parse where 

import qualified Lambda.Enriched as E (Exp (..))
import qualified Lambda.Parser as EP (parseExpression)
import Miranda.Syntax (Prog (..), Exp, Def)
import Miranda.Parser (parseProgram, parseDef, parseExp)

class Parse a where 
  parse :: String -> Either String a

instance Parse Prog where 
  parse = parseProgram

instance Parse Exp where 
  parse = parseExp

instance Parse Def where 
  parse = parseDef

instance Parse E.Exp where 
  parse = EP.parseExpression
