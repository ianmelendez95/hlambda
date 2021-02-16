module Parse where 

import qualified Lambda.Enriched as E (Exp (..))
import qualified Lambda.Parser as EP (parseExpression)
import Miranda.Syntax (Prog (..))
import Miranda.Parser (parseProgram)

class Parse a where 
  parse :: String -> Either String a

instance Parse Prog where 
  parse = parseProgram

instance Parse E.Exp where 
  parse = EP.parseExpression
