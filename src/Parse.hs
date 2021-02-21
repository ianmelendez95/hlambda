module Parse where 

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
