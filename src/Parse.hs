module Parse where 

import Miranda.Syntax (Prog (..), Exp, Decl)
import Miranda.Parser (parseProgram, parseDecl, parseExp)

class Parse a where 
  parse :: String -> Either String a

instance Parse Prog where 
  parse = parseProgram

instance Parse Exp where 
  parse = parseExp

instance Parse Decl where 
  parse = parseDecl
