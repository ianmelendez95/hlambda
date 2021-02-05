module Lambda.Syntax where 

data Exp = Constant String 
         | Variable Variable 
         | Apply Exp Exp 
         | Lambda String Exp
         deriving Show

data Variable = RawVar String 
              | FreeVar String 
              | BoundVar String
              deriving Show

varName :: Variable -> String 
varName (RawVar n) = n
varName (FreeVar n) = n
varName (BoundVar n) = n