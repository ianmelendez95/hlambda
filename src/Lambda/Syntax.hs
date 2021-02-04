module Lambda.Syntax where 

data Exp = Constant String 
         | Variable String 
         | Apply Exp Exp 
         | Lambda String Exp
         deriving Show