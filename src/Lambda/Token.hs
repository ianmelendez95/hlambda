module Lambda.Token (Token (..)) where 

data Token = Constant String 
           | Variable String 
           | Lambda
           | Dot 
           | LP
           | RP
           deriving Show