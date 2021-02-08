module Lambda.Token (Token (..), Function (..), Constant (..)) where 

data Token = Function Function
           | Constant Constant
           | Variable String 
           | Lambda
           | Dot 
           | LP
           | RP
           deriving Show

data Function = FPlus
              | FMinus 
              | FMult
              | FDiv
              | FAnd
              | FOr 
              | FNot
              | FIf
              | FCons
              | FHead 
              | FTail
              deriving Show

data Constant = CNat Int
              | CChar Char
              | CTrue 
              | CFalse
              deriving Show
