module Lambda.Token 
  ( Token (..)
  , Function (..)
  , Constant (..)
  ) where 

-- data LocToken = LToken {
--     locLine  :: Int 
--   , locCol   :: Int 
--   , locToken :: Token
--   }

data Token = Function Function
           | Constant Constant
           | Variable String 
           | Lambda
           | Dot 
           | LP
           | RP
           | Let 
           | In
           | Equal
           | EOF
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
              | CBool Bool 
              deriving Show