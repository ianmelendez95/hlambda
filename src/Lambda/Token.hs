module Lambda.Token 
  ( LocToken (..)
  , Token (..)
  , Function (..)
  , Constant (..)
  ) where 

data LocToken = LToken {
    locLine  :: Int 
  , locCol   :: Int 
  , locToken :: Token
  } deriving Show

data Token = Function Function
           | Constant Constant
           | Variable String 
           | Lambda
           | Dot 
           | LP
           | RP
           | LC
           | RC
           | Semi
           | Let 
           | Letrec
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
              | FY
              deriving Show

data Constant = CNat Int
              | CChar Char
              | CBool Bool 
              deriving Show
