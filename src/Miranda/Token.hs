module Miranda.Token 
  ( LocToken (..)
  , Token (..)
  , InfixOp (..)
  , Constant (..)
  ) where 

import Lambda.Pretty

data LocToken = LToken {
    locLine  :: Int 
  , locCol   :: Int 
  , locToken :: Token
  } deriving Show

data Token = Constant Constant
           | Variable String 
           | InfixOp InfixOp
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

data InfixOp = IPlus
             | IMinus 
             | IMult
             | IDiv 
             deriving Show

data Constant = CNat Int
              | CChar Char
              | CBool Bool 

instance PrettyLambda InfixOp where 
  prettyDoc IPlus  = pretty "+"
  prettyDoc IMinus = pretty "-"
  prettyDoc IMult  = pretty "*"
  prettyDoc IDiv   = pretty "/"

instance Show Constant where 
  show (CNat x) = show x
  show (CChar c) = ['\'', c , '\'']
  show (CBool b) = show b
