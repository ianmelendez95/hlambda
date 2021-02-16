module Miranda.Token 
  ( LocToken (..)
  , Token (..)
  , InfixOp (..)
  , Constant (..)
  ) where 

import Lambda.Pretty
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S

data LocToken = LToken {
    locLine  :: Int 
  , locCol   :: Int 
  , locToken :: Token
  } deriving Show

data Token = Constant Constant
           | Constructor String
           | Variable String 
           | InfixOp InfixOp
           | TypeEq
           | VertBar
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
             | IVar String
             deriving Show

data Constant = CNat Int
              | CChar Char
              | CBool Bool 

instance PrettyLambda InfixOp where 
  prettyDoc IPlus    = pretty "+"
  prettyDoc IMinus   = pretty "-"
  prettyDoc IMult    = pretty "*"
  prettyDoc IDiv     = pretty "/"
  prettyDoc (IVar v) = pretty "$" <> pretty v

instance Show Constant where 
  show (CNat x) = show x
  show (CChar c) = ['\'', c , '\'']
  show (CBool b) = show b

instance E.ToEnriched Constant where 
  toEnriched (CNat x)  = E.Pure . S.Constant . S.CNat  $ x
  toEnriched (CChar c) = E.Pure . S.Constant . S.CChar $ c
  toEnriched (CBool b) = E.Pure . S.Constant . S.CBool $ b

instance E.ToEnriched InfixOp where 
  toEnriched IPlus    = E.Pure . S.Function $ S.FPlus
  toEnriched IMinus   = E.Pure . S.Function $ S.FMinus
  toEnriched IMult    = E.Pure . S.Function $ S.FMult
  toEnriched IDiv     = E.Pure . S.Function $ S.FDiv
  toEnriched (IVar v) = E.Pure . S.Variable $ S.RawVar v

