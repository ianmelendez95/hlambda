module Miranda.Token 
  ( LocToken (..)
  , Token (..)
  , InfixOp (..)
  , Constant (..)
  , constantToLambda
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
           | GenTypeVar Int -- generic type variable: *, **, ***, ...
           | If
           | Where
           | TypeEq
           | VertBar
           | Dot 

           -- (): [P]arens - https://www.cis.upenn.edu/~matuszek/General/JavaSyntax/parentheses.html#:~:text=Braces%20are%20used%20to%20group%20the%20statements%20in%20an%20if,loop%2C%20or%20other%20control%20structures.&text=Brackets%20are%20used%20to%20index%20into%20an%20array.&text=Parentheses%20are%20used%20for%20two,to%20a%20constructor%20or%20method.
           | LP
           | RP
           -- []: [B]rackets
           | LB
           | RB
           -- {}: [C]urly brace
           | LC
           | RC

           | Semi
           | Comma
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
             | ICons
             | IEq
             | ILt
             | IGt
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
  prettyDoc ICons    = pretty ":"
  prettyDoc IEq      = pretty "=="
  prettyDoc ILt      = pretty "<"
  prettyDoc IGt      = pretty ">"
  prettyDoc (IVar v) = pretty "$" <> pretty v

instance Show Constant where 
  show (CNat x) = show x
  show (CChar c) = ['\'', c , '\'']
  show (CBool b) = show b

instance E.ToEnriched Constant where 
  toEnriched (CNat x)  = E.Pure . S.toConstantExp $ x
  toEnriched (CChar c) = E.Pure . S.toConstantExp $ c
  toEnriched (CBool b) = E.Pure . S.toConstantExp $ b

constantToLambda :: Constant -> S.Constant 
constantToLambda (CNat x)  = S.CNat x
constantToLambda (CChar c) = S.CChar c
constantToLambda (CBool b) = S.CBool b


instance E.ToEnriched InfixOp where 
  toEnriched IPlus    = E.Pure . S.mkFunction $ S.FPlus
  toEnriched IMinus   = E.Pure . S.mkFunction $ S.FMinus
  toEnriched IMult    = E.Pure . S.mkFunction $ S.FMult
  toEnriched IDiv     = E.Pure . S.mkFunction $ S.FDiv
  toEnriched ICons    = E.Pure . S.mkFunction $ S.FCons
  toEnriched IEq      = E.Pure . S.mkFunction $ S.FEq
  toEnriched ILt      = E.Pure . S.mkFunction $ S.FLt
  toEnriched IGt      = E.Pure . S.mkFunction $ S.FGt
  toEnriched (IVar v) = E.Pure . S.mkVariable $ v

