{
module Miranda.Parser 
  ( ParseResult, 
    parser, 
    parseDef,
    parseProgram
  ) where 

import Data.Char
import qualified Miranda.Syntax as S
import qualified Miranda.Token as T
import Miranda.Lexer (alexScanTokens, scanTokens, scanTokensEither)
}

%name parser program
%name defParser def
%monad { ParseResult } { >>= } { return }
%tokentype { T.Token }
%error { parseError }

%token 
  const       { T.Constant $$       }
  constr      { T.Constructor $$    }
  var         { T.Variable $$       }
  plus        { T.InfixOp T.IPlus       }
  minus       { T.InfixOp T.IMinus      }
  mult        { T.InfixOp T.IMult       }
  div         { T.InfixOp T.IDiv        }
  infix_var   { T.InfixOp (T.IVar _)    } -- $<var-name>

  gtype_2plus { T.GenTypeVar $$     }   -- '2plus' because it's two or more '**', since '*' is mult

  '='         { T.Equal             }
  '::='       { T.TypeEq            }
  '('         { T.LP                }
  ')'         { T.RP                }
  '['         { T.LB                }
  ']'         { T.RB                }
  ';'         { T.Semi              }
  ','         { T.Comma             }
  ':'         { T.Colon             }
  '{'         { T.LC                }
  '}'         { T.RC                }
  '|'         { T.VertBar           }

%%

program :: { S.Prog }
program : '{' exp '}'          { S.Prog [] $2 }
        | '{' defs ';' exp '}' { S.Prog (reverse $2) $4}

-------------------------------------------------------------------------------
-- Defs

defs :: { [S.Def] }
defs : defs ';' def    { $3 : $1 }
     | def             { [$1] }

def :: { S.Def }
def : funcDef { $1 }
    | varDef  { $1 }
    | typeDef { $1 }

-------------------------------------------------------------------------------
-- Func Def

funcDef :: { S.Def }
funcDef : var funcParams '=' exp    { S.FuncDef $1 (reverse $2) $4 }

-- REVERSE!!: funcParams have *at least one* variable (otherwise it would be a var definition)
funcParams :: { [S.Pattern] }
funcParams : funcParams patt     { $2 : $1 }
           | patt                { [$1] }

patt :: { S.Pattern }
patt : var                 { S.PVar $1    }
     | constr              { S.PConstr ($1, []) }
     | '(' constructor ')' { S.PConstr $2 }

-------------------------------------------------------------------------------
-- Var Def

varDef :: { S.Def }
varDef : var '=' exp            { S.VarDef $1 $3 }

-------------------------------------------------------------------------------
-- Type Def

typeDef :: { S.Def }
typeDef : var genTypeVars '::=' constructors  { S.TypeDef $1 (reverse $2) (reverse $4) }

-- REVERSE!!
constructors :: { [S.Constr] }
constructors : constructors '|' constructor { $3 : $1 }
             | constructor                  { [$1] }

constructor :: { S.Constr } -- Constr = (String, [ConstrArg])
constructor : constr constrTypes   { ($1, reverse $2) }

-- REVERSE!!
constrTypes :: { [S.ConstrArg] }
constrTypes : constrTypes constrArg { $2 : $1 }
            | {- empty -}           { [] }

constrArg :: { S.ConstrArg } -- ConstrArg = CAVar String | CAGenTypeVar GenTypeVar
constrArg : var                 { S.CAVar $1 }
          | genTypeVar          { S.CAGenTypeVar $1 }
          | '(' constrArgs ')'  { S.CAList (reverse $2) }

-- REVERSE!!
constrArgs :: { [S.ConstrArg] }
constrArgs : constrArgs constrArg     { $2 : $1 }
           | {- empty -}              { [] }

-- REVERSE!!
genTypeVars :: { [S.GenTypeVar] }
genTypeVars : genTypeVars genTypeVar  { $2 : $1 }
            | {- empty -}             { [] }

genTypeVar :: { S.GenTypeVar } -- GenTypeVar = Int
genTypeVar : mult             { 1 }
           | gtype_2plus      { $1 }

-------------------------------------------------------------------------------
-- Expressions

exp :: { S.Exp }
exp : apply         { $1 }
    | infixApp      { $1 }
    | listLit       { $1 }
    | term          { $1 }

apply :: { S.Exp }
apply : exp term   { S.Apply $1 $2 }

infixApp :: { S.Exp }
infixApp : exp infixOp exp  { S.InfixApp $2 $1 $3 }

infixOp :: { T.InfixOp }
infixOp : plus      { getInfixOp $1 }
        | minus     { getInfixOp $1 }
        | mult      { getInfixOp $1 }
        | div       { getInfixOp $1 }
        | infix_var { getInfixOp $1 }

term :: { S.Exp }            
term : variable         { $1 }
     | constr           { S.Constructor $1 }
     | constant         { $1 }
     | '(' exp ')'      { $2 }

variable :: { S.Exp }       
variable : var               { S.Variable $1 }

constant :: { S.Exp }
constant : const             { S.Constant $1 }

-------------------------------------------------------------------------------
-- Special List Syntax

listLit :: { S.Exp }
listLit : '[' ']'             { S.ListLit [] }
        | '[' commaSepExp ']' { S.ListLit (reverse $2) }

-- REVERSE!!
commaSepExp :: { [S.Exp] }
commaSepExp : commaSepExp ',' exp   { $3 : $1 }
            | exp                   { [$1] }

{
type ParseResult = Either String

getInfixOp :: T.Token -> T.InfixOp
getInfixOp (T.InfixOp i) = i
getInfixOp tok = error $ "Not an infix op: " ++ show tok

parseError :: [T.Token] -> ParseResult a
parseError tokens = Left $ "Parse Error, tokens left: " ++ show tokens

parseDef :: String -> ParseResult S.Def
parseDef input = scanTokensEither input >>= defParser . tail . init

parseProgram :: String -> ParseResult S.Prog
parseProgram input = scanTokensEither input >>= parser
}