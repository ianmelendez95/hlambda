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
  const       { T.Constant $$    }
  constr      { T.Constructor $$ }
  var         { T.Variable $$    }
  infixOp     { T.InfixOp  $$    }
  '='         { T.Equal   }
  typeeq      { T.TypeEq  }
  '('         { T.LP      }
  ')'         { T.RP      }
  ';'         { T.Semi    }
  '{'         { T.LC      }
  '}'         { T.RC      }
  '|'         { T.VertBar }

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
typeDef : var typeeq constructors  { S.TypeDef $1 (reverse $3) }

-- REVERSE!!
constructors :: { [(String, [String])] }
constructors : constructors '|' constructor { $3 : $1 }
             | constructor                  { [$1] }

constructor :: { (String, [String]) }
constructor : constr constrTypes   { ($1, reverse $2) }

-- REVERSE!!
constrTypes :: { [String] }
constrTypes : constrTypes var    { $2 : $1 }
            | {- empty -}        { [] }

-------------------------------------------------------------------------------
-- Expressions

exp :: { S.Exp }
exp : apply         { $1 }
    | infixApp      { $1 }
    | term          { $1 }

apply :: { S.Exp }
apply : exp term   { S.Apply $1 $2 }

infixApp :: { S.Exp }
infixApp : exp infixOp exp  { S.InfixApp $2 $1 $3 }

term :: { S.Exp }            
term : variable         { $1 }
     | constr           { S.Constructor $1 }
     | constant         { $1 }
     | '(' exp ')'      { $2 }

variable :: { S.Exp }       
variable : var               { S.Variable $1 }

constant :: { S.Exp }
constant : const             { S.Constant $1 }

{
type ParseResult = Either String

parseError :: [T.Token] -> ParseResult a
parseError tokens = Left $ "Parse Error, tokens left: " ++ show tokens

parseDef :: String -> ParseResult S.Def
parseDef input = scanTokensEither input >>= defParser . tail . init

parseProgram :: String -> ParseResult S.Prog
parseProgram input = scanTokensEither input >>= parser
}