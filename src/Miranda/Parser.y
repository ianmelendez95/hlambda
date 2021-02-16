{
module Miranda.Parser 
  ( ParseResult, 
    parser, 
    funcDefParser,
    parseProgram
  ) where 

import Data.Char
import qualified Miranda.Syntax as S
import qualified Miranda.Token as T
import Miranda.Lexer (alexScanTokens, scanTokens, scanTokensEither)
}

%name parser program
%name funcDefParser funcDef
%monad { ParseResult } { >>= } { return }
%tokentype { T.Token }
%error { parseError }

%token 
  const    { T.Constant $$ }
  var      { T.Variable $$ }
  infixOp  { T.InfixOp  $$ }
  '='      { T.Equal  }
  '('      { T.LP     }
  ')'      { T.RP     }
  ';'      { T.Semi   }
  '{'      { T.LC     }
  '}'      { T.RC     }

%%

program :: { S.Prog }
program : '{' exp '}'          { S.Prog [] $2 }
        | '{' defs ';' exp '}' { S.Prog (reverse $2) $4}

defs :: { [S.Def] }
defs : defs ';' def    { $3 : $1 }
     | def             { [$1] }

def :: { S.Def }
def : funcDef { $1 }
    | varDef  { $1 }

funcDef :: { S.Def }
funcDef : var funcParams '=' exp    { S.FuncDef $1 (reverse $2) $4 }

-- funcParams have *at least one* variable (otherwise it would be a var definition)
funcParams :: { [String] }
funcParams : funcParams var     { $2 : $1 }
           | var                { [$1] }

varDef :: { S.Def }
varDef : var '=' exp            { S.VarDef $1 $3 }

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

parseProgram :: String -> ParseResult S.Prog
parseProgram input = scanTokensEither input >>= parser
}