{
module Lambda.Parser (parser, parseExpression) where 

import Data.Char
import qualified Lambda.Syntax as S
import qualified Lambda.Token as T
import Lambda.Lexer (alexScanTokens)
}

%name parser 
%tokentype { T.Token }
%error { parseError }

%token 
  const    { T.Constant $$ }
  var      { T.Variable $$ }
  func     { T.Function $$ }
  '\\'     { T.Lambda }
  '.'      { T.Dot }
  '('      { T.LP }
  ')'      { T.RP }

%%

exp :: { S.Exp }
exp : term      { $1 }
    | apply     { $1 }

apply :: { S.Exp }
apply : exp term   { S.Apply $1 $2}

term :: { S.Exp }            
term : function    { $1 }
     | variable    { $1 }
     | constant    { $1 }
     | lambda      { $1 }
     | '(' exp ')' { $2 }

lambda :: { S.Exp }
lambda : '\\' var '.' exp    { S.Lambda $2 $4 }

variable :: { S.Exp }       
variable : var               { S.Variable (S.RawVar $1) }

function :: { S.Exp }
function : func              { S.fromFunctionToken $1 }

constant :: { S.Exp }
constant : const             { S.fromConstantToken $1 }

{
parseError :: [T.Token] -> a
parseError tokens = error $ "Parse Error: " ++ show tokens

parseExpression :: String -> S.Exp
parseExpression = parser . alexScanTokens
}