{
module Lambda.Parser (parser, parseExpression) where 

import Data.Char
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S
import qualified Lambda.Token as T
import Lambda.Lexer (alexScanTokens)
}

%name parser 
%tokentype { T.Token }
%error { parseError }

%token 
  let      { T.Let }
  in       { T.In  }
  const    { T.Constant $$ }
  var      { T.Variable $$ }
  func     { T.Function $$ }
  '='      { T.Equal  }
  '\\'     { T.Lambda }
  '.'      { T.Dot    }
  '('      { T.LP     }
  ')'      { T.RP     }

%%

exp :: { E.Exp }
exp : letExpression { $1 }
    | apply         { $1 }
    | term          { $1 }

letExpression :: { E.Exp }
letExpression : let var '=' exp in exp   { E.Let $2 $4 $6 }

apply :: { E.Exp }
apply : exp term   { E.Apply $1 $2 }

term :: { E.Exp }            
term : function         { E.Pure $1 }
     | variable         { E.Pure $1 }
     | constant         { E.Pure $1 }
     | lambda           { $1 }
     | '(' exp ')'      { $2 }

lambda :: { E.Exp }
lambda : '\\' var '.' exp    { E.Lambda $2 $4 }

function :: { S.Exp }
function : func              { S.fromFunctionToken $1 }
         | '='               { S.Function S.FEq       }

variable :: { S.Exp }       
variable : var               { S.Variable (S.RawVar $1) }

constant :: { S.Exp }
constant : const             { S.fromConstantToken $1 }

{
parseError :: [T.Token] -> a
parseError tokens = error $ "Parse Error: " ++ show tokens

parseExpression :: String -> E.Exp
parseExpression = parser . alexScanTokens
}