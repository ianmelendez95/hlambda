{
module Lambda.Parser (parser, parseString) where 

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
exp : '\\' var '.' exp        { S.Lambda $2 $4 }
    | apply                   { $1 }
    | term                    { $1 }

term :: { S.Exp }    
     : constant              { $1 }
     | functionTerm          { $1 }

functionTerm :: { S.Exp }     
functionTerm : function    { $1 }
             | variable    { $1 }
             | '(' exp ')' { $2 }

apply :: { S.Exp }     
apply : functionTerm exp     { S.Apply $1 $2 } 

variable :: { S.Exp }       
variable : var               { S.Variable (S.RawVar $1) }

function :: { S.Exp }
function : func              { S.fromFunctionToken $1 }

constant :: { S.Exp }
constant : const             { S.fromConstantToken $1 }

{
parseError :: [T.Token] -> a
parseError tokens = error $ "Parse Error: " ++ show tokens

parseString :: String -> S.Exp
parseString = parser . alexScanTokens
}