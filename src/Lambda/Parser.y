{
module Lambda.Parser (ParseResult, parser, parseExpression) where 

import Data.Char
import qualified Lambda.Enriched as E
import qualified Lambda.Syntax as S
import qualified Lambda.Token as T
import Lambda.Lexer (alexScanTokens, scanTokens)
}

%name parser exp
%monad { ParseResult } { >>= } { return }
%tokentype { T.Token }
%error { parseError }

%token 
  letrec   { T.Letrec }
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
  ';'     { T.Semi   }
  '{'      { T.LC     }
  '}'      { T.RC     }

%%

exp :: { E.Exp }
exp : letExpression { $1 }
    | apply         { $1 }
    | term          { $1 }

letExpression :: { E.Exp }
letExpression : let    '{' letBindings '}' in exp       { E.Let $3 $6 }
              | letrec '{' letBindings '}' in exp       { E.Letrec $3 $6 }

letBindings :: { [(String, E.Exp)] }
letBindings : letBindings ';' letBinding { $3 : $1 }
            | letBindings ';'            { $1 }
            | letBinding                 { [$1] }
            | {- empty -}                { [] }

letBinding :: { (String, E.Exp )}
letBinding : var '=' exp { ($1, $3) }

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
type ParseResult = Either String

parseError :: [T.Token] -> ParseResult a
parseError tokens = Left $ "Parse Error, tokens left: " ++ show tokens

parseExpression :: String -> ParseResult E.Exp
parseExpression = parser . scanTokens                        
}