{
module Lambda.Parser (parser) where 

import Data.Char
import qualified Lambda.Syntax as S
import qualified Lambda.Token as T
}

%name parser 
%tokentype { T.Token }
%error { parseError }

%token 
  constant { T.Constant $$ }
  variable { T.Variable $$ }
  '\\'     { T.Lambda }
  '.'      { T.Dot }
  '('      { T.LP }
  ')'      { T.RP }

%%

exp :: { S.Exp }
exp : '\\' variable '.' exp { S.Lambda $2 $4 }
    | exp term              { S.Apply $1 $2 }
    | term                  { $1 }

term :: { S.Exp }    
     : constant              { S.Constant $1 }
     | variable              { S.Variable $1 }
     | '(' exp ')'           { $2 }
     

{
parseError :: [T.Token] -> a
parseError tokens = error $ "Parse Error: " ++ show tokens
}