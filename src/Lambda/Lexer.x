{
module Lambda.Lexer (alexScanTokens) where 

import qualified Lambda.Token as T
}

%wrapper "basic"

@alphaconstant = [A-Z][a-zA-Z0-9']*
@number        = [0-9]+
@symbol        = [\+\-\*\/]
@variable      = [a-z][a-zA-Z0-9']*

tokens :- 
  $white+ ;
  @alphaconstant      { \c -> T.Constant c }
  @number             { \n -> T.Constant n }
  @symbol             { \s -> T.Constant s }
  @variable           { \v -> T.Variable v }
  \\                  { \_ -> T.Lambda     }
  \.                  { \_ -> T.Dot        }
  \(                  { \_ -> T.LP         }
  \)                  { \_ -> T.RP         }