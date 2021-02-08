{
module Lambda.Lexer (alexScanTokens) where 

import qualified Lambda.Token as T
}

%wrapper "basic"

-- arithmetic functions
$plus          = \+
$minus         = \-
$mult          = \*
$div           = \/

-- logical functions
@and           = A(nd|ND)
@or            = O(r|R)
@not           = N(ot|OT)
@if            = I[fF]

-- constants
@true          = T(rue|RUE)
@false         = F(alse|ALSE)
@number        = [0-9]+
@char          = \'[a-zA-Z]\'

-- variable
@variable      = [a-z][a-zA-Z0-9']*

tokens :- 
  $white+ ;

  $plus               { \_ -> T.Function T.FPlus }
  $minus              { \_ -> T.Function T.FMinus }
  $mult               { \_ -> T.Function T.FMult }
  $div                { \_ -> T.Function T.FDiv }
  @and                { \_ -> T.Function T.FAnd }
  @or                 { \_ -> T.Function T.FOr }
  @not                { \_ -> T.Function T.FNot }
  @if                 { \_ -> T.Function T.FIf }

  @true               { \_ -> T.Constant T.CTrue }
  @false              { \_ -> T.Constant T.CFalse }
  @number             { \n -> T.Constant $ T.CNat (read n) }
  @char               { \c -> T.Constant $ T.CChar (head c) }

  @variable           { \v -> T.Variable v }
  \\                  { \_ -> T.Lambda     }
  \.                  { \_ -> T.Dot        }
  \(                  { \_ -> T.LP         }
  \)                  { \_ -> T.RP         }