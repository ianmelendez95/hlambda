{
module Lambda.Lexer (alexScanTokens, scanTokens) where 

import qualified Lambda.Token as T
}

%wrapper "monadUserState"

-- keywords
@let           = let
@in            = in

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
@cons          = C(ons|ONS)
@head          = H(ead|EAD)
@tail          = T(ail|AIL)

-- constants
@true          = T(rue|RUE)
@false         = F(alse|ALSE)
@number        = [0-9]+
@char          = \'[a-zA-Z]\'

-- variable
@variable      = [a-z][a-zA-Z0-9']*

tokens :- 
  $white+ ;

  @let                { located $ \_ -> T.Let }
  @in                 { located $ \_ -> T.In  }

  $plus               { located $ \_ -> T.Function T.FPlus }
  $minus              { located $ \_ -> T.Function T.FMinus }
  $mult               { located $ \_ -> T.Function T.FMult }
  $div                { located $ \_ -> T.Function T.FDiv }
  @and                { located $ \_ -> T.Function T.FAnd }
  @or                 { located $ \_ -> T.Function T.FOr }
  @not                { located $ \_ -> T.Function T.FNot }
  @if                 { located $ \_ -> T.Function T.FIf }
  @cons               { located $ \_ -> T.Function T.FCons }
  @head               { located $ \_ -> T.Function T.FHead }
  @tail               { located $ \_ -> T.Function T.FTail }
  \=                  { located $ \_ -> T.Equal }

  @true               { located $ \_ -> T.Constant (T.CBool True) }
  @false              { located $ \_ -> T.Constant (T.CBool False) }
  @number             { located $ \n -> T.Constant $ T.CNat (read n) }
  @char               { located $ \c -> T.Constant $ T.CChar (head c) }

  @variable           { located $ \v -> T.Variable v }
  \\                  { located $ \_ -> T.Lambda     }
  \.                  { located $ \_ -> T.Dot        }
  \(                  { located $ \_ -> T.LP         }
  \)                  { located $ \_ -> T.RP         }

{
-- %wrapper "posn"  => { ... } :: AlexPosn -> String -> token

-- %wrapper "monad" => { ... } :: AlexAction result

-- [Available]

-- type AlexInput         = (AlexPosn, Char, [Byte], String)

-- type AlexAction result = AlexInput -> Int -> Alex result

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

-- runAlex    :: String -> Alex a -> Either String a

type AlexUserState = ()
alexInitUserState = ()

data LayoutState = LNone 
                 | LStart  Int -- col
                 | LActive Int -- line

alexEOF :: Alex T.LocToken
alexEOF = return (T.LToken 0 0 T.EOF)

located :: (String -> T.Token) -> AlexAction T.LocToken
located f = token (\((AlexPn _ line col), _, _, input) len -> T.LToken line col $ f (take len input))

alexMonadScanAll :: Alex [T.LocToken]
alexMonadScanAll = do t <- alexMonadScan
                      case t of 
                        (T.LToken _ _ T.EOF) -> return []
                        _     -> do ts <- alexMonadScanAll
                                    return (t : ts)

alexScanTokens :: String -> [T.LocToken]
alexScanTokens input = case runAlex input alexMonadScanAll of 
                         Left err -> error err 
                         Right res -> res

scanTokens :: String -> [T.Token]
scanTokens = map T.locToken . alexScanTokens
}