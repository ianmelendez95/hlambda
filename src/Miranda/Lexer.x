{
module Miranda.Lexer (alexScanTokens, scanTokens, scanTokensEither) where 

import qualified Miranda.Token as T
}

%wrapper "monadUserState"

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

-- identifiers
@variable      = [a-z][a-zA-Z0-9']*
@constructor   = [A-Z][a-zA-Z0-9']*
@typeeq        = \:\:\=
@infix_var     = \$@variable
@gentypevar    = \*\*+   -- single asterisk consumed by $mult

-- layout
$semi          = \;

tokens :- 
  $white+ ;

  \=                  { located $ \_ -> T.Equal }
  @typeeq             { located $ \_ -> T.TypeEq }

  $plus               { located $ \_ -> T.InfixOp T.IPlus }
  $minus              { located $ \_ -> T.InfixOp T.IMinus }
  $mult               { located $ \_ -> T.InfixOp T.IMult }
  $div                { located $ \_ -> T.InfixOp T.IDiv }

  @number             { located $ \n -> T.Constant $ T.CNat (read n) }
  @char               { located $ \c -> T.Constant $ T.CChar (head c) }

  @variable           { located $ \v -> T.Variable v    }
  @constructor        { located $ \c -> T.Constructor c }
  @infix_var          { located $ \v -> T.InfixOp . T.IVar $ tail v }
  @gentypevar         { located $ \v -> T.GenTypeVar (length v) }

  \(                  { located $ \_ -> T.LP         }
  \)                  { located $ \_ -> T.RP         }
  \|                  { located $ \_ -> T.VertBar    }

  $semi               { located $ \_ -> T.Semi       }

{
-- %wrapper "posn"  => { ... } :: AlexPosn -> String -> token

-- %wrapper "monad" => { ... } :: AlexAction result

-- [Available]

-- type AlexInput         = (AlexPosn, Char, [Byte], String)

-- type AlexAction result = AlexInput -> Int -> Alex result

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

-- runAlex    :: String -> Alex a -> Either String a

type AlexUserState = LayoutState

data LayoutState = LNone 
                 | LActive Int Int -- line, col

alexInitUserState = LNone

alexEOF :: Alex T.LocToken
alexEOF = return (T.LToken 0 0 T.EOF)

located :: (String -> T.Token) -> AlexAction T.LocToken
located f = token (\((AlexPn _ line col), _, _, input) len -> T.LToken line col $ f (take len input))

getLayoutState :: Alex LayoutState
getLayoutState = Alex $ \s@AlexState{alex_ust=layout_state} -> 
  Right (s, layout_state)

putLayoutState :: LayoutState -> Alex ()
putLayoutState new_layout_state = Alex $ \s -> Right (s{alex_ust=new_layout_state}, ())

alexMonadScanAll :: Alex [T.LocToken]
alexMonadScanAll = 
  do lt@(T.LToken l c tok) <- alexMonadScan
     layout <- getLayoutState
     case tok of 
       T.EOF -> 
         case layout of 
           LNone       -> return []
           LActive _ _ -> return [T.LToken l c T.RC]
       _  -> let mkTok = T.LToken l c in
         case layout of 
           LActive lay_line lay_col
             | l <  lay_line -> alexError "Illegal state: current line before layout line"
             | l == lay_line -> (lt :) <$> alexMonadScanAll
             | c <  lay_col  -> alexError "Layout: token left of layout"
             | c == lay_col  -> ([mkTok T.Semi, lt] ++) <$> alexMonadScanAll
             | otherwise     -> (lt :) <$> alexMonadScanAll
           LNone -> do putLayoutState (LActive l c)
                       ([mkTok T.LC, lt] ++) <$> alexMonadScanAll

alexScanTokens :: String -> [T.LocToken]
alexScanTokens input = case runAlex input alexMonadScanAll of 
                         Left err -> error err 
                         Right res -> res

scanTokensEither :: String -> Either String [T.Token]
scanTokensEither input = map T.locToken <$> runAlex input alexMonadScanAll

scanTokens :: String -> [T.Token]
scanTokens = map T.locToken . alexScanTokens
}