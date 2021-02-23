{
module Miranda.Lexer (alexScanTokens, scanTokens, scanTokensEither) where 

import qualified Miranda.Token as T
}

%wrapper "monadUserState"

-- keywords
@where         = where

-- arithmetic functions
$plus          = \+
$minus         = \-
$mult          = \*
$div           = \/
@equal         = \=\=
$lt            = \<
$gt            = \>

-- logical functions
@and           = A(nd|ND)
@or            = O(r|R)
@not           = N(ot|OT)
@if            = I[fF]
@cons          = C(ons|ONS)
@head          = H(ead|EAD)
@tail          = T(ail|AIL)

-- constants
-- @true          = T(rue|RUE)
-- @false         = F(alse|ALSE)
@number        = [0-9]+
@char          = \'[a-zA-Z]\'

-- identifiers
@variable      = [a-z][a-zA-Z0-9']*
@constructor   = [A-Z][a-zA-Z0-9']*
@typeeq        = \:\:\=
@infix_var     = \$@variable
@gentypevar    = \*\*+   -- single asterisk consumed by $mult

tokens :- 
  $white+ ;

  @where              { located $ \_ -> T.Where }

  \=                  { located $ \_ -> T.Equal }
  @typeeq             { located $ \_ -> T.TypeEq }

  $plus               { located $ \_ -> T.InfixOp T.IPlus }
  $minus              { located $ \_ -> T.InfixOp T.IMinus }
  $mult               { located $ \_ -> T.InfixOp T.IMult }
  $div                { located $ \_ -> T.InfixOp T.IDiv }
  @equal              { located $ \_ -> T.InfixOp T.IEq }
  $lt                 { located $ \_ -> T.InfixOp T.ILt }
  $gt                 { located $ \_ -> T.InfixOp T.IGt }

  @number             { located $ \n -> T.Constant $ T.CNat (read n) }
  @char               { located $ \c -> T.Constant $ T.CChar (c !! 1) }

  @variable           { located $ \v -> T.Variable v    }
  @constructor        { located $ \c -> T.Constructor c }
  @infix_var          { located $ \v -> T.InfixOp . T.IVar $ tail v }
  @gentypevar         { located $ \v -> T.GenTypeVar (length v) }

  \(                  { located $ \_ -> T.LP         }
  \)                  { located $ \_ -> T.RP         }
  \[                  { located $ \_ -> T.LB         }
  \]                  { located $ \_ -> T.RB         }
  \|                  { located $ \_ -> T.VertBar    }
  \,                  { located $ \_ -> T.Comma      }
  \;                  { located $ \_ -> T.Semi       }
  \:                  { located $ \_ -> T.InfixOp T.ICons }

{
-- %wrapper "posn"  => { ... } :: AlexPosn -> String -> token

-- %wrapper "monad" => { ... } :: AlexAction result

-- [Available]

-- type AlexInput         = (AlexPosn, Char, [Byte], String)

-- type AlexAction result = AlexInput -> Int -> Alex result

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

-- runAlex    :: String -> Alex a -> Either String a

type AlexUserState = [LayoutState] -- layout state is now a stack of layout states

data LayoutState = LStart          -- starting a layout context, waiting for first token
                 | LActive Int Int -- line, col

alexInitUserState = [LStart]

alexEOF :: Alex T.LocToken
alexEOF = return (T.LToken 0 0 T.EOF)

located :: (String -> T.Token) -> AlexAction T.LocToken
located f = token (\((AlexPn _ line col), _, _, input) len -> T.LToken line col $ f (take len input))

getLayoutState :: Alex LayoutState
getLayoutState = Alex $ \s@AlexState{alex_ust=layout_states} -> 
  if null layout_states 
    then Left "Empty layout_states" 
    else return $ (s, head layout_states) 

putLayoutState :: LayoutState -> Alex ()
putLayoutState new_layout_state = Alex $ \s@AlexState{alex_ust=layout_states} -> 
  Right (s{alex_ust=(setHead new_layout_state layout_states)}, ())

pushLayoutState :: LayoutState -> Alex ()
pushLayoutState new_layout_state = Alex $ \s@AlexState{alex_ust=layout_states} -> 
  Right (s{alex_ust=(new_layout_state : layout_states)}, ())

popLayoutState :: Alex ()
popLayoutState = Alex $ \s@AlexState{alex_ust=layout_states} -> 
  if null layout_states 
    then Left "Layout error, cannot pop layout state"
    else return (s{alex_ust=(tail layout_states)}, ())

setHead :: a -> [a] -> [a]
setHead x [] = [x]
setHead x (_:xs) = (x:xs)

alexMonadScanAll :: Alex [T.LocToken]
alexMonadScanAll = handleLayout =<< alexMonadScan

handleLayout :: T.LocToken -> Alex [T.LocToken]
handleLayout lt@(T.LToken l c tok) = 
  do layout <- getLayoutState
     let mkTok = T.LToken l c
     case tok of 
       T.EOF -> 
         case layout of 
           LStart      -> alexError "Layout error, expecting token but EOF"
           LActive _ _ -> return [T.LToken l c T.RC]
       T.Where -> -- upon where, push a new LStart onto the layout stack
         do pushLayoutState LStart
            (lt :) <$> alexMonadScanAll
       _  ->
         case layout of 
           LActive lay_line lay_col
             | l <  lay_line -> alexError "Illegal state: current line before layout line"
             | l == lay_line -> (lt :) <$> alexMonadScanAll
             | c <  lay_col  -> -- end of current layout
               do popLayoutState
                  (mkTok T.RC :) <$> handleLayout lt
             | c == lay_col  -> ([mkTok T.Semi, lt] ++) <$> alexMonadScanAll
             | otherwise     -> (lt :) <$> alexMonadScanAll
           LStart -> do putLayoutState (LActive l c)
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