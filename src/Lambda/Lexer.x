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

data AlexUserState = UserState { 
   user_layout_state :: LayoutState,
   user_m_next_token :: Maybe T.LocToken
 }

data LayoutState = LNone 
                 | LStart  Int -- col
                 | LActive Int -- line

alexInitUserState = UserState LNone Nothing

alexEOF :: Alex T.LocToken
alexEOF = return (T.LToken 0 0 T.EOF)

located :: (String -> T.Token) -> AlexAction T.LocToken
located f = token (\((AlexPn _ line col), _, _, input) len -> T.LToken line col $ f (take len input))

getLayoutState :: Alex LayoutState
getLayoutState = Alex $ \s@AlexState{alex_ust=(UserState layout_state _)} 
  -> Right (s, layout_state)

getMNextToken :: Alex (Maybe T.LocToken)
getMNextToken = Alex $ \s@AlexState{alex_ust=(UserState _ next_token)} 
  -> Right (s, next_token)

putMNextToken :: T.LocToken -> Alex ()
putMNextToken next_token = Alex $ \s@AlexState{alex_ust=ust} 
  -> Right (s{alex_ust=(putInUserState ust)}, ())
  where 
    putInUserState :: AlexUserState -> AlexUserState 
    putInUserState (UserState l Nothing) = UserState l (Just next_token)
    putInUserState (UserState _ (Just t)) = error $ "Token already in 'next token': " ++ show t

alexMonadScanAll :: Alex [T.LocToken]
alexMonadScanAll 
  = do nt <- getMNextToken
       case nt of 
         Just t -> (t:) <$> alexMonadScanAll
         Nothing -> do t <- alexMonadScan
                       layout <- getLayoutState
                       case t of 
                         (T.LToken l c T.EOF) -> case layout of 
                                                   LStart _  -> alexError "EOF: Expecting layout start token"
                                                   LNone     -> return []
                                                   LActive _ -> return [T.LToken l c T.RC]
                         (T.LToken _ _ T.Let) -> case layout of 
                                                   LNone -> undefined -- TODO: do dis
                                                   LStart _ -> alexError "Layout: 'let' within layout bounds"
                                                   LActive _ -> undefined -- TODO: do dis
                         _     -> do ts <- alexMonadScanAll
                                     return (t : ts)

alexScanTokens :: String -> [T.LocToken]
alexScanTokens input = case runAlex input alexMonadScanAll of 
                         Left err -> error err 
                         Right res -> res

scanTokens :: String -> [T.Token]
scanTokens = map T.locToken . alexScanTokens
}