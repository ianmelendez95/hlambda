{
module Lambda.Lexer (alexScanTokens, scanTokens) where 

import qualified Lambda.Token as T

}

%wrapper "monadUserState"

-- keywords
@letrec        = letrec
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

-- the Y Combinator (!!)
$ycomb         = Y

-- constants
@true          = T(rue|RUE)
@false         = F(alse|ALSE)
@number        = [0-9]+
@char          = \'[a-zA-Z]\'

-- variable
@variable      = [a-z][a-zA-Z0-9']*

-- layout
$semi          = \;

tokens :- 
  $white+ ;

  @letrec             { located $ \_ -> T.Letrec }
  @let                { located $ \_ -> T.Let    }
  @in                 { located $ \_ -> T.In     }

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
  $ycomb              { located $ \_ -> T.Function T.FY}
  \=                  { located $ \_ -> T.Equal }

  @true               { located $ \_ -> T.Constant (T.CBool True) }
  @false              { located $ \_ -> T.Constant (T.CBool False) }
  @number             { located $ \n -> T.Constant $ T.CNat (read n) }
  @char               { located $ \c -> T.Constant $ T.CChar (c !! 1) }

  @variable           { located $ \v -> T.Variable v }
  \\                  { located $ \_ -> T.Lambda     }
  \.                  { located $ \_ -> T.Dot        }
  \(                  { located $ \_ -> T.LP         }
  \)                  { located $ \_ -> T.RP         }

  $semi               { located $ \_ -> T.Semi       }

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
                 | LStart  Int Int -- line, col
                 | LActive Int Int -- line, col

alexInitUserState = UserState LNone Nothing

alexEOF :: Alex T.LocToken
alexEOF = return (T.LToken 0 0 T.EOF)

located :: (String -> T.Token) -> AlexAction T.LocToken
located f = token (\((AlexPn _ line col), _, _, input) len -> T.LToken line col $ f (take len input))

getLayoutState :: Alex LayoutState
getLayoutState = Alex $ \s@AlexState{alex_ust=(UserState layout_state _)} 
  -> Right (s, layout_state)

putLayoutState :: LayoutState -> Alex ()
putLayoutState layout_state = Alex $ \s@AlexState{alex_ust=ust} 
  -> Right (s{alex_ust=(putInUserState ust)}, ())
  where 
    -- TODO: perform check on update
    putInUserState :: AlexUserState -> AlexUserState
    putInUserState (UserState _ next_token) = UserState layout_state next_token

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
         Nothing -> 
           do t <- alexMonadScan
              layout <- getLayoutState
              case t of 
                (T.LToken l c T.EOF) -> 
                  case layout of 
                    LStart _ _  -> alexError "EOF: Expecting layout start token"
                    LNone       -> return []
                    LActive _ _ -> return [T.LToken l c T.RC]
                lt@(T.LToken l c T.Let) -> 
                  case layout of 
                    LNone       -> do putLayoutState (LStart l c)
                                      (lt:) <$> alexMonadScanAll
                    LStart _ _  -> alexError "Layout: 'let' within layout bounds"
                    LActive _ _ -> alexError "Layout: 'let' within layout bounds"
                lt@(T.LToken l c T.Letrec) -> 
                  case layout of 
                    LNone       -> do putLayoutState (LStart l c)
                                      (lt:) <$> alexMonadScanAll
                    LStart _ _  -> alexError "Layout: 'letrec' within layout bounds"
                    LActive _ _ -> alexError "Layout: 'letrec' within layout bounds"
                lt@(T.LToken l c T.In) -> let mkTok = T.LToken l c in
                  case layout of 
                    LActive _ _ -> do putLayoutState LNone
                                      ([mkTok T.RC, lt] ++) <$> alexMonadScanAll
                    LStart  _ _ -> do putLayoutState LNone
                                      ([mkTok T.LC, mkTok T.RC] ++) <$> alexMonadScanAll
                    LNone       -> alexError "Layout: 'in' without preceding 'let'"
                lt@(T.LToken l c T.Semi) -> let mkTok = T.LToken l c in
                  case layout of 
                    LNone       -> alexError "Layout: ';' outside of layout context"
                    _           -> (lt :) <$> alexMonadScanAll
                lt@(T.LToken l c _)  -> let mkTok = T.LToken l c in
                  case layout of 
                    LStart  _ _ -> do putLayoutState (LActive l c)
                                      ([mkTok T.LC, lt] ++) <$> alexMonadScanAll
                    LActive lay_line lay_col
                      | l <  lay_line -> alexError "Illegal state: current line before layout line"
                      | l == lay_line -> (lt :) <$> alexMonadScanAll
                      | c <  lay_col  -> alexError "Layout: token left of layout"
                      | c == lay_col  -> ([mkTok T.Semi, lt] ++) <$> alexMonadScanAll
                      | otherwise     -> (lt :) <$> alexMonadScanAll
                    LNone       -> (lt :) <$> alexMonadScanAll

alexScanTokens :: String -> [T.LocToken]
alexScanTokens input = case runAlex input alexMonadScanAll of 
                         Left err -> error err 
                         Right res -> res

scanTokens :: String -> [T.Token]
scanTokens = map T.locToken . alexScanTokens
}