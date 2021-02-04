module Main where

import System.Environment (getArgs)
import System.Console.Haskeline
import Text.Pretty.Simple (pPrint)
import Control.Monad.State.Lazy (liftIO)

import Lambda.Parser (parser)
import Lambda.Lexer (alexScanTokens)

main :: IO ()
main = do args <- getArgs  
          case args of 
            [] -> runInputT (defaultSettings { historyFile = Just "hjs-history" }) 
                            loop 
            (file : _) -> do content <- readFile file
                             let parsed = parser . alexScanTokens $ content
                             pPrint parsed

loop :: InputT IO ()
loop = 
  do minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just ".exit" -> return ()
       Just input -> do liftIO $ evalLambda input
                        loop

evalLambda :: String -> IO ()
evalLambda input = do let parsed = parser . alexScanTokens $ input
                      pPrint parsed
