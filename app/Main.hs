{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Console.Haskeline
-- import Text.Pretty.Simple (pPrint)
import Control.Monad.State.Lazy (liftIO)
import Control.Exception (SomeException, catch)

import Prettyprinter.Render.Terminal

import Lambda.FreeBound
import Lambda.Lexer (alexScanTokens)
import Lambda.Parser (parser)
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Reduce (reduce)
import Lambda.Eval (eval)
import qualified System.IO

main :: IO ()
main = do args <- getArgs  
          case args of 
            [] -> runInputT (defaultSettings { historyFile = Just "hjs-history" }) 
                            loop 
            (file : _) -> do content <- readFile file
                             evalLambda content

loop :: InputT IO ()
loop = 
  do minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just ".exit" -> return ()
       Just input -> do liftIO $ evalLambda input
                        loop

evalLambda :: String -> IO ()
evalLambda input = do let tokens = alexScanTokens input
                          parsed = parser tokens
                          marked = markBoundFree parsed
                          reduced = reduce marked
                          evaled = eval marked
                      putStr "tokens: "        >> catchAll (print tokens)
                      putStr "parsed: "        >> catchAll (print parsed)
                      putStr "raw: "           >> catchAll (pPrint parsed)
                      putStr "marked: "        >> catchAll (pPrint marked)
                      putStr "beta reduced: "  >> catchAll (pPrint reduced)
                      putStr "evaled: "        >> catchAll (pPrint evaled)

catchAll :: IO () -> IO ()
catchAll io = catch io (print :: SomeException -> IO ())

pPrint :: PrettyLambda a => a -> IO ()
pPrint expr = renderIO System.IO.stdout (ansiPrettyDoc expr) >> putChar '\n'
                    