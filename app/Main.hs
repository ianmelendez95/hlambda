{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Console.Haskeline
-- import Text.Pretty.Simple (pPrint)
import Control.Monad.State.Lazy (liftIO)

import Prettyprinter.Render.Terminal

import Lambda.FreeBound
import Lambda.Lexer (alexScanTokens)
import Lambda.Parser (parser)
import Lambda.Beta (betaReduce)
import Lambda.Syntax (Exp, ansiPrettyExp)
import Lambda.Eval (evalAfterBeta)
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
                          reduced = betaReduce marked
                          evaled = evalAfterBeta reduced
                      putStr "tokens: "        >> print tokens
                      putStr "parsed: "        >> print parsed
                      putStr "raw: "           >> pPrint parsed
                      putStr "marked: "        >> pPrint marked
                      putStr "beta reduced: "  >> pPrint reduced
                      putStr "evaled: "        >> pPrint evaled

pPrint :: Exp -> IO ()
pPrint expr = renderIO System.IO.stdout (ansiPrettyExp expr) >> putChar '\n'
                    