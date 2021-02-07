{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Console.Haskeline
-- import Text.Pretty.Simple (pPrint)
import Control.Monad.State.Lazy (liftIO)

import Prettyprinter.Render.Terminal

import Lambda.FreeBound
import Lambda.Parser (parseExpression)
import Lambda.Beta (betaReduce)
import Lambda.Syntax (Exp, ansiPrettyExp)
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
evalLambda input = do let parsed = parseExpression input
                          marked = markBoundFree parsed
                          reduced = betaReduce marked
                      putStr "raw: "           >> pPrint parsed
                      putStr "marked: "        >> pPrint marked
                      putStr "beta reduced: "  >> pPrint reduced

pPrint :: Exp -> IO ()
pPrint expr = renderIO System.IO.stdout (ansiPrettyExp expr) >> putChar '\n'
                    