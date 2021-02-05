{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Console.Haskeline
-- import Text.Pretty.Simple (pPrint)
import Control.Monad.State.Lazy (liftIO)

import Prettyprinter
import Prettyprinter.Util (putDocW)

import Lambda.FreeBound
import Lambda.Parser (parser)
import Lambda.Lexer (alexScanTokens)
import Lambda.Beta (betaReduce)
import Lambda.Syntax (prettyExp)

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
evalLambda input = do let parsed = parser . alexScanTokens $ input
                          marked = markBoundFree parsed
                          reduced = betaReduce marked
                      putStr "raw: "           >> print parsed
                      putStr "marked: "        >> print marked
                      putStr "beta reduced: "  >> print (betaReduce marked)
                      putStrLn "pretty: "
                      putDocW 80 $ prettyExp marked
                      putStrLn ""
                    
testPretty :: IO ()
testPretty = do let prettyType :: [Doc ()] -> Doc ()
                    prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
                    prettyDecl n tys = pretty n <+> prettyType tys
                    doc = prettyDecl ("example" :: String) ["Int" :: Doc (), "Bool", "Char", "IO ()"]
                putDocW 20 doc
                putStrLn ""