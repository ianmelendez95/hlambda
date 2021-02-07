module Main where 

import Test.Hspec 
import Debug.Trace (trace)

import Lambda.Parser
import Lambda.Lexer
import Lambda.Syntax

main :: IO ()
main = hspec 
  $ do describe "Raw Parsing"
     $ do it "p9: parses '(+ 4 5)'"
       $ do let parsed = trace (show $alexScanTokens "(+ 4 5)") $ parseExpression "(+ 4 5)"
            show parsed `shouldBe` "+ 4 5"