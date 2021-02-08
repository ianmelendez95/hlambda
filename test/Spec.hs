module Main where 

import Test.Hspec 
import Debug.Trace (trace)

import Lambda.Parser
import Lambda.Lexer
import Lambda.Syntax

main :: IO ()
main = hspec $ do 
  describe "Raw Parsing" $ do 
    it "p9: parses '(+ 4 5)'" $ do 
      let parsed = parseExpression "(+ 4 5)"
      pShow parsed `shouldBe` "+ 4 5"
    it "p10: parses '(+ (* 5 6) (* 8 3))'" $ do 
      let parsed = parseExpression "(+ (* 5 6) (* 8 3))"
      print parsed >> putChar '\n'
      pShow parsed `shouldBe` "+ (* 5 6) (* 8 3)"
