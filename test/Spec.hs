module Main where 

import Test.Hspec 

import Lambda.Parser
import Lambda.Syntax
import Lambda.Eval

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
    it "p10: evaluates '(+ (* 5 6) (* 8 3))'" $ do 
      let parsed = parseExpression "(+ (* 5 6) (* 8 3))"
          evaled = evalRaw parsed 
      pShow evaled `shouldBe` "54"
    it "p10: handles explicit currying '((+ 3) 4)'" $ do 
      let parsed = parseExpression "((+ 3) 4)"
          evaled = evalRaw parsed 
      pShow evaled `shouldBe` "7"