module Main where 

import Test.Hspec 

import Lambda.Parser
import Lambda.Syntax
import Lambda.Reduce (reduce)

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
          evaled = reduce parsed 
      pShow evaled `shouldBe` "54"
    it "p10: handles explicit currying '((+ 3) 4)'" $ do 
      let parsed = parseExpression "((+ 3) 4)"
          evaled = reduce parsed 
      pShow evaled `shouldBe` "7"
    it "p12: reduces AND 'AND TRUE FALSE'" $ do 
      let parsed = parseExpression "AND TRUE FALSE"
          evaled = reduce parsed 
      pShow evaled `shouldBe` "FALSE"
    it "p12: reduces IF 'IF TRUE 1 2' && 'IF FALSE 1 2'" $ do
      pShow (reduce . parseExpression $ "IF TRUE 1 2") `shouldBe` "1"
      pShow (reduce . parseExpression $ "IF FALSE 1 2") `shouldBe` "2"
    it "p12: reduces CONS access '(CONS 1 2)'" $ do 
      pShow (reduce . parseExpression $ "HEAD (CONS 1 2)") `shouldBe` "1"
      pShow (reduce . parseExpression $ "TAIL (CONS 1 2)") `shouldBe` "2"
    it "p13: reduces lambda abstr '(\\x. + x 1) 4" $ do
      pShow (reduce . parseExpression $ "(\\x. + x 1) 4") `shouldBe` "5"