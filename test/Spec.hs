module Main where 

import Test.Hspec 

import Lambda.Parser ( parseExpression )
import Lambda.Syntax ( pShow, showMarked )
import Lambda.FreeBound (markBoundFree)
import Lambda.Reduce (reduce)

main :: IO ()
main = hspec $ do 
  describe "Raw Parsing" $ do 

    it "p9: parses '(+ 4 5)'" $ do 
      showParsed "(+ 4 5)" `shouldBe` "+ 4 5"

    it "p10: parses '(+ (* 5 6) (* 8 3))'" $ do 
      showParsed "(+ (* 5 6) (* 8 3))" `shouldBe` "+ (* 5 6) (* 8 3)"

    it "p10: evaluates '(+ (* 5 6) (* 8 3))'" $ do 
      showReduced "(+ (* 5 6) (* 8 3))" `shouldBe` "54"

    it "p10: handles explicit currying '((+ 3) 4)'" $ do 
      showReduced "((+ 3) 4)" `shouldBe` "7"

    it "p12: reduces AND 'AND TRUE FALSE'" $ do 
      showReduced "AND TRUE FALSE" `shouldBe` "FALSE"

    it "p12: reduces IF 'IF TRUE 1 2' && 'IF FALSE 1 2'" $ do
      showReduced "IF TRUE 1 2" `shouldBe` "1"
      showReduced "IF FALSE 1 2" `shouldBe` "2"

    it "p12: reduces CONS access '(CONS 1 2)'" $ do 
      showReduced "HEAD (CONS 1 2)" `shouldBe` "1"
      showReduced "TAIL (CONS 1 2)" `shouldBe` "2"

    it "p13: reduces lambda abstr '(\\x. + x 1) 4" $ do
      showReduced "(\\x. + x 1) 4" `shouldBe` "5"

    it "p14: identifies bound and free '(\\x. + x y) 4'" $ do
      showMarked' "(\\x. + x y) 4" `shouldBe` "(\\x:b. + x:b y:f) 4"

  where 
    showParsed = pShow . parseExpression
    showReduced = pShow . reduce . parseExpression
    showMarked' = showMarked . markBoundFree . parseExpression