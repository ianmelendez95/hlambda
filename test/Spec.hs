{-# LANGUAGE QuasiQuotes #-}

module Main where 

import Test.Hspec 
import Text.RawString.QQ(r)

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

    it "p10: reduces '(+ (* 5 6) (* 8 3))'" $ do 
      showReduced "(+ (* 5 6) (* 8 3))" `shouldBe` "54"

    it "p10: reduces explicit currying '((+ 3) 4)'" $ do 
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

    it "p14: identifies bound and free" $ do
      showMarked' "(\\x. + x y) 4" `shouldBe` "(\\x:b. + x:b y:f) 4"
      showMarked' "\\x. + ((\\y. + y z) 7) x" `shouldBe` "\\x:b. + ((\\y:b. + y:b z:f) 7) x:b"
      showMarked' "+ x ((\\x. + x 1) 4)" `shouldBe` "+ x:f ((\\x:b. + x:b 1) 4)"

    it "p15: reduces simple lambda" $ do 
      showReduced "(\\x. + x 1) 4" `shouldBe` "5"

    it "p16: reduces multiple occurrence lambda" $ do
      showReduced "(\\x. + x x) 5" `shouldBe` "10"

    it "p16: reduces nested lambdas" $ do
      showReduced "(\\x.(\\y. - y x)) 4 5" `shouldBe` "1"

    it "p16: reduces lambda func app" $ do 
      showReduced "(\\f. f 3) (\\x. + x 1)" `shouldBe` "4"

    it "p17: accounts for nested lambda var conflicts" $ do 
      showReduced "(\\x.(\\x. + (- x 1)) x 3) 9" `shouldBe` "11"
      showReduced "(\\x.(\\x. x)) 1 2" `shouldBe` "2"

    it "p17: purely functional cons" $ do 
      -- HEAD (CONS p q) = CONS p q (\a.\b.a) = (\a.\b.\f. f a b) p q (\a.\b.a) 
      showReduced [r|(\a.\b.\f. f a b) p q (\a.\b.a)|] `shouldBe` "p"

    it "p19: eta reduces simple function application" $ do 
      showReduced "(\\x. + 1 x)" `shouldBe` "+ 1"

  where 
    showParsed = pShow . parseExpression
    showReduced = pShow . reduce . parseExpression
    showMarked' = showMarked . markBoundFree . parseExpression