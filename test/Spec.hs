{-# LANGUAGE QuasiQuotes #-}

module Main where 

import Test.Hspec 
import Test.HUnit.Base (assertFailure)
import Text.RawString.QQ(r)

import Parse
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Reduce (reduce)
import qualified Lambda.Enriched as E (Exp (..))
import qualified Miranda.Syntax as M (Prog (..), Def)

main :: IO ()
main = hspec $ do 
  describe "<= 2.4" $ do 

    -- -- retiring parse checks, later attempts at reduction
    -- -- clearly exercise this functionality
    -- it "p9: parses '(+ 4 5)'" $ do 
    --   showParsed "(+ 4 5)" `ioShouldBe` "+ 4 5"
    -- it "p10: parses '(+ (* 5 6) (* 8 3))'" $ do 
    --   showParsed "(+ (* 5 6) (* 8 3))" `ioShouldBe` "+ (* 5 6) (* 8 3)"

    it "p10: reduces '(+ (* 5 6) (* 8 3))'" $ do 
      showReducedEnriched "(+ (* 5 6) (* 8 3))" `ioShouldBe` "54"

    it "p10: reduces explicit currying '((+ 3) 4)'" $ do 
      showReducedEnriched "((+ 3) 4)" `ioShouldBe` "7"

    it "p12: reduces AND 'AND TRUE FALSE'" $ do 
      showReducedEnriched "AND TRUE FALSE" `ioShouldBe` "FALSE"

    it "p12: reduces IF 'IF TRUE 1 2' && 'IF FALSE 1 2'" $ do
      showReducedEnriched "IF TRUE 1 2" `ioShouldBe` "1"
      showReducedEnriched "IF FALSE 1 2" `ioShouldBe` "2"

    it "p12: reduces CONS access '(CONS 1 2)'" $ do 
      showReducedEnriched "HEAD (CONS 1 2)" `ioShouldBe` "1"
      showReducedEnriched "TAIL (CONS 1 2)" `ioShouldBe` "2"

    it "p13: reduces lambda abstr '(\\x. + x 1) 4" $ do
      showReducedEnriched "(\\x. + x 1) 4" `ioShouldBe` "5"

    -- -- retiring this test, as it's almost entirely useless 
    -- -- since bound/free is really only relevant in nested contexts,
    -- -- and is accomplished operationally, not by marking variables in the AST
    -- it "p14: identifies bound and free" $ do
    --   showMarked' "(\\x. + x y) 4" `ioShouldBe` "(\\x:b. + x:b y:f) 4"
    --   showMarked' "\\x. + ((\\y. + y z) 7) x" `ioShouldBe` "\\x:b. + ((\\y:b. + y:b z:f) 7) x:b"
    --   showMarked' "+ x ((\\x. + x 1) 4)" `ioShouldBe` "+ x:f ((\\x:b. + x:b 1) 4)"

    it "p15: reduces simple lambda" $ do 
      showReducedEnriched "(\\x. + x 1) 4" `ioShouldBe` "5"

    it "p16: reduces multiple occurrence lambda" $ do
      showReducedEnriched "(\\x. + x x) 5" `ioShouldBe` "10"

    it "p16: reduces nested lambdas" $ do
      showReducedEnriched "(\\x.(\\y. - y x)) 4 5" `ioShouldBe` "1"

    it "p16: reduces lambda func app" $ do 
      showReducedEnriched "(\\f. f 3) (\\x. + x 1)" `ioShouldBe` "4"

    it "p17: accounts for nested lambda var conflicts" $ do 
      showReducedEnriched "(\\x.(\\x. + (- x 1)) x 3) 9" `ioShouldBe` "11"
      showReducedEnriched "(\\x.(\\x. x)) 1 2" `ioShouldBe` "2"

    it "p17: purely functional cons" $ do 
      -- HEAD (CONS p q) = CONS p q (\a.\b.a) = (\a.\b.\f. f a b) p q (\a.\b.a) 
      showReducedEnriched [r|(\a.\b.\f. f a b) p q (\a.\b.a)|] `ioShouldBe` "p"

    it "p19: eta reduces simple function application" $ do 
      showReducedEnriched "(\\x. + 1 x)" `ioShouldBe` "+ 1"

    it "p20: equivalence by applying arbitrary argument" $ do 
      ioShouldBe (showReducedEnriched "If True ((\\p.p) 3) w") =<< showReducedEnriched "(\\x.3) w"

    it "p21: resolving name capture by alpha-conversion" $ do 
      showReducedEnriched "(\\f.\\x. f x) x" `ioShouldBe` "\\y. x y"
      showReducedEnriched "(\\f.\\x. f (f x)) x" `ioShouldBe` "\\y. x (x y)"

    it "p21: accounts for partial name-capture" $ do 
      showReducedEnriched "(\\x. x (\\y. x y)) y" `ioShouldBe` "y (\\z. y z)"
    
    it "p21: isn't too eager to alpha convert" $ do 
      showReducedEnriched "(\\x. x (\\y. y)) y" `ioShouldBe` "y (\\y. y)"

    it "p27: evaluates recursive fibonacci" $ do 
      showReducedEnriched [r|(\h.(\x. h (x x)) (\x. h (x x))) (\fac.\n. If (= n 0) 1 (* n (fac (- n 1)))) 4|] 
        `ioShouldBe` "24"

    it "p28: supports builtin Y combinator" $ do 
      showReducedEnriched "Y (\\fac.\\n. IF (= n 0) 1 (* n (fac (- n 1)))) 4"
        `ioShouldBe` "24"
  
  -- -- retiring eval for now, since for the lambda calculus
  -- -- all of the interesting operational behavior is in reduction,
  -- -- where *this* eval just checks for normal form and returns 'bottom' 
  -- -- if not
  -- describe "2.5 The Denotational Semantics" $ do 
  --   it "p29: performs simple eval" $ do 
  --     showEvaled "+ 3 4" `ioShouldBe` "7" 

  describe "3.2 The Enriched Lambda Calculus" $ do
    it "p41: evaluates simple let expression" $ do
      showReducedEnriched "let x = 3 in (* x x)" `ioShouldBe` "9"
    it "p41: evaluates let expression in lambda expr" $ do 
      showReducedEnriched "+ 1 (let x = 3 in (* x x))" `ioShouldBe` "10"
    it "p41: evaluates nested let expression" $ do 
      showReducedEnriched "let x = 3 in (let y = 4 in (* x y))" `ioShouldBe` "12"
    it "p41: evaluates multiple let expression" $ do 
      showReducedEnriched "let x = 3\n    y = 4\n in (* x y)" `ioShouldBe` "12"
    it "evaluates multiple single line let expression" $ do 
      showReducedEnriched "let x = 3; y = 4 in (* x y)" `ioShouldBe` "12"
  
  describe "3.2.2 Simple letrec Expressions" $ do
    it "p42: evaluates fib letrec" $ do 
      showReducedEnriched "letrec factorial = \\n. IF (= n 0) 1 (* n (factorial (- n 1))) in factorial 4"
       `ioShouldBe` "24"

  describe "3.3 Translating Miranda" $ do
    it "p44: evaluates simple program" $ do 
      showReducedMiranda "square n = n * n\n2 * (square 5)"
        `ioShouldBe` "50"
    it "p47: evaluates user-defined infix" $ do 
      showReducedMiranda "mult x y = x * y\n2 $mult 3" `ioShouldBe` "6"

  describe "3.6 An Example" $ do
    it "p48: average example" $ do 
      showReducedMiranda "average a b = (a+b)/2\naverage 2 (3+5)"  `ioShouldBe` "5"
      showReducedMiranda "average a b = (a+b)/2\n2 $average (3+5)" `ioShouldBe` "5"
    
  describe "4.1 Introduction to Structured Types" $ do 
    it "p52: parses simple tree type" $ do 
      parseDefIO "tree ::= LEAF num | BRANCH tree tree" 
        `ioShouldBe` "tree ::= LEAF num | BRANCH tree tree"

    it "p52: parses constructors as functions" $ do
      parseDefIO "tree1 = BRANCH (BRANCH (LEAF 1) (LEAF 2)) (LEAF 3)"
        `ioShouldBe` "tree1 = BRANCH (BRANCH (LEAF 1) (LEAF 2)) (LEAF 3)"

    it "p52: parses constructors as args" $ do
      parseMatchesDef "reflect (LEAF n) = LEAF n" 
      parseMatchesDef "reflect (BRANCH t1 t2) = BRANCH (reflect t2) (reflect t1)"

    it "p53: parses constructor with type variables" $ do 
      parseDefIO "tree * ::= LEAF * | BRANCH (tree *) (tree *)"
        `ioShouldBe` "tree * ::= LEAF * | BRANCH (tree *) (tree *)"

    it "p53: parses constructor in expressions" $ do 
      parseMirandaExpIO "BRANCH (LEAF 'a') (LEAF 'b')"
        `ioShouldBe` "BRANCH (LEAF 'a') (LEAF 'b')"

    it "p53: parses list definition" $ do
      parseMatchesDef "list * ::= NIL | CONS * (list *)"
    
    it "p53: parses list special syntax" $ do
      parseMirandaExpIO "[]"      `ioShouldBe` "[]"
      parseMirandaExpIO "(x:xs)"  `ioShouldBe` "x : xs"
      parseMirandaExpIO "[x,y,z]" `ioShouldBe` "[x,y,z]"

    it "p53: translates list special syntax" $ do 
      showReducedMiranda "[]"      `ioShouldBe` "NIL"
      showReducedMiranda "(x:y:xs)"  `ioShouldBe` "CONS x (CONS y xs)"
      showReducedMiranda "[x,y,z]" `ioShouldBe` "CONS x (CONS y (CONS z NIL))"

    it "p54: translates tuple type sigs" $ do 
      parseMatchesDef "pair * ** ::= PAIR * **"
      parseMatchesDef "triple * ** ::= TRIPLE * **"
      parseMatchesDef "quadruple * ** ::= QUADRUPLE * **"
    
    it "p54: translates tuple special syntax" $ do 
      showReducedMiranda "(x, y)"    `ioShouldBe` "PAIR x y"
      showReducedMiranda "(x, y, z)" `ioShouldBe` "TRIPLE x y z"
      showReducedMiranda "(3, TRUE)" `ioShouldBe` "PAIR 3 TRUE"
      showReducedMiranda "('a', (3, 2))" `ioShouldBe` "PAIR 'a' (PAIR 3 2)"

    it "p55: parses no arg constructors" $ do 
      parseDefIO "color ::= VERMILLION | PUCE | LAVENDER"
        `ioShouldBe` "color ::= VERMILLION | PUCE | LAVENDER"
      parseDefIO "bool ::= TRUE | FALSE"
        `ioShouldBe` "bool ::= TRUE | FALSE"
      parseProgIO "bool ::= TRUE | FALSE\nif TRUE e1 e2 = e1\nif FALSE e1 e2 = e2\nif TRUE 1 2"
        `ioShouldBe` "bool ::= TRUE | FALSE\nif TRUE e1 e2 = e1\nif FALSE e1 e2 = e2\nif TRUE 1 2" 

    it "p57: parses multiple function defs" $ do 
      parseMatchesProg "factorial 0 = 1\nfactorial n = n * factorial (n - 1)\nfactorial 2"

  where 
    ioShouldBe :: (Show a, Eq a) => IO a -> a -> IO ()
    ioShouldBe io val = (`shouldBe` val) =<< io

    showReducedMiranda :: String -> IO String
    showReducedMiranda input = pShow . reduce <$> (parseHunit input :: IO M.Prog)

    parseProgIO :: String -> IO String
    parseProgIO input = pShow <$> (parseHunit :: String -> IO M.Prog) input

    parseMirandaExpIO :: String -> IO String 
    parseMirandaExpIO input = pShow <$> (parseHunit :: String -> IO M.Prog) input

    -- parsesDefTo :: String -> String -> IO ()
    -- parsesDefTo input res = parseDefIO input `ioShouldBe` res

    parseMatchesProg :: String -> IO ()
    parseMatchesProg input = parseProgIO input `ioShouldBe` input

    parseMatchesDef :: String -> IO ()
    parseMatchesDef input = parseDefIO input `ioShouldBe` input

    parseDefIO :: String -> IO String
    parseDefIO input = pShow <$> (parseHunit :: String -> IO M.Def) input 

    showReducedEnriched :: String -> IO String
    showReducedEnriched input = pShow . reduce <$> (parseHunit input :: IO E.Exp)

    parseHunit :: Parse a => String -> IO a
    parseHunit = eitherHUnit . parse

    eitherHUnit :: Either String a -> IO a
    eitherHUnit = either assertFailure pure