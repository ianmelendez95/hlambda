module Miranda.TransformSpec where

import Test.Hspec

import SpecUtil

-- | High level transformations
spec :: Spec
spec = do
  xdescribe "3.3 Translating Miranda" $ do
    it "p44: evaluates simple program" $ do 
      showReducedMiranda "square n = n * n\n2 * (square 5)"
        `ioShouldBe` "50"
    it "p47: evaluates user-defined infix" $ do 
      showReducedMiranda "mult x y = x * y\n2 $mult 3" `ioShouldBe` "6"

  xdescribe "3.6 An Example" $ do
    it "p48: average example" $ do 
      showReducedMiranda "average a b = (a+b)/2\naverage 2 (3+5)"  `ioShouldBe` "5"
      showReducedMiranda "average a b = (a+b)/2\n2 $average (3+5)" `ioShouldBe` "5"
    
  describe "4.1 Introduction to Structured Types" $ do 
    it "p52: parses simple tree type" $ do 
      parseDeclIO "tree ::= LEAF num | BRANCH tree tree" 
        `ioShouldBe` "tree ::= LEAF num | BRANCH tree tree"

    it "p52: parses constructors as functions" $ do
      parseDeclIO "tree1 = BRANCH (BRANCH (LEAF 1) (LEAF 2)) (LEAF 3)"
        `ioShouldBe` "tree1 = BRANCH (BRANCH (LEAF 1) (LEAF 2)) (LEAF 3)"

    it "p52: parses constructors as args" $ do
      parseMatchesDef "reflect (LEAF n) = LEAF n" 
      parseMatchesDef "reflect (BRANCH t1 t2) = BRANCH (reflect t2) (reflect t1)"

    it "p53: parses constructor with type variables" $ do 
      parseDeclIO "tree * ::= LEAF * | BRANCH (tree *) (tree *)"
        `ioShouldBe` "tree * ::= LEAF * | BRANCH (tree *) (tree *)"

    it "p53: parses constructor in expressions" $ do 
      parseMirandaExpIO "BRANCH (LEAF 'a') (LEAF 'b')"
        `ioShouldBe` "BRANCH (LEAF 'a') (LEAF 'b')"

    it "p53: parses list definition" $ do
      parseMatchesDef "list * ::= NIL | CONS * (list *)"
    
    it "p53: parses list special syntax" $ do
      parseMirandaExpIO "[]"      `ioShouldBe` "[]"
      parseMirandaExpIO "(x:xs)"  `ioShouldBe` "x : xs"
      parseMirandaExpIO "(x:y:[])" `ioShouldBe` "x : y : []"
      parseMirandaExpIO "[x,y,z]" `ioShouldBe` "[x,y,z]"

    xit "p53: translates list special syntax" $ do 
      showReducedMiranda "[]"      `ioShouldBe` "NIL"
      showReducedMiranda "(x:y:xs)"  `ioShouldBe` "CONS x (CONS y xs)"
      showReducedMiranda "[x,y,z]" `ioShouldBe` "CONS x (CONS y (CONS z NIL))"

    it "p54: translates tuple type sigs" $ do 
      parseMatchesDef "pair * ** ::= PAIR * **"
      parseMatchesDef "triple * ** ::= TRIPLE * **"
      parseMatchesDef "quadruple * ** ::= QUADRUPLE * **"
    
    xit "p54: translates tuple special syntax" $ do 
      showReducedMiranda "(x, y)"    `ioShouldBe` "PAIR x y"
      showReducedMiranda "(x, y, z)" `ioShouldBe` "TRIPLE x y z"
      showReducedMiranda "(3, TRUE)" `ioShouldBe` "PAIR 3 TRUE"
      showReducedMiranda "('a', (3, 2))" `ioShouldBe` "PAIR 'a' (PAIR 3 2)"

    it "p55: parses no arg constructors" $ do 
      parseDeclIO "color ::= VERMILLION | PUCE | LAVENDER"
        `ioShouldBe` "color ::= VERMILLION | PUCE | LAVENDER"
      parseDeclIO "bool ::= TRUE | FALSE"
        `ioShouldBe` "bool ::= TRUE | FALSE"
      parseProgIO "bool ::= TRUE | FALSE\nif TRUE e1 e2 = e1\nif FALSE e1 e2 = e2\nif TRUE 1 2"
        `ioShouldBe` "bool ::= TRUE | FALSE\nif TRUE e1 e2 = e1\nif FALSE e1 e2 = e2\nif TRUE 1 2" 

  describe "4.2 Translating Miranda into the Enriched Lambda Calculus" $ do
    it "p57: parses multiple function defs" $ do 
      parseMatchesProg "factorial 0 = 1\nfactorial n = n * factorial (n - 1)\nfactorial 2"

    it "p58: parses string literals in patterns" $ do 
      parseProgIO "lastElt (x:[]) = x\nlastElt (x:xs) = lastElt xs\nlastElt [1,2,3]"
        `ioShouldBe` "lastElt (x : []) = x\nlastElt (x : xs) = lastElt xs\nlastElt [1,2,3]"

    it "p58: parses several arguments" $ do 
      parseMatchesProg "xor False y = y\nxor True False = True\nxor True True = False\nxor True True"

    it "p58: parses conditional equations" $ do 
      parseMatchesProg "factorial n = 1, n == 0\n            = n * factorial (n - 1)\nfactorial 5"

    it "p58: parses funnyLastElt" $ do 
      parseMatchesProg "funnyLastElt (x : xs) = x, x < 0\nfunnyLastElt (x : []) = x\nfunnyLastElt (x : xs) = funnyLastElt xs\nfunnyLastElt [1,2,3]"

    it "p60: [single-pattern-arg] enriches pattern arg definition" $ do 
      let test_file_base = "single-pattern-arg"
      mcontent <- readMiranda test_file_base
      elcontent <- readEnriched test_file_base
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
    
    it "p62: [hd-incomplete-patt] enriches incomplete pattern matching" $ do
      let test_file_base = "hd-incomplete-patt"
      mcontent <- readMiranda test_file_base
      elcontent <- readEnriched test_file_base
      showEnrichedMiranda mcontent `ioShouldBe` elcontent

    it "p63: [mult-const-patterns] translates multiple arguments" $ do 
      let test_file_base = "mult-const-patterns"
      mcontent <- readMiranda test_file_base
      elcontent <- readEnriched test_file_base

      e_result <- showEnrichedMiranda mcontent
      e_result `shouldBe` elcontent
    
    it "p63: [conditional-no-default] translates conditional equation" $ do 
      mcontent <- readMiranda "conditional-no-default"
      elcontent <- readEnriched "conditional-no-default"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      
    it "p63: [conditional-list-patts] translates conditional equation" $ do
      let test_file_base = "conditional-list-patts"
      mcontent <- readMiranda test_file_base
      elcontent <- readEnriched test_file_base
      showEnrichedMiranda mcontent `ioShouldBe` elcontent

    it "p64: translates conditional with base clause equation" $ do
      showEnrichedMiranda "factorial n = 1, n==0\n            = n * factorial (n-1)\nfactorial 4"
        `ioShouldBe` "letrec factorial = \\_u1. IF (= _u1 0) 1 (* _u1 (factorial (- _u1 1)))\nin factorial 4"

    it "p66: [sumsq] enriches where clauses" $ do
      mcontent <- readFile "test-ref/sumsq.m"
      elcontent <- readFile "test-ref/sumsq.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent

    it "p66: [gcd] enriches gcd, which has a little bit of everything" $ do 
      mcontent <- readMiranda "gcd"
      elcontent <- readEnriched "gcd"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent

    it "p66: [no-last-guard] doesn't if against FAIL if no last guard" $ do
      mcontent <- readFile "test-ref/no-last-guard.m"
      elcontent <- readFile "test-ref/no-last-guard.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      -- showReducedMiranda mcontent `ioShouldBe` "5"
    
    it "p66: [true-last-guard] doesn't if against FAIL if last guard is True" $ do
      mcontent <- readFile "test-ref/true-last-guard.m"
      elcontent <- readFile "test-ref/true-last-guard.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      -- showReducedMiranda mcontent `ioShouldBe` "5"
    
    it "p67: [lhs-pattern] enriches a where lhs pattern expression" $ do
      mcontent <- readFile "test-ref/lhs-pattern.m"
      elcontent <- readFile "test-ref/lhs-pattern.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
    
    it "[simple-fact] reduces simple factorial" $ do 
      mcontent <- readMiranda "simple-fact"
      elcontent <- readEnriched "simple-fact"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      -- showReducedMiranda mcontent `ioShouldBe` "24"

    it "p75: [reflect] transforms reflect to case" $ do 
      mcontent <- readMiranda "reflect"
      elcontent <- readEnriched "reflect"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent

  describe "Dependendy Analysis" $ do
    it "transforms simple bindings" $ do
      mcontent <- readMiranda "x_fac_z_sum"
      elcontent <- readEnriched "x_fac_z_sum"
      lcontent <- readLambda "x_fac_z_sum"

      el_result <- showEnrichedMiranda mcontent
      el_result `shouldBe` elcontent

      l_result <- showLambdadMiranda mcontent
      l_result `shouldBe` lcontent