module Main where 

import Test.Hspec 
import Test.HUnit.Base (assertFailure)

import Parse
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Reduce (reduce)
import Lambda.Syntax (ToLambda (..))
import Lambda.Enriched (ToEnriched (..))
import qualified Miranda.Syntax as M (Prog (..), Decl)

main :: IO ()
main = hspec $ do 
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

    it "p60: enriches pattern arg definition" $ do 
      showEnrichedMiranda "fst (x,y) = x\nfst (1,2)"
        `ioShouldBe` "let fst = \\a. (\\PAIR x y. x) a | ERROR\nin fst (PAIR 1 2)"

    it "p62: enriches multiple pattern matching" $ do 
      showEnrichedMiranda "reflect (LEAF n) = LEAF n\nreflect (BRANCH t1 t2) = BRANCH (reflect t2) (reflect t1)\nreflect (LEAF 1)"
        `ioShouldBe` "let reflect = \\a. (\\LEAF n. LEAF n) a | (\\BRANCH t1 t2. BRANCH (reflect t2) (reflect t1)) a | ERROR\nin reflect (LEAF 1)" 
    
    it "p62: enriches incomplete pattern matching" $ do
      showEnrichedMiranda "hd (x:xs) = x\nhd [1,2,3]"
        `ioShouldBe` "let hd = \\a. (\\CONS x xs. x) a | ERROR\nin hd (CONS 1 (CONS 2 (CONS 3 NIL)))"

    it "p63: translates multiple arguments" $ do 
      let test_file_base = "mult-const-patterns"
      mcontent <- readMiranda test_file_base
      elcontent <- readEnriched test_file_base
      lcontent <- readLambda test_file_base
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      showLambdadMiranda mcontent `ioShouldBe` lcontent
      -- showReducedMiranda mcontent `ioShouldBe` "7"
    
    it "p63: translates conditional equation" $ do 
      showEnrichedMiranda "gcd a b = gcd (a-b) b, a>b\n        = gcd a (b-a), a<b\n        = a, a==b\ngcd 6 9"
        `ioShouldBe` "let gcd = \\a. \\b. IF (> a b) (gcd (- a b) b) (IF (< a b) (gcd a (- b a)) (IF (= a b) a FAIL))\nin gcd 6 9"
      
      showEnrichedMiranda "funnyLastElt (x:xs) = x, x<0\nfunnyLastElt (x:[]) = x\nfunnyLastElt (x:xs) = funnyLastElt xs\nfunnyLastElt [1,2,3]"
        `ioShouldBe` "let funnyLastElt = \\a. (\\CONS x xs. IF (< x 0) x FAIL) a | (\\CONS x NIL. x) a | (\\CONS x xs. funnyLastElt xs) a | ERROR\nin funnyLastElt (CONS 1 (CONS 2 (CONS 3 NIL)))"

    it "p64: translates conditional with base clause equation" $ do
      showEnrichedMiranda "factorial n = 1, n==0\n            = n * factorial (n-1)\nfactorial 4"
        `ioShouldBe` "let factorial = \\n. IF (= n 0) 1 (* n (factorial (- n 1)))\nin factorial 4"

    it "p66: enriches where clauses" $ do
      mcontent <- readFile "test-ref/sumsq.m"
      elcontent <- readFile "test-ref/sumsq.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      showReducedMiranda mcontent `ioShouldBe` "13"

    it "p66: enriches gcd, which has a little bit of everything" $ do 
      mcontent <- readMiranda "gcd"
      elcontent <- readEnriched "gcd"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent

    it "p66: doesn't if against FAIL if no last guard" $ do
      mcontent <- readFile "test-ref/no-last-guard.m"
      elcontent <- readFile "test-ref/no-last-guard.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      showReducedMiranda mcontent `ioShouldBe` "5"
    
    it "p66: doesn't if against FAIL if last guard is True" $ do
      mcontent <- readFile "test-ref/true-last-guard.m"
      elcontent <- readFile "test-ref/true-last-guard.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      showReducedMiranda mcontent `ioShouldBe` "5"
    
    it "p67: enriches a where lhs pattern expression" $ do
      mcontent <- readFile "test-ref/lhs-pattern.m"
      elcontent <- readFile "test-ref/lhs-pattern.el"
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
    
    it "p69: lambdifies a simple constant pattern lambda" $ do 
      let test_file_base = "constant-pattern-lambda"
      mcontent <- readMiranda test_file_base
      elcontent <- readEnriched test_file_base
      lcontent <- readLambda test_file_base
      showEnrichedMiranda mcontent `ioShouldBe` elcontent
      showLambdadMiranda mcontent `ioShouldBe` lcontent
      showReducedMiranda mcontent `ioShouldBe` "7"

  where 
    readMiranda :: FilePath -> IO String
    readMiranda base_name = readFile $ "test-ref/" ++ base_name ++ ".m"

    readEnriched :: FilePath -> IO String
    readEnriched base_name = readFile $ "test-ref/" ++ base_name ++ ".el"
  
    readLambda :: FilePath -> IO String
    readLambda base_name = readFile $ "test-ref/" ++ base_name ++ ".l"

    ioShouldBe :: (Show a, Eq a) => IO a -> a -> IO ()
    ioShouldBe io val = (`shouldBe` val) =<< io

    showReducedMiranda :: String -> IO String
    showReducedMiranda input = pShow . reduce <$> (parseHunit input :: IO M.Prog)

    showLambdadMiranda :: String -> IO String
    showLambdadMiranda input = pShow . toLambda <$> (parseHunit input :: IO M.Prog)

    showEnrichedMiranda :: String -> IO String 
    showEnrichedMiranda input = do parsed_prog <- parseHunit input :: IO M.Prog
                                   return $ pShow (toEnriched parsed_prog)

    parseProgIO :: String -> IO String
    parseProgIO input = pShow <$> (parseHunit :: String -> IO M.Prog) input

    parseMirandaExpIO :: String -> IO String 
    parseMirandaExpIO input = pShow <$> (parseHunit :: String -> IO M.Prog) input

    -- parsesDefTo :: String -> String -> IO ()
    -- parsesDefTo input res = parseDeclIO input `ioShouldBe` res

    parseMatchesProg :: String -> IO ()
    parseMatchesProg input = parseProgIO input `ioShouldBe` input

    parseMatchesDef :: String -> IO ()
    parseMatchesDef input = parseDeclIO input `ioShouldBe` input

    parseDeclIO :: String -> IO String
    parseDeclIO input = pShow <$> (parseHunit :: String -> IO M.Decl) input 

    parseHunit :: Parse a => String -> IO a
    parseHunit = eitherHUnit . parse

    eitherHUnit :: Either String a -> IO a
    eitherHUnit = either assertFailure pure