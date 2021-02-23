module Main where 

import Test.Hspec 
import Test.HUnit.Base (assertFailure)

import Parse
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Reduce (reduce)
import Lambda.Enriched (ToEnriched (..))
import qualified Miranda.Syntax as M (Prog (..), Def)

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
      parseDefIO "color ::= VERMILLION | PUCE | LAVENDER"
        `ioShouldBe` "color ::= VERMILLION | PUCE | LAVENDER"
      parseDefIO "bool ::= TRUE | FALSE"
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

    it "p59: parses noDups" $ do 
      parseMatchesProg "noDups [] = []\nnoDups [x] = [x]\nnoDups (x : x : xs) = noDups (x : xs)\nnoDups (x : y : ys) = x : noDups (y : ys)\nnoDups [1,2,2,3]"

    it "p60: enriches pattern arg definition" $ do 
      showEnrichedMiranda "fst (x,y) = x\nfst (1,2)"
        `ioShouldBe` "let fst = \\a. (\\PAIR x y. x) a | ERROR  in fst (PAIR 1 2)"

    it "p62: enriches multiple pattern matching" $ do 
      showEnrichedMiranda "reflect (LEAF n) = LEAF n\nreflect (BRANCH t1 t2) = BRANCH (reflect t2) (reflect t1)\nreflect (LEAF 1)"
        `ioShouldBe` "let reflect = \\a. (\\LEAF n. LEAF n) a | (\\BRANCH t1 t2. BRANCH (reflect t2) (reflect t1)) a | ERROR  in reflect (LEAF 1)" 
    
    it "p62: enriches incomplete pattern matching" $ do
      showEnrichedMiranda "hd (x:xs) = x\nhd [1,2,3]"
        `ioShouldBe` "let hd = \\a. (\\CONS x xs. x) a | ERROR  in hd (CONS 1 (CONS 2 (CONS 3 NIL)))"

    it "p63: translates multiple arguments" $ do 
      showEnrichedMiranda "xor False y = y\nxor True False = True\nxor True True = False\nxor True True"
        `ioShouldBe` "let xor = \\a. \\b. (\\FALSE. \\y. y) a b | (\\TRUE. \\FALSE. TRUE) a b | (\\TRUE. \\TRUE. FALSE) a b | ERROR  in xor TRUE TRUE"
    
    it "p63: translates conditional equation" $ do 
      showEnrichedMiranda "gcd a b = gcd (a-b) b, a>b\n        = gcd a (b-a), a<b\n        = a, a==b\ngcd 6 9"
        `ioShouldBe` "let gcd = \\a. \\b. (\\a. \\b. IF (> a b) (gcd (- a b) b) (IF (< a b) (gcd a (- b a)) (IF (= a b) a FAIL))) a b | ERROR  in gcd 6 9"
      
      showEnrichedMiranda "funnyLastElt (x:xs) = x, x<0\nfunnyLastElt (x:[]) = x\nfunnyLastElt (x:xs) = funnyLastElt xs\nfunnyLastElt [1,2,3]"
        `ioShouldBe` "let funnyLastElt = \\a. (\\CONS x xs. IF (< x 0) x FAIL) a | (\\CONS x NIL. x) a | (\\CONS x xs. funnyLastElt xs) a | ERROR  in funnyLastElt (CONS 1 (CONS 2 (CONS 3 NIL)))"

    it "p64: translates conditional with base clause equation" $ do
      showEnrichedMiranda "factorial n = 1, n==0\n            = n * factorial (n-1)\nfactorial 4"
        `ioShouldBe` "let factorial = \\a. (\\n. IF (= n 0) 1 (* n (factorial (- n 1)))) a | ERROR  in factorial 4"

  where 
    ioShouldBe :: (Show a, Eq a) => IO a -> a -> IO ()
    ioShouldBe io val = (`shouldBe` val) =<< io

    showReducedMiranda :: String -> IO String
    showReducedMiranda input = pShow . reduce <$> (parseHunit input :: IO M.Prog)

    showEnrichedMiranda :: String -> IO String 
    showEnrichedMiranda input = do parsed_prog <- parseHunit input :: IO M.Prog
                                   return $ pShow (toEnriched parsed_prog)

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

    parseHunit :: Parse a => String -> IO a
    parseHunit = eitherHUnit . parse

    eitherHUnit :: Either String a -> IO a
    eitherHUnit = either assertFailure pure