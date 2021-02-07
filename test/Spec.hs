module Main where 

import Test.Hspec 

main :: IO ()
main = hspec $ do
  describe "Raw Parsing"
    it "parses '(+ 4 5)'" $ do 
      
