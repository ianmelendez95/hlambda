module Miranda.TypeCheckSpec where

import Test.Hspec

import SpecUtil

-- | High level transformations
spec :: Spec
spec = do
  describe "9 A Type Checker" $ do
    it "type checks simple polymorphic type" $ do 
      let test_path = "type-checks/simple-type"
      mcontent  <- readMiranda test_path
      elcontent <- readEnriched test_path
      lcontent  <- readLambda test_path

      el_result <- showEnrichedMiranda mcontent
      l_result  <- showLambdadMiranda mcontent

      el_result `shouldBe` elcontent
      l_result `shouldBe` lcontent