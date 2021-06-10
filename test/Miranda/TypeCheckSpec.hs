module Miranda.TypeCheckSpec where

import Data.Either (either)
import Test.Hspec
import Test.HUnit.Base (assertFailure)

import qualified Lambda.Syntax as S
import Miranda.Compiler (CompileError, compileStr)

import SpecUtil

-- | High level transformations
spec :: Spec
spec = do
  describe "9 A Type Checker" $ do
    it "type checks simple polymorphic type" $ do 
      let test_path = "type-checks/simple-type"
      mcontent  <- readMiranda test_path
      lcontent  <- readLambda test_path

      l_compiled <- either assertFailure pure (eitherToString $ compileStr mcontent)
      l_compiled `shouldBe` lcontent

eitherToString :: (Show a, Show b) => Either a b -> Either String String
eitherToString (Left x)  = Left (show x)
eitherToString (Right x) = Right (show x)