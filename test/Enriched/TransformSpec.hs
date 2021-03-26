module Enriched.TransformSpec where 

import Test.Hspec
import Lambda.ToLambda
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

import SpecUtil

spec :: Spec
spec = do
  describe "Enriched Transformations" $ do
    it "p105: transforms constant lambda expressions" $ do
      let enr = E.Lambda (E.PConstant (S.CNat 0)) (E.Pure (S.mkConstant (S.CNat 1)))
          lam = S.Lambda "_u1" (S.mkIf (S.mkApply [S.mkFunction S.FEq, 
                                                   S.toConstantExp (0 :: Int),
                                                   S.mkVariable "_u1"])
                                       (S.toConstantExp (1 :: Int))
                                       (S.mkConstant S.CFail))
      toLambda enr `shouldBe` lam
      