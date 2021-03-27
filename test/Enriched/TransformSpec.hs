module Enriched.TransformSpec where 

import Test.Hspec
import Lambda.ToLambda
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

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
    
    it "p106: transforms PAIR pattern lambda expressions" $ do
      -- \PAIR x y. + x y
      -- UNPACK-PRODUCT-PAIR (\_u1. \_u2. + _u1 _u2)
      let enr = E.Lambda (E.PConstructor "PAIR" [E.PVariable "x", E.PVariable "y"]) 
                         (E.mkApply [E.Pure (S.mkFunction S.FPlus), 
                                     E.Pure (S.mkVariable "x"),
                                     E.Pure (S.mkVariable "y")])
          lam = S.mkApply [S.mkVariable "UNPACK-PRODUCT-PAIR", 
                           S.mkLambda ["x", "y"] 
                                      (S.mkApply [S.mkFunction S.FPlus, 
                                                  S.mkVariable "x",
                                                  S.mkVariable "y"])]
      toLambda enr `shouldBe` lam

    it "p107: transforma TREE pattern lambda expresssions" $ do 
      -- \LEAF n. LEAF n
      -- UNPACK-SUM-LEAF (\n. LEAF n)
      let enr = E.Lambda (E.PConstructor "LEAF" [E.PVariable "n"]) 
                         (E.mkApply [E.Pure (S.mkVariable "LEAF"), 
                                     E.Pure (S.mkVariable "n")]) 
          lam = S.mkApply [S.mkVariable "UNPACK-SUM-LEAF", 
                           S.mkLambda ["n"] (S.mkApply [S.mkVariable "LEAF", 
                                                        S.mkVariable "n"])] 
      toLambda enr `shouldBe` lam