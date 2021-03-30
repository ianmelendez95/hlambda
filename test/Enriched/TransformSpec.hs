module Enriched.TransformSpec where 

import Test.Hspec
import Lambda.ToLambda
import qualified Lambda.Syntax as S
import Lambda.Enriched
import qualified Lambda.Enriched as E
import qualified Lambda.Constructor as C

spec :: Spec
spec = do
  describe "ConstanTransformations" $ do

    it "p105: transforms constant lambda expressions" $ do
      let enr = E.Lambda (E.PConstant (S.CNat 0)) (E.Pure (S.mkConstant (S.CNat 1)))
          lam = S.Lambda "_u1" (S.mkIf (S.mkApply [S.mkFunction S.FEq, 
                                                   S.toConstantExp (0 :: Int),
                                                   S.mkVariable "_u1"])
                                       (S.toConstantExp (1 :: Int))
                                       (S.mkConstant S.CFail))
      toLambda enr `shouldBe` lam
  
  describe "Constructor Transformations" $ do
    
    it "p106: transforms PAIR pattern lambda expressions" $ do
      -- \PAIR x y. + x y
      -- UNPACK-PRODUCT-PAIR (\_u1. \_u2. + _u1 _u2)
      let enr = E.Lambda (E.PConstructor (C.fromString "PAIR") [E.PVariable "x", E.PVariable "y"]) 
                         (E.mkApply [E.Pure (S.mkFunction S.FPlus), 
                                     E.Pure (S.mkVariable "x"),
                                     E.Pure (S.mkVariable "y")])
          lam = S.mkApply [S.mkVariable "UNPACK-PRODUCT-2", 
                           S.mkLambda ["x", "y"] 
                                      (S.mkApply [S.mkFunction S.FPlus, 
                                                  S.mkVariable "x",
                                                  S.mkVariable "y"])]
      toLambda enr `shouldBe` lam

    it "p107: transforma TREE (LEAF) pattern lambda expresssions" $ do 
      -- \LEAF n. LEAF n
      -- UNPACK-SUM-LEAF (\n. LEAF n)
      let enr = E.Lambda (E.PConstructor (C.fromString "LEAF") [E.PVariable "n"]) 
                         (E.mkApply [E.Pure (S.mkVariable "LEAF"), 
                                     E.Pure (S.mkVariable "n")]) 
          lam = S.mkApply [S.mkVariable "UNPACK-SUM-1-1", 
                           S.mkLambda ["n"] (S.mkApply [S.mkVariable "LEAF", 
                                                        S.mkVariable "n"])] 
      toLambda enr `shouldBe` lam

    it "p107: transforma TREE (BRANCH) pattern lambda expresssions" $ do 
      -- \BRANCH t1 t2. BRANCH (reflect t2) (reflect t1)
      -- UNPACK-SUM-BRANCH (\t1. \t2. BRANCH (reflect t2) (reflect t1))
      let enr = E.Lambda (E.PConstructor (C.fromString "BRANCH") [E.PVariable "t1",
                                                   E.PVariable "t2"]) 
                         (E.mkApply [E.Pure (S.mkVariable "BRANCH"), 
                                     E.Apply (E.Pure (S.mkVariable "reflect")) 
                                             (E.Pure (S.mkVariable "t2")),
                                     E.Apply (E.Pure (S.mkVariable "reflect")) 
                                             (E.Pure (S.mkVariable "t1"))]) 
          lam = S.mkApply [S.mkVariable "UNPACK-SUM-2-2", 
                           S.mkLambda ["t1", "t2"] 
                                      (S.mkApply [S.mkVariable "BRANCH", 
                                                  S.Apply (S.mkVariable "reflect") 
                                                          (S.mkVariable "t2"),
                                                  S.Apply (S.mkVariable "reflect") 
                                                          (S.mkVariable "t1")])] 
      toLambda enr `shouldBe` lam
  
  describe "6.2.3 Transforming Simple Lets" $ do
    it "p112: transforms simple let" $ do
      -- let x = 4 in (+ x 6) => (\x.+ x 6) 4
      let enr = Let [(PVariable "x", Pure (S.toConstantExp (4 :: Int)))] 
                    (mkApply [Pure (S.mkFunction S.FPlus), 
                              Pure (S.mkVariable "x"),
                              Pure (S.toConstantExp (6 :: Int))])
          lam = S.Apply (S.Lambda "x" (S.mkApply [S.mkFunction S.FPlus, 
                                                  S.mkVariable "x",
                                                  S.toConstantExp (6 :: Int)]))
                        (S.toConstantExp (4 :: Int))
      toLambda enr `shouldBe` lam
  
  describe "6.2.4 Transforming Irrefutable Lets" $ do
    it "p112: transforms pattern let" $ do
      {-
        let (PAIR x y) = PAIR 2 5 in + x y

          =>

        let _u1 = PAIR 2 5 in (let x = SEL-2-1 _u1
                                   y = SEL-2-2 _u1
                               in  + x y)
      -}

      let enr = Let [(PConstructor (C.fromString "PAIR") 
                                   [PVariable "x", PVariable "y"], 
                      mkApply [Pure (S.mkVariable "PAIR"),
                               Pure (S.toConstantExp (2 :: Int)),
                               Pure (S.toConstantExp (5 :: Int))])] 

                    (mkApply [Pure (S.mkFunction S.FPlus), 
                              Pure (S.mkVariable "x"),
                              Pure (S.mkVariable "y")])

          lam = S.Let [("_u1", 
                        S.mkApply [S.mkVariable "PAIR",
                                   S.toConstantExp (2 :: Int),
                                   S.toConstantExp (5 :: Int)])]

                      (S.Let [("x", S.mkApply [S.mkVariable "SEL-2-1",
                                               S.mkVariable "_u1"]),
                              ("y", S.mkApply [S.mkVariable "SEL-2-2",
                                               S.mkVariable "_u1"])]

                             (S.mkApply [S.mkFunction S.FPlus,
                                         S.mkVariable "x",
                                         S.mkVariable "y"]))

      toLambda enr `shouldBe` lam

  describe "6.2.5 Transforming Irrefutable Letrecs" $ do
    it "p112: transforms pattern letrec" $ do
      {-
        letrec (PAIR x y) = PAIR 2 5 in + x y

          =>

        letrec _u1 = PAIR 2 5 
               x = SEL-2-1 _u1
               y = SEL-2-2 _u1
            in + x y)
      -}

      let enr = Letrec [(PConstructor (C.fromString "PAIR") 
                                    [PVariable "x", PVariable "y"], 
                         mkApply [Pure (S.mkVariable "PAIR"),
                                  Pure (S.toConstantExp (2 :: Int)),
                                  Pure (S.toConstantExp (5 :: Int))])] 

                       (mkApply [Pure (S.mkFunction S.FPlus), 
                                 Pure (S.mkVariable "x"),
                                 Pure (S.mkVariable "y")])

          lam = S.Letrec [("_u1", 
                           S.mkApply [S.mkVariable "PAIR",
                                      S.toConstantExp (2 :: Int),
                                      S.toConstantExp (5 :: Int)]),
                          ("x", S.mkApply [S.mkVariable "SEL-2-1",
                                           S.mkVariable "_u1"]),
                          ("y", S.mkApply [S.mkVariable "SEL-2-2",
                                           S.mkVariable "_u1"])]

                         (S.mkApply [S.mkFunction S.FPlus,
                                            S.mkVariable "x",
                                            S.mkVariable "y"])

      toLambda enr `shouldBe` lam

  describe "6.2.6 Transforming Irrefutable Letrecs into Irrefutable Lets" $ do
    it "p114: transforms patterns letrec" $ do
      {-
        letrec x = CONS 1 y
               y = CONS 2 x
        in x

          =>

        let _u1 = PAIR 2 5 
            x = SEL-2-1 _u1
            y = SEL-2-2 _u1
            in + x y)
      -}

      let enr = Letrec [(PVariable "x", 
                         mkApply [Pure (S.mkFunction S.FCons),
                                  Pure (S.toConstantExp (1 :: Int)),
                                  Pure (S.mkVariable "y")]),
                        (PVariable "y", 
                         mkApply [Pure (S.mkFunction S.FCons),
                                  Pure (S.toConstantExp (2 :: Int)),
                                  Pure (S.mkVariable "x")])] 

                       (Pure (S.mkVariable "x"))

          lam = init $ unlines 
            [ "let _u1 = Y (UNPACK-PRODUCT-2 (\\x. \\y. PAIR (CONS 1 y) (CONS 2 x)))"
            , "in let x = SEL-2-1 _u1"
            , "       y = SEL-2-2 _u1"
            , "   in x" ]

      show (toLambda enr) `shouldBe` lam