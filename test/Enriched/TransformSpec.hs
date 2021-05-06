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
                                                   S.mkVariable "_u1",
                                                   S.toConstantExp (0 :: Int)])
                                       (S.toConstantExp (1 :: Int))
                                       (S.mkConstant S.CFail))
      toLambda enr `shouldBe` lam
  
  describe "Constructor Transformations" $ do
    
    it "p106: transforms PAIR pattern lambda expressions" $ do
          -- \PAIR x y. + x y
      let enr = E.Lambda (E.PConstructor (C.fromString "PAIR") [E.PVariable "x", E.PVariable "y"]) 
                         (E.mkApply [E.Pure (S.mkFunction S.FPlus), 
                                     E.Pure (S.mkVariable "x"),
                                     E.Pure (S.mkVariable "y")])
          lam = init $ unlines 
            [ "\\_u1. let _u3 = SEL-2-2 _u1",
              "      in let _u2 = SEL-2-1 _u1",
              "         in + _u2 _u3" ]
      show (toLambda enr) `shouldBe` lam

    it "p107: transforma TREE (LEAF) pattern lambda expresssions" $ do 
      -- \LEAF n. LEAF n
      let enr = E.Lambda (E.PConstructor (C.fromString "LEAF") [E.PVariable "n"]) 
                         (E.mkApply [E.Pure (S.mkVariable "LEAF"), 
                                     E.Pure (S.mkVariable "n")]) 
          lam = init $ unlines 
            [ "\\_u1. CASE-2 _u1 (let _u2 = SEL-1-1 _u1",
              "                  in LEAF _u2) (let _u3 = SEL-2-2 _u1",
              "                                in let _u2 = SEL-2-1 _u1", 
              "                                   in FAIL)" ]
      show (toLambda enr) `shouldBe` lam

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
          lam = init $ unlines 
            [ "\\_u1. CASE-2 _u1 (let _u2 = SEL-1-1 _u1",
              "                  in FAIL) (let _u3 = SEL-2-2 _u1",
              "                            in let _u2 = SEL-2-1 _u1", 
              "                               in BRANCH (reflect _u3) (reflect _u2))" ]
      show (toLambda enr) `shouldBe` lam
  
  describe "6.2.3 Transforming Simple Lets" $ do
    it "p112: transforms simple let" $ do
      -- let x = 4 in (+ x 6) => (\x.+ x 6) 4
      let enr = Let [(PVariable "x", Pure (S.toConstantExp (4 :: Int)))] 
                    (mkApply [Pure (S.mkFunction S.FPlus), 
                              Pure (S.mkVariable "x"),
                              Pure (S.toConstantExp (6 :: Int))])
          lam = init $ unlines 
            [ "let x = 4",
              "in + x 6" ]
      show (toLambda enr) `shouldBe` lam
  
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
                                               S.mkVariable "_u1"])]
                             (S.Let [("y", S.mkApply [S.mkVariable "SEL-2-2",
                                                      S.mkVariable "_u1"])]
                              (S.mkApply [S.mkFunction S.FPlus,
                                          S.mkVariable "x",
                                          S.mkVariable "y"])))

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

          lam = init $ unlines 
            [ "let _u1 = PAIR 2 5",
              "in let x = SEL-2-1 _u1",
              "   in let y = SEL-2-2 _u1",
              "      in + x y" ]
          -- lam = S.Letrec [("_u1", 
          --                  S.mkApply [S.mkVariable "PAIR",
          --                             S.toConstantExp (2 :: Int),
          --                             S.toConstantExp (5 :: Int)]),
          --                 ("x", S.mkApply [S.mkVariable "SEL-2-1",
          --                                  S.mkVariable "_u1"]),
          --                 ("y", S.mkApply [S.mkVariable "SEL-2-2",
          --                                  S.mkVariable "_u1"])]

          --                (S.mkApply [S.mkFunction S.FPlus,
          --                                   S.mkVariable "x",
          --                                   S.mkVariable "y"])

      show (toLambda enr) `shouldBe` lam

  describe "6.2.7 Transforming general let(rec)s into irrefutable let(rec)s" $ do
    {-
    let (CONS y ys) = NIL
    in 6
    =>
    let _u1 = let _u1 = NIL
              in (\CONS y ys. PAIR y ys) _u1

    let _u1 = let _u1 = NIL
              in (\\_u1. CASE-2 _u1 FAIL (let _u2 = SEL-2-1 _u1
                                          in let _u3 = SEL-2-2 _u1
                                             in PAIR _u2 _u3)) 
                 _u1
    in let y = SEL-2-1 _u1
       in let ys = SEL-2-2 _u1
          in 6
    -}
    it "p116 transforms lazy cons" $ do 
      let enr = Let [(PConstructor (C.fromString "CONS") 
                                   [PVariable "y",
                                    PVariable "ys"], 
                      Pure $ S.mkVariable "NIL")] 
                    (Pure $ S.toConstantExp (6 :: Int))

          lam = init $ unlines 
            [ "let _u1 = let _u1 = NIL",
              "          in (\\_u1. CASE-2 _u1 FAIL (let _u3 = SEL-2-2 _u1",
              "                                     in let _u2 = SEL-2-1 _u1",
              "                                        in PAIR _u2 _u3)) _u1",
              "in let y = SEL-2-1 _u1",
              "   in let ys = SEL-2-2 _u1",
              "      in 6" ]

      show (toLambda enr) `shouldBe` lam
