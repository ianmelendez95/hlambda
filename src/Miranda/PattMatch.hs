module Miranda.PattMatch where 

import Control.Monad.State.Lazy
import Data.List (foldl1')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Miranda.Syntax as M
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

data PattTree = PTConstant (Numd S.Constant) PattTree
              | PTVariable (Numd S.Variable) PattTree
              | PTConstructor (Numd E.Constructor) PattTree
              | PTExp E.Exp
              deriving Show

data MatchTree = MTConstant Int [S.Constant] MatchTree
               | MTVariable Int (Set.Set S.Variable) MatchTree
               | MTConstructor Int (Map.Map E.Constructor MatchTree)
               | MTFatBar MatchTree MatchTree
               | MTExp E.Exp
               deriving Show

--------------------------------------------------------------------------------
-- The 'numbering' monad

data Numd a = Numd Int a
            deriving Show

type NumdM = State Int

numdNextN :: NumdM Int
numdNextN = state (\n -> (n, n+1))

numdNext :: a -> NumdM (Numd a)
numdNext x = (`Numd` x) <$> numdNextN

--------------------------------------------------------------------------------
-- Test Input

test_def1 :: ([E.Pattern], E.Exp)
test_def1 = 
  ( [ E.PVariable "f", 
      E.PConstructor "NIL" [],
      E.PVariable "ys" ],
    E.mkApply [E.Pure (S.mkVariable "A"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "ys")])


test_def2 :: ([E.Pattern], E.Exp)
test_def2 = 
  ( [ E.PVariable "f", 
      E.PVariable "xs",
      E.PConstructor "NIL" [] ],
    E.mkApply [E.Pure (S.mkVariable "B"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "xs")])

test_def3 :: ([E.Pattern], E.Exp)
test_def3 = 
  ( [ E.PVariable "f", 
      E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
      E.PConstructor "CONS" [E.PVariable "y", E.PVariable "ys"] ],
    E.mkApply [mkV "C", mkV "f", mkV "xs", mkV "ys"])
  where 
    mkV :: String -> E.Exp
    mkV = E.Pure . S.mkVariable

test_tree1 :: PattTree
test_tree1 = uncurry mkTree test_def1

test_tree2 :: PattTree
test_tree2 = uncurry mkTree test_def2

test_tree3 :: PattTree
test_tree3 = uncurry mkTree test_def3

test_trees :: [PattTree]
test_trees = [test_tree1, test_tree2, test_tree3]

merged_trees :: MatchTree
merged_trees = mergePTrees test_trees
{-
MTVariable 1 ["f","f","f"] 
    (MTFatBar 
        (MTFatBar 
            (MTConstructor 2 ["NIL"] (MTVariable 3 ["ys"] (MTExp A f ys))) 
            (MTVariable 2 ["xs"] (MTConstructor 3 ["NIL"] (MTExp B f xs)))) 
        (MTConstructor 2 ["CONS"] 
            (MTVariable 4 ["x"] 
                (MTVariable 5 ["xs"] 
                    (MTConstructor 3 ["CONS"] 
                        (MTVariable 6 ["y"] 
                            (MTVariable 7 ["ys"] 
                                (MTExp C f xs ys))))))))
-}

mappairs_defs :: [([E.Pattern], E.Exp)]
mappairs_defs = 
  [ ( [ E.PVariable    "f", 
        E.PConstructor "NIL" [],
        E.PVariable    "ys" ],
      E.Pure $ mkV "NIL"),
    ( [ E.PVariable    "f", 
        E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
        E.PConstructor "NIL" []],
      E.Pure $ mkV "NIL"),
    ( [ E.PVariable    "f", 
        E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
        E.PConstructor "CONS" [E.PVariable "y", E.PVariable "ys"] ],
      E.Pure (S.mkApply [mkV "CONS", S.mkApply [mkV "f", mkV "x", mkV "y"],
                                     S.mkApply [mkV "mappairs", mkV "f", mkV "xs", mkV "ys"]])) ]
  where 
    mkV :: String -> S.Exp
    mkV = S.mkVariable

{-
MTVariable 1 ["f"] 
  (MTConstructor 2 [
    ("CONS",  MTVariable 4 ["x"] 
                (MTVariable 5 ["xs"] 
                  (MTConstructor 3 [
                    ("CONS",  MTVariable 6 ["y"] 
                                (MTVariable 7 ["ys"] 
                                  (MTExp CONS (f x y) (mappairs f xs ys)))),
                    ("NIL",   MTExp NIL)]))),
    ("NIL",MTVariable 3 ["ys"] (MTExp NIL))])
-}

--------------------------------------------------------------------------------
-- Making the 'pattern tree'

mkTree :: [E.Pattern] -> E.Exp -> PattTree
mkTree ps e = evalState doMk 1
  where
    doMk :: NumdM PattTree
    doMk = do ps' <- numd_ps
              mkTree' ps' e

    numd_ps :: NumdM [Numd E.Pattern]
    numd_ps = mapM numdNext ps

-- mkTree [] e = PTExp e
-- mkTree (E.PConstant c : ps) e = PTConstant c (mkTree ps e)
-- mkTree (E.PVariable v : ps) e = PTVariable v (mkTree ps e)
-- mkTree (E.PConstructor c cps : ps) e = PTConstructor c (mkTree (cps ++ ps) e)

mkTree' :: [Numd E.Pattern] -> E.Exp -> NumdM PattTree
mkTree' [] e = return $ PTExp e
mkTree' (Numd n p:ps) e = 
  case p of 
    (E.PConstant c) -> PTConstant (Numd n c) <$> mkTree' ps e
    (E.PVariable v) -> PTVariable (Numd n v) <$> mkTree' ps e
    (E.PConstructor c cps) -> 
      do numd_cps <- mapM numdNext cps
         PTConstructor (Numd n c) <$> mkTree' (numd_cps ++ ps) e

--------------------------------------------------------------------------------
-- merging into 'match tree'

pattToMatchTree :: PattTree -> MatchTree
pattToMatchTree (PTConstant (Numd n c) t) = MTConstant n [c] (pattToMatchTree t)
pattToMatchTree (PTConstructor (Numd n c) t) = MTConstructor n (Map.singleton c (pattToMatchTree t))
pattToMatchTree (PTVariable (Numd n v) t) = MTVariable n (Set.singleton v) (pattToMatchTree t)
pattToMatchTree (PTExp e) = MTExp e

mergePTrees :: [PattTree] -> MatchTree
mergePTrees = mergeMTrees . map pattToMatchTree

mergeMTrees :: [MatchTree] -> MatchTree
mergeMTrees = foldl1' mergeMTree

mergeMTree :: MatchTree -> MatchTree -> MatchTree
mergeMTree m1@(MTVariable n1 vs1 t1) m2@(MTVariable n2 vs2 t2) = 
  MTVariable (assertEqual (mTreeDivergeMsg m1 m2) n1 n2) (Set.union vs1 vs2) (mergeMTree t1 t2)

mergeMTree m1@(MTConstructor n1 ctrees1) m2@(MTConstructor n2 ctrees2) = 
  MTConstructor (assertEqual (mTreeDivergeMsg m1 m2) n1 n2) (Map.unionWith mergeMTree ctrees1 ctrees2)

mergeMTree m1@(MTConstant n1 cs1 t1) m2@(MTConstant n2 cs2 t2) = 
  MTConstant (assertEqual (mTreeDivergeMsg m1 m2) n1 n2) (cs1 ++ cs2) (mergeMTree t1 t2)

mergeMTree (MTExp e) (MTExp _) = MTExp e
mergeMTree m1 m2 = MTFatBar m1 m2

treeDivergeMsg :: PattTree -> PattTree -> String
treeDivergeMsg p1 p2 = "Pattern trees diverged in number: " ++ show p1 ++ " =/= " ++ show p2

mTreeDivergeMsg :: MatchTree -> MatchTree -> String
mTreeDivergeMsg p1 p2 = "Pattern trees diverged in number: " ++ show p1 ++ " =/= " ++ show p2

assertEqual :: (Eq a) => String -> a -> a -> a
assertEqual msg x y = if x == y then x else error msg