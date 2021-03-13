module Miranda.PattMatch where 

import Control.Monad.State.Lazy

import qualified Miranda.Syntax as M
import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E

data PattTree = PTConstant (Numd S.Constant) PattTree
              | PTVariable (Numd S.Variable) PattTree
              | PTConstructor (Numd E.Constructor) PattTree
              | PTExp E.Exp
              deriving Show

data MatchTree = MTConstant Int [S.Constant] MatchTree
               | MTVariable Int [S.Variable] MatchTree
               | MTConstructor Int [E.Constructor] MatchTree
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
    E.mkApply [E.Pure (S.mkVariable "B"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "xs")])

test_tree1 :: PattTree
test_tree1 = uncurry mkTree test_def1

test_tree2 :: PattTree
test_tree2 = uncurry mkTree test_def2

test_tree3 :: PattTree
test_tree3 = uncurry mkTree test_def3

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
pattToMatchTree (PTConstructor (Numd n c) t) = MTConstructor n [c] (pattToMatchTree t)
pattToMatchTree (PTVariable (Numd n v) t) = MTVariable n [v] (pattToMatchTree t)
pattToMatchTree (PTExp e) = MTExp e

mergePTrees :: PattTree -> PattTree -> MatchTree
mergePTrees p1@(PTVariable (Numd n1 v1) t1) p2@(PTVariable (Numd n2 v2) t2) = 
  MTVariable (assertEqual (treeDivergeMsg p1 p2) n1 n2) [v1, v2] (mergePTrees t1 t2)

mergePTrees p1@(PTConstructor (Numd n1 c1) t1) p2@(PTConstructor (Numd n2 c2) t2) = 
  MTConstructor (assertEqual (treeDivergeMsg p1 p2) n1 n2) [c1, c2] (mergePTrees t1 t2)

mergePTrees p1@(PTConstant (Numd n1 c1) t1) p2@(PTConstant (Numd n2 c2) t2) = 
  MTConstant (assertEqual (treeDivergeMsg p1 p2) n1 n2) [c1, c2] (mergePTrees t1 t2)

mergePTrees (PTExp e) (PTExp _) = MTExp e
mergePTrees p1 p2 = MTFatBar (pattToMatchTree p1) (pattToMatchTree p2)

treeDivergeMsg :: PattTree -> PattTree -> String
treeDivergeMsg p1 p2 = "Pattern trees diverged in number: " ++ show p1 ++ " =/= " ++ show p2

assertEqual :: (Eq a) => String -> a -> a -> a
assertEqual msg x y = if x == y then x else error msg