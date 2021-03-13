module Miranda.PattMatch where 

import Control.Monad.State.Lazy
import Data.List (foldl1')
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E
import Lambda.AlphaConv

data Root a = Root [Int] a
            deriving Show

rootNums :: Root a -> [Int]
rootNums (Root ns _) = ns

rootTree :: Root a -> a
rootTree (Root _ t) = t

data PattTree = PTConstant (Numd S.Constant) PattTree
              | PTVariable (Numd S.Variable) PattTree
              | PTConstructor (Numd E.Constructor) (Root PattTree)
              | PTExp E.Exp
              deriving Show

data MatchTree = MTConstant Int [S.Constant] MatchTree
               | MTVariable Int (Set.Set S.Variable) MatchTree
               | MTConstructor Int ConstructorMap
               | MTFatBar MatchTree MatchTree
               | MTExp E.Exp
               deriving Show

type ConstructorMap = Map.Map E.Constructor (Root MatchTree)

--------------------------------------------------------------------------------
-- The 'numbering' monad

type IntS = State Int

nextInt :: IntS Int
nextInt = state (\n -> (n, n+1))

data Numd a = Numd Int a
            deriving Show

type NumdM = State Int

numdNum :: Numd a -> Int
numdNum (Numd n _) = n

numdNextN :: NumdM Int
numdNextN = state (\n -> (n, n+1))

numdNext :: a -> NumdM (Numd a)
numdNext x = (`Numd` x) <$> numdNextN

--------------------------------------------------------------------------------
-- Test Input

demo_def1 :: ([E.Pattern], E.Exp)
demo_def1 = 
  ( [ E.PVariable "f", 
      E.PConstructor "NIL" [],
      E.PVariable "ys" ],
    E.mkApply [E.Pure (S.mkVariable "A"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "ys")])


demo_def2 :: ([E.Pattern], E.Exp)
demo_def2 = 
  ( [ E.PVariable "f", 
      E.PVariable "xs",
      E.PConstructor "NIL" [] ],
    E.mkApply [E.Pure (S.mkVariable "B"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "xs")])

demo_def3 :: ([E.Pattern], E.Exp)
demo_def3 = 
  ( [ E.PVariable "f", 
      E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
      E.PConstructor "CONS" [E.PVariable "y", E.PVariable "ys"] ],
    E.mkApply [mkV "C", mkV "f", mkV "xs", mkV "ys"])
  where 
    mkV :: String -> E.Exp
    mkV = E.Pure . S.mkVariable

demo_ptree1 :: Root PattTree
demo_ptree1 = uncurry mkTree demo_def1

demo_ptree2 :: Root PattTree
demo_ptree2 = uncurry mkTree demo_def2

demo_ptree3 :: Root PattTree
demo_ptree3 = uncurry mkTree demo_def3

demo_ptrees :: [Root PattTree]
demo_ptrees = [demo_ptree1, demo_ptree2, demo_ptree3]

demo_mtree :: Root MatchTree
demo_mtree = mergePRoots demo_ptrees
{-
MTVariable 1 ["f"] 
  (MTFatBar (MTFatBar (MTConstructor 2 [
                        ("NIL",MTVariable 3 ["ys"] (MTExp A f ys))]) 
                      (MTVariable 2 ["xs"] (MTConstructor 3 [("NIL",MTExp B f xs)]))) 
            (MTConstructor 2 [
              ("CONS",  MTVariable 4 ["x"] 
                (MTVariable 5 ["xs"] 
                  (MTConstructor 3 [ 
                    ("CONS",  MTVariable 6 ["y"] (MTVariable 7 
                                ["ys"] (MTExp C f xs ys))) ])))]))

Root [1,2,3] 
  MTVariable 1 (fromList ["f"]) 
    (MTFatBar 
      (MTFatBar (MTConstructor 2 (fromList [
                  ("NIL", Root [] 
                    MTVariable 3 (fromList ["ys"]) 
                      (MTExp A f ys))])) 
                (MTVariable 2 (fromList ["xs"]) 
                  (MTConstructor 3 (fromList [
                    ("NIL",Root [] 
                      (MTExp B f xs))])))) 
      (MTConstructor 2 (fromList [
        ("CONS", Root [4,5] 
          MTVariable 4 (fromList ["x"]) 
            (MTVariable 5 (fromList ["xs"]) 
              (MTConstructor 3 (fromList [
                ("CONS",Root [6,7] 
                  MTVariable 6 (fromList ["y"]) 
                    (MTVariable 7 (fromList ["ys"]) 
                      (MTExp C f xs ys)))]))))])))
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

mappairs_tree :: Root MatchTree
mappairs_tree = mergePRoots (map (uncurry mkTree) mappairs_defs)

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

Root [1,2,3] 
  (MTVariable 1 (fromList ["f"]) 
    (MTConstructor 2 (fromList [
      ("CONS", Root [4,5] 
                 (MTVariable 4 (fromList ["x"]) 
                   (MTVariable 5 (fromList ["xs"]) 
                     (MTConstructor 3 (fromList [
                       ("CONS", Root [6,7] 
                                  (MTVariable 6 (fromList ["y"]) 
                                    (MTVariable 7 (fromList ["ys"]) 
                                      (MTExp CONS (f x y) (mappairs f xs ys))))),
                       ("NIL",  Root [] 
                                  (MTExp NIL))]))))),
      ("NIL",Root [] (MTVariable 3 (fromList ["ys"]) (MTExp NIL)))])))


-}

--------------------------------------------------------------------------------
-- Making the 'pattern tree'

mkTree :: [E.Pattern] -> E.Exp -> Root PattTree
mkTree ps e = evalState doMk 1
  where
    doMk :: NumdM (Root PattTree)
    doMk = do ps' <- numd_ps
              Root (map numdNum ps') <$> mkTree' ps' e

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
         PTConstructor (Numd n c) . Root (map numdNum numd_cps) <$> mkTree' (numd_cps ++ ps) e

--------------------------------------------------------------------------------
-- merging into 'match tree'

pattToMatchTree :: PattTree -> MatchTree
pattToMatchTree (PTConstant (Numd n c) t) = MTConstant n [c] (pattToMatchTree t)
pattToMatchTree (PTConstructor (Numd n c) (Root ns t)) = 
  MTConstructor n (Map.singleton c (Root ns $ pattToMatchTree t))
pattToMatchTree (PTVariable (Numd n v) t) = MTVariable n (Set.singleton v) (pattToMatchTree t)
pattToMatchTree (PTExp e) = MTExp e

mergePRoots :: [Root PattTree] -> Root MatchTree
mergePRoots [] = error "No pattern trees"
mergePRoots all_ts@((Root nums _):ts) 
  | all ((nums ==) . rootNums) ts = Root nums (mergePTrees (map rootTree all_ts))
  | otherwise = error "mismatched root numbers"

mergeRootsWith :: Show a => (a -> a -> a) -> Root a -> Root a -> Root a
mergeRootsWith f r1@(Root ns1 t1) r2@(Root ns2 t2)
  | ns1 == ns2 = Root ns1 (f t1 t2)
  | otherwise = error $ "Mismatched root numbers: " ++ show r1 ++ " /= " ++ show r2

mergePTrees :: [PattTree] -> MatchTree
mergePTrees = mergeMTrees . map pattToMatchTree

mergeMTrees :: [MatchTree] -> MatchTree
mergeMTrees = foldl1' mergeMTree

mergeMTree :: MatchTree -> MatchTree -> MatchTree
mergeMTree m1@(MTVariable n1 vs1 t1) m2@(MTVariable n2 vs2 t2) = 
  MTVariable (assertEqual (mTreeDivergeMsg m1 m2) n1 n2) (Set.union vs1 vs2) (mergeMTree t1 t2)

mergeMTree m1@(MTConstructor n1 ctrees1) m2@(MTConstructor n2 ctrees2) = 
  MTConstructor (assertEqual (mTreeDivergeMsg m1 m2) n1 n2) 
                (Map.unionWith (mergeRootsWith mergeMTree) ctrees1 ctrees2)

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

--------------------------------------------------------------------------------
-- Producing Enriched from Tree

instance E.ToEnriched MatchTree where 
  toEnriched = enrichMTree

type Names = IMap.IntMap (Set.Set String)

enrichMRoot :: Root MatchTree -> E.Exp
enrichMRoot (Root nums t) = E.mkLambda (map (E.PVariable . numVar) nums) (enrichMTree t)

enrichMTree :: MatchTree -> E.Exp
enrichMTree = enrichMTree' IMap.empty 

enrichMTree' :: Names -> MatchTree -> E.Exp
enrichMTree' _ MTConstant{} = error "Don't know how to enrich constant branches"
enrichMTree' ns (MTVariable i vs t) = enrichMTree' (IMap.insertWith Set.union i vs ns) t
enrichMTree' ns (MTConstructor i c_map) = 
  E.Case (numVar i) (consMapToClauses ns c_map)
enrichMTree' ns (MTFatBar t1 t2) = 
  E.FatBar (enrichMTree' ns t1) (enrichMTree' ns t2)
enrichMTree' ns (MTExp expr) = alphaConvExp ns expr

-- TODO: - account for all constructors of type
consMapToClauses :: Names -> ConstructorMap -> [E.CaseClause]
consMapToClauses ns = Map.foldrWithKey' foldrF []
  where 
    foldrF :: E.Constructor -> Root MatchTree -> [E.CaseClause] -> [E.CaseClause]
    foldrF c_name rt clauses = consToClause ns c_name rt : clauses

consToClause :: Names -> E.Constructor -> Root MatchTree -> E.CaseClause -- 
consToClause ns c_name (Root is t) = 
  ( E.PConstructor c_name (map (E.PVariable . numVar) is), 
    enrichMTree' ns t )

-- TODO: PERFORMANCE - unsafeAlphaConv in batch, instead of traversing for each rename
alphaConvExp :: Names -> E.Exp -> E.Exp
alphaConvExp ns e = IMap.foldrWithKey foldrF e ns
  where 
    foldrF :: Int -> Set.Set String -> E.Exp -> E.Exp
    foldrF i vs acc_e = 
      let i_name = numVar i
       in Set.foldl' (\conv_e v -> unsafeAlphaConv v i_name conv_e) acc_e vs

numVar :: Int -> S.Variable
numVar n = "_u" ++ show n