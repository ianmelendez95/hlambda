module Miranda.PattMatch (PattEq, patternEquationsToEnriched) where 

import Control.Monad.State.Lazy
import Data.List (foldl1')
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Lambda.Syntax as S
import qualified Lambda.Enriched as E
import Lambda.AlphaConv

import Lambda.Constructor

--------------------------------------------------------------------------------
-- Interface

patternEquationsToEnriched :: [([E.Pattern], E.Exp)] -> E.Exp
patternEquationsToEnriched patt_eqs 
  | not . allEqual $ map (length . fst) patt_eqs = 
      error $ "Equations have different number of arguments: " ++ show patt_eqs
  | otherwise = enrichMRoot . eqsToMTree $ patt_eqs
  where 
    allEqual :: Eq a => [a] -> Bool
    allEqual [] = True
    allEqual (x:xs) = all (== x) xs 


eqsToMTree :: [([E.Pattern], E.Exp)] -> Root MatchTree
eqsToMTree = mergePRoots . map (uncurry mkTree)

--------------------------------------------------------------------------------
-- Root

data Root a = Root [Int] a
            deriving Show

instance Functor Root where 
  fmap f (Root ns x) = Root ns (f x)

rootNums :: Root a -> [Int]
rootNums (Root ns _) = ns

rootTree :: Root a -> a
rootTree (Root _ t) = t

--------------------------------------------------------------------------------
-- Trees

-- | Honestly not a tree
-- | it's the linear pattern structure of any given function definition,
-- | formulated in such a way that it can be easily constructed and subsequently
-- | converted to the actual 'match' tree 
data PattTree = PTConstant (Numd S.Constant) PattTree
              | PTVariable (Numd S.Variable) PattTree
              | PTConstructor (Numd Constructor) Int (Root PattTree) -- Int = the starting number for constructor args
              | PTExp E.Exp
              deriving Show

data MatchTree = MTVariable Int (Set.Set S.Variable) MatchTree
               | MTConstant Int (Map.Map S.Constant MatchTree)
               | MTConstructor Int ConsMap
               | MTFatBar MatchTree MatchTree
               | MTExp E.Exp
               | MTFail
               deriving Show

type PattEq = ([E.Pattern], E.Exp)

--------------------------------------------------------------------------------
-- Constructor Map

-- TODO: candidate for a lazy map, not all values inserted will be used
type ConsMap = Map.Map Constructor (Root MatchTree)

newConsMap :: Int -> Constructor -> Root MatchTree -> ConsMap
newConsMap an1 cons tree = insertConsMap cons tree (emptyConsMap an1 cons)

insertConsMap :: Constructor -> Root MatchTree -> ConsMap -> ConsMap
insertConsMap = Map.insertWith (mergeRootsWith mergeCTrees)
  where 
    mergeCTrees :: MatchTree -> MatchTree -> MatchTree
    mergeCTrees MTFail t2     = t2
    mergeCTrees t1     MTFail = t1
    mergeCTrees t1 t2 = mergeMTree t1 t2

emptyConsMap :: Int -> Constructor -> ConsMap
emptyConsMap n c = foldr insertConstr Map.empty (siblings c)
  where 
    insertConstr :: Constructor -> ConsMap -> ConsMap
    insertConstr c' = Map.insert c' (mkFailRoot c')

    mkFailRoot :: Constructor -> Root MatchTree
    mkFailRoot c' = Root (take (arity c') [n..]) MTFail

--------------------------------------------------------------------------------
-- The 'numbering' monad

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
      do a1n <- get
         numd_cps <- mapM numdNext cps
         PTConstructor (Numd n c) a1n . Root (map numdNum numd_cps) <$> mkTree' (numd_cps ++ ps) e

--------------------------------------------------------------------------------
-- merging into 'match tree'

pattToMatchTree :: PattTree -> MatchTree
pattToMatchTree (PTConstant (Numd n c) t) = MTConstant n (Map.singleton c (pattToMatchTree t))
pattToMatchTree (PTConstructor (Numd n c) a1_n (Root ns t)) = 
  MTConstructor n (newConsMap a1_n c (Root ns $ pattToMatchTree t))
pattToMatchTree (PTVariable (Numd n v) t) = MTVariable n (Set.singleton v) (pattToMatchTree t)
pattToMatchTree (PTExp e) = MTExp e

mergePRoots :: [Root PattTree] -> Root MatchTree
mergePRoots [] = error "No pattern trees"
mergePRoots all_ts@((Root nums _):ts) 
  | all ((nums ==) . rootNums) ts = Root nums (mergePTrees (map rootTree all_ts))
  | otherwise = error "mismatched root numbers"

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

mergeMTree m1@(MTConstant n1 cmap1) m2@(MTConstant n2 cmap2) =
  MTConstant (assertEqual (mTreeDivergeMsg m1 m2) n1 n2)
             (Map.unionWith mergeMTree cmap1 cmap2)

mergeMTree (MTExp e) (MTExp _) = MTExp e

mergeMTree MTFail t = t
mergeMTree t MTFail = t

mergeMTree m1 m2 = MTFatBar m1 m2

mergeRootsWith :: Show a => (a -> a -> a) -> Root a -> Root a -> Root a
mergeRootsWith f r1@(Root ns1 t1) r2@(Root ns2 t2)
  | ns1 == ns2 = Root ns1 (f t1 t2)
  | otherwise =
      error $ "Mismatched root numbers: " ++ show r1 ++ " /= " ++ show r2

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
enrichMTree' ns (MTVariable i vs t) = enrichMTree' (IMap.insertWith Set.union i vs ns) t
enrichMTree' ns (MTConstant i ct) = enrichConstantBranch ns i ct
enrichMTree' ns (MTConstructor i c_map) = 
  E.Case (numVar i) (consMapToClauses ns c_map)
enrichMTree' ns (MTFatBar t1 t2) = 
  E.FatBar (enrichMTree' ns t1) (enrichMTree' ns t2)
enrichMTree' ns (MTExp expr) = alphaConvExp ns expr
enrichMTree' _  MTFail = E.Pure (S.mkConstant S.CFail)

enrichConstantBranch :: Names -> Int -> Map.Map S.Constant MatchTree -> E.Exp
enrichConstantBranch ns i = Map.foldrWithKey' foldF (E.Pure $ S.mkConstant S.CFail)
  where 
    foldF :: S.Constant -> MatchTree -> E.Exp -> E.Exp
    foldF c ct = 
      E.mkIf (E.Pure (S.mkApply [S.mkFunction S.FEq, S.mkVariable (numVar i), S.mkConstant c])) 
             (enrichMTree' ns ct)

-- TODO: - account for all constructors of type
consMapToClauses :: Names -> ConsMap -> [E.CaseClause]
consMapToClauses ns = Map.foldrWithKey' foldrF []
  where 
    foldrF :: Constructor -> Root MatchTree -> [E.CaseClause] -> [E.CaseClause]
    foldrF c_name rt clauses = consToClause ns c_name rt : clauses

consToClause :: Names -> Constructor -> Root MatchTree -> E.CaseClause -- 
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

--------------------------------------------------------------------------------
-- Test Input

-- demo_def1 :: ([E.Pattern], E.Exp)
-- demo_def1 = 
--   ( [ E.PVariable "f", 
--       E.PConstructor "NIL" [],
--       E.PVariable "ys" ],
--     E.mkApply [E.Pure (S.mkVariable "A"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "ys")])


-- demo_def2 :: ([E.Pattern], E.Exp)
-- demo_def2 = 
--   ( [ E.PVariable "f", 
--       E.PVariable "xs",
--       E.PConstructor "NIL" [] ],
--     E.mkApply [E.Pure (S.mkVariable "B"), E.Pure (S.mkVariable "f"), E.Pure (S.mkVariable "xs")])

-- demo_def3 :: ([E.Pattern], E.Exp)
-- demo_def3 = 
--   ( [ E.PVariable "f", 
--       E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
--       E.PConstructor "CONS" [E.PVariable "y", E.PVariable "ys"] ],
--     E.mkApply [mkV "C", mkV "f", mkV "x", mkV "xs", mkV "y", mkV "ys"])
--   where 
--     mkV :: String -> E.Exp
--     mkV = E.Pure . S.mkVariable

-- demo_defs :: [([E.Pattern], E.Exp)]
-- demo_defs = [demo_def1, demo_def2, demo_def3]

-- demo_ptree1 :: Root PattTree
-- demo_ptree1 = uncurry mkTree demo_def1

-- demo_ptree2 :: Root PattTree
-- demo_ptree2 = uncurry mkTree demo_def2

-- demo_ptree3 :: Root PattTree
-- demo_ptree3 = uncurry mkTree demo_def3

-- demo_ptrees :: [Root PattTree]
-- demo_ptrees = [demo_ptree1, demo_ptree2, demo_ptree3]

-- demo_mtree :: Root MatchTree
-- demo_mtree = mergePRoots demo_ptrees
{-
\_u1. \_u2. \_u3. case _u2 of
                    CONS _u4 _u5 => FAIL
                    NIL => A _u1 _u3
                  | case _u3 of
                      CONS _u4 _u5 => FAIL
                      NIL => B _u1 _u2
                  | case _u2 of
                      CONS _u4 _u5 => case _u3 of
                                        CONS _u6 _u7 => C _u1 _u4 _u5 _u6 _u7
                                        NIL => FAIL
                      NIL => FAIL
-}

-- mappairs_defs :: [([E.Pattern], E.Exp)]
-- mappairs_defs = 
--   [ ( [ E.PVariable    "f", 
--         E.PConstructor "NIL" [],
--         E.PVariable    "ys" ],
--       E.Pure $ mkV "NIL"),
--     ( [ E.PVariable    "f", 
--         E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
--         E.PConstructor "NIL" []],
--       E.Pure $ mkV "NIL"),
--     ( [ E.PVariable    "f", 
--         E.PConstructor "CONS" [E.PVariable "x", E.PVariable "xs"],
--         E.PConstructor "CONS" [E.PVariable "y", E.PVariable "ys"] ],
--       E.Pure (S.mkApply [mkV "CONS", S.mkApply [mkV "f", mkV "x", mkV "y"],
--                                      S.mkApply [mkV "mappairs", mkV "f", mkV "xs", mkV "ys"]])) ]
--   where 
--     mkV :: String -> S.Exp
--     mkV = S.mkVariable

-- mappairs_tree :: Root MatchTree
-- mappairs_tree = eqsToMTree mappairs_defs

-- mappairs_exp :: E.Exp
-- mappairs_exp = patternEquationsToEnriched mappairs_defs

{-
\_u1. \_u2. \_u3. case _u2 of
                    CONS _u4 _u5 => case _u3 of
                                      CONS _u6 _u7 => CONS (_u1 _u4 _u6) (mappairs _u1 _u5 _u7)
                                      NIL => NIL
                    NIL => NIL

-}

-- unwieldy_defs :: [PattEq]
-- unwieldy_defs = 
--   [ ( [ E.PConstructor "NIL" [],
--         E.PConstructor "NIL" [] ],
--       E.Pure $ mkV "A"),
--     ( [ E.PVariable    "xs", 
--         E.PVariable    "ys" ],
--       E.Pure (S.mkApply [mkV "B", mkV "xs", mkV "ys" ])) ]
--   where 
--     mkV :: String -> S.Exp
--     mkV = S.mkVariable

{-
\_u1. \_u2. case _u1 of
              CONS _u3 _u4 => FAIL
              NIL => case _u2 of
                       CONS _u3 _u4 => FAIL
                       NIL => A
            | B _u1 _u2
-}
