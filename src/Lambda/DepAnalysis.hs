module Lambda.DepAnalysis (depAnalysis) where

import qualified Lambda.Syntax as S

import Data.Foldable (toList)
import Data.Graph (Vertex, graphFromEdges, scc)

type Binding = (S.Variable, S.Exp)

depAnalysis :: [Binding] -> [[Binding]]
depAnalysis bs =
  let -- key = S.Variable
      -- node = (Binding, S.Variable, [S.Variable])
      (graph, vertexToNode, _) = graphFromEdges (map bindingToNode bs)

      scc_flat :: [[Vertex]]
      scc_flat = map toList (scc graph)
   in (map . map) (fst3 . vertexToNode) scc_flat
  where
    fst3 (binding, _, _) = binding

bindingToNode :: Binding -> (Binding, S.Variable, [S.Variable])
bindingToNode binding@(var, expr) = (binding, var, S.freeVariables expr)

{-
[ Node {rootLabel = 0, subForest = []},
  Node {rootLabel = 1, subForest = []},
  Node { rootLabel = 4, 
         subForest = [ Node { rootLabel = 5, subForest = []},
                       Node { rootLabel = 6, subForest = []}] },
  Node { rootLabel = 2, 
         subForest = [Node {rootLabel = 3, subForest = []}]}]
[ (0, []), 
  (1, []),
  (4, )]
-}

-- testRun :: IO ()
-- testRun = 
--   do let (g, vToN, cToV) = testGraph
--          g_scc = scc g
--          g_scc_lists = map toList g_scc
--      print g
--      print g_scc
--      print g_scc_lists

-- testGraph :: (Graph, Vertex -> (Char, Char, [Char]), Char -> Maybe Vertex)
-- testGraph = graphFromEdges $ map (\(v, vs) -> (v, v, vs))
--   [ ('a', ""),
--     ('b', "a"),
--     ('c', "hbd"),
--     ('d', "c"),
--     ('f', "gha"),
--     ('g', "f"),
--     ('h', "g") ]

