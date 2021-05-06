module Lambda.DepAnalysis (depAnalysis) where

import qualified Lambda.Syntax as S

import Data.Foldable (toList)
import Data.Graph (Vertex, graphFromEdges, scc, path)

import Debug.Trace (trace)

-- terminology borrowed from GHC Core: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type
data Bind = NonRec Binding
          | Rec    [Binding]
          deriving Show

type Binding = (S.Variable, S.Exp)

depAnalysis :: [Binding] -> S.Exp -> S.Exp
depAnalysis bs expr = 
  let analyzed = depAnalysis' bs
   in foldr foldrF expr (trace (show analyzed) analyzed) 
  where 
    foldrF :: Bind -> S.Exp -> S.Exp
    foldrF (NonRec b') e' = S.Let [b'] e'
    foldrF (Rec bs') e' =   S.Letrec bs' e'

depAnalysis' :: [Binding] -> [Bind]
depAnalysis' bs =
  let -- key = S.Variable
      -- node = (Binding, S.Variable, [S.Variable])
      (graph, vertexToNode, _) = graphFromEdges (map bindingToNode bs)

      fst3 (x, _, _) = x

      vertexToBinding :: Vertex -> Binding
      vertexToBinding = fst3 . vertexToNode

      sccToBind :: [Vertex] -> Bind
      sccToBind [] = error "Empty strongly connected component group"
      sccToBind [vert] = 
        let (b, var, vars) = vertexToNode vert
         in if var `elem` vars then Rec [b] else NonRec b
      sccToBind vs = Rec (map vertexToBinding vs)

      scc_flat :: [[Vertex]]
      scc_flat = map toList (scc graph)
   in map sccToBind scc_flat

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

