module Miranda.PattMatch
  ( PattTree (..),
    pTreeFromSpec,

    -- testing
    test_lone
  ) where 

import qualified Miranda.Syntax as S
import qualified Miranda.Token as T
import qualified Lambda.Enriched as E

data PattTree = PTree E.Pattern [PattTree]
              | PLeaf E.Pattern E.Exp
              deriving Show

test_lone :: S.DefSpec
test_lone = 
  S.mkDefSpec [S.PVariable "x", S.mkApplyPatt [S.PConstructor "CONS", 
                                               S.PVariable "y", 
                                               S.PVariable "ys"]] 
              [S.BaseClause 
                (S.mkApply [S.Variable "x", S.Constant $ T.CNat 1])]
              []

pTreeFromSpec :: S.DefSpec -> PattTree
pTreeFromSpec (S.DefSpec ps rhs) = 
  mkPattTree (map S.funcPatternToPattern ps) (E.toEnriched rhs)

mkPattTree :: [E.Pattern] -> E.Exp -> PattTree
mkPattTree [] _ = error "No patterns provided"
mkPattTree [p] e = PLeaf p e
mkPattTree (p:ps) e = PTree p [mkPattTree ps e]
