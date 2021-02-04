module Lambda.FreeBound (findBoundFree) where 

import Lambda.Syntax
import Data.List (nub, (\\))

findBoundFree :: Exp -> ([String], [String])
findBoundFree expr = let bound = boundVars expr 
                      in (bound, allVars expr \\ bound)

boundVars :: Exp -> [String]
boundVars = nub . boundVars'
  where 
    boundVars' (Constant _) = []
    boundVars' (Variable _) = []
    boundVars' (Apply expr expr') = boundVars' expr ++ boundVars' expr'
    boundVars' (Lambda var expr) = var : boundVars' expr

allVars :: Exp -> [String]
allVars = nub . allVars'
  where 
    allVars' (Constant _) = []
    allVars' (Variable var) = [var]
    allVars' (Apply expr expr') = allVars' expr ++ allVars' expr'
    allVars' (Lambda var expr) = var : allVars' expr