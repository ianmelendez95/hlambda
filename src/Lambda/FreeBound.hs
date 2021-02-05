module Lambda.FreeBound (markBoundFree) where 

import Lambda.Syntax 
-- import Data.List (nub, (\\))

markBoundFree :: Exp -> Exp
markBoundFree = withBound []
  where 
    withBound :: [String] -> Exp -> Exp
    withBound _ c@(Constant _) = c
    withBound bs (Variable var) = let name = varName var
                                     in if name `elem` bs 
                                          then Variable (BoundVar name)
                                          else Variable (FreeVar name)
    withBound bs (Apply e e') = Apply (withBound bs e) (withBound bs e')
    withBound bs (Lambda name e) = Lambda name (withBound (name : bs) e)

-- findBoundFree :: Exp -> ([String], [String])
-- findBoundFree expr = let bound = boundVars expr 
--                       in (bound, allVars expr \\ bound)

-- boundVars :: Exp -> [String]
-- boundVars = nub . boundVars'
--   where 
--     boundVars' (Constant _) = []
--     boundVars' (Variable _) = []
--     boundVars' (Apply expr expr') = boundVars' expr ++ boundVars' expr'
--     boundVars' (Lambda var expr) = var : boundVars' expr

-- allVars :: Exp -> [String]
-- allVars = nub . allVars'
--   where 
--     allVars' (Constant _) = []
--     allVars' (Variable var) = [varName var]
--     allVars' (Apply expr expr') = allVars' expr ++ allVars' expr'
--     allVars' (Lambda var expr) = var : allVars' expr