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