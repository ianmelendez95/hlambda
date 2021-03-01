module Miranda.PattMatch where 

data Pattern = Var Variable 
             | Con Constructor [Pattern]
             deriving Show

type Variable = String
type Constructor = String

data Expression = Case Variable [Clause]
                | FatBar Expression Expression
                --- ...
                deriving Show

data Clause = Clause Constructor [Variable] Expression
            deriving Show

type Equation = ([Pattern], Expression)

arity :: Constructor -> Int
arity "CONS" = 2
arity "NIL" = 0 
arity constr = error $ "Unknown contructor: " ++ constr

constructors :: Constructor -> [Constructor]
constructors "CONS" = ["CONS", "NIL"]
constructors "NIL" = ["CONS", "NIL"]
constructors constr = error $ "Unknown contructor: " ++ constr

subst :: Expression -> Variable -> Variable -> Expression
subst = undefined

isVar :: Equation -> Bool
isVar eq = case mGetCon eq of 
             Nothing -> False
             Just _  -> True

isCon :: Equation -> Bool 
isCon = not . isVar

getCon :: Equation -> Constructor 
getCon eq = case mGetCon eq of 
              Nothing -> error "Not a contructor equation: " ++ show eq
              Just c -> c

mGetCon :: Equation -> Maybe Constructor 
mGetCon (Var _ : _, _) = Nothing
mGetCon (Con c _ : _, _) = Just c
mGetCon eq@([], _) = error $ "Invalid equation: " ++ show eq

makeVar :: Int -> Variable 
makeVar k = "_u" ++ show k

partition :: (a -> Bool) -> [a] -> [[a]]
partition p = partitionSpan
  where 
    partitionSpan xs = 
      case span p xs of 
        ([], []) -> []  
        ([], ys) -> partitionBreak ys 
        (ps, ys) -> ps : partitionBreak ys
    
    partitionBreak xs = 
      case break p xs of 
        ([], []) -> []  
        ([], ys) -> partitionSpan ys 
        (nps, ys) -> nps : partitionSpan ys

-- match k us qs def
-- k = number for naming new variables
-- us = variables
-- qs = equations 
-- def = default expression
match :: Int -> [Variable] -> [Equation] -> Expression -> Expression
match _ [] qs def = foldr (FatBar . snd) def qs
match k (u:us) qs def = foldr (matchVarCon k (u:us)) def (partition isVar qs)

matchVarCon :: Int -> [Variable] -> [Equation] -> Expression -> Expression
matchVarCon k us qs@(q:_) def
  | isVar q = matchVar k us (q:qs) def
  | otherwise = matchCon k us qs def
matchVarCon _ _ _ _ = undefined

matchVar :: Int -> [Variable] -> [Equation] -> Expression -> Expression
matchVar k (u:us) qs def = 
  match k us (map applyVarRule qs) def
  where 
    -- the var rule drops the first variable argument, 
    -- replacing instances of it with the function arg name (u)
    applyVarRule :: Equation -> Equation
    applyVarRule (Var v : ps, e) = (ps, subst e u v)
    applyVarRule _ = undefined
matchVar _ _ _ _ = undefined

matchCon :: Int -> [Variable] -> [Equation] -> Expression -> Expression
matchCon k (u:us) (q:qs) def = 
  Case u [matchClause c k (u:us) (choose c qs) def | c <- cs]
  where 
    cs = constructors (getCon q)
matchCon _ _ _ _ = undefined


choose :: Constructor -> [Equation] -> [Equation]
choose c = filter ((== c) . getCon)

matchClause :: Constructor -> Int -> [Variable] -> [Equation] -> Expression -> Clause
matchClause c k (_:us) qs def = 
  Clause c us' (match (k' + k)
                      (us' ++ us)
                      (map flattenConstructorArgs qs)
                      def)
  where 
    k' = arity c
    us' = [makeVar (i+k) | i <- [1..k']]

    flattenConstructorArgs :: Equation -> Equation
    flattenConstructorArgs (Con _ ps' : ps, e) = (ps' ++ ps, e)
    flattenConstructorArgs equ = error $ "Not a constructor equation: " ++ show equ
matchClause _ _ _ _ _ = undefined
