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

-- span :: lhs matches
-- break :: lhs not matches
partition :: (a -> Bool) -> [a] -> [[a]]
partition p xs = case span p xs of 
                   ([], []) -> []  
                   ([], ys) -> partition (not . p) ys 
                   (match, ys) -> match : partition (not . p) ys