module Lambda.Syntax where 

data Exp = Constant String 
         | Variable Variable 
         | Apply Exp Exp 
         | Lambda String Exp

data Variable = RawVar String 
              | FreeVar String 
              | BoundVar String

varName :: Variable -> String 
varName (RawVar n) = n
varName (FreeVar n) = n
varName (BoundVar n) = n

instance Show Exp where 
  showsPrec _ (Constant str) = showString str
  showsPrec _ (Variable var) = showString $ show var
  showsPrec d (Lambda name expr) = showParen (d > lambda_prec) $ 
    showString ("\\" ++ name ++ ". ") . shows expr
    where lambda_prec = 5
  showsPrec d (Apply expr expr') = showParen (d > apply_prec) $
    showsPrec 6 expr . showString " " . showsPrec 11 expr'
    where apply_prec = 10

instance Show Variable where 
  show (RawVar var) = var
  show (FreeVar var) = var
  show (BoundVar var) = var

-- infixr 5 :^:
-- data Tree a = Leaf a  
--             | Tree a :^: Tree a

-- instance (Show a) => Show (Tree a) where

--        showsPrec d (Leaf m) = showParen (d > app_prec) $
--             showString "Leaf " . showsPrec (app_prec+1) m
--          where app_prec = 10

--        showsPrec d (u :^: v) = showParen (d > up_prec) $
--             showsPrec (up_prec+1) u .
--             showString " :^: "      .
--             showsPrec (up_prec+1) v
--          where up_prec = 5