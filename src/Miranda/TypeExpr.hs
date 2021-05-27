module Miranda.TypeExpr 
  ( TypeExpr (..)
  , mapTVars
  , tvarsIn
  ) where

data TypeExpr = TVar String 
              | TCons String [TypeExpr]

instance Show TypeExpr where
  showsPrec d (TVar n) = showsPrec d n

  showsPrec d (TCons "Arrow" [t1, t2]) = showParen (d > 10) $
    showsPrec 11 t1    .
    showString " -> " .
    showsPrec 9 t2

  showsPrec d (TCons "List" [t]) = 
    showString "[" . showsPrec d t . showString "]"

  showsPrec d (TCons type_str []) = showsPrec d type_str

  showsPrec _ (TCons type_str _) = error $ "Don't know how to show compound type: " ++ type_str

mapTVars :: (String -> TypeExpr) -> TypeExpr -> TypeExpr
mapTVars f (TVar v) = f v
mapTVars f (TCons c es) = TCons c (map (mapTVars f) es)

foldrTVars :: (String -> a -> a) -> a -> TypeExpr -> a
foldrTVars f acc (TVar v) = f v acc
foldrTVars f acc (TCons _ es) = foldr (flip (foldrTVars f)) acc es 

tvarsIn :: TypeExpr -> [String]
tvarsIn = foldrTVars (:) id