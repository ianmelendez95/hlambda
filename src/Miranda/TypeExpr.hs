module Miranda.TypeExpr 
  ( TypeExpr (TCons)
  , mkUnboundTVar

  , mapTVars
  , mapUnboundTVars
  , tvarsIn
  ) where

data TypeExpr = TVar IsBound String 
              | TCons String [TypeExpr]

type IsBound = Bool

instance Show TypeExpr where
  showsPrec d (TVar _ n) = showsPrec d n

  showsPrec d (TCons "Arrow" [t1, t2]) = showParen (d > 10) $
    showsPrec 11 t1    .
    showString " -> " .
    showsPrec 9 t2

  showsPrec d (TCons "List" [t]) = 
    showString "[" . showsPrec d t . showString "]"

  showsPrec d (TCons type_str []) = showsPrec d type_str

  showsPrec _ (TCons type_str _) = error $ "Don't know how to show compound type: " ++ type_str

--------------------------------------------------------------------------------
-- Constructors

mkUnboundTVar :: String -> TypeExpr
mkUnboundTVar = TVar False

--------------------------------------------------------------------------------
-- Functions

mapTVars :: (IsBound -> String -> TypeExpr) -> TypeExpr -> TypeExpr
mapTVars f (TVar is_bound v) = f is_bound v
mapTVars f (TCons c es) = TCons c (map (mapTVars f) es)

mapUnboundTVars :: (String -> TypeExpr) -> TypeExpr -> TypeExpr
mapUnboundTVars f = mapTVars (\is_bound v -> if is_bound then TVar is_bound v else f v)

foldrTVars :: (String -> a -> a) -> a -> TypeExpr -> a
foldrTVars f acc (TVar _ v) = f v acc
foldrTVars f acc (TCons _ es) = foldr (flip (foldrTVars f)) acc es 

tvarsIn :: TypeExpr -> [String]
tvarsIn = foldrTVars (:) []