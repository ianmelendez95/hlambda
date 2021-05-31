module Miranda.TypeExpr 
  ( TypeExpr (TCons)
  , mkBoundTVar
  , mkUnboundTVar

  , mapTVars
  , mapUnboundTVars
  , mapUnboundTVarsM
  ) where

import Data.Functor.Identity

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

mkBoundTVar, mkUnboundTVar :: String -> TypeExpr
mkBoundTVar   = TVar True
mkUnboundTVar = TVar False

--------------------------------------------------------------------------------
-- Functions

mapTVarsM :: Monad m => (IsBound -> String -> m TypeExpr) -> TypeExpr -> m TypeExpr
mapTVarsM fM (TVar is_bound v) = fM is_bound v
mapTVarsM fM (TCons c es) = TCons c <$> mapM (mapTVarsM fM) es

mapUnboundTVarsM :: Monad m => (String -> m TypeExpr) -> TypeExpr -> m TypeExpr
mapUnboundTVarsM fM = mapTVarsM (\is_bound v -> if is_bound then pure $ TVar is_bound v else fM v)

mapTVars :: (IsBound -> String -> TypeExpr) -> TypeExpr -> TypeExpr
mapTVars f e = runIdentity (mapTVarsM fM e)
  where 
    fM is_bound v = Identity $ f is_bound v

mapUnboundTVars :: (String -> TypeExpr) -> TypeExpr -> TypeExpr
mapUnboundTVars f = mapTVars (\is_bound v -> if is_bound then TVar is_bound v else f v)