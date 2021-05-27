module Miranda.TypeExpr 
  ( TypeExpr (..)
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

tvarsIn :: TypeExpr -> [String]
tvarsIn (TVar n) = [n]
tvarsIn (TCons _ es) = concatMap tvarsIn es