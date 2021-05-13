module ATypeChecker where

type VName = String

data VExp = Var VName
          | Lambda VName VExp
          | Ap VExp VExp
          | Let [VName] [VExp] VExp
          | Letrec [VName] [VExp] VExp
          deriving Show

{-
example =

let S = \x.\y.\z. x z (y z)
    K = \x.\y. x
in S K K
-}
example :: VExp
example = Let ["S", "K"] [rhs_S, rhs_K] main
  where
    var_S = Var "S"
    var_K = Var "K"
    var_x = Var "x"
    var_y = Var "y"
    var_z = Var "z"
    main = Ap (Ap var_S var_K) var_K
    rhs_S = plambda ["x", "y", "z"] body_S  
    rhs_K = plambda ["x", "y"]      body_K  
    body_S = Ap (Ap var_x var_z) (Ap var_y var_z)
    body_K = var_x
    plambda vs e = foldr Lambda e vs