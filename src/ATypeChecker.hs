module ATypeChecker where

--------------------------------------------------------------------------------
-- PROGRAMS

type VName = String

data VExp = Var VName
          | Lambda VName VExp
          | Ap VExp VExp
          | Let [VName] [VExp] VExp
          | Letrec [VName] [VExp] VExp
          deriving Show

--------------------------------------------------------------------------------
-- TYPE EXPRESSIONS

type TVName = String 

data TypeExp = TVar TVName 
             | TCons String [TypeExp]
             deriving Show

arrow :: TypeExp -> TypeExp -> TypeExp
arrow t1 t2 = TCons "arrow" [t1, t2]

int :: TypeExp
int = TCons "int" []

cross :: TypeExp -> TypeExp -> TypeExp
cross t1 t2 = TCons "cross" [t1, t2]

list :: TypeExp -> TypeExp
list t = TCons "list" [t]

tvarsIn :: TypeExp -> [TVName]
tvarsIn (TVar n) = [n]
tvarsIn (TCons _ es) = concatMap tvarsIn es

--------------------------------------------------------------------------------
-- Success and Failure

type Reply = Maybe

--------------------------------------------------------------------------------
-- Substitution

type Subst = TVName -> TypeExp

subType :: Subst -> TypeExp -> TypeExp
subType phi (TVar n) = phi n
subType phi (TCons n vs) = TCons n (map (subType phi) vs)

sComp :: Subst -> Subst -> Subst
sComp s2 s1 = subType s1 . s2

idSubst :: Subst
idSubst = TVar

delta :: TVName -> TypeExp -> Subst 
delta n t n' 
  | n == n' = t
  | otherwise = TVar n'

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