module ATypeChecker where

import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

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

type TVName = [Int] 

data TypeExp = TVar TVName 
             | TCons String [TypeExp]
             deriving (Eq, Show)

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

-- | produce a substitution that will replace all instances of 
-- | the var with the expression
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

--------------------------------------------------------------------------------
-- Unification

-- | extend: handle the case where TVar tvn = t
-- | so we 'extend' the substition to have tvn -> t
extend :: Subst -> TVName -> TypeExp -> Maybe Subst
extend phi tvn t 
  | t == TVar tvn = Just phi
  | tvn `elem` tvarsIn t = Nothing -- why, if the type var exists in t, do we not extend phi with it? To avoid non-termination?
  | otherwise = Just (delta tvn t `sComp` phi)

unify :: Subst -> (TypeExp, TypeExp) -> Maybe Subst
unify phi (TVar tvn, t) 
  | phi tvn == TVar tvn = extend phi tvn (subType phi t)
  | otherwise           = unify phi (phitvn, phit)
  where 
    phitvn = phi tvn
    phit = subType phi t

unify phi (TCons tcn ts, TVar tvn) = 
  unify phi (TVar tvn, TCons tcn ts)

unify phi (TCons tcn ts, TCons tcn' ts') 
  | tcn == tcn' = unifyl phi (ts `zip` ts')
  | otherwise = Nothing

unifyl :: Subst -> [(TypeExp, TypeExp)] -> Maybe Subst
unifyl phi = foldr (\eqn -> (>>= (`unify` eqn))) (Just phi)

unifyl' :: Subst -> [(TypeExp, TypeExp)] -> Maybe Subst
unifyl' phi [] = Just phi
unifyl' phi (eqn : eqns) = 
  do phi' <- unifyl' phi eqns
     unify phi' eqn

--------------------------------------------------------------------------------
-- Keeping Track of Types

data TypeScheme = Scheme [TVName] TypeExp
                deriving Show

-- | gets unknowns in the type scheme
-- | where 'unknown' is a name in t that doesn't occur in scvs
unknownsScheme :: TypeScheme -> [TVName]
unknownsScheme (Scheme scvs t) = tvarsIn t \\ scvs

subScheme :: Subst -> TypeScheme -> TypeScheme
subScheme phi (Scheme scvs t) =
  Scheme scvs (subType exclude t)
  where
    -- neuter the substition for schematic variables, since
    -- they are 'locally bound' (analogy to lambda abstractions)
    exclude tvn
      | tvn `elem` scvs = TVar tvn
      | otherwise       = phi tvn

--------------------------------------------------------------------------------
-- Type Expressions

type TypeEnv = Map.Map VName TypeScheme

dom :: TypeEnv -> [VName] 
dom = Map.keys

val :: TypeEnv -> VName -> TypeScheme
val m n = fromMaybe (error $ "Variable is not in type environment: " ++ n) 
                    (Map.lookup n m)

install :: TypeEnv -> VName -> TypeScheme -> TypeEnv
install env n s = Map.insert n s env

rng :: TypeEnv -> [TypeScheme]
rng = Map.elems

unknownsTE :: TypeEnv -> [TVName]
unknownsTE gamma = concatMap unknownsScheme (rng gamma)

subTE :: Subst -> TypeEnv -> TypeEnv
subTE phi = Map.map (subScheme phi)

--------------------------------------------------------------------------------
-- New Variables

type NameSupply = TVName

nextName :: NameSupply -> TVName
nextName = id

deplete :: NameSupply -> NameSupply
deplete [] = error "Empty name"
deplete (n:ns) = n+2 : ns

split :: NameSupply -> (NameSupply, NameSupply)
split ns = (0:ns, 1:ns)

nameSequence :: NameSupply -> [TVName]
nameSequence ns = nextName ns : nameSequence (deplete ns)

--------------------------------------------------------------------------------
-- The Type Checker

tc :: TypeEnv -> NameSupply -> VExp -> Maybe (Subst, TypeExp)
tc gamma ns (Var x)          = tcvar    gamma ns x
tc gamma ns (Ap e1 e2)       = tcap     gamma ns e1 e2
tc gamma ns (Lambda x e)     = tclambda gamma ns x e
tc gamma ns (Let xs es e)    = tclet    gamma ns xs es e
tc gamma ns (Letrec xs es e) = tcletrec gamma ns xs es e

-- | type check across the expressions, using the 'learned' context
-- | in later expressions
tcl :: TypeEnv -> NameSupply -> [VExp] -> Maybe (Subst, [TypeExp])
tcl _     _  []     = Just (idSubst, [])
tcl gamma ns (e:es) = 
  let (ns0, ns1) = split ns
   in do (phi, t)  <- tc gamma ns1 e
         (psi, ts) <- tcl (subTE phi gamma) ns0 es
         return (psi `sComp` phi, subType psi t : ts)

tcvar :: a
tcvar    = undefined

tcap :: a
tcap     = undefined

tclambda :: a
tclambda = undefined

tclet :: a
tclet    = undefined

tcletrec :: a
tcletrec = undefined