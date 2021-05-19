module ATypeChecker where

import Data.List ((\\), nub, foldl')
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

mkAp :: [VExp] -> VExp
mkAp [] = error "Empty ap"
mkAp (e:es) = foldl' Ap e es

--------------------------------------------------------------------------------
-- TYPE EXPRESSIONS

newtype TVName = TVName [Int] 
               deriving (Eq, Ord)

instance Show TVName where
  show (TVName ns) = concatMap show ns

data TypeExp = TVar TVName 
             | TCons String [TypeExp]
             deriving (Eq)

instance Show TypeExp where
  showsPrec d (TVar n) = showsPrec d n

  showsPrec d (TCons "Arrow" [t1, t2]) = showParen (d > 10) $
    showsPrec 11 t1    .
    showString " -> " .
    showsPrec 9 t2

  showsPrec d (TCons "List" [t]) = 
    showString "[" . showsPrec d t . showString "]"

  showsPrec d (TCons type_str []) = showsPrec d type_str

  showsPrec _ (TCons type_str _) = error $ "Don't know how to show compound type: " ++ type_str


arrow :: TypeExp -> TypeExp -> TypeExp
arrow t1 t2 = TCons "Arrow" [t1, t2]

int_type :: TypeExp
int_type = TCons "Int" []

text_type :: TypeExp
text_type = TCons "Text" []

char_type :: TypeExp
char_type = TCons "Char" []

-- cross :: TypeExp -> TypeExp -> TypeExp
-- cross t1 t2 = TCons "Cross" [t1, t2]

list :: TypeExp -> TypeExp
list t = TCons "List" [t]

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

scomp :: Subst -> Subst -> Subst
scomp s2 s1 = subType s1 . s2

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
  | otherwise = Just (delta tvn t `scomp` phi)

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

dom :: Map.Map a b -> [a]
dom = Map.keys

val :: (Ord a, Show a) => Map.Map a b -> a -> b
val m n = fromMaybe (error $ "Variable is not in type environment: " ++ show n) 
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
deplete (TVName []) = error "Empty name"
deplete (TVName (n:ns)) = TVName (n+2 : ns)

split :: NameSupply -> (NameSupply, NameSupply)
split (TVName ns) = (TVName (0:ns), TVName (1:ns))

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

--------------------------------------------------------------------------------
-- Type Checking List of Expressions

-- | type check across the expressions, using the 'learned' context
-- | in later expressions
tcl :: TypeEnv -> NameSupply -> [VExp] -> Maybe (Subst, [TypeExp])
tcl _     _  []     = Just (idSubst, [])
tcl gamma ns (e:es) = 
  let (ns0, ns1) = split ns
   in do (phi, t)  <- tc gamma ns1 e
         (psi, ts) <- tcl (subTE phi gamma) ns0 es
         pure (psi `scomp` phi, subType psi t : ts)

--------------------------------------------------------------------------------
-- Type Checking Variables

-- | get a new instance of the schematic related to the variable
-- | (assumes the type variable has an associated schematic)
tcvar :: TypeEnv -> NameSupply -> VName -> Maybe (Subst, TypeExp)
tcvar gamma ns x = Just (idSubst, newInstance ns (val gamma x))

-- | substitute all scheme variables (scvs) in type expression (t)
-- | with new names according to name supply (ns)
newInstance :: NameSupply -> TypeScheme -> TypeExp
newInstance ns (Scheme scvs t) = 
  subType (alToSubst (scvs `zip` nameSequence ns)) t

alToSubst :: [(TVName, TVName)] -> Subst
alToSubst = mapToSubst . Map.fromList

-- | create a Subst from a mapping of var names
mapToSubst :: Map.Map TVName TVName -> Subst
mapToSubst var_map tvn =
  TVar $ fromMaybe tvn (Map.lookup tvn var_map)

--------------------------------------------------------------------------------
-- Type Checking Application

tcap :: TypeEnv -> NameSupply -> VExp -> VExp -> Maybe (Subst, TypeExp)
tcap gamma ns e1 e2 = 
  let tvn = nextName ns
      ns' = deplete ns
   in do (phi, [t1, t2]) <- tcl gamma ns' [e1, e2]
         phi'            <- unify phi (t1, t2 `arrow` TVar tvn)
         pure (phi', phi' tvn)

--------------------------------------------------------------------------------
-- Type Checking Lambda

tclambda :: TypeEnv -> NameSupply -> VName -> VExp -> Maybe (Subst, TypeExp)
tclambda gamma ns x e = 
  let ns'    = deplete ns
      tvn    = nextName ns

      -- gamma' = gamma with a 'bound' x (x is now a type scheme with no vars)  
      gamma' = Map.insert x (Scheme [] (TVar tvn)) gamma
   in do (phi, t) <- tc gamma' ns' e
         pure (phi, phi tvn `arrow` t)

--------------------------------------------------------------------------------
-- Type Checking Let

tclet :: TypeEnv -> NameSupply -> [VName] -> [VExp] -> VExp -> Maybe (Subst, TypeExp)
tclet gamma ns xs es e = 
  let (ns0, ns1, ns2) = split3 ns
   in do (phi, ts) <- tcl gamma ns2 es
         let gamma'  = subTE phi gamma
             gamma'' = addDecls gamma' ns0 xs ts
         (phi', t) <- tc gamma'' ns1 e
         pure (phi' `scomp` phi, t)
        --  tclet1 gamma ns0 xs e (tcl gamma ns1 es)

split3 :: NameSupply -> (NameSupply, NameSupply, NameSupply)
split3 ns = let (ns0, ns1) = split ns
                (ns2, ns3) = split ns0
             in (ns2, ns3, ns1)

-- | add the declarations [VName] => [TypeExp]
-- | to the type environment
addDecls :: TypeEnv -> NameSupply -> [VName] -> [TypeExp] -> TypeEnv
addDecls gamma ns xs ts = 
  let unknowns = unknownsTE gamma
      schemes = map (genbar unknowns ns) ts
   in foldr (uncurry Map.insert) gamma (xs `zip` schemes)

-- | identify the type scheme from the unknowns and type expresssion
-- | 
-- | unknowns = the unknowns in the greater type environment
-- | (which may be bound so they are not to be messed with, hence
-- |  not being schematic vars (?))
-- | further, all the schematic type vars are then given 
-- | a new name in the final schematic to avoid clashing
genbar :: [TVName] -> NameSupply -> TypeExp -> TypeScheme
genbar unknowns ns t = 
  let scvs = nub (tvarsIn t) \\ unknowns
      al   = scvs `zip` nameSequence ns
      t'   = subType (alToSubst al) t
   in Scheme (map snd al) t'

--------------------------------------------------------------------------------
-- Type Checking Letrec

tcletrec :: TypeEnv -> NameSupply -> [VName] -> [VExp] -> VExp -> Maybe (Subst, TypeExp)
tcletrec gamma ns xs es e = 
  do let (ns0, ns') = split ns
         (ns1, ns2) = split ns'
         nbvs       = newBVars xs ns2

     -- phi, ts of type checking the declaration expressions (with bound binding vars)
     (phi, ts) <- tcl (Map.union nbvs gamma) ns1 es

     -- perform substitutions derived from above type checking
     -- on the environment and bound vars
     -- then unify the type expressions with and without these substitutions
     let gamma' = subTE phi gamma
         nbvs'  = subTE phi nbvs
         ts'    = map oldBVar (Map.elems nbvs')

     -- phi' = substitutions after unification of original typechecking and 
     --        application of substitutions from original typechecking
     phi' <- unifyl phi (ts `zip` ts')

     -- further propogate the new substitutions after unification
     -- into the new bound variables
     let (ns3, ns4) = split ns0
         nbvs''     = subTE phi' nbvs'
         ts''       = map oldBVar (Map.elems nbvs'')
         gamma'''   = addDecls (subTE phi' gamma') ns3 (Map.keys nbvs') ts''

     (phi'', t) <- tc gamma''' ns4 e
     pure (phi'' `scomp` phi', t)

newBVars :: [VName] -> NameSupply -> TypeEnv
newBVars xs ns = Map.fromList $ zip xs (map (Scheme [] . TVar) (nameSequence ns))

oldBVar :: TypeScheme -> TypeExp
oldBVar (Scheme [] t) = t
oldBVar s = error $ "Not a b-var scheme" ++ show s

--------------------------------------------------------------------------------
-- Examples

-- simple case, p151
vexp1 :: VExp 
vexp1 = Lambda "x" (Lambda "y" (Lambda "z" 
          (Ap (Ap (Var "x") (Var "z")) 
              (Ap (Var "y") (Var "z")))))

test_vexp1 :: IO ()
test_vexp1 = 
  let t_env = Map.fromList [("x", Scheme [] $ arrow (arrow int_type text_type) char_type),
                            ("y", Scheme [] $ arrow int_type text_type),
                            ("z", Scheme [] text_type)]
   in test_tc_env t_env vexp1

vexp_var :: (TypeEnv, VExp)
vexp_var = 
  ( Map.fromList [("x", Scheme [] int_type)],
    Var "x")

vexp_simple_ap :: (TypeEnv, VExp)
vexp_simple_ap =
  ( Map.fromList [("f", Scheme [] (arrow int_type int_type)),
                  ("x", Scheme [] text_type)],
    Ap (Var "f") (Var "x") ) 

vexp_lambda_ap :: (TypeEnv, VExp)
vexp_lambda_ap = 
  ( Map.fromList [("a", Scheme [] $ arrow int_type (arrow text_type char_type)),
                  ("b", Scheme [] $ arrow int_type text_type),
                  ("c", Scheme [] int_type)],
    mkAp [ Lambda "x" (Lambda "y" (Lambda "z" 
             (Ap (Ap (Var "x") (Var "z")) 
                 (Ap (Var "y") (Var "z"))))),
           Var "a",
           Var "b",
           Var "c" ] )

test_tc :: VExp -> IO ()
test_tc vexp = 
  let type_env = Map.empty
      name_sup = TVName [0]
      checked = tc type_env name_sup vexp
   in case checked of 
        Nothing -> error "Did not type check"
        Just (_, t) -> print t

test_tc_env :: TypeEnv -> VExp -> IO ()
test_tc_env type_env vexp = 
  let name_sup = TVName [0]
      checked = tc type_env name_sup vexp
   in case checked of 
        Nothing -> error "Did not type check"
        Just (s, t) -> print (subType s t)