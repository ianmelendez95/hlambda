{-# LANGUAGE TupleSections #-}

module MyTypeChecker where

import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type VName = String

data VExp = Var VName
          | Lambda VName VExp
          | Ap VExp VExp
          | Let VName VExp VExp
          | Letrec [VName] [VExp] VExp
          deriving Show
    
type TVName = String

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

type SubstEnv = Map.Map TVName TypeExp

data TypeScheme = Scheme [TVName] TypeExp
                deriving Show

type TypeEnv = Map.Map VName TypeScheme

data CheckerEnv = CheckerEnv {
  cenvNextNameNum :: Int,
  cenvSubstEnv :: SubstEnv
}

type CheckerS = StateT CheckerEnv Maybe

--------------------------------------------------------------------------------
-- Type Expressions

tvarsIn :: TypeExp -> [TVName]
tvarsIn (TVar n) = [n]
tvarsIn (TCons _ es) = concatMap tvarsIn es

--------------------------------------------------------------------------------
-- Type Checking

tc :: TypeEnv -> VExp -> CheckerS TypeExp
tc env (Var v_name) = tcVar env v_name 
tc env (Lambda var body) = tcLambda env var body
tc env (Ap e1 e2) = tcAp env e1 e2
tc env (Let b_var b_expr body) = tcLet env b_var b_expr body
tc _ e = error $ "tc: not impl: " ++ show e

tcVar :: TypeEnv -> VName -> CheckerS TypeExp
tcVar = getTypeInstance

tcLambda :: TypeEnv -> VName -> VExp -> CheckerS TypeExp
tcLambda env lvar lbody = 
  do (bound_name, new_env) <- bindTVar lvar env
     body_t <- tc new_env lbody
     arrow_type (TVar bound_name) body_t

tcAp :: TypeEnv -> VExp -> VExp -> CheckerS TypeExp
tcAp env e1 e2 = 
  do e1_te <- tc env e1
     e2_te <- tc env e2

     res_type <- getNextName
     e1_te'   <- arrow_type e2_te (TVar res_type)
     unify e1_te e1_te'

     pure $ TVar res_type

tcLet :: TypeEnv -> VName -> VExp -> VExp -> CheckerS TypeExp
tcLet env b_var b_expr body = undefined

--------------------------------------------------------------------------------
-- Unification

-- | TODO: fixpoint check requires full comparison
unify :: TypeExp -> TypeExp -> CheckerS ()
unify e1@(TVar v) e2 =
  do e1' <- getSubst v
     if e1 == e1'
       then insertSubst v e2
       else unify e1' e2

unify e1 e2@(TVar _) = unify e2 e1

unify e1@(TCons c1 tes1) e2@(TCons c2 tes2)
  | c1 /= c2         = error $ "Type Constructors don't match: " ++ show e1 ++ " /= " ++ show e2
  | length tes1 
      /= length tes2 = error $ "Number of type vars don't match: " ++ show e1 ++ " /= " ++ show e2
  | otherwise        = mapM_ (uncurry unify) (zip tes1 tes2)

--------------------------------------------------------------------------------
-- TypeExps

mapTVars :: (TVName -> TypeExp) -> TypeExp -> TypeExp
mapTVars f (TVar n) = f n
mapTVars f (TCons cons tes) = TCons cons (map (mapTVars f) tes)

--------------------------------------------------------------------------------
-- Names

getNextNamesMapped :: [a] -> CheckerS (Map.Map String a)
getNextNamesMapped xs = 
  Map.fromList <$> mapM (\x -> 
                          do next_name <- getNextName
                             pure (next_name, x)) 
                        xs

getNextName :: CheckerS String
getNextName = ("_T" ++) . show <$> _getNextNameNum

_getNextNameNum :: CheckerS Int
_getNextNameNum = 
  do next_n_num <- gets cenvNextNameNum
     modify (\c_env -> c_env { cenvNextNameNum = next_n_num + 1 })
     pure next_n_num

--------------------------------------------------------------------------------
-- Type Environments

-- | 'binds' the type variable in the type environment, making it a 
-- | bound scheme var and returning the generated name for the var
-- |
-- | returns (gen'd scheme name, env with bound var)
bindTVar :: TVName -> TypeEnv -> CheckerS (TVName, TypeEnv)
bindTVar v env = 
  do scheme_name <- getNextName
     let new_env = putSimpleVarBinding v scheme_name env
     pure (scheme_name, new_env)

putSimpleVarBinding :: TVName -> TVName -> TypeEnv -> TypeEnv
putSimpleVarBinding orig_name new_name = 
  Map.insert orig_name (Scheme [] (TVar new_name))

--------------------------------------------------------------------------------
-- TypeSchemes

getTypeInstance :: TypeEnv -> VName -> CheckerS TypeExp
getTypeInstance tenv name =
  do tscheme <- lift $ Map.lookup name tenv
     newSchemeInstance tscheme

newSchemeInstance :: TypeScheme -> CheckerS TypeExp
newSchemeInstance (Scheme scheme_vars type_exp) = 
  do scheme_to_instance_vars <- getNextNamesMapped scheme_vars
     let lookupVar v = TVar $ fromMaybe v (Map.lookup v scheme_to_instance_vars)
     pure $ mapTVars lookupVar type_exp

--------------------------------------------------------------------------------
-- Substitutions

getSubst :: TVName -> CheckerS TypeExp
getSubst n = 
  do subst <- getSubstEnv
     pure $ applySubst subst n

getExpSubst :: TypeExp -> CheckerS TypeExp
getExpSubst (TVar v) = getSubst v
getExpSubst (TCons c_name es) = 
  do es' <- mapM getExpSubst es
     pure $ TCons c_name es'

getAppliedSubst :: (SubstEnv -> a -> b) -> a -> CheckerS b
getAppliedSubst substF x = 
  do subst <- getSubstEnv
     pure $ substF subst x

getSubstEnv :: CheckerS SubstEnv
getSubstEnv = gets cenvSubstEnv

insertSubst :: TVName -> TypeExp -> CheckerS ()
insertSubst n e
  | TVar n == e = pure ()
  | n `elem` tvarsIn e = error $ "Cannot construct infinite type: " ++ n ++ " -> " ++ show e
  | otherwise = 
    do s_env <- getSubstEnv
       modify (\cenv -> cenv { cenvSubstEnv = Map.insert n e s_env })

applySubst :: SubstEnv -> TVName -> TypeExp
applySubst env n = fromMaybe (TVar n) (Map.lookup n env)

applySubstTExp :: SubstEnv -> TypeExp -> TypeExp
applySubstTExp subst (TVar v) = applySubst subst v
applySubstTExp subst (TCons c_name es) = TCons c_name (map (applySubstTExp subst) es)

applySubstTScheme :: SubstEnv -> TypeScheme -> TypeScheme
applySubstTScheme subst (Scheme s_vars s_exp) = 
  let temp_subst = Map.withoutKeys subst (Set.fromList s_vars)
   in Scheme s_vars $ applySubstTExp temp_subst s_exp 

applySubstTEnv :: SubstEnv -> TypeEnv -> TypeEnv
applySubstTEnv subst = Map.map (applySubstTScheme subst)

id_subst :: SubstEnv
id_subst = Map.empty

--------------------------------------------------------------------------------
-- Examples

arrow_type :: TypeExp -> TypeExp -> CheckerS TypeExp 
arrow_type t1 t2 = 
  do t1' <- getExpSubst t1
     t2' <- getExpSubst t2
     pure $ TCons "Arrow" [t1', t2']

int_type :: TypeExp
int_type = TCons "Int" []

vexp_var :: (TypeEnv, VExp)
vexp_var = 
  ( Map.fromList [("x", Scheme [] int_type)],
    Var "x")

vexp_lambda_var :: (TypeEnv, VExp)
vexp_lambda_var = 
  ( Map.empty,
    Lambda "x" (Var "x"))

test_tc :: TypeEnv -> VExp -> IO ()
test_tc tenv vexp = 
  let check_env = CheckerEnv {
        cenvNextNameNum = 0,
        cenvSubstEnv = id_subst
      }
      checked = evalStateT (tc tenv vexp) check_env
   in case checked of 
        Nothing -> error "Did not type check"
        Just t -> print t