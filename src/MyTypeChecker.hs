{-# LANGUAGE TupleSections #-}

module MyTypeChecker where

import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type VName = String

data VExp = Var VName
          | Lambda VName VExp
          | Ap VExp VExp
          | Let [VName] [VExp] VExp
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

type Subst = [Map.Map TVName TypeExp]

data TypeScheme = Scheme [TVName] TypeExp
                deriving Show

type TypeEnv = Map.Map VName TypeScheme

data CheckerEnv = CheckerEnv {
  cenvNextNameNum :: Int,
  cenvTypeEnv :: TypeEnv
}

type CheckerS = StateT CheckerEnv Maybe

--------------------------------------------------------------------------------
-- Type Checking

tc :: VExp -> CheckerS (Subst, TypeExp)
tc (Var v_name) = (id_subst,) <$> tcVar v_name 
tc (Lambda var body) = tcLambda var body
tc e = error $ "tc: not impl: " ++ show e

tcVar :: VName -> CheckerS TypeExp
tcVar = getTypeInstance

tcLambda :: VName -> VExp -> CheckerS (Subst, TypeExp)
tcLambda lvar lbody = 
  do new_name <- getNextName
     putSimpleVarBinding lvar new_name
     (subst, new_t) <- tc lbody
     pure (subst, arrow_type (applySubst subst new_name) new_t)

--------------------------------------------------------------------------------
-- Type Checker Environment

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> CheckerS ()
modifyTypeEnv f = modify (\cenv -> cenv { cenvTypeEnv = f (cenvTypeEnv cenv) })

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
getNextName = ('T':) . show <$> _getNextNameNum

_getNextNameNum :: CheckerS Int
_getNextNameNum = 
  do next_n_num <- gets cenvNextNameNum
     modify (\c_env -> c_env { cenvNextNameNum = next_n_num + 1 })
     pure next_n_num

--------------------------------------------------------------------------------
-- Type Environments

putSimpleVarBinding :: TVName -> TVName -> CheckerS ()
putSimpleVarBinding orig_name new_name = 
  modifyTypeEnv $ Map.insert orig_name (Scheme [] (TVar new_name))

--------------------------------------------------------------------------------
-- TypeSchemes

getTypeInstance :: VName -> CheckerS TypeExp
getTypeInstance name =
  do tenv <- gets cenvTypeEnv
     tscheme <- lift $ Map.lookup name tenv
     newSchemeInstance tscheme

newSchemeInstance :: TypeScheme -> CheckerS TypeExp
newSchemeInstance (Scheme scheme_vars type_exp) = 
  do scheme_to_instance_vars <- getNextNamesMapped scheme_vars
     let lookupVar v = TVar $ fromMaybe v (Map.lookup v scheme_to_instance_vars)
     pure $ mapTVars lookupVar type_exp

--------------------------------------------------------------------------------
-- Substitutions

applySubst :: Subst -> TVName -> TypeExp
applySubst [] n = TVar n
applySubst (m:ms) n = 
  let n_subst = fromMaybe (TVar n) (Map.lookup n m)
   in mapTVars (applySubst ms) n_subst

id_subst :: Subst 
id_subst = []

--------------------------------------------------------------------------------
-- Examples

arrow_type :: TypeExp -> TypeExp -> TypeExp 
arrow_type t1 t2 = TCons "Arrow" [t1, t2]

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
        cenvTypeEnv = tenv
      }
      checked = evalStateT (tc vexp) check_env
   in case checked of 
        Nothing -> error "Did not type check"
        Just (_, t) -> print t