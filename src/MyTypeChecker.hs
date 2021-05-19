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

-- TODO: add simple test and commit
tc :: VExp -> CheckerS (Subst, TypeExp)
tc (Var v_name) = (id_subst,) <$> tcVar v_name 
tc e = error $ "tc: not impl: " ++ show e

tcVar :: VName -> CheckerS TypeExp
tcVar = getTypeInstance

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

id_subst :: Subst 
id_subst = []
