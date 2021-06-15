{-# LANGUAGE TupleSections #-}

module Miranda.TypeChecker 
  ( TCState
  , TCError
  , TypeEnv
  , runTypeChecker
  , typeCheck
  , progTypeEnv
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M

import Control.Monad.State.Lazy

import Miranda.TypeExpr
    ( TypeExpr(..), mkBoundTVar, mkUnboundTVar, mapUnboundTVarsM )

-- | prog domain => type domain
type TypeEnv = Map.Map String TypeExpr

-- | type domain => type domain
type SubstEnv = Map.Map String TypeExpr

data TCEnv = TCEnv {
    tcenvCurNameIndex :: Int,
    tcenvSubstEnv :: SubstEnv
  }

type TCState = StateT TCEnv (Either TCError)

newtype TCError = TCError String

instance Show TCError where
  show (TCError msg) = "Type Check Error: " ++ msg

--------------------------------------------------------------------------------
-- Type Checker State

runTypeChecker :: TCState a -> Either TCError a
runTypeChecker tcs = evalStateT tcs empty_TCEnv

empty_TCEnv :: TCEnv
empty_TCEnv = TCEnv { tcenvCurNameIndex = 0, tcenvSubstEnv = Map.empty }

newTVarName :: TCState String
newTVarName = 
  do cur_n <- gets tcenvCurNameIndex
     modify (\tcenv -> tcenv { tcenvCurNameIndex = cur_n + 1 })
     pure $ "_t" ++ show cur_n

getTVarSubst :: String -> TCState (Maybe TypeExpr)
getTVarSubst var = gets (\TCEnv{tcenvSubstEnv = senv} -> Map.lookup var senv)

putSubst :: String -> TypeExpr -> TCState ()
putSubst name expr = modify (\tcenv@TCEnv{tcenvSubstEnv = senv} -> 
                               tcenv{ tcenvSubstEnv = Map.insert name expr senv })

--------------------------------------------------------------------------------
-- Unification

-- | both considers substitutions, 
-- | and checks against constructors
unify :: TypeExpr -> TypeExpr -> TCState ()
unify e1@(TVar _ v) e2 = 
  do e1' <- fromMaybe e1 <$> getTVarSubst v
     if e1 == e1'
       then putSubst v e2
       else unify e1' e2 

unify e tv@(TVar _ _) = unify tv e
     
unify cons1@(TCons c1 es1) cons2@(TCons c2 es2) 
  | c1 /= c2 = lift . Left $ 
      TCError $ "Constructed types don't match:\n" ++ show cons1 ++ " /= " ++ show cons2 
  | length es1 /= length es2 = error $ 
      "Constructed types have differing number of constituent expressions:\n" ++ show cons1 ++ " /= " ++ show cons2
  | otherwise = mapM_ (uncurry unify) (zip es1 es2)

--------------------------------------------------------------------------------
-- Type Environment

primTypeEnv :: TypeEnv
primTypeEnv = Map.empty

insertScheme :: String -> TypeExpr -> TypeEnv -> TypeEnv 
insertScheme = Map.insert

progTypeEnv :: M.Prog -> TCState TypeEnv
progTypeEnv (M.Prog decls _) = foldM declTypeEnv primTypeEnv decls

declTypeEnv :: TypeEnv -> M.Decl -> TCState TypeEnv
declTypeEnv _   tdef@(M.TypeDef _) = error $ "Type Definitions not supported yet " ++ show tdef
declTypeEnv env (M.TypeSpec (M.TSpec name expr)) = pure $ Map.insert name expr env
declTypeEnv env (M.AssignDef (M.FuncDef (M.FDef fname fspec))) = 
  funcDefEnv env fname fspec
declTypeEnv env (M.AssignDef _) = pure env 

-- TODO: possibly an opportunity for guessing based on the 
--       patterns, like constructors and constants
-- | Adds a new type scheme for the function from the func definition
-- | (if not already defined)
-- |
funcDefEnv :: TypeEnv -> String -> M.DefSpec -> TCState TypeEnv
funcDefEnv env fname (M.DefSpec ps _) = 
  if Map.member fname env
    then pure env 
    else do scheme <- funcSpecScheme
            pure $ Map.insert fname scheme env
  where 
    funcSpecScheme :: TCState TypeExpr
    funcSpecScheme = 
      do scheme_vars <- replicateM (length ps + 1) newTVarName
         pure $ mkArrowFromList (map mkUnboundTVar scheme_vars)

--------------------------------------------------------------------------------
-- Type Expressions

newSchemeInstance :: TypeExpr -> TCState TypeExpr
newSchemeInstance = mapUnboundTVarsM (\_ -> mkUnboundTVar <$> newTVarName)

mkArrow :: TypeExpr -> TypeExpr -> TypeExpr
mkArrow l r = TCons "Arrow" [l, r]

mkArrow' :: [TypeExpr] -> TypeExpr
mkArrow' = mkArrowFromList

mkArrowFromList :: [TypeExpr] -> TypeExpr
mkArrowFromList [] = error "mkArrowFromList: No vars provided"
mkArrowFromList vs = foldr1 mkArrow vs

-- arithmetic

bin_int_type :: TypeExpr
bin_int_type = mkArrow' [int_type, int_type, int_type]

bin_bool_type :: TypeExpr
bin_bool_type = mkArrow' [bool_type, bool_type, bool_type]

-- constants

int_type :: TypeExpr
int_type = TCons "Int" []

char_type :: TypeExpr
char_type = TCons "Char" []

bool_type :: TypeExpr
bool_type = TCons "Bool" []

-- misc

listType :: TypeExpr -> TypeExpr
listType elem_type = TCons "List" [elem_type]

tupleType :: [TypeExpr] -> TypeExpr
tupleType types = TCons ("Tuple-" ++ show (length types)) types

anyType :: TCState TypeExpr
anyType = mkUnboundTVar <$> newTVarName

-- type checking

typeCheck :: TypeEnv -> S.Exp -> TCState TypeExpr
typeCheck env (S.Term (S.Variable v)) = tcVariable env v
typeCheck _ (S.Term (S.Constant c)) = tcConstant c
typeCheck _ (S.Term (S.Function f)) = tcFunction f
typeCheck env (S.Lambda v b) = tcLambda env v b
typeCheck env (S.Apply e1 e2) = tcApply env e1 e2
typeCheck env (S.Let (bv, be) e) = tcLet env bv be e
typeCheck env (S.Letrec bs e) = tcLetrec env bs e

-- Let 

tcLet :: TypeEnv -> S.Variable -> S.Exp -> S.Exp -> TCState TypeExpr
tcLet env bind_var bind_expr body_expr = 
  do bind_scheme <- typeCheck env bind_expr

     bind_type <- checkIfProvidedType env bind_var bind_scheme

     typeCheck (Map.insert bind_var bind_type env) body_expr

-- Letrec

tcLetrec :: TypeEnv -> [(S.Variable, S.Exp)] -> S.Exp -> TCState TypeExpr
tcLetrec env bindings body =
  do bname_to_bound_tvar <- 
       mapM (\(name, _) -> do new_tvar <- mkBoundTVar <$> newTVarName
                              pure (name, new_tvar)) 
            bindings
      
     let temp_env = foldr (uncurry Map.insert) env bname_to_bound_tvar

     bexpr_types <- 
       Map.fromList <$> mapM (\(name, expr) -> do texpr <- typeCheck temp_env expr
                                                  texpr' <- checkIfProvidedType env name texpr
                                                  pure (name, texpr')) 
                             bindings

     -- check bexpr_types for accuracy (by unifying against provided types, if available)
     

     typeCheck (Map.union bexpr_types env) body

checkIfProvidedType :: TypeEnv -> S.Variable -> TypeExpr -> TCState TypeExpr
checkIfProvidedType env pvar texpr = 
  case Map.lookup pvar env of 
    Nothing -> pure texpr
    Just existing -> unify existing texpr >> pure existing

-- Variables

tcVariable :: TypeEnv -> S.Variable -> TCState TypeExpr
tcVariable env v = 
  case Map.lookup v env of 
    Nothing -> error $ 
                 "Variable not in type environment: " ++ show v 
                   ++ "\nCurrent Environment: \n" ++ show env
    Just scheme -> newSchemeInstance scheme

-- Lambdas 

tcLambda :: TypeEnv -> S.Variable -> S.Exp -> TCState TypeExpr
tcLambda env lvar lbody = 
  do body_svar <- mkBoundTVar <$> newTVarName
     let env' = insertScheme lvar body_svar env
     lbody_type <- typeCheck env' lbody
     pure $ mkArrow body_svar lbody_type 

-- Apply

tcApply :: TypeEnv -> S.Exp -> S.Exp -> TCState TypeExpr
tcApply env e1 e2 = 
  do e1_type <- typeCheck env e1
     e2_type <- typeCheck env e2

     res_type <- mkUnboundTVar <$> newTVarName
     let e1_arrow_type = mkArrow e2_type res_type
     unify e1_arrow_type e1_type

     pure res_type

-- Constants

tcConstant :: S.Constant -> TCState TypeExpr
tcConstant (S.CNat _)  = pure int_type
tcConstant (S.CChar _) = pure char_type
tcConstant (S.CBool _) = pure char_type
tcConstant S.CNil      = pure char_type
tcConstant S.CFail     = anyType
tcConstant S.CError    = anyType

-- Functions

tcFunction :: S.Function -> TCState TypeExpr
tcFunction S.FPlus      = pure bin_int_type
tcFunction S.FMinus     = pure bin_int_type
tcFunction S.FMult      = pure bin_int_type
tcFunction S.FDiv       = pure bin_int_type

tcFunction S.FAnd       = pure bin_bool_type
tcFunction S.FOr        = pure bin_bool_type
tcFunction S.FNot       = pure $ mkArrow bool_type bool_type

tcFunction S.FEq        = _compType
tcFunction S.FNEq       = _compType
tcFunction S.FLt        = _compType
tcFunction S.FGt        = _compType
     
tcFunction S.FHead      = _listElemType
tcFunction S.FTail      = _listElemType
tcFunction S.FCons      = 
  do elem_type <- anyType
     let list_type = listType elem_type
     pure $ mkArrow' [elem_type, list_type, list_type]

tcFunction (S.FTuple n) = 
  do tuple_vars <- mapM (const anyType) [1..n]
     pure $ tupleType tuple_vars

tcFunction S.FIf        = 
  do res_type <- anyType
     pure $ mkArrow' [bool_type, res_type, res_type]

_listElemType :: TCState TypeExpr
_listElemType = 
  do elem_type <- anyType
     let list_type = listType elem_type
     pure $ mkArrow list_type elem_type

_compType :: TCState TypeExpr
_compType = 
  do type_var <- anyType
     pure $ mkArrow' [type_var, type_var, bool_type]
