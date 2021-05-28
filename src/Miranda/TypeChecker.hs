{-# LANGUAGE TupleSections #-}

module Miranda.TypeChecker 
  ( TCState
  , TypeEnv
  , runTypeChecker
  , typeCheck
  , progTypeEnv
  ) where

import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M

import Control.Monad.State.Lazy

import Miranda.TypeExpr

type TypeEnv = Map.Map String TypeScheme

type SubstEnv = Map.Map String TypeExpr

data TypeScheme = TScheme [String] TypeExpr
                deriving Show

data TCEnv = TypeCheckerEnv {
    tcenvCurNameIndex :: Int,
    tcenvSubstEnv :: SubstEnv
  }

type TCState = StateT TCEnv (Either TypeCheckError)

data TypeCheckError = TCError String
                    deriving Show

runTypeChecker :: TCState a -> Either TypeCheckError a
runTypeChecker tcs = evalStateT tcs empty_TCEnv

empty_TCEnv :: TCEnv
empty_TCEnv = TypeCheckerEnv { tcenvCurNameIndex = 0, tcenvSubstEnv = Map.empty }

--------------------------------------------------------------------------------
-- Util

safeLookup :: Ord a => a -> Map.Map a a -> a
safeLookup k m = fromMaybe k (Map.lookup k m)

--------------------------------------------------------------------------------
-- Default Type Environment

primTypeEnv :: TypeEnv
primTypeEnv = Map.empty

--------------------------------------------------------------------------------
-- Names

nNewSchematicNames :: Int -> [String]
nNewSchematicNames n = map newSchematicVar [1..n]

newTypeVar, newSchematicVar :: Int -> String
newTypeVar      = ("_t" ++) . show
newSchematicVar = (++ "'") . newTypeVar

--------------------------------------------------------------------------------
-- Type Schemes

schemeFromTypeExpr :: TypeExpr -> TCState TypeScheme
schemeFromTypeExpr expr = 
  do tv_to_sv_assoc <- 
       mapM (\v -> (v,) <$> newTVarName) (tvarsIn expr)

     let scheme_vars = map snd tv_to_sv_assoc
         tv_to_sv    = Map.fromList tv_to_sv_assoc
         scheme_expr =
           mapTVars (\v -> TVar $ safeLookup v tv_to_sv) expr

     pure $ TScheme scheme_vars scheme_expr

newSchemeInstance :: TypeScheme -> TCState TypeExpr
newSchemeInstance (TScheme svars sexpr) = 
  do svar_to_ivar <- sVarsToInstNames svars
     pure $ mapTVars 
              (\v -> TVar $ fromMaybe v (Map.lookup v svar_to_ivar)) 
              sexpr
  where 
    sVarsToInstNames :: [String] 
                     -> TCState (Map.Map String String)
    sVarsToInstNames vs = 
      Map.fromList <$> mapM (\v -> (v,) <$> newTVarName) vs

typeSchemeVars :: TypeScheme -> [String]
typeSchemeVars (TScheme _ expr) = tvarsIn expr

--------------------------------------------------------------------------------
-- Type Constructors

-- Arrow

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
anyType = TVar <$> newTVarName

--------------------------------------------------------------------------------
-- Parse Type Environments

progTypeEnv :: M.Prog -> TCState TypeEnv
progTypeEnv (M.Prog decls _) = _validateTypeEnv <$> foldM declTypeEnv primTypeEnv decls

_validateTypeEnv :: TypeEnv -> TypeEnv
_validateTypeEnv = varsUnique
  where 
    varsUnique :: TypeEnv -> TypeEnv
    varsUnique env = 
      let vars = Map.foldr (\scheme vs -> typeSchemeVars scheme ++ vs) [] env
       in case findDup vars of
            Nothing -> env
            Just dup -> error $ "Type Environment has duplicated variable: " ++ dup
    
    findDup :: [String] -> Maybe String
    findDup = findDupSorted . sort

    findDupSorted :: [String] -> Maybe String
    findDupSorted []  = Nothing
    findDupSorted [_] = Nothing
    findDupSorted (x:y:xs) = if x == y then Just x else findDupSorted (y:xs)

declTypeEnv :: TypeEnv -> M.Decl -> TCState TypeEnv
declTypeEnv _   tdef@(M.TypeDef _) = error $ "Type Definitions not supported yet " ++ show tdef
declTypeEnv env (M.TypeSpec (M.TSpec name expr)) = pure $ typeSpecEnv env name expr
declTypeEnv env (M.AssignDef (M.FuncDef (M.FDef fname fspec))) = 
  funcDefEnv env fname fspec
declTypeEnv env (M.AssignDef _) = pure env 

typeSpecEnv :: TypeEnv -> String -> TypeExpr -> TypeEnv
typeSpecEnv env name expr = Map.insert name (TScheme (tvarsIn expr) expr) env

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
    funcSpecScheme :: TCState TypeScheme
    funcSpecScheme = 
      do scheme_vars <- replicateM (length ps + 1) newTVarName
         pure $ TScheme scheme_vars (mkArrowFromList (map TVar scheme_vars))

--------------------------------------------------------------------------------
-- Type Checker State

insertScheme :: String -> TypeScheme -> TypeEnv -> TypeEnv 
insertScheme = Map.insert

newTVar :: TCState TypeExpr
newTVar = TVar <$> newTVarName

newTVarName :: TCState String
newTVarName = undefined

--------------------------------------------------------------------------------
-- Unification

unify :: TypeExpr -> TypeExpr -> TCState ()
unify = undefined

--------------------------------------------------------------------------------
-- TypeCheck

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
  do bind_scheme <- typeCheck env bind_expr >>= schemeFromTypeExpr

     let env' = insertScheme bind_var bind_scheme env
     typeCheck env' body_expr

-- Letrec

tcLetrec :: TypeEnv -> [(S.Variable, S.Exp)] -> S.Exp -> TCState TypeExpr
tcLetrec env bindings body =
  do bname_to_temp_scheme <- 
       mapM ((\bname -> (bname,) <$> newBoundVarScheme) . fst) bindings
      
     let temp_env = foldr (uncurry Map.insert) env bname_to_temp_scheme

     bexpr_schemes <- mapM ((schemeFromTypeExpr <=< typeCheck temp_env) . snd) bindings

     let bname_to_scheme :: [(String, TypeScheme)]
         bname_to_scheme = zip (map fst bname_to_scheme) bexpr_schemes

         env' = Map.union (Map.fromList bname_to_scheme) env
     
     typeCheck env' body
  where 
    newBoundVarScheme :: TCState TypeScheme
    newBoundVarScheme = TScheme [] <$> newTVar

_letBindingScheme :: TypeEnv -> S.Exp -> TCState TypeScheme
_letBindingScheme env e = typeCheck env e >>= schemeFromTypeExpr

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
  do body_svar <- newTVarName
     let env' = insertScheme lvar (TScheme [] (TVar body_svar)) env
     lbody_type <- typeCheck env' lbody
     pure $ mkArrow (TVar body_svar) lbody_type 

-- Apply

tcApply :: TypeEnv -> S.Exp -> S.Exp -> TCState TypeExpr
tcApply env e1 e2 = 
  do e1_type <- typeCheck env e1
     e2_type <- typeCheck env e2

     res_type <- newTVar
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

tcFunction S.FAnd       = pure $ bin_bool_type
tcFunction S.FOr        = pure $ bin_bool_type
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