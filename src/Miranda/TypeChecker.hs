module Miranda.TypeChecker 
  ( TypeEnv
  , typeCheck
  , progTypeEnv
  ) where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M

import Control.Monad.State.Lazy

import Miranda.TypeExpr

type TypeEnv = Map.Map String TypeScheme

type SubstEnv = Map.Map String TypeExpr

data TypeScheme = TScheme [String] TypeExpr

data TCEnv = TypeCheckerEnv {
    tcenvCurNameIndex :: Int,
    tcenvSubstEnv :: SubstEnv
  }

type TCState = StateT TCEnv (Either TypeCheckError)

data TypeCheckError = TCError String
                    deriving Show

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
-- Type Constructors

-- Arrow

mkArrow :: TypeExpr -> TypeExpr -> TypeExpr
mkArrow l r = TCons "Arrow" [l, r]

mkArrowFromList :: [TypeExpr] -> TypeExpr
mkArrowFromList [] = error "mkArrowFromList: No vars provided"
mkArrowFromList vs = foldr1 mkArrow vs

-- Int

int_type :: TypeExpr
int_type = TCons "Int" []

char_type :: TypeExpr
char_type = TCons "Char" []

bool_type :: TypeExpr
bool_type = TCons "Bool" []

list_type :: TypeExpr
list_type = TCons "List" []

anyType :: TCState TypeExpr
anyType = TVar <$> newVarName

--------------------------------------------------------------------------------
-- Parse Type Environments

progTypeEnv :: M.Prog -> TypeEnv
progTypeEnv (M.Prog decls _) = foldl' declTypeEnv primTypeEnv decls

declTypeEnv :: TypeEnv -> M.Decl -> TypeEnv
declTypeEnv _   tdef@(M.TypeDef _) = error $ "Type Definitions not supported yet " ++ show tdef
declTypeEnv env (M.TypeSpec (M.TSpec name expr)) = typeSpecEnv env name expr
declTypeEnv env (M.AssignDef (M.FuncDef (M.FDef fname fspec))) = 
  funcDefEnv env fname fspec
declTypeEnv env (M.AssignDef _) = env 

typeSpecEnv :: TypeEnv -> String -> TypeExpr -> TypeEnv
typeSpecEnv env name expr = Map.insert name (TScheme (tvarsIn expr) expr) env

-- TODO: possibly an opportunity for guessing based on the 
--       patterns, like constructors and constants
-- | Adds a new type scheme for the function from the func definition
-- | (if not already defined)
-- |
funcDefEnv :: TypeEnv -> String -> M.DefSpec -> TypeEnv
funcDefEnv env fname (M.DefSpec ps _) = 
  if Map.member fname env
    then env 
    else Map.insert fname func_spec_scheme env
  where 
    func_spec_scheme :: TypeScheme
    func_spec_scheme = 
      let scheme_vars = nNewSchematicNames (length ps + 1)
       in TScheme scheme_vars 
            (mkArrowFromList (map TVar scheme_vars))

--------------------------------------------------------------------------------
-- Type Checker State

newVarName :: TCState String
newVarName = undefined

--------------------------------------------------------------------------------
-- TypeCheck

typeCheck :: TypeEnv -> S.Exp -> TCState TypeExpr
typeCheck _ (S.Term (S.Constant c)) = tcConstant c

tcConstant :: S.Constant -> TCState TypeExpr
tcConstant (S.CNat _)  = pure int_type
tcConstant (S.CChar _) = pure char_type
tcConstant (S.CBool _) = pure char_type
tcConstant S.CNil      = pure char_type
tcConstant S.CFail  = anyType
tcConstant S.CError = anyType