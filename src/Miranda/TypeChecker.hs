module Miranda.TypeChecker 
  ( TypeEnv
  , typeCheckLambda
  , progTypeEnv
  ) where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M

import Miranda.TypeExpr

type TypeEnv = Map.Map String TypeScheme

data TypeScheme = TScheme [String] TypeExpr

--------------------------------------------------------------------------------
-- Default Type Environment

primTypeEnv :: TypeEnv
primTypeEnv = Map.empty

--------------------------------------------------------------------------------
-- Names

newSchematicNames :: Int -> [String]
newSchematicNames n = map (\i -> "_t" ++ show i ++ "'") [1..n]

--------------------------------------------------------------------------------
-- Type Constructors

mkArrow :: TypeExpr -> TypeExpr -> TypeExpr
mkArrow l r = TCons "Arrow" [l, r]

mkArrowFromList :: [TypeExpr] -> TypeExpr
mkArrowFromList [] = error "mkArrowFromList: No vars provided"
mkArrowFromList vs = foldr1 mkArrow vs

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
      let scheme_vars = newSchematicNames (length ps + 1)
       in TScheme scheme_vars 
            (mkArrowFromList (map TVar scheme_vars))

--------------------------------------------------------------------------------
-- TypeCheck

typeCheckLambda :: TypeEnv -> S.Exp -> S.Exp
typeCheckLambda = undefined
