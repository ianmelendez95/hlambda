module Miranda.Compiler 
  ( CompileError (..)
    
  , compileStr
  , compileProg
  ) where

import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M
import Miranda.TypeChecker

import Lambda.ToLambda
import Parse

data CompileError = ParseError String 
                  | TypeError TCError

instance Show CompileError where 
  show (ParseError msg) = "Parse Error: " ++ msg
  show (TypeError err)  = show err

compileStr :: String -> Either CompileError S.Exp
compileStr str = 
  do parsed <- either (Left . ParseError) Right (parse str)
     compileProg parsed

compileProg :: M.Prog -> Either CompileError S.Exp
compileProg prog = 
  either (Left . TypeError) Right (runTypeChecker (compileProg' prog))

compileProg' :: M.Prog -> TCState S.Exp
compileProg' prog = 
  do env <- progTypeEnv prog
     let lam = toLambda prog
     _ <- typeCheck env lam
     pure lam