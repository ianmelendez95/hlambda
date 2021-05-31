module Miranda.Compiler where

import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M
import Miranda.TypeChecker

import Lambda.ToLambda
import Parse

compileStr :: String -> S.Exp
compileStr str = compileProg (either error id (parse str))

compileProg :: M.Prog -> S.Exp
compileProg prog = 
  case runTypeChecker (compileProg' prog) of
    Left e -> error $ show e
    Right lam -> lam

compileProg' :: M.Prog -> TCState S.Exp
compileProg' prog = 
  do env <- progTypeEnv prog
     let lam = toLambda prog
     _ <- typeCheck env lam
     pure lam