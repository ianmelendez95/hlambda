module Miranda.Compiler where

import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M
import Miranda.TypeChecker

import Lambda.ToLambda
import Parse

compileStr :: String -> S.Exp
compileStr str = 
  let prog = either error id (parse str)
   in typeCheckLambda (progTypeEnv prog) (toLambda prog)