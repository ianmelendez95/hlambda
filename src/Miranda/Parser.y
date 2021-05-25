{
module Miranda.Parser 
  ( ParseResult, 
    parser, 
    parseDecl,
    parseExp,
    parseProgram
  ) where 

import Data.Char
import Data.Either (either, isRight, rights)
import Data.List (nub)
import Data.Maybe (fromMaybe)

import qualified Miranda.Syntax as S
import qualified Miranda.Token as T
import Miranda.Lexer (alexScanTokens, scanTokens, scanTokensEither)

import Debug.Trace
}

%name parser program
%name declParser decl
%name expParser exp
%monad { ParseResult } { >>= } { return }
%tokentype { T.Token }
%error { parseError }

%token 

  '='         { T.Equal             }
  '::='       { T.TypeEq            }
  '::'        { T.DblColon          }
  '->'        { T.Arrow             }
  '('         { T.LP                }
  ')'         { T.RP                }
  '['         { T.LB                }
  ']'         { T.RB                }
  ';'         { T.Semi              }
  ','         { T.Comma             }
  '{'         { T.LC                }
  '}'         { T.RC                }
  '|'         { T.VertBar           }

  'where'     { T.Where             }
  plus        { T.InfixOp T.IPlus       }
  minus       { T.InfixOp T.IMinus      }
  mult        { T.InfixOp T.IMult       }
  div         { T.InfixOp T.IDiv        }
  equal       { T.InfixOp T.IEq         }
  lt          { T.InfixOp T.ILt         }
  gt          { T.InfixOp T.IGt         }
  ':'         { T.InfixOp T.ICons       }
  infix_var   { T.InfixOp (T.IVar _)    } -- $<var-name>
  const       { T.Constant $$       }
  constr      { T.Constructor $$    }
  var         { T.Variable $$       }

  gtype_2plus { T.GenTypeVar $$     }   -- '2plus' because it's two or more '**', since '*' is mult

%%

program :: { S.Prog }
program : '{' stmts '}'          { mkProg (reverse $2) }

-- REVERSE!!
stmts :: { [Stmt] }
stmts : stmts ';' stmt   { $3 : $1 }
      | stmt             { [$1] }

stmt :: { Stmt }
stmt : exp '::=' constructors    { Right $ checkTypeDef $1 (reverse $3) }
     | var '::'  typeExpr        { Right $ S.TypeSpec (S.TSpec $1 $3) }
     | assignDef                 { Right $ S.AssignDef $1 }
     | exp                       { Left  $1 }

--------------------------------------------------------------------------------
-- Assignment Def

assignDef :: { S.AssignDef }
assignDef : exp '=' frhs maybeWhere { checkAssignDef $1 (reverse $3) (fromMaybe [] $4) }

-- REVERSE!!!
assignDefs :: { [S.AssignDef] }
assignDefs : assignDefs ';' assignDef   { $3 : $1 }
           | assignDef                  { [$1] }

--------------------------------------------------------------------------------
-- Func Def

-- REVERSE!!
frhs :: { [S.RhsClause] }
frhs : frhs '=' clause      { $3 : $1 }
     | clause               { [$1] }

clause :: { S.RhsClause }
clause : exp                    { S.BaseClause $1 }
       | exp ',' exp            { S.CondClause $1 $3 }          

maybeWhere :: { Maybe [S.AssignDef] }
maybeWhere : 'where' whereDefs   { Just $2 }
           | {- empty -}         { Nothing }

whereDefs :: { [S.AssignDef] }
whereDefs : '{' assignDefs '}'     { reverse $2 }

--------------------------------------------------------------------------------
-- Type Expressions

typeExpr :: { S.TypeExpr }
typeExpr : var                      { S.TypeVar  $1 }
         | constr typeExprs         { S.TypeCons $1 (reverse $2)  }
         | typeExpr '->' typeExpr   { S.TypeCons "Arrow" [$1, $3] }
         | '(' typeExpr ')'         { $2 }

typeExprs :: { [S.TypeExpr] }
typeExprs : typeExprs typeExpr { $2 : $1 }
          | typeExpr           { [$1] }
          | {- empty -}        { [] }

-------------------------------------------------------------------------------
-- Def

decl :: { S.Decl }
decl : exp '::=' constructors { (checkTypeDef $1 (reverse $3)) }
     | assignDef              { S.AssignDef $1 }

-- REVERSE!!
constructors :: { [S.Constr] }
constructors : constructors '|' exp         { checkConstr $3 : $1 }
             | exp                          { [checkConstr $1] }

-------------------------------------------------------------------------------
-- Expressions

exp :: { S.Exp }
exp : apply         { $1 }
    | infixApp      { $1 }
    | term          { $1 }

infixApp :: { S.Exp }
infixApp : exp infixOp exp  { S.InfixApp $2 $1 $3 }

apply :: { S.Exp }
apply : exp term   { S.Apply $1 $2 }

infixOp :: { T.InfixOp }
infixOp : plus      { T.IPlus  }
        | minus     { T.IMinus }
        | mult      { T.IMult  }
        | div       { T.IDiv   }
        | equal     { T.IEq    }
        | lt        { T.ILt    }
        | gt        { T.IGt    }
        | ':'       { T.ICons  }
        | infix_var { getInfixOp $1 }

term :: { S.Exp }            
term : variable         { $1 }
     | specialLit       { $1 }
     | genTypeVar       { S.EGenTypeVar $1 }
     | constr           { S.Constructor $1 }
     | constant         { $1 }
     | '(' exp ')'      { $2 }
     | infixOp          { S.InfixOp $1 }

variable :: { S.Exp }       
variable : var               { S.Variable $1 }

constant :: { S.Exp }
constant : const             { S.Constant $1 }

genTypeVar :: { S.GenTypeVar } -- GenTypeVar = Int
genTypeVar : gtype_2plus      { $1 }

-------------------------------------------------------------------------------
-- Special Data Syntax (Lists, Tuples)

specialLit :: { S.Exp }
specialLit : listLit      { $1 }
           | tupleLit     { $1 }

listLit :: { S.Exp }
listLit : '[' ']'             { S.ListLit [] }
        | '[' commaSepExp ']' { S.ListLit (reverse $2) }

tupleLit :: { S.Exp }
tupleLit : '(' commaSepExp ')'      { S.Tuple (reverse $2) }

-- REVERSE!!
commaSepExp :: { [S.Exp] }
commaSepExp : commaSepExp ',' exp   { $3 : $1 }
            | exp                   { [$1] }

{
type ParseResult = Either String

--------------------------------------------------------------------------------
-- Parsers

parseError :: [T.Token] -> ParseResult a
parseError tokens = Left $ "Parse Error, tokens left: " ++ show tokens

parseExp :: String -> ParseResult S.Exp
parseExp input = scanTokensEither input >>= expParser . tail . init

parseDecl :: String -> ParseResult S.Decl
parseDecl input = scanTokensEither input >>= declParser . tail . init

parseProgram :: String -> ParseResult S.Prog
parseProgram input = scanTokensEither input >>= parser

getInfixOp :: T.Token -> T.InfixOp
getInfixOp (T.InfixOp i) = i
getInfixOp tok = error $ "Not an infix op: " ++ show tok

--------------------------------------------------------------------------------
-- The Realm of Ambiguity

type Stmt = Either S.Exp S.Decl

mkProg :: [Stmt] -> S.Prog
mkProg stmts = case span isRight stmts of 
                 (defs, [Left expr]) -> S.Prog (rights defs) expr
                 (_, _)        -> error "Expecting a single expression at the end of the program"

--------------------------------------------------------------------------------
-- Coerce Exp -> a

checkAssignDef :: S.Exp -> [S.RhsClause] -> [S.AssignDef] -> S.AssignDef
checkAssignDef lhs rhs wdefs = 
  case flattenApplyLHS lhs of 
    (S.Variable func_name : rest) -> 
      -- want to check there aren't repeated variable names
      let params = map checkPattern rest
          dupes xs = not $ length xs == length (nub xs)
       in if dupes . concatMap S.funcPatternVars $ params
            then error $ "Repeated variable names in lhs: " ++ show lhs ++ ", " ++ show rhs
            else S.FuncDef $ S.mkFuncDef func_name params rhs wdefs
    _ -> S.PattDef $ S.mkPattDef (checkPattern lhs) rhs wdefs

checkTypeDef :: S.Exp -> [S.Constr] -> S.Decl
checkTypeDef lhs constrs = 
  case flattenApplyLHS lhs of 
    (S.Variable type_name : rest) -> S.TypeDef $ S.TDef type_name (map checkGenTypeVar rest) constrs
    _ -> error $ "Invalid type declaration lhs: " ++ show lhs

checkFuncClauses :: [S.RhsClause] -> [S.RhsClause]
checkFuncClauses [] = error "Expecting at least one clause"
checkFuncClauses clause@[_] = clause
checkFuncClauses (clause@(S.CondClause _ _) : rest) = clause : checkFuncClauses rest
checkFuncClauses (clause@(S.BaseClause _) : _) = error "expecting base clause to be last"

-- coerce an expression into a pattern
checkPattern :: S.Exp -> S.Pattern
checkPattern (S.Constant c) = S.PConstant c
checkPattern (S.Variable v) = S.PVariable v
checkPattern (S.Constructor c) = S.PConstructor c
checkPattern (S.Apply e1 e2) = S.PApply (checkPattern e1) (checkPattern e2)
checkPattern (S.InfixApp T.ICons e1 e2) = S.PCons (checkPattern e1) (checkPattern e2)
checkPattern (S.ListLit exprs) = S.PListLit (map checkPattern exprs)
checkPattern (S.Tuple exprs) = S.PTuple (map checkPattern exprs)

checkConstr :: S.Exp -> S.Constr
checkConstr expr = case flattenApplyLHS expr of 
                     (S.Constructor c : rest) -> (c, map checkConstrArg rest)
                     _ -> error $ "Not a valid constructor: " ++ show expr

checkConstrArg :: S.Exp -> S.ConstrArg
checkConstrArg (S.Variable v) = if all (== '*') v then S.CAGenTypeVar (length v)
                                                  else S.CAVar v
checkConstrArg app@(S.Apply _ _) = S.CAList $ map checkConstrArg (flattenApplyLHS app)
checkConstrArg (S.EGenTypeVar v) = S.CAGenTypeVar v
checkConstrArg (S.InfixOp T.IMult) = S.CAGenTypeVar 1
checkConstrArg expr = error $ "Not a valid constructor arg: " ++ show expr

checkGenTypeVar :: S.Exp -> S.GenTypeVar
checkGenTypeVar e@(S.Variable v) = if not . all (== '*') $ v then error $ "Not a generalized type variable: " ++ show e
                                                             else length v
checkGenTypeVar (S.InfixOp T.IMult) = 1
checkGenTypeVar (S.EGenTypeVar var) = var
checkGenTypeVar expr = error $ "Not a generalized type variable: " ++ show expr

-- flatten apply for left hand side expressions
--   uniquely flattens infix applications to only recognize '*' infix (interpreted as a gtv)
flattenApplyLHS :: S.Exp -> [S.Exp]
flattenApplyLHS (S.Apply e1 e2) = flattenApplyLHS e1 ++ [e2]
flattenApplyLHS (S.InfixApp T.IMult e1 e2) = flattenApplyLHS e1 ++ [S.EGenTypeVar 1] ++ [e2]
flattenApplyLHS (S.InfixOp T.IMult) = [S.EGenTypeVar 1]
flattenApplyLHS expr = [expr]
}