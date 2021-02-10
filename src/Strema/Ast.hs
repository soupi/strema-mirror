
module Strema.Ast where

import qualified Data.Text as T
import qualified Data.Map as M

data File
  = File [Definition]
  deriving Show

data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving Show

type Sub = [Statement]

data Statement
  = SExpr Expr
  | SDef Definition
  deriving Show

data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | ERecord (Record Expr)
  | EFfi T.Text [Expr]
  deriving Show

type Record a
  = M.Map Var a

data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving Show


type Var = T.Text
