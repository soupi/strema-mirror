{- | Strema AST

-}

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
  | EVariant T.Text Expr
  | ERecord (Record Expr)
  | ERecordAccess Expr Label
  | ECase Expr [(Pattern, Expr)]
  | EFfi T.Text [Expr]
  deriving Show

type Label = T.Text

type Record a
  = M.Map Label a

data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving Show

data Pattern
  = PWildcard
  | PVar Var
  | PLit Lit
  | PVariant T.Text Pattern
  | PRecord (Record Pattern)
  deriving Show

type Var = T.Text
