{- | Strema AST

-}

module Strema.Ast
  ( module Strema.Ast
  , module Strema.Common
  , module Strema.Types.Types
  )
where

import qualified Data.Text as T

import Strema.Types.Types
import Strema.Common

data File
  = File [Definition]
  deriving Show

data Definition
  = TypeDef Datatype
  | TermDef TermDef
  deriving Show

data TermDef
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving Show

type Sub = [Statement]

data Datatype
  = Datatype Constr [TypeVar] [Variant Type]
  deriving Show

data Statement
  = SExpr Expr
  | SDef TermDef
  deriving Show

data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | EVariant (Variant Expr)
  | ERecord (Record Expr)
  | ERecordAccess Expr Label
  | ERecordExtension (Record Expr) Expr
  | ECase Expr [(Pattern, Expr)]
  | EFfi T.Text [Expr]
  deriving Show

data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving Show

data Pattern
  = PWildcard
  | PVar Var
  | PLit Lit
  | PVariant (Variant Pattern)
  | PRecord (Record Pattern)
  deriving Show

type Var = T.Text
