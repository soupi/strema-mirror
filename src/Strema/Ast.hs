{- | Strema AST

-}

module Strema.Ast
  ( module Strema.Ast
  , module Strema.Common
  , module Strema.Types.Types
  )
where

import Data.Data (Data)
import qualified Data.Text as T
import qualified Data.Map as M

import Strema.Types.Types
import Strema.Common

data File a
  = File [Definition a]
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

data Definition a
  = TypeDef Datatype
  | TermDef a (TermDef a)
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

data TermDef a
  = Variable Var (Expr a)
  | Function Var [Var] (Sub a)
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

type Sub a = [Statement a]

data Datatype
  = Datatype Constr [TypeVar] [Variant Type]
  deriving (Show, Eq, Ord, Data)

data Statement a
  = SExpr (Expr a)
  | SDef a (TermDef a)
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

data Expr a
  = EAnnotated a (Expr a)
  | ELit Lit
  | EVar Var
  | EFun [Var] (Sub a)
  | EFunCall (Expr a) [Expr a]
  | EVariant (Variant (Expr a))
  | ERecord (Record (Expr a))
  | ERecordAccess (Expr a) Label
  | ERecordExtension (Record (Expr a)) (Expr a)
  | ECase (Expr a) [(Pattern, Sub a)]
  | EFfi T.Text [Expr a]
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving (Show, Eq, Ord, Data)

data Pattern
  = PWildcard
  | PVar Var
  | PLit Lit
  | PVariant (Variant Pattern)
  | PRecord (Record Pattern)
  deriving (Show, Eq, Ord, Data)

type Var = T.Text

mkERec :: [(Label, Expr a)] -> Maybe (Expr a) -> Expr a
mkERec fields = maybe
  (ERecord $ M.fromListWith (flip const) fields)
  (ERecordExtension $ M.fromListWith (flip const) fields)
