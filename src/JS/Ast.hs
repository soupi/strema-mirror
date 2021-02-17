-- | A subset of JavaScript AST

module JS.Ast where

import qualified Data.Text as T
import qualified Data.Map as M

data File
  = File [Statement]
  deriving Show

data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving Show

type Sub = [Statement]

data Statement
  = SExpr Expr
  | SRet Expr
  | SDef Definition
  | SIf Expr Sub
  | SRecordClone Var Expr
  | SRecordAssign Var Label Expr -- Expr.label = Expr
  deriving Show

data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | ERecord (Record Expr)
  | EAnd [Expr]
  | EEquals Expr Expr
  | EBinOp T.Text Expr Expr
  | ERecordAccess Expr Label
  | ERaw T.Text
  deriving Show

type Label = T.Text

type Record a
  = M.Map Var a

data Lit
  = LInt Int
  | LBool Bool
  | LFloat Float
  | LString T.Text
  deriving Show

type Var = T.Text
