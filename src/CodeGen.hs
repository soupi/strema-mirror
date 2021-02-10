{-# language OverloadedStrings #-}

module CodeGen where

-- import qualified Data.Text as T
-- import qualified Data.Map as M

import Strema.Ast
import qualified JS.Ast as JS


translateFile :: File -> JS.File
translateFile file@(File defs) =
  JS.File $ concat
    [ map (JS.SDef . translateDef) defs
    , [JS.SExpr $ JS.EFunCall (JS.EVar "main") []
      | hasMain file
      ]
    ]

hasMain :: File -> Bool
hasMain (File defs) =
  any
    ( \case
      Function "main" [] _ ->
        True
      Variable "main" (EFun [] _) ->
        True
      _ ->
        False
    )
    defs

translateDef :: Definition -> JS.Definition
translateDef = \case
  Variable var expr ->
    JS.Variable var (translateExpr expr)
  Function var args body ->
    JS.Function var args (translateSub body)

translateSub :: Sub -> JS.Sub
translateSub stmts =
  case reverse stmts of
    [] -> []
    SExpr expr : rest ->
      reverse
        (JS.SRet (translateExpr expr) : map translateStmt rest)
    _ ->
      map translateStmt stmts

translateStmt :: Statement -> JS.Statement
translateStmt = \case
  SExpr expr ->
    JS.SExpr (translateExpr expr)
  SDef def ->
    JS.SDef (translateDef def)

translateExpr :: Expr -> JS.Expr
translateExpr = \case
  ELit lit ->
    JS.ELit (translateLit lit)
  EVar var ->
    JS.EVar var
  EFun args body ->
    JS.EFun args (translateSub body)
  EFunCall fun args ->
    JS.EFunCall (translateExpr fun) (map translateExpr args)
  ERecord record ->
    JS.ERecord (fmap translateExpr record)
  EFfi fun args ->
    JS.EFunCall (JS.EVar fun) (map translateExpr args)

translateLit :: Lit -> JS.Lit
translateLit = \case
  LInt i -> JS.LInt i
  LFloat f -> JS.LFloat f
  LString s -> JS.LString s

