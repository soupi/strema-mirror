{- | Prettyprint JavaScript

In this module we convert our JS AST to plaintext, so we can run it using
an interpreter or a browser.

-}

{-# language OverloadedStrings #-}

module JS.Pretty where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import JS.Ast

---------------
-- Rendering --
---------------

pp :: (a -> Doc ann) -> a -> T.Text
pp f = render . f

render :: Doc a -> T.Text
render = T.unlines . pure . renderStrict . layoutPretty defaultLayoutOptions

------------
-- Pretty --
------------

ppFile :: File -> Doc ann
ppFile (File stmts) =
  vsep
    [ "\"use strict\";"
    , vsep $ map ppStmt stmts
    ]

ppSub :: Sub -> Doc ann
ppSub = vsep . map ppStmt

ppStmt :: Statement -> Doc ann
ppStmt = \case
  SExpr expr ->
    ppExpr expr <> ";"
  SRet expr ->
    "return" <+> ppExpr expr <> ";"
  SDef def ->
    ppDef def <> ";"
  SIf cond sub ->
    vsep
      [ "if " <> parens (ppExpr cond) <+> "{"
      , indent 4 $ ppSub sub
      , "}"
      ]
  SRecordClone var expr ->
    "var" <+> pretty var <+> "=" <+> "Object.assign({}," <+> ppExpr expr <> ");"
  SRecordAssign var lbl newval ->
    pretty var <> "." <> pretty lbl <+> "=" <+> ppExpr newval <+> ";"


ppDef :: Definition -> Doc ann
ppDef = \case
  Variable var e ->
    "var" <+> pretty var <+> "=" <+> ppExpr e
  Function name args body ->
    "var" <+> pretty name <+> "=" <+>
      vsep
        [ "function" <> tupled (map pretty args) <+> "{"
        , indent 4 $ ppSub body
        , "}"
        ]

ppExpr :: Expr -> Doc a
ppExpr = \case
  ELit lit ->
    ppLit lit

  EVar var ->
    pretty var

  ERecord record ->
    ppRecord ppExpr record

  ERecordAccess expr label ->
    (if isSimple expr then id else parens) (ppExpr expr)
      <> "." <> pretty label

  EEquals e1 e2 ->
    ppExpr e1 <+> "===" <+> ppExpr e2

  EBinOp op e1 e2 ->
    ppExpr e1 <+> pretty op <+> ppExpr e2

  EAnd exprs ->
    encloseSep "" "" " && " (map ppExpr exprs)

  EFun args body ->
    vsep
      [ "function" <> tupled (map pretty args) <+> "{"
      , indent 4 $ ppSub body
      , "}"
      ]

  EFunCall fun args ->
    cat
      [ (if isSimple fun then id else parens) (ppExpr fun)
      , tupled (map ppExpr args)
      ]

  ERaw rawString ->
    pretty rawString

ppRecord :: (a -> Doc ann) -> Record a -> Doc ann
ppRecord ppf record =
  encloseSep "{" "}" ", " $
    map
      (\(k, v) -> pretty (show k) <+> ":" <+> ppf v)
      (M.toList record)

ppLit :: Lit -> Doc ann
ppLit = \case
  LInt i -> pretty i
  LFloat f -> pretty f
  LString s -> pretty (show s)
  LBool True -> "true"
  LBool False -> "false"

-- | Simple expressions do not need parenthesis around them
isSimple :: Expr -> Bool
isSimple = \case
  ELit{} -> True
  EVar{} -> True
  ERecord{} -> False
  EFun{} -> False
  EFunCall{} -> True
  EAnd{} -> True
  EEquals{} -> False
  EBinOp{} -> False
  ERaw{} -> False
  ERecordAccess{} -> True
