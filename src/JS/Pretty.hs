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
render = renderStrict . layoutPretty defaultLayoutOptions

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
  LString s -> pretty s
  LBool True -> "true"
  LBool False -> "false"

isSimple :: Expr -> Bool
isSimple = \case
  ELit{} -> True
  EVar{} -> True
  ERecord{} -> False
  EFun{} -> False
  EFunCall{} -> True
