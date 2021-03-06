{- | Prettyprint JavaScript

In this module we convert our JS AST to plaintext, so we can run it using
an interpreter or a browser.

-}

{-# language OverloadedStrings #-}

module Language.Backend.JS.Pretty where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Language.Backend.JS.Ast
import Language.Backend.JS.ReservedWords

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

ppBlock :: Block -> Doc ann
ppBlock = vsep . map ppStmt

ppStmt :: Statement -> Doc ann
ppStmt = \case
  SExpr expr ->
    ppExpr expr <> ";"
  SRet expr ->
    nest 4 (group $ "return" <+> ppExpr expr) <> ";"
  SDef def ->
    ppDef def <> ";"
  SIf cond block ->
    vsep
      [ "if " <> parens (ppExpr cond) <+> "{"
      , indent 4 $ ppBlock block
      , "}"
      ]
  SRecordClone var expr ->
    "var" <+> pretty (sanitizeReserved var) <+> "=" <+> "Object.assign({}," <+> ppExpr expr <> ");"
  SRecordAssign var lbl newval ->
    pretty (sanitizeReserved var) <> "." <> pretty (sanitizeReserved lbl) <+> "=" <+> ppExpr newval <+> ";"


ppDef :: Definition -> Doc ann
ppDef = \case
  Variable var e ->
    "var" <+> pretty (sanitizeReserved var) <+> "=" <+> ppExpr e
  Function name args body ->
    group ("var" <+> pretty (sanitizeReserved name) <+> "=") <+> ppExpr (EFun args body)

sanitizeReserved :: T.Text -> T.Text
sanitizeReserved var =
  (if S.member var reservedWords then (<>) "__" else id) var

ppExpr :: Expr -> Doc a
ppExpr = \case
  ELit lit ->
    ppLit lit

  EVar var ->
    pretty (sanitizeReserved var)

  ERecord record ->
    ppRecord ppExpr record

  ERecordAccess expr label ->
    (if isSimple expr then id else align . parens) (ppExpr expr)
      <> "." <> pretty label

  EEquals e1 e2 ->
    ppExpr e1 <+> "===" <+> ppExpr e2

  EBinOp op e1 e2 ->
    ppExpr e1 <+> pretty op <+> ppExpr e2

  ENot e ->
    "!" <> ppExpr e

  EAnd exprs ->
    group $ encloseSep "" "" " && " (map ppExpr exprs)

  EFun args body ->
    vsep
      [ "function" <> tupled' (map pretty args) <+> "{"
      , flatAlt (indent 4 $ ppBlock body) (ppBlock body)
      , "}"
      ]

  EFunCall fun args ->
    group $ cat
      [ (if isSimple fun then id else parens) (ppExpr fun)
      , tupled' (map ppExpr args)
      ]

  ERaw rawString ->
    pretty rawString

ppRecord :: (a -> Doc ann) -> Record a -> Doc ann
ppRecord ppf record =
  record' $
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
  ENot{} -> True
  ERaw{} -> False
  ERecordAccess{} -> True


------------
-- * Helpers

record' :: [Doc ann] -> Doc ann
record' = encloseSep' "{" "}" ", "

tupled' :: [Doc ann] -> Doc ann
tupled' = encloseSep' "(" ")" ", "

encloseSep' :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSep' open close seperator =
  align . group . encloseSep
    (flatAlt (open <> " ") open)
    (flatAlt (hardline <> close) close)
    seperator . map align
