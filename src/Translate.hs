{- | Translate Strema to JavaScript

-}

{-# language OverloadedStrings #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}

module Translate where

import Data.Traversable
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.State

import Strema.Ast
import qualified JS.Ast as JS

-- Types and utilities --

type TranState = Int
type Translate m = MonadState TranState m

genVar :: Translate m => T.Text -> m Var
genVar prefix = do
  n <- get
  modify (+1)
  pure ("_" <> prefix <> "_" <> T.pack (show n))

translate :: (a -> State TranState b) -> a -> b
translate tran ast = evalState (tran ast) 0

-- Translation --

translateFile :: Translate m => File -> m JS.File
translateFile file@(File defs) =
  fmap JS.File $ (<>)
    <$> traverse (fmap JS.SDef . translateDef) defs
    <*> pure
      [JS.SExpr $ JS.EFunCall (JS.EVar "main") []
      | hasMain file
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

translateDef :: Translate m => Definition -> m JS.Definition
translateDef = \case
  Variable var expr ->
    JS.Variable var <$> translateExpr expr
  Function var args body ->
    JS.Function var args <$> translateSub body

translateSub :: Translate m => Sub -> m JS.Sub
translateSub stmts =
  case reverse stmts of
    [] -> pure []
    SExpr expr : rest ->
      fmap reverse $ (:)
        <$> (JS.SRet <$> translateExpr expr)
        <*> traverse translateStmt rest
    _ ->
      traverse translateStmt stmts

translateStmt :: Translate m => Statement -> m JS.Statement
translateStmt = \case
  SExpr expr ->
    JS.SExpr <$> translateExpr expr
  SDef def ->
    JS.SDef <$> translateDef def

translateExpr :: Translate m => Expr -> m JS.Expr
translateExpr = \case
  ELit lit ->
    pure $ JS.ELit (translateLit lit)
  EVar var ->
    pure $ JS.EVar var
  EFun args body ->
    JS.EFun args <$> translateSub body
  EFunCall fun args ->
    JS.EFunCall
      <$> translateExpr fun
      <*> traverse translateExpr args
  EVariant tag dat -> do
    dat' <- translateExpr dat
    pure $ JS.ERecord $ M.fromList
      [ ("_tag", JS.ELit $ JS.LString tag)
      , ("_field", dat')
      ]
  ERecord record ->
    JS.ERecord <$> traverse translateExpr record
  ERecordAccess expr label ->
    JS.ERecordAccess
      <$> translateExpr expr
      <*> pure label
  ECase expr patterns -> do
    expr' <- translateExpr expr
    var <- genVar "case"
    patterns' <- translatePatterns (JS.EVar var) patterns
    pure $ JS.EFunCall
      (JS.EFun [var] patterns')
      [expr']
  EFfi fun args ->
    JS.EFunCall (JS.EVar fun) <$> traverse translateExpr args

translatePatterns :: Translate m => JS.Expr -> [(Pattern, Expr)] -> m JS.Sub
translatePatterns outer = traverse $ \(pat, expr) -> do
  result' <- translateExpr expr
  PatResult conds matches <- translatePattern outer pat
  let (matchersV, matchersE) = unzip matches
  pure $ JS.SIf (JS.EAnd conds)
    [ JS.SRet $ JS.EFunCall
      ( JS.EFun matchersV [ JS.SRet result' ] )
      matchersE
    ]

{-

We want to translate ~pat -> result~ to something that looks like this:

if (<conditions>) {
    return (function(<matchersV>) { return result })(<matchersE>);
}

-}

data PatResult
  = PatResult
    { conditions :: [JS.Expr]
    , matchers :: [(Var, JS.Expr)]
    }

instance Semigroup PatResult where
  (<>) (PatResult c1 m1) (PatResult c2 m2) =
    PatResult (c1 <> c2) (m1 <> m2)
instance Monoid PatResult where
  mempty = PatResult [] []

translatePattern :: Translate m => JS.Expr -> Pattern -> m PatResult
translatePattern expr = \case
  PWildcard ->
    pure $ PatResult
      { conditions = [JS.ELit $ JS.LBool True]
      , matchers = []
      }
  PVar v ->
    pure $ PatResult
      { conditions = [JS.ELit $ JS.LBool True]
      , matchers = [(v, expr)]
      }
  PLit lit ->
    pure $ PatResult
      { conditions = [JS.EEquals (JS.ELit $ translateLit lit) expr]
      , matchers = []
      }
  PVariant tag pat -> do
    pat' <- translatePattern (JS.ERecordAccess expr "_field") pat
    pure $ PatResult
      { conditions =
        ( JS.EEquals
          (JS.ELit $ JS.LString tag)
          (JS.ERecordAccess expr "_tag")
        ) : conditions pat'
      , matchers = matchers pat'
      }
  PRecord (M.toList -> fields) -> do
    fmap mconcat $ for fields $ \(field, pat) ->
      translatePattern (JS.ERecordAccess expr field) pat

translateLit :: Lit -> JS.Lit
translateLit = \case
  LInt i -> JS.LInt i
  LFloat f -> JS.LFloat f
  LString s -> JS.LString s

