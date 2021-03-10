{- | Translate Strema to JavaScript

-}

{-# language OverloadedStrings #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}

module Language.Strema.Compiler.Translate where

import Data.Maybe
import Data.Traversable
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

import Language.Strema.Syntax.Ast
import Language.Strema.Builtins
import qualified Language.Backend.JS as JS

-- Types and utilities --

type Ann = ()

type TranState = Int
type Translate m =
  ( MonadState TranState m
  , MonadReader Builtins m
  )

genVar :: Translate m => T.Text -> m Var
genVar prefix = do
  n <- get
  modify (+1)
  pure ("_" <> prefix <> "_" <> T.pack (show n))

translate :: (a -> StateT TranState (Reader Builtins) b) -> Builtins -> a -> b
translate tran built =
  ( flip runReader built
  . flip evalStateT 0
  . tran
  )

-- Translation --

translateFile :: Translate m => File Ann -> m JS.File
translateFile (File alldefs) = do
  let
    -- we don't need to compile data type definitions
    defs =
      mapMaybe
      ( \case
        TermDef _ def -> Just def
        TypeDef{} -> Nothing
      )
      alldefs

  fmap JS.File $ (<>)
    <$> traverse (fmap JS.SDef . translateDef) defs
    <*> pure
      [JS.SExpr $ JS.EFunCall (JS.EVar "main") []
      | hasMain defs
      ]

hasMain :: [TermDef Ann] -> Bool
hasMain =
  any
    ( \case
      Function "main" [] _ ->
        True
      Variable "main" (EFun [] _) ->
        True
      _ ->
        False
    )


translateDef :: Translate m => TermDef Ann -> m JS.Definition
translateDef = \case
  Variable var expr ->
    JS.Variable var <$> translateExpr expr
  Function var args body ->
    JS.Function var args <$> translateBlock body

translateBlock :: Translate m => Block Ann -> m JS.Block
translateBlock stmts =
  case reverse stmts of
    [] -> pure []
    SExpr expr : rest ->
      fmap reverse $ (:)
        <$> (JS.SRet <$> translateExpr expr)
        <*> traverse translateStmt rest
    _ ->
      traverse translateStmt stmts

translateStmt :: Translate m => Statement Ann -> m JS.Statement
translateStmt = \case
  SExpr expr ->
    JS.SExpr <$> translateExpr expr
  SDef _ def ->
    JS.SDef <$> translateDef def

translateExpr :: Translate m => Expr Ann -> m JS.Expr
translateExpr = \case
  EAnnotated _ e ->
    translateExpr e

  ELit lit ->
    pure $ JS.ELit (translateLit lit)

  EVariant (Variant "True" (ERecord record))
    | M.null record -> do
      pure $ JS.ELit (JS.LBool True)

  EVariant (Variant "False" (ERecord record))
    | M.null record -> do
      pure $ JS.ELit (JS.LBool False)

  EVar var -> do
    mbuiltin <- asks (M.lookup var)
    case mbuiltin of
      Nothing ->
        pure $ JS.EVar var
      Just Builtin{ bImpl = impl } ->
        case impl of
          Func fun ->
            pure $ JS.ERaw fun
          BinOp op ->
            pure $ JS.EFun ["x", "y"]
              [ JS.SRet (JS.EBinOp op (JS.EVar "x") (JS.EVar "y")) ]

  EFun args body ->
    JS.EFun args <$> translateBlock body
  EFunCall fun args ->
    JS.EFunCall
      <$> translateExpr fun
      <*> traverse translateExpr args
  EVariant (Variant tag dat) -> do
    dat' <- translateExpr dat
    pure $ JS.ERecord $ M.fromList
      [ ("_constr", JS.ELit $ JS.LString tag)
      , ("_field", dat')
      ]
  ERecord record ->
    JS.ERecord <$> traverse translateExpr record
  ERecordAccess expr label ->
    JS.ERecordAccess
      <$> translateExpr expr
      <*> pure label
  ERecordExtension record expr -> do
    expr' <- translateExpr expr
    record' <- traverse translateExpr record
    let
      fun = JS.EFun ["_record"] $ concat
        [ [ JS.SRecordClone "_record_copy" (JS.EVar "_record") ]
        , map
          (\(lbl, val) -> JS.SRecordAssign "_record_copy" lbl val)
          (M.toList record')
        , [ JS.SRet (JS.EVar "_record_copy") ]
        ]
    pure $ JS.EFunCall fun [expr']
  ECase expr patterns -> do
    expr' <- translateExpr expr
    var <- genVar "case"
    patterns' <- translatePatterns (JS.EVar var) patterns
    pure $ JS.EFunCall
      (JS.EFun [var] patterns')
      [expr']
  EFfi fun args ->
    JS.EFunCall (JS.EVar fun) <$> traverse translateExpr args

translatePatterns :: Translate m => JS.Expr -> [(Pattern, Block Ann)] -> m JS.Block
translatePatterns outer = traverse $ \(pat, block) -> do
  result' <- translateBlock block
  PatResult conds matches <- translatePattern outer pat
  let (matchersV, matchersE) = unzip matches
  pure $ JS.SIf (JS.EAnd conds)
    [ JS.SRet $ JS.EFunCall
      ( JS.EFun matchersV result' )
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

  PVariant (Variant "True" (PRecord r))
    | M.null r ->
      pure $ PatResult
        { conditions = [ expr ]
        , matchers = []
        }

  PVariant (Variant "False" (PRecord r))
    | M.null r ->
      pure $ PatResult
        { conditions = [ JS.ENot expr ]
        , matchers = []
        }

  PVariant (Variant tag pat) -> do
    pat' <- translatePattern (JS.ERecordAccess expr "_field") pat
    pure $ PatResult
      { conditions =
        ( JS.EEquals
          (JS.ELit $ JS.LString tag)
          (JS.ERecordAccess expr "_constr")
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

