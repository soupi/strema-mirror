{- | Type inference

Our type inference algorithm works in stages:

First, we *elaborate* the AST and collect *constraints*.
Then, we solve these constraints and get back a *substitution*.
We go over the AST again, replacing the type variables we added in the elaboration stage
with concrete types.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Strema.Types.Infer where

import Strema.Ast
import Strema.Builtins
import Utils
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Strema.Parser as Parser

import qualified Data.Map as M
import qualified Data.Set as S

-- * Run

infer :: File Ann -> Either TypeErrorA (File Type)
infer file = do
  (elaborated, constraints) <- elaborate file
  sub <- solve constraints
  substitute sub elaborated

-- * Types

type Ann = Parser.Ann

type TypeErrorA = (Ann, TypeError)

data TypeError
  = TypeMismatch Type Type
  | UnboundVar Var
  deriving Show

type ConstraintA = (Ann, Constraint)

data Constraint
  = Equality Type Type
  deriving (Show, Eq, Ord, Data)

type Constraints = Set ConstraintA

type Substitution
  = Map TypeVar Type

-- * Utils

throwErr :: MonadError TypeErrorA m => Ann -> TypeError -> m a
throwErr ann err = throwError (ann, err)

-- * Elaborate

-- ** Types

data ElabState
  = ElabState
    { esTypeVarCounter :: Int
    , esConstraints :: Constraints
    }

data ElabEnv
  = ElabEnv
    { eeTypeEnv :: Map Var Type
    }

type Elaborate m
  = ( MonadState ElabState m
    , MonadReader ElabEnv m
    , MonadError TypeErrorA m
    )

-- ** Utils

lookupVar :: Elaborate m => Ann -> Var -> m Type
lookupVar ann var = do
  env <- asks eeTypeEnv
  maybe
    (throwErr ann $ UnboundVar var)
    pure
    (M.lookup var env)

-- ** Algorithm

elaborate :: File Ann -> Either TypeErrorA (File Type, Constraints)
elaborate =
  ( fmap (fmap esConstraints)
  . runExcept
  . flip runStateT (ElabState 0 mempty)
  . flip runReaderT (ElabEnv mempty)
  . elaborateFile
  )

elaborateFile :: Elaborate m => File Ann -> m (File Type)
elaborateFile (File defs) =
  File <$> traverse elaborateDef defs

data ElabTerm
  = ElabTerm
    { elabType :: Type
    , elabResult :: TermDef Type
    }

elaborateDef :: Elaborate m => Definition Ann -> m (Definition Type)
elaborateDef = \case
  TermDef _ def -> do
    elab <- elaborateTermDef def
    pure $ TermDef (elabType elab) (elabResult elab)

elaborateTermDef :: Elaborate m => TermDef Ann -> m ElabTerm
elaborateTermDef = \case
  Variable name expr -> do
    expr' <- elaborateExpr
      (error $ toString $ "Found an expression with source position: " <> pShow expr)
      expr
    pure $ ElabTerm
      { elabType = getType expr'
      , elabResult = Variable name expr'
      }

getType :: Expr Type -> Type
getType = \case
  EAnnotated typ _ -> typ

elaborateExpr :: Elaborate m => Ann -> Expr Ann -> m (Expr Type)
elaborateExpr ann = \case
  EAnnotated ann' e ->
    elaborateExpr ann' e

  ELit lit ->
    pure $ EAnnotated (getLitType lit) (ELit lit)

  EVar var -> do
    typ <- lookupVar ann var
    pure $ EAnnotated typ $
      EVar var

getLitType :: Lit -> Type
getLitType = \case
  LInt{} -> tInt
  LString{} -> tString
  LFloat{} -> tFloat

--------------------------

-- * Solve constraints

-- ** Types

type Solve m
  = ( MonadError TypeErrorA m
    )

-- ** Algorithm

solve :: Constraints -> Either TypeErrorA Substitution
solve =
  ( runExcept
  . solveConstraints mempty
  . S.toList
  )

solveConstraints :: Solve m => Substitution -> [ConstraintA] -> m Substitution
solveConstraints sub = \case
  [] -> pure sub

solveConstraint :: Solve m => ConstraintA -> m ([ConstraintA], Substitution)
solveConstraint = undefined

--------------------------
-- * Substitute

-- ** Types

type Substitute m
  = ( MonadError TypeErrorA m
    )

substitute :: Substitute m => Substitution -> File Type -> m (File Type)
substitute _ = pure
