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
import qualified Data.Generics.Uniplate.Data as U

-- * Run

infer :: File Ann -> Either TypeErrorA (File Type)
infer file = do
  (elaborated, constraints) <- elaborate file
  -- ltraceM "elaborated" elaborated
  -- ltraceM "constraints" constraints
  sub <- solve constraints
  substitute sub elaborated

-- * Types

type Ann = Parser.Ann

type TypeErrorA = ([Ann], TypeError)

data TypeError
  = TypeMismatch Type Type
  | UnboundVar Var
  | InfiniteType TypeVar Type
  | ArityMismatch Type Type
  deriving Show

type ConstraintA = (Ann, Constraint)

data Constraint
  = Equality Type Type
  deriving (Show, Eq, Ord, Data)

type Constraints = Set ConstraintA

type Substitution
  = Map TypeVar Type

-- * Utils

throwErr :: MonadError TypeErrorA m => [Ann] -> TypeError -> m a
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
    (throwErr [ann] $ UnboundVar var)
    pure
    (M.lookup var env)

insertToEnv :: [(Var, TypeVar)] -> ElabEnv -> ElabEnv
insertToEnv vars (ElabEnv env) =
  ElabEnv $ M.union
    (fmap TypeVar $ M.fromList $ vars)
    env

withEnv :: Elaborate m => [(Var, TypeVar)] -> m a -> m a
withEnv = local . insertToEnv

genTypeVar :: Elaborate m => Text -> m TypeVar
genTypeVar prefix = do
  n <- esTypeVarCounter <$> get
  modify $ \s -> s { esTypeVarCounter = n + 1 }
  pure $ prefix <> toText (show n)

constrain :: Elaborate m => Ann -> Constraint -> m ()
constrain ann constraint =
  modify $ \s -> s { esConstraints = S.insert (ann, constraint) (esConstraints s) }

noAnn expr = error $ toString $ "Found an expression with source position: " <> pShow expr

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
      (noAnn expr)
      expr
    pure $ ElabTerm
      { elabType = getType expr'
      , elabResult = Variable name expr'
      }

elaborateSub :: Elaborate m => Sub Ann -> m (Type, Sub Type)
elaborateSub = \case
  [ SExpr expr ] -> do
    expr' <- elaborateExpr (noAnn expr) expr
    pure ( getType expr', [ SExpr  expr' ] )

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

  EFun args sub -> do
    tfun <- genTypeVar "t"
    targsEnv <- traverse (\arg -> (,) arg <$> genTypeVar "t") args
    (tret, sub') <- withEnv targsEnv $ elaborateSub sub

    constrain ann $ Equality
      (TypeVar tfun)
      (TypeFun (map (TypeVar . snd) targsEnv) tret)

    pure $ EAnnotated (TypeVar tfun) $
      EFun args sub'

  EFunCall f args -> do
    f' <- elaborateExpr ann f
    args' <- traverse (elaborateExpr ann) args
    tret <- TypeVar <$> genTypeVar "t"
    constrain ann $ Equality
      (getType f')
      (TypeFun (map getType args') tret)
    pure $ EAnnotated tret $
      EFunCall f' args'

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
  c : cs -> do
    (newCons, newSub) <- solveConstraint c
    cs' <- substitute newSub (newCons <> cs)
    sub' <- substitute newSub sub
    solveConstraints (M.union newSub sub') cs'

solveConstraint :: Solve m => ConstraintA -> m ([ConstraintA], Substitution)
solveConstraint (ann, constraint) =
  -- case ltrace "constraint" constraint of
  case constraint of
    Equality t1 t2
      | t1 == t2 ->
        pure (mempty, mempty)

    Equality (TypeVar tv) t2 ->
      pure
        ( mempty
        , M.singleton tv t2
        )

    Equality t1 (TypeVar tv) ->
      solveConstraint (ann, Equality (TypeVar tv) t1)

    Equality t1@(TypeFun args1 ret1) t2@(TypeFun args2 ret2) -> do
      unless (length args1 == length args2) $
        throwErr [ann] $ ArityMismatch t1 t2

      pure
        ( map ((,) ann) $ zipWith Equality (ret1 : args1) (ret2 : args2)
        , mempty
        )

--------------------------
-- * Substitute

-- ** Types

type Substitute m
  = ( MonadError TypeErrorA m
    )


substitute
  :: Substitute m
  => Data f
  => Substitution -> f -> m f
substitute sub = U.transformBiM (replaceTypeVar sub)

replaceTypeVar :: Substitute m => Substitution -> Type -> m Type
replaceTypeVar sub = \case
  TypeVar v ->
    maybe
      (pure $ TypeVar v)
      (occursCheck v)
      (M.lookup v sub)

  other ->
    pure other


-- | protect against infinite types
occursCheck :: Substitute m => TypeVar -> Type -> m Type
occursCheck v = \case
  TypeVar v'
    | v == v' ->
      pure $ TypeVar v'
  -- types that contain the type variable we are trying to replace
  -- are forbidden
  t -> do
    let
      tvars = [ () | TypeVar tv <- U.universe t, tv == v ]
    unless (null tvars) $ throwErr [] $ InfiniteType v t
    pure t



