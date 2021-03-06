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

-- | Infer the types for all expressions in a source file
infer :: File InputAnn -> Either TypeErrorA (File Ann)
infer file = do
  (elaborated, constraints) <- elaborate file
  -- ltraceM "elaborated" elaborated
  -- ltraceM "constraints" constraints
  sub <- solve constraints
  substitute sub elaborated

-- * Types

-- | The annotation of the input
type InputAnn = Parser.Ann

-- | The annotation of the output: the input + the type
data Ann
  = Ann
    { annInput :: InputAnn
    , annType :: Type
    }
  deriving (Show, Eq, Ord, Data)

type TypeErrorA = ([InputAnn], TypeError)

data TypeError
  = TypeMismatch Type Type
  | UnboundVar Var
  | InfiniteType TypeVar Type
  | ArityMismatch Type Type
  deriving Show

type ConstraintA = (Constraint, InputAnn)

data Constraint
  = Equality Type Type
  | InstanceOf Type Type
  deriving (Show, Eq, Ord, Data)

type Constraints = Set ConstraintA

type Substitution
  = Map TypeVar Type

data Typer
  = Concrete Type
  | Instance TypeVar
  deriving (Show, Eq, Ord, Data)

-- * Utils

throwErr :: MonadError TypeErrorA m => [InputAnn] -> TypeError -> m a
throwErr ann err = throwError (ann, err)

getTermName :: TermDef a -> Var
getTermName = \case
  Variable name _ -> name
  Function name _ _ -> name

getTermDef :: Definition a -> Maybe (TermDef a)
getTermDef = \case
  TermDef _ def -> Just def
  TypeDef{} -> Nothing

getTypeDef :: Definition a -> Maybe Datatype
getTypeDef = \case
  TermDef{} -> Nothing
  TypeDef def -> Just def


-- * Elaborate

-- ** Types

type Env a = Map Var a

data ElabState
  = ElabState
    { esTypeVarCounter :: Int
    , esConstraints :: Constraints
    }

data ElabEnv
  = ElabEnv
    { eeTypeEnv :: Env Typer
    }

type Elaborate m
  = ( MonadState ElabState m
    , MonadReader ElabEnv m
    , MonadError TypeErrorA m
    )

data ElabInfo a
  = ElabInfo
    { eiResult :: a
    , eiType :: Type
    , eiNewEnv :: Env Typer
    }

-- ** Utils

lookupVar :: Elaborate m => InputAnn -> Var -> m Typer
lookupVar ann var = do
  env <- asks eeTypeEnv
  maybe
    (throwErr [ann] $ UnboundVar var)
    pure
    (M.lookup var env)

lookupVarMaybe :: Elaborate m => Var -> m (Maybe Typer)
lookupVarMaybe var = do
  env <- asks eeTypeEnv
  pure (M.lookup var env)

insertToEnv :: Env Typer -> ElabEnv -> ElabEnv
insertToEnv vars (ElabEnv env) =
  ElabEnv $ M.union
    vars
    env

removeFromEnv :: [Var] -> ElabEnv -> ElabEnv
removeFromEnv vars (ElabEnv env) =
  ElabEnv $ foldr M.delete env vars

withEnv :: Elaborate m => [(Var, Typer)] -> m a -> m a
withEnv = local . insertToEnv . M.fromList

withEnv' :: Elaborate m => Env Typer -> m a -> m a
withEnv' = local . insertToEnv

withoutEnv :: Elaborate m => [Var] -> m a -> m a
withoutEnv = local . removeFromEnv

genTypeVar :: MonadState ElabState m => Text -> m TypeVar
genTypeVar prefix = do
  n <- esTypeVarCounter <$> get
  modify $ \s -> s { esTypeVarCounter = n + 1 }
  pure $ prefix <> toText (show n)

constrain :: Elaborate m => InputAnn -> Constraint -> m ()
constrain ann constraint =
  modify $ \s -> s { esConstraints = S.insert (constraint, ann) (esConstraints s) }

noAnn expr = error $ toString $ "Found an expression with source position: " <> pShow expr

-- ** Algorithm

elaborate :: File InputAnn -> Either TypeErrorA (File Ann, Constraints)
elaborate =
  ( fmap (fmap esConstraints)
  . runExcept
  . flip runStateT (ElabState 0 mempty)
  . flip runReaderT (ElabEnv mempty)
  . elaborateFile
  )

elaborateFile :: Elaborate m => File InputAnn -> m (File Ann)
elaborateFile (File defs) = do
  let
    termDefs = mapMaybe getTermDef defs
    names = map getTermName termDefs
  vars <- traverse (\name -> (,) name . Instance <$> genTypeVar "t") names
  File <$> withEnv vars (traverse elaborateDef defs)

elaborateDef :: Elaborate m => Definition InputAnn -> m (Definition Ann)
elaborateDef = \case
  TermDef ann def -> do
    -- we need to change the type of def in the current environment for recursive functions
    let name = getTermName def
    t <- genTypeVar name
    elab <- withEnv [(name, Concrete $ TypeVar t)] $ elaborateTermDef ann def
    lookupVarMaybe name >>= \case
      Just (Instance t') ->
        constrain ann $ InstanceOf (TypeVar t') (eiType elab)
      _ ->
        pure ()
    pure $ TermDef (Ann ann $ eiType elab) (eiResult elab)

elaborateTermDef :: Elaborate m => InputAnn -> TermDef InputAnn -> m (ElabInfo (TermDef Ann))
elaborateTermDef ann = \case
  Variable name expr -> do
    expr' <- elaborateExpr
      (noAnn expr)
      expr
    pure $ ElabInfo
      { eiType = getType expr'
      , eiResult = Variable name expr'
      , eiNewEnv = mempty
      }
  Function name args sub -> do
    tfun <- genTypeVar "t"
    targsEnv <- traverse (\arg -> (,) arg <$> genTypeVar "t") args
    (tret, sub') <- withEnv
      ((name, Concrete $ TypeVar tfun) : fmap (fmap (Concrete . TypeVar)) targsEnv)
      (elaborateSub sub)

    constrain ann $ Equality
      (TypeVar tfun)
      (TypeFun (map (TypeVar . snd) targsEnv) tret)

    pure $ ElabInfo
      { eiType = TypeVar tfun
      , eiResult = Function name args sub'
      , eiNewEnv = mempty
      }


elaborateSub :: Elaborate m => Sub InputAnn -> m (Type, Sub Ann)
elaborateSub sub = do
  sub' <- foldM
    ( \subinfo stmt -> do
      stmt' <- withEnv' (eiNewEnv subinfo) $ elaborateStmt stmt
      pure $ ElabInfo
        { eiResult = eiResult stmt' : eiResult subinfo
        , eiType = eiType stmt'
        , eiNewEnv = M.union (eiNewEnv stmt') (eiNewEnv subinfo)
        }
    )
    (ElabInfo [] tUnit mempty)
    sub
  pure (eiType sub', reverse (eiResult sub'))

-- we want to fold over the list of statements, and for each new definition,
-- add it to the environment for the elaboration of the next expressions

elaborateStmt :: Elaborate m => Statement InputAnn -> m (ElabInfo (Statement Ann))
elaborateStmt = \case
  SExpr expr -> do
    expr' <- elaborateExpr (noAnn expr) expr
    pure $ ElabInfo
      { eiResult = SExpr expr'
      , eiType = getType expr'
      , eiNewEnv = mempty
      }

  SDef ann def -> do
    ElabInfo def' t _ <- elaborateTermDef ann def
    t' <- genTypeVar "t"
    constrain ann $ InstanceOf (TypeVar t') t
    pure $ ElabInfo
      { eiResult = SDef (Ann ann t) def'
      , eiType = t
      , eiNewEnv = M.singleton (getTermName def') (Instance t')
      }

getExprAnn :: Expr a -> a
getExprAnn = \case
  EAnnotated typ _ -> typ

getType :: Expr Ann -> Type
getType = annType . getExprAnn

elaborateExpr :: Elaborate m => InputAnn -> Expr InputAnn -> m (Expr Ann)
elaborateExpr ann = \case
  EAnnotated ann' e ->
    elaborateExpr ann' e

  ELit lit ->
    pure $ EAnnotated (Ann ann $ getLitType lit) (ELit lit)

  EVar var -> do
    typer <- lookupVar ann var
    typ <- case typer of
      Concrete t -> pure t
      Instance t -> do
        tv <- genTypeVar "t"
        constrain ann $ InstanceOf (TypeVar tv) (TypeVar t)
        pure $ TypeVar tv

    pure $ EAnnotated (Ann ann typ) $
      EVar var

  EFun args sub -> do
    tfun <- genTypeVar "t"
    targsEnv <- traverse (\arg -> (,) arg <$> genTypeVar "t") args
    (tret, sub') <- withEnv (fmap (fmap (Concrete . TypeVar)) targsEnv) $ elaborateSub sub

    constrain ann $ Equality
      (TypeVar tfun)
      (TypeFun (map (TypeVar . snd) targsEnv) tret)

    pure $ EAnnotated (Ann ann $ TypeVar tfun) $
      EFun args sub'

  EFunCall f args -> do
    f' <- elaborateExpr ann f
    args' <- traverse (elaborateExpr ann) args
    tret <- TypeVar <$> genTypeVar "t"
    constrain ann $ Equality
      (getType f')
      (TypeFun (map getType args') tret)
    pure $ EAnnotated (Ann ann tret) $
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
    , MonadState ElabState m
    )

-- ** Algorithm

solve :: Constraints -> Either TypeErrorA Substitution
solve =
  ( runExcept
  . flip evalStateT (ElabState 0 mempty)
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
solveConstraint (constraint, ann) =
  -- case ltrace "constraint" constraint of
  case constraint of
    InstanceOf t1 t2 -> do
      t2' <- instantiate t2
      pure ([(Equality t1 t2', ann)], mempty)

    Equality t1 t2
      | t1 == t2 ->
        pure (mempty, mempty)

    Equality (TypeVar tv) t2 ->
      pure
        ( mempty
        , M.singleton tv t2
        )

    Equality t1 (TypeVar tv) ->
      solveConstraint (Equality (TypeVar tv) t1, ann)

    Equality t1@(TypeFun args1 ret1) t2@(TypeFun args2 ret2) -> do
      unless (length args1 == length args2) $
        throwErr [ann] $ ArityMismatch t1 t2

      pure
        ( map (flip (,) ann) $ zipWith Equality (ret1 : args1) (ret2 : args2)
        , mempty
        )

    Equality t1 t2 ->
      throwErr [ann] $ TypeMismatch t1 t2


instantiate :: Solve m => Type -> m Type
instantiate typ = do
  -- collect all type variables, generate a new type variable for each one
  -- replace old type variables with new ones
  env <- fmap M.fromList $ sequence
    [ (,) tv <$> genTypeVar "ti"
    | TypeVar tv <- U.universe typ
    ]
  pure $ flip U.transform typ $ \case
    TypeVar tv
      | Just tv' <- M.lookup tv env ->
        TypeVar tv'
    other -> other



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



