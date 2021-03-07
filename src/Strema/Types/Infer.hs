{- | Type inference

Our type inference algorithm works in stages:

First, we __elaborate__ the AST and collect __constraints__.
Then, we solve these constraints and get back a __substitution__.
We go over the AST again, replacing the type variables we added in the elaboration stage
with concrete types.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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

---------------
-- * Run

-- | Infer the types for all expressions in a source file
infer :: File InputAnn -> Either TypeErrorA (File Ann)
infer file = do
  (elaborated, constraints) <- elaborate (fmap bType builtins) file
  -- ltraceM "elaborated" elaborated
  -- ltraceM "constraints" constraints
  sub <- solve constraints
  substitute sub elaborated

---------------
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
  | UnboundTypeVarsInType Datatype
  | DuplicateTypeVarsInSig Datatype
  | DuplicateConstrs Datatype
  | DuplicateConstrs2 [(Constr, (VariantSig InputAnn, VariantSig InputAnn))]
  | NotSuchVariant Constr
  deriving Show

-- | Represents the constraints on types we collect during the elaboration phase.
data Constraint
  -- | The two type should be equal.
  = Equality Type Type
  -- | The first type is an instantiation of the second (see @instantiate@).
  | InstanceOf Type Type
  deriving (Show, Eq, Ord, Data)

-- | A constraint with the input annotation.
type ConstraintA = (Constraint, InputAnn)

type Constraints = Set ConstraintA

-- | A mapping from type variable to types. This is the output of the constraint solving phase. 
type Substitution
  = Map TypeVar Type

-- | Used during elaboration to represent the type of names in scope.
data Typer
  -- | This type is a concrete type.
  = Concrete Type
  -- | This type represents an instantiation of this type.
  | Instance TypeVar
  deriving (Show, Eq, Ord, Data)

------------------
-- * General Utils

throwErr :: MonadError TypeErrorA m => [InputAnn] -> TypeError -> m a
throwErr ann err = throwError (ann, err)

getTermName :: TermDef a -> Var
getTermName = \case
  Variable name _ -> name
  Function name _ _ -> name

getTermDef :: Definition InputAnn -> Maybe (InputAnn, TermDef InputAnn)
getTermDef = \case
  TermDef ann def -> Just (ann, def)
  TypeDef{} -> Nothing

getTypeDef :: Definition InputAnn -> Maybe (InputAnn, Datatype)
getTypeDef = \case
  TermDef{} -> Nothing
  TypeDef ann def -> Just (ann, def)

-- | Retrieve the annotation of an expression. Will explode when used on a non @EAnnotated@ node.
getExprAnn :: Expr a -> a
getExprAnn = \case
  EAnnotated typ _ -> typ

-- | Retrieve the type of an expression. Will explode when used on a non @EAnnotated@ node.
getType :: Expr Ann -> Type
getType = annType . getExprAnn

-----------------------------------------------
-- * Elaborate

{- |

During the elaboration phase we annotate the AST with the types
we know. When we don't know what the type of something is, we generate
a new type variable as a placeholder and mark that type with a @Constraint@
depending on its syntactic use.

- Input: AST
- Output: Annotated AST with types (to the best of our abilities) + constraints on those types

-}

elaborate :: Env Type -> File InputAnn -> Either TypeErrorA (File Ann, Constraints)
elaborate = runElaborate

------------
-- ** Types

type Env a = Map Var a


-- data Datatype
--   = Datatype Constr [TypeVar] [Variant Type]
--   deriving (Show, Eq, Ord, Data)

type VariantEnv a
  = Map Constr (VariantSig a)

data VariantSig a
  = VariantSig
    { vsDatatype :: Type
    , vsTemplate :: Type
    , vsAnn :: a
    }
  deriving (Show, Eq, Ord, Data)

-- | The state we keep during elaboration.
data ElabState
  = ElabState
    { esTypeVarCounter :: Int
    -- ^ Used to generate unique type variable names.
    , esConstraints :: Constraints
    -- ^ The constraints we collect.
    , esVariantEnv :: VariantEnv InputAnn
    -- ^ Mapping from a data constructor name to the data type
    --   and variant type signature.
    }

-- | The environment we use.
data ElabEnv
  = ElabEnv
    { eeTypeEnv :: Env Typer
    -- ^ Represents the variables in scope and their types (including if they are let-polymorphic or not).
    , eeBuiltins :: Env Type
    }

-- | The monadic capabilities for the elaboration phase.
type Elaborate m
  = ( MonadState ElabState m
    , MonadReader ElabEnv m
    , MonadError TypeErrorA m
    )

-- | Return type for some elaboration functions.
data ElabInfo a
  = ElabInfo
    { eiResult :: a
      -- ^ The result of the current elaboration
    , eiType :: Type
      -- ^ The type of the result
    , eiNewEnv :: Env Typer
      -- ^ A definition to add to the environment for the next statements, if needed.
    }

-------------
-- ** Utils

-- | Try to find the type of a variable in scope.
lookupVarMaybe :: Elaborate m => Var -> m (Maybe Typer)
lookupVarMaybe var = do
  env <- asks eeTypeEnv
  pure (M.lookup var env)

-- | Same as @lookupVarMaybe@ but throws an @UnboundVar@ error on failure.
lookupVar :: Elaborate m => InputAnn -> Var -> m Typer
lookupVar ann var = do
  env <- asks eeTypeEnv
  maybe
    (throwErr [ann] $ UnboundVar var)
    pure
    (M.lookup var env)

-- | Look for a builtin value/function. Throws an @UnboundVar@ error on failure.
lookupBuiltin :: Elaborate m => InputAnn -> Var -> m Type
lookupBuiltin ann var = do
  env <- asks eeBuiltins
  maybe
    (throwErr [ann] $ UnboundVar var)
    pure
    (M.lookup var env)

-- | Adding new variable into the scope
insertToEnv :: Env Typer -> ElabEnv -> ElabEnv
insertToEnv vars elabEnv =
  elabEnv
    { eeTypeEnv = M.union vars (eeTypeEnv elabEnv)
    }

-- | Remove variables from the scope
removeFromEnv :: [Var] -> ElabEnv -> ElabEnv
removeFromEnv vars elabEnv =
  elabEnv
    { eeTypeEnv = foldr M.delete (eeTypeEnv elabEnv) vars
    }

-- | Run an elaboration function with extra variables in scope.
withEnv :: Elaborate m => [(Var, Typer)] -> m a -> m a
withEnv = local . insertToEnv . M.fromList

-- | Run an elaboration function with extra variables in scope (Map version).
withEnv' :: Elaborate m => Env Typer -> m a -> m a
withEnv' = local . insertToEnv

-- | Run an elaboration function without some variables in scope.
withoutEnv :: Elaborate m => [Var] -> m a -> m a
withoutEnv = local . removeFromEnv

-- | Lookup variant in env
lookupVariant :: Elaborate m => InputAnn -> Constr -> m (VariantSig InputAnn)
lookupVariant ann constr = do
  maybe (throwErr [ann] $ NotSuchVariant constr) pure . M.lookup constr . esVariantEnv =<< get

-- | Add a datatype information into the environment
addVariantSigs :: Elaborate m => VariantEnv InputAnn -> m ()
addVariantSigs venv = do
  s <- get
  let
    env = esVariantEnv s
    duplicates = M.toList $ M.intersectionWith (,) venv env

  -- check for duplicate variants
  case duplicates of
    [] -> pure ()
    (d, v) : rest -> do
      throwErr [vsAnn $ fst v] $ DuplicateConstrs2 ((d,v ) : rest)

  put $ s { esVariantEnv = env `M.union` venv }

-- | Generate a new type variable.
genTypeVar :: MonadState ElabState m => Text -> m TypeVar
genTypeVar prefix = do
  n <- esTypeVarCounter <$> get
  modify $ \s -> s { esTypeVarCounter = n + 1 }
  pure $ prefix <> toText (show n)

-- | Add a new constraint.
constrain :: Elaborate m => InputAnn -> Constraint -> m ()
constrain ann constraint =
  modify $ \s -> s { esConstraints = S.insert (constraint, ann) (esConstraints s) }

-- | An @InputAnn@ that will explode if evaluated. Use with care.
noAnn :: Show a1 => a1 -> InputAnn
noAnn expr = error $ toString $ "Found an expression with source position: " <> pShow expr

------------------
-- ** Algorithm

-- | Run the algorithm.
runElaborate :: Env Type -> File InputAnn -> Either TypeErrorA (File Ann, Constraints)
runElaborate builtins =
  ( fmap (fmap esConstraints)
  . runExcept
  . flip runStateT (ElabState 0 mempty mempty) -- @TODO: add builtins variants?
  . flip runReaderT (ElabEnv mempty builtins)
  . elaborateFile
  )

-- | Elaborate a source file
elaborateFile :: Elaborate m => File InputAnn -> m (File Ann)
elaborateFile (File defs) = do
  let
    termDefs = mapMaybe getTermDef defs
    typeDefs = mapMaybe getTypeDef defs
    names = map (getTermName . snd) termDefs
  -- We invent types for top level term definitions.
  -- These should be let polymorphic so we use @Instance@ instead of @Concrete@
  typeDefs' <- traverse (uncurry elaborateTypeDef) typeDefs
  vars <- traverse (\name -> (,) name . Instance <$> genTypeVar "t") names
  File . (<>) typeDefs' <$> withEnv vars (traverse (uncurry elaborateDef) termDefs)

elaborateTypeDef :: Elaborate m => InputAnn -> Datatype -> m (Definition Ann)
elaborateTypeDef ann = \case
  dt@(Datatype typename args variants) -> do
    let
      datatype = foldl' TypeApp (TypeCon typename) (map TypeVar args)
      boundvars = S.fromList args
      variantsvars = [ t | TypeVar t <- U.universeBi variants ]
      constrs = S.fromList $ map (\(Variant constr _) -> constr) variants
    
    -- check for duplicate data constructors, duplicate type variables
    -- and unbound type vars
    unless (length boundvars == length args) $
      throwErr [ann] $ DuplicateTypeVarsInSig dt

    unless (length variants == length constrs) $
      throwErr [ann] $ DuplicateConstrs dt

    unless (all (`S.member` boundvars) variantsvars) $
      throwErr [ann] $ UnboundTypeVarsInType dt

    -- convert to @VariantSig@s
    let
      variantsigs = M.fromList $
        map (\(Variant constr template) -> (constr, VariantSig datatype template ann)) variants
    addVariantSigs variantsigs
    
    pure $ TypeDef (Ann ann tUnit) dt

-- | Elaborate a @TermDef@
elaborateDef :: Elaborate m => InputAnn -> TermDef InputAnn -> m (Definition Ann)
elaborateDef ann def = do
  -- we need to change the type of def in the current environment for recursive functions
  let name = getTermName def
  t <- genTypeVar name
  -- When evaluating a definition, the type should not be let polymorphic.
  elab <- withEnv [(name, Concrete $ TypeVar t)] $ elaborateTermDef ann def
  lookupVar ann name >>= \case
    Instance t' ->
      -- @t'@ is the type other definitions see.
      -- It is an instantiation of the type we just elaborated for def.
      constrain ann $ InstanceOf (TypeVar t') (eiType elab)
    _ ->
      pure ()
  pure $ TermDef (Ann ann $ eiType elab) (eiResult elab)

-- | Elaborate a term definition helper function
elaborateTermDef :: Elaborate m => InputAnn -> TermDef InputAnn -> m (ElabInfo (TermDef Ann))
elaborateTermDef ann = \case
  Variable name expr -> do
    expr' <- elaborateExpr (noAnn expr) expr
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

-- | Elaborate a list of statements.
--   Returns the type of the final statement as well.
elaborateSub :: Elaborate m => Sub InputAnn -> m (Type, Sub Ann)
elaborateSub sub = do
  -- we want to fold over the list of statements, and for each new definition,
  -- add it to the environment for the elaboration of the next expressions
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

-- | Elaborate a single statement.
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

-- | Elaborate an expression.
--   Traverses the expression Top-Down.
elaborateExpr :: Elaborate m => InputAnn -> Expr InputAnn -> m (Expr Ann)
elaborateExpr ann = \case
  -- Replace the current annotation an evaluate the inner expression
  EAnnotated ann' e ->
    elaborateExpr ann' e

  -- The types of literals are known.
  ELit lit ->
    pure $ EAnnotated (Ann ann $ getLitType lit) (ELit lit)

  -- For variables, we look it up in the environment
  EVar var -> do
    typer <- lookupVarMaybe var
    typ <- case typer of
      Just (Concrete t) -> pure t
      -- For let-polymorphism
      Just (Instance t) -> do
        tv <- genTypeVar "t"
        constrain ann $ InstanceOf (TypeVar tv) (TypeVar t)
        pure $ TypeVar tv
      Nothing -> do
        lookupBuiltin ann var

    pure $ EAnnotated (Ann ann typ) $
      EVar var

  -- Generate type variables for function arguments, and annotate
  -- the body with the arguments in the environment.
  -- The result type should be a function from the arguments types to the type of the body
  EFun args body -> do
    targsEnv <- traverse (\arg -> (,) arg <$> genTypeVar "t") args
    (tret, body') <- withEnv (fmap (fmap (Concrete . TypeVar)) targsEnv) $ elaborateSub body

    let tfun = TypeFun (map (TypeVar . snd) targsEnv) tret

    pure $ EAnnotated (Ann ann tfun) $
      EFun args body'

  -- Generate a return type and constrain the type of the function as
  -- a function from the arguments types to the generated return type
  EFunCall f args -> do
    f' <- elaborateExpr ann f
    args' <- traverse (elaborateExpr ann) args
    tret <- TypeVar <$> genTypeVar "t"
    constrain ann $ Equality
      (getType f')
      (TypeFun (map getType args') tret)
    pure $ EAnnotated (Ann ann tret) $
      EFunCall f' args'

  -- lookup variant in the environment
  EVariant (Variant constr expr) -> do
    expr' <- elaborateExpr ann expr
    vs@VariantSig{ vsDatatype, vsTemplate } <- lookupVariant ann constr
    (dtvars, dt) <- instantiate vsDatatype
    let
      specialized = U.transform
        ( \case
          TypeVar t ->
            -- we already checked this
            maybe
              (error $ toString $ "Found unbound type variable " <> pShow vs)
              TypeVar
              (M.lookup t dtvars)
          t -> t
        )
        vsTemplate

    constrain ann $ Equality specialized (getType expr')

    pure $ EAnnotated (Ann ann dt) $
      EVariant (Variant constr expr')

  ECase expr patterns -> do
    expr' <- elaborateExpr ann expr
    patT <- TypeVar <$> genTypeVar "t"
    patterns' <- elaboratePatterns ann (getType expr') patT patterns
    pure $ EAnnotated (Ann ann patT) $
      ECase expr' patterns'

-- | Elaborate patterns in case expressions
elaboratePatterns
  :: Elaborate m
  => InputAnn -> Type -> Type -> [(Pattern, Sub InputAnn)] -> m [(Pattern, Sub Ann)]
elaboratePatterns ann exprT bodyT pats = do
  traverse (elaboratePattern ann exprT bodyT) pats

-- | Elaborate a single pattern match
elaboratePattern
  :: Elaborate m
  => InputAnn -> Type -> Type -> (Pattern, Sub InputAnn) -> m (Pattern, Sub Ann)
elaboratePattern ann exprT bodyT (outerPat, body) = do
  case outerPat of
    PWildcard -> do
      (t, body') <- elaborateSub body
      constrain ann $ Equality bodyT t
      pure (outerPat, body')

    PVar v -> do
      (t, body') <- withEnv [(v, Concrete exprT)] $ elaborateSub body
      constrain ann $ Equality bodyT t
      pure (outerPat, body')

    PLit lit -> do
      constrain ann $ Equality (getLitType lit) exprT
      (t, body') <- elaborateSub body
      constrain ann $ Equality bodyT t
      pure (outerPat, body')

    PVariant (Variant constr innerPat) -> do
      vs@VariantSig{ vsDatatype, vsTemplate } <- lookupVariant ann constr
      (dtvars, dt) <- instantiate vsDatatype
      let
        innerPatT = U.transform
          ( \case
            TypeVar t ->
              -- we already checked this
              maybe
                (error $ toString $ "Found unbound type variable " <> pShow vs)
                TypeVar
                (M.lookup t dtvars)
            t -> t
          )
          vsTemplate

      constrain ann $ Equality exprT dt
      (pat', body') <- elaboratePattern ann innerPatT bodyT (innerPat, body)
      pure (PVariant (Variant constr pat'), body')



-- | The type of a literal
getLitType :: Lit -> Type
getLitType = \case
  LInt{} -> tInt
  LString{} -> tString
  LFloat{} -> tFloat

----------------------------------------

-- * Solve constraints

{- |
In this phase we go over the constraints one by one and try to __unify__ them.

For example, if we see @Equality (TypeVar "t1") (TypeCon "Int")@, we create
a mapping from @t1@ to @Int@ (called a __substitution__) and
we go over the rest of the constraints and replace @t1@ with @Int@ (__apply the substitution__).

We also keep all the substitutions we created from the constraints (and merge them to one substitution
but applying new substitution to the accumulated substitution).

If we see ~Equality (TypeCon "Int") (TypeCon "String")~, we throw a type error,
because the two types do not match.

We keep going until there are no more constraints or until we encountered an error.

The result of the algorithm is the accumulated substitution.

-}
solve :: Constraints -> Either TypeErrorA Substitution
solve = runSolve

-- ** Types

-- | Monadic capabilities for the Solve algorithm
type Solve m
  = ( MonadError TypeErrorA m
    , MonadState ElabState m -- We highjack the ElabState type for the genTypeVar function.
    )

-- ** Algorithm

runSolve :: Constraints -> Either TypeErrorA Substitution
runSolve =
  ( runExcept
  . flip evalStateT (ElabState 0 mempty mempty)
  . solveConstraints mempty
  . S.toList
  )

-- | Recursively the constraints in order, passing the accumulated substitution.
solveConstraints :: Solve m => Substitution -> [ConstraintA] -> m Substitution
solveConstraints sub = \case
  [] -> pure sub
  c : cs -> do
    (newCons, newSub) <- solveConstraint c
    cs' <- substitute newSub (newCons <> cs) -- apply the new substitution on the rest of the constraints
    sub' <- substitute newSub sub -- apply the new substitution to the accumulative substitution
    solveConstraints (M.union newSub sub') cs'

-- | Solve a constraint.
--   Returns new constraints that may arise from this constraint and a substitution
solveConstraint :: Solve m => ConstraintA -> m ([ConstraintA], Substitution)
solveConstraint (constraint, ann) =
  -- case ltrace "constraint" constraint of
  case constraint of
    -- For let polymorphism. Instantiate a type.
    InstanceOf t1 t2 -> do
      (_, t2') <- instantiate t2
      pure ([(Equality t1 t2', ann)], mempty)

    -- When the two types are equals, there's nothing to do.
    Equality t1 t2
      | t1 == t2 ->
        pure (mempty, mempty)

    -- Map a type variable to the other type
    Equality (TypeVar tv) t2 ->
      pure
        ( mempty
        , M.singleton tv t2
        )

    Equality t1 (TypeVar tv) ->
      solveConstraint (Equality (TypeVar tv) t1, ann)

    -- Match the arguments and the return types
    Equality t1@(TypeFun args1 ret1) t2@(TypeFun args2 ret2) -> do
      unless (length args1 == length args2) $
        throwErr [ann] $ ArityMismatch t1 t2

      pure
        ( map (flip (,) ann) $ zipWith Equality (ret1 : args1) (ret2 : args2)
        , mempty
        )

    Equality (TypeApp f1 a1) (TypeApp f2 a2) ->
      pure
        ( map (flip (,) ann) $ [Equality f1 f2, Equality a1 a2]
        , mempty
        )

    -- When all else fails, throw an error.
    Equality t1 t2 ->
      throwErr [ann] $ TypeMismatch t1 t2

-- | Create an instance of a type.
--   (The type serves as a template for specialized types)
instantiate :: Solve m => Type -> m (Env TypeVar, Type)
instantiate typ = do
  -- collect all type variables, generate a new type variable for each one
  -- replace old type variables with new ones
  env <- fmap M.fromList $ sequence
    [ (,) tv <$> genTypeVar "ti"
    | TypeVar tv <- U.universe typ
    ]
  pure $ (,) env $ flip U.transform typ $ \case
    TypeVar tv
      | Just tv' <- M.lookup tv env ->
        TypeVar tv'
    other -> other



--------------------------
-- * Substitute

{- |

Apply a substitution

-}

-- ** Types

type Substitute m
  = ( MonadError TypeErrorA m
    )

-- | Replaces all type variables for any data type that has an instance of Data using uniplate magic
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
      (occursCheck v) -- Maybe sure we don't have an infinite type
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



