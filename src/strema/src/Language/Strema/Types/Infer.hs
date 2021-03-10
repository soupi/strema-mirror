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
{-# LANGUAGE ViewPatterns #-}

module Language.Strema.Types.Infer where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Generics.Uniplate.Data as U

import Utils
import Language.Strema.Syntax.Ast
import Language.Strema.Builtins
import qualified Language.Strema.Syntax.Parser as Parser

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

-- | The type of type errors.
data TypeError
  = TypeMismatch Type Type
  | UnboundVar Var
  | InfiniteType TypeVar Type
  | ArityMismatch Type Type
  | UnboundTypeVarsInType Datatype
  | DuplicateTypeVarsInSig Datatype
  | DuplicateConstrs Datatype
  | DuplicateConstrs2 [(Constr, (VariantSig InputAnn, VariantSig InputAnn))]
  | NoSuchVariant Constr
  | RecordDiff Type Type (S.Set Label)
  | NotARecord Type
  | DuplicateVarsInPattern Pattern
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

-- | A @Set@ of constraints.
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

-- | Throw an error with annotation.
throwErr :: MonadError TypeErrorA m => [InputAnn] -> TypeError -> m a
throwErr ann err = throwError (ann, err)

-- | Get the name of a term definition.
getTermName :: TermDef a -> Var
getTermName = \case
  Variable name _ -> name
  Function name _ _ -> name

-- | Extract a term definition and annotation.
getTermDef :: Definition InputAnn -> Maybe (InputAnn, TermDef InputAnn)
getTermDef = \case
  TermDef ann def -> Just (ann, def)
  TypeDef{} -> Nothing

-- | Extract a type definition and annotation.
getTypeDef :: Definition InputAnn -> Maybe (InputAnn, Datatype)
getTypeDef = \case
  TermDef{} -> Nothing
  TypeDef ann def -> Just (ann, def)

-- | Retrieve the annotation of an expression. Will explode when used on a non @EAnnotated@ node.
getExprAnn :: Show a => Expr a -> a
getExprAnn = \case
  EAnnotated typ _ -> typ
  e -> error $ toString $ "Expr is not annotated: " <> pShow e

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

-- | An environment from a name to something.
type Env a = Map Var a

-- | A mapping from a data constructor to the type that defines it and the type it holds.
type VariantEnv a
  = Map Constr (VariantSig a)

-- | Relevant information about a data constructor.
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
  maybe (throwErr [ann] $ NoSuchVariant constr) pure . M.lookup constr . esVariantEnv =<< get

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
runElaborate builtinsTypes =
  ( fmap (fmap esConstraints)
  . runExcept
  . flip runStateT (ElabState 0 mempty mempty) -- @TODO: add builtins variants?
  . flip runReaderT (ElabEnv mempty builtinsTypes)
  . elaborateFile
  )

-- | Elaborate a source file
elaborateFile :: Elaborate m => File InputAnn -> m (File Ann)
elaborateFile (File defs) = do
  _ <- traverse (uncurry elaborateTypeDef) $
    map ((,) $ Parser.dummyAnn "builtin") builtinDatatypes 
  let
    termDefs = mapMaybe getTermDef defs
    typeDefs = mapMaybe getTypeDef defs
    names = map (getTermName . snd) termDefs
  -- We invent types for top level term definitions.
  -- These should be let polymorphic so we use @Instance@ instead of @Concrete@
  typeDefs' <- traverse (uncurry elaborateTypeDef) typeDefs
  vars <- traverse (\name -> (,) name . Instance <$> genTypeVar "top") names
  File . (<>) typeDefs' <$> withEnv vars (traverse (uncurry elaborateDef) termDefs)

-- | Add a data type to the VariantEnv and elaborate it with a dummy type.
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
  t <- lookupVar ann name >>= \case
    Instance t' ->
      pure t'
      -- @t'@ is the type other definitions see.
      -- It is an instantiation of the type we just elaborated for def.
    Concrete t' ->
      error $ toString $ "unexpected Typer: " <> pShow t'
  -- When evaluating a definition, the type should not be let polymorphic.
  elab <- withEnv [(name, Concrete $ TypeVar t)] $ elaborateTermDef ann def
  constrain ann $ InstanceOf (TypeVar t) (eiType elab)
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
  Function name args body -> do
    tfun <- genTypeVar "tfun"
    targsEnv <- traverse (\arg -> (,) arg <$> genTypeVar "targ") args
    (tret, body') <- withEnv
      ((name, Concrete $ TypeVar tfun) : fmap (fmap (Concrete . TypeVar)) targsEnv)
      (elaborateBlock body)

    constrain ann $ Equality
      (TypeVar tfun)
      (TypeFun (map (TypeVar . snd) targsEnv) tret)

    pure $ ElabInfo
      { eiType = (TypeFun (map (TypeVar . snd) targsEnv) tret)
      , eiResult = Function name args body'
      , eiNewEnv = mempty
      }

-- | Elaborate a list of statements.
--   Returns the type of the final statement as well.
elaborateBlock :: Elaborate m => Block InputAnn -> m (Type, Block Ann)
elaborateBlock block = do
  -- we want to fold over the list of statements, and for each new definition,
  -- add it to the environment for the elaboration of the next expressions
  block' <- foldM
    ( \blockinfo stmt -> do
      stmt' <- withEnv' (eiNewEnv blockinfo) $ elaborateStmt stmt
      pure $ ElabInfo
        { eiResult = eiResult stmt' : eiResult blockinfo
        , eiType = eiType stmt'
        , eiNewEnv = M.union (eiNewEnv stmt') (eiNewEnv blockinfo)
        }
    )
    (ElabInfo [] tUnit mempty)
    block
  pure (eiType block', reverse (eiResult block'))

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
        tv <- genTypeVar $ t <> "_i"
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
    (tret, body') <- withEnv (fmap (fmap (Concrete . TypeVar)) targsEnv) $ elaborateBlock body

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

  -- We're going to only allow FFI that returns tUnit atm.
  -- Later on we'll add a type annotation to the FFI
  EFfi f args -> do
    args' <- traverse (elaborateExpr ann) args
    pure $ EAnnotated (Ann ann tUnit) $
      EFfi f args'

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

  ERecord record -> do
    record' <- traverse (elaborateExpr ann) record
    let
      recordT = TypeRec $ M.toList $ fmap getType record'

    pure $ EAnnotated (Ann ann recordT) $
      ERecord record'

  -- the "expr" should be a record with at least the "label" label
  ERecordAccess expr label -> do
    labelT <- TypeVar <$> genTypeVar "t"
    ext <- genTypeVar "t"
    expr' <- elaborateExpr ann expr
    constrain ann $ Equality
      (getType expr')
      (TypeRecExt [(label, labelT)] ext)
    pure $ EAnnotated (Ann ann labelT) $
      ERecordAccess expr' label

  ERecordExtension record expr -> do
    expr' <- elaborateExpr ann expr
    record' <- traverse (elaborateExpr ann) record
    et <- genTypeVar "t"
    constrain ann $ Equality (TypeVar et) (getType expr')
    let
      recordTypes = M.toList $ fmap getType record'
      t = TypeRecExt recordTypes et
    pure $ EAnnotated (Ann ann t) $
      ERecordExtension record' expr'

-- | Elaborate patterns in case expressions
elaboratePatterns
  :: Elaborate m
  => InputAnn -> Type -> Type -> [(Pattern, Block InputAnn)] -> m [(Pattern, Block Ann)]
elaboratePatterns ann exprT bodyT pats = do
  for pats $ \(pat, body) -> do
    env <- elaboratePattern ann exprT pat
    (t, body') <- withEnv env $ elaborateBlock body
    constrain ann $ Equality bodyT t
    pure (pat, body')
  

-- | Elaborate a single pattern match
elaboratePattern
  :: Elaborate m
  => InputAnn -> Type -> Pattern -> m [(Var, Typer)]
elaboratePattern ann exprT outerPat = do
  case outerPat of
    PWildcard ->
      pure mempty

    PVar v ->
      pure [(v, Concrete exprT)]

    PLit lit -> do
      constrain ann $ Equality (getLitType lit) exprT
      pure mempty

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
      elaboratePattern ann innerPatT innerPat

    PRecord record -> do
      record' <- for record $ \pat -> do
        t <- TypeVar <$> genTypeVar "t"
        (,) t <$> elaboratePattern ann t pat

      ext <- genTypeVar "t"
      constrain ann $ Equality
        exprT
        (TypeRecExt (M.toList $ fmap fst record') ext)

      -- check if we have duplicate names in the environment
      let
        envs = map (M.fromList . snd) $ M.elems record'
        env = M.unions $ map (M.fromList . snd) $ M.elems record'
      
      unless (sum (map length envs) == length env) $ do
        throwErr [ann] $ DuplicateVarsInPattern outerPat
      pure $ M.toList env

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

If we see @Equality (TypeCon "Int") (TypeCon "String")@, we throw a type error,
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
    -- See the comment on @instantiate@ for more information.
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

    -- all record labels and type should match
    Equality t1@(TypeRec (M.fromList -> rec1)) t2@(TypeRec (M.fromList -> rec2)) -> do
      let
        matches = M.elems $ M.intersectionWith Equality rec1 rec2
        diff = M.keysSet rec1 `S.difference` M.keysSet rec2
      unless (S.null diff) $
        throwErr [ann] $ RecordDiff t1 t2 diff
      pure
        ( map (flip (,) ann) matches
        , mempty
        )

    -- we want to make sure rec1 and rec2 do not contain matching labels with non matching types
    -- we want to match the labels from rec1 that do not exist in rec2 to match "ext"
    Equality (TypeRec (M.fromList -> rec1)) (TypeRecExt (M.fromList -> rec2) ext) -> do
      let
        matches = M.elems $ M.intersectionWith Equality rec1 rec2
        onlyLeft = M.difference rec1 rec2
      pure
        ( map (flip (,) ann) $ Equality (TypeRec $ M.toList onlyLeft) (TypeVar ext) : matches
        , mempty
        )
    Equality t1@TypeRecExt{} t2@TypeRec{} -> do
      solveConstraint (Equality t2 t1, ann)

    Equality (TypeRecExt (M.fromList -> rec1) ext1) (TypeRecExt (M.fromList -> rec2) ext2) -> do
      let
        matches = M.elems $ M.intersectionWith Equality rec1 rec2
        onlyLeft = M.difference rec1 rec2
        onlyRight = M.difference rec2 rec1
      ext1' <- genTypeVar "ext"
      ext2' <- genTypeVar "ext"
      pure
        ( map (flip (,) ann)
          $ Equality (TypeRecExt (M.toList onlyLeft) ext1') (TypeVar ext2)
          : Equality (TypeRecExt (M.toList onlyRight) ext2') (TypeVar ext1)
          : matches
        , mempty
        )

    -- When all else fails, throw an error.
    Equality t1 t2 ->
      throwErr [ann] $ TypeMismatch t1 t2

{- | Create an instance of a type. (The type serves as a template for specialized types)

Let polymorphism gives us the ability to use a generic function in more contexts.

For example, @id@ is a function that can work for @x@ of any type. But our algorithm
collects constraints globally, including that:

@
def id := fun(x) -> x

def one := id(1)          -- constrain that the type of id __is equal to__ the type [Int] -> tN

def hello := id("hello")  -- constrain that the type of id __is equal to__ the type [String] -> tM
@

We need to invent a new constraint that will define the relationship between the type of id
and the arguments passing to it as an __Instance of__ relationship.

@InstanceOf t1 t2@ relationship means that @t1@ is an @instantiation@ of @t2@
(or a special case if you will).
What we'll do is copy the type of @t2@, generate new type variables in place of all type variables
inside of it, and then say that this new type @t3@ has an equality relationship with @t1@.

It's important to solve the equality constraints for each function before solving the InstanceOf
constraints, so that when we instantiate we already have the final type of the function.

We will highjack the @Ord@ instance deriving (constructors defined later are bigger)
and the fact that @Set@ is ordered to accomplish that.


-}
instantiate :: Solve m => Type -> m (Env TypeVar, Type)
instantiate typ = do
  -- collect all type variables, generate a new type variable for each one
  -- replace old type variables with new ones
  env1 <- fmap M.fromList $ sequence
    [ (,) tv <$> genTypeVar "ti"
    | TypeVar tv <- U.universe typ
    ]
  env2 <- fmap M.fromList $ sequence
    [ (,) tv <$> genTypeVar "ti"
    | TypeRecExt _ tv <- U.universe typ
    ]
  let
    env = env1 <> env2
  pure $ (,) env $ flip U.transform typ $ \case
    TypeVar tv
      | Just tv' <- M.lookup tv env ->
        TypeVar tv'
    TypeRecExt r tv
      | Just tv' <- M.lookup tv env ->
        TypeRecExt r tv'
    other -> other



--------------------------
-- * Substitute

{- | Apply a substitution.

-}

-- ** API

-- | Replaces all type variables for any data type that
--   has an instance of Data using uniplate magic
substitute :: Substitute m => Data f => Substitution -> f -> m f
substitute sub = U.transformBiM (replaceTypeVar sub)

-- ** Types

-- | Monadic capabilities of Substitute
type Substitute m
  = ( MonadError TypeErrorA m
    )

-- ** Algorithm

-- | Find type variables that appear in the substitution and replace them.
replaceTypeVar :: Substitute m => Substitution -> Type -> m Type
replaceTypeVar sub = \case
  TypeVar v ->
    maybe
      (pure $ TypeVar v)
      (occursCheck v) -- Maybe sure we don't have an infinite type
      (M.lookup v sub)

  TypeRecExt r1 ext
    | Just r2 <- M.lookup ext sub -> do
      void $ occursCheck ext r2
      mergeRecords r1 r2

  other ->
    pure other

mergeRecords :: Substitute m => [(Label, Type)] -> Type -> m Type
mergeRecords r1 = \case
  TypeRec r2 ->
    pure . TypeRec . M.toList $ M.fromListWith (flip const) (r1 <> r2)
  TypeVar ext' ->
    pure $ TypeRecExt r1 ext'
  TypeRecExt r2 ext ->
    pure . flip TypeRecExt ext . M.toList $ M.fromListWith (flip const) (r1 <> r2)
  t ->
    throwErr [] $ NotARecord t

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



