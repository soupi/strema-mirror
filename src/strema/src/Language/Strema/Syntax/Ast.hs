{- | Strema AST definition
-}

module Language.Strema.Syntax.Ast
  ( -- * Language definition
    module Language.Strema.Syntax.Ast
  -- * Common data types
  , module Language.Strema.Common
  -- * Strema types
  , module Language.Strema.Types.Types
  )
where

import Data.Data (Data)
import qualified Data.Text as T
import qualified Data.Map as M

import Language.Strema.Types.Types
import Language.Strema.Common

-- * Language definition

-- | The type of a source file
data File a
  = File [Definition a]
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

-- | A top level definition
data Definition a
  = TypeDef a Datatype -- ^ A type definition
  | TermDef a (TermDef a) -- ^ A term definition
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

-- | A term definition
data TermDef a
  = Variable Var (Expr a) -- ^ A variable
  | Function Var [Var] (Block a) -- ^ A function
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

-- | A Block is a list of statements
type Block a = [Statement a]

-- | A data type definition
data Datatype
  = Datatype Constr [TypeVar] [Variant Type]
  deriving (Show, Eq, Ord, Data)

-- | A Statement
data Statement a
  = SExpr (Expr a) -- ^ An expression
  | SDef a (TermDef a) -- ^ A term definition
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

-- | An expression
data Expr a
  -- | Annotates the child expression with some annotation
  = EAnnotated a (Expr a)
  -- | A Literal
  | ELit Lit
  -- | A variable name
  | EVar Var
  -- | An anonymous function
  | EFun [Var] (Block a)
  -- | A function call
  | EFunCall (Expr a) [Expr a]
  -- | A variant (or data constructor) and expression attached
  | EVariant (Variant (Expr a))
  -- | A record
  | ERecord (Record (Expr a))
  -- | Record access to a specific label
  | ERecordAccess (Expr a) Label
  -- | Extends a record
  | ERecordExtension (Record (Expr a)) (Expr a)
  -- | A case expression (pattern matching)
  | ECase (Expr a) [(Pattern, Block a)]
  -- | A foreign function interface call
  | EFfi T.Text [Expr a]
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

-- | A literal
data Lit
  = LInt Int -- ^ Integers
  | LFloat Float -- ^ Floating point numbers
  | LString T.Text -- ^ Strings
  deriving (Show, Eq, Ord, Data)

-- | A pattern
data Pattern
  = PWildcard -- ^ A catch all pattern
  | PVar Var -- ^ A variable capture pattern
  | PLit Lit -- ^ A literal pattern
  | PVariant (Variant Pattern) -- ^ A variant pattern
  | PRecord (Record Pattern) -- ^ A record pattern
  deriving (Show, Eq, Ord, Data)

-- | A variable name
type Var = T.Text

-- * Utils

-- | Create a record expression (or extension) from a a list of (label, expression)
--   and a expression to extend (if needed).
--
--   Note that this function is left-biased - duplicate fields will preserve the first field.
mkERec :: [(Label, Expr a)] -> Maybe (Expr a) -> Expr a
mkERec fields = maybe
  (ERecord $ M.fromListWith (flip const) fields)
  (ERecordExtension $ M.fromListWith (flip const) fields)
