-- | Common data types

module Language.Strema.Common where

import Data.Data (Data)
import qualified Data.Text as T
import qualified Data.Map as M

-- | A type variable name
type TypeVar = T.Text
-- | A type constructor name
type TypeCon = T.Text
-- | A data constructor
type Constr = T.Text
-- | A record label
type Label = T.Text

-- | A general record data type
type Record a
  = M.Map Label a

-- | A general variant data type
data Variant a
  = Variant Constr a
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)
