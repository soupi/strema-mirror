module Language.Strema.Common where

import Data.Data (Data)
import qualified Data.Text as T
import qualified Data.Map as M

type TypeVar = T.Text
type TypeCon = T.Text
type Constr = T.Text
type Label = T.Text

type Record a
  = M.Map Label a

data Variant a
  = Variant Constr a
  deriving (Show, Eq, Ord, Data, Functor, Foldable, Traversable)
