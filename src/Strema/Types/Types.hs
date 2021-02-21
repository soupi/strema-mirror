{- | Type definitions

-}
module Strema.Types.Types where

import Strema.Common
import Data.Data (Data)

-- | A data type representing types
data Type
  = TypeVar TypeVar -- ^ type variables, like @a@
  | TypeCon TypeCon -- ^ type constructors, like @List@ and @Int@
  | TypeApp Type Type -- ^ type application, like @List a@
  | TypeFun [Type] Type -- ^ the type of functions
  | TypeRec [(Label, Type)] -- ^ the type of a record, like { x : Int, y : String }
  deriving (Show, Eq, Ord, Data)

