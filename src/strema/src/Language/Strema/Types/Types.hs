{- | Type definitions

-}
module Language.Strema.Types.Types where

import Language.Strema.Common
import Data.Data (Data)

-- | A data type representing types
data Type
  = TypeVar TypeVar -- ^ type variables, like @a@
  | TypeCon TypeCon -- ^ type constructors, like @List@ and @Int@
  | TypeApp Type Type -- ^ type application, like @List a@
  | TypeFun [Type] Type -- ^ the type of functions
  | TypeRec [(Label, Type)] -- ^ the type of a record, like { x : Int, y : String }
  | TypeRecExt [(Label, Type)] TypeVar
    -- ^ a record with an extension, such as { x : Int, y : String | r }
    --   means "this record has at least { x : Int, y : String }, but can have more
  deriving (Show, Eq, Ord, Data)

