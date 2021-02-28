
module Utils
  ( module Utils
  , module Export
  )
  where

import Data.Bifunctor as Export
import Data.Functor as Export
import Data.Data as Export (Data)
import Data.Text as Export (Text)
import Data.Set as Export (Set)
import Data.Map as Export (Map)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Pretty.Simple as PS

pShow :: Show a => a -> Text
pShow = TL.toStrict . PS.pShow

toString :: Text -> String
toString = T.unpack

toText :: String -> Text
toText = T.pack
