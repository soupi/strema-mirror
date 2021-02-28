{-# language OverloadedStrings #-}

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

import Debug.Trace

ltrace :: Show a => Text -> a -> a
ltrace lbl x = trace (toString $ lbl <> ": " <> pShow x) x
{-# warning ltrace "ltrace left in code" #-}

ltraceM :: Applicative m => Show a => Text -> a -> m ()
ltraceM lbl x = traceM (toString $ lbl <> ": " <> pShow x)
{-# warning ltraceM "ltraceM left in code" #-}

pShow :: Show a => a -> Text
pShow = TL.toStrict . PS.pShow

toString :: Text -> String
toString = T.unpack

toText :: String -> Text
toText = T.pack
