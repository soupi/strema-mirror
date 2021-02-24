module Strema.Rewrites where

import Utils
import Strema.Ast
import qualified Strema.Rewrites.RemoveAnn as RemoveAnn

rewrites :: Data a => File a -> File ()
rewrites = RemoveAnn.removeAnn
