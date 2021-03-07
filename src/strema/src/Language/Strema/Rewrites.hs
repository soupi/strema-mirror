module Language.Strema.Rewrites where

import Utils
import Language.Strema.Syntax.Ast
import qualified Language.Strema.Rewrites.RemoveAnn as RemoveAnn

rewrites :: Data a => File a -> File ()
rewrites = RemoveAnn.removeAnn
