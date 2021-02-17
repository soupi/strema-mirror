{- | Compilation runner
-}

module Compile where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Strema.Ast as Strm
import qualified Strema.Builtins as Strm
import qualified JS
import Translate

compile :: Strm.File -> T.Text
compile = JS.pp JS.ppFile . translate translateFile Strm.builtins

compileIO :: Strm.File -> IO ()
compileIO = T.putStrLn . compile
