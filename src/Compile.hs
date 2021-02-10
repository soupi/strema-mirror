module Compile where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Strema.Ast as Strm
import qualified JS
import CodeGen

compile :: Strm.File -> T.Text
compile = JS.pp JS.ppFile . translateFile

compileIO :: Strm.File -> IO ()
compileIO = T.putStrLn . compile
