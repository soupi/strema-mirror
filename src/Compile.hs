{- | Compilation runner
-}

module Compile where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Strema.Ast as Strm
import qualified Strema.Builtins as Strm
import qualified Strema.Parser as Strm
import qualified JS
import Translate

compileText :: FilePath -> T.Text -> Either T.Text T.Text
compileText file src = do
  ast <- Strm.runParser Strm.parseFile file src
  pure $
    ( JS.pp JS.ppFile
    . translate translateFile Strm.builtins
    . fmap (const ())
    ) ast

compile :: Strm.File () -> T.Text
compile = JS.pp JS.ppFile . translate translateFile Strm.builtins

compileIO :: Strm.File () -> IO ()
compileIO = T.putStrLn . compile
