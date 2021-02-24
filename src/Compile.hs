{- | Compilation runner
-}

module Compile where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Strema.Ast as Strm
import qualified Strema.Builtins as Strm
import qualified Strema.Parser as Strm
import qualified Strema.Rewrites as Strm
import qualified JS
import Translate

compile :: FilePath -> T.Text -> Either T.Text T.Text
compile file src = do
  ast <- Strm.runParser Strm.parseFile file src
  pure $
    ( JS.pp JS.ppFile
    . translate translateFile Strm.builtins
    . Strm.rewrites
    ) ast




translate' :: Strm.File () -> T.Text
translate' = JS.pp JS.ppFile . translate translateFile Strm.builtins

translate'IO :: Strm.File () -> IO ()
translate'IO = T.putStrLn . translate'
