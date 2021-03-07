{- | Compilation runner
-}

module Language.Strema.Compiler.Compile where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Language.Strema.Syntax.Ast as Strm
import qualified Language.Strema.Syntax.Parser as Strm
import qualified Language.Strema.Builtins as Strm
import qualified Language.Strema.Rewrites as Strm
import qualified Language.Backend.JS as JS
import Language.Strema.Compiler.Translate

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
