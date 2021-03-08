{- | Exporting the important stuff from the strema frontend
-}

module Language.Strema
  ( module Language.Strema.Syntax.Ast
  , module Language.Strema.Syntax.Parser
  , module Language.Strema.Types.Infer
  , module Language.Strema.Builtins
  , module Language.Strema.Rewrites
  , module Language.Strema.Rewrites.RemoveAnn
  , module Text.Megaparsec
  , module Language.Strema
  )
where

import qualified Data.Text.IO as T

import Utils

import Language.Strema.Syntax.Ast
import Language.Strema.Syntax.Parser (runParser, parseFile)
import Language.Strema.Types.Infer (infer, TypeError(..), TypeErrorA, Ann(..))
import Language.Strema.Builtins
import Language.Strema.Rewrites
import Language.Strema.Rewrites.RemoveAnn (removeAnn, removeAnn')
import Text.Megaparsec (SourcePos)

parse :: FilePath -> Text -> Either Text (File SourcePos)
parse = runParser parseFile

inferPipeline :: FilePath -> Text -> Either Text (File Ann)
inferPipeline path src = do
  parsed <- parse path src
  inferred <- first pShow $ infer parsed
  pure inferred

testInfer :: Text -> IO ()
testInfer = either T.putStrLn (T.putStrLn . pShow) . inferPipeline "test"

