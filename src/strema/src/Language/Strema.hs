{- | Exporting the important stuff from the strema frontend
-}

module Language.Strema
  ( module Export
  , module Language.Strema
  )
where

import qualified Data.Text.IO as T

import Utils

import Language.Strema.Syntax.Ast as Export
import Language.Strema.Types.Infer as Export (infer, TypeError(..), TypeErrorA, Ann(..))
import Language.Strema.Builtins as Export
import Language.Strema.Rewrites as Export
import Language.Strema.Rewrites.RemoveAnn as Export (removeAnn, removeAnn')
import Language.Strema.Syntax.Parser (runParser, parseFile)
import Text.Megaparsec as Export (SourcePos)

parse :: FilePath -> Text -> Either Text (File SourcePos)
parse = runParser parseFile

inferPipeline :: FilePath -> Text -> Either Text (File Ann)
inferPipeline path src = do
  parsed <- parse path src
  inferred <- first pShow $ infer parsed
  pure inferred

testInfer :: Text -> IO ()
testInfer = either T.putStrLn (T.putStrLn . pShow) . inferPipeline "test"

