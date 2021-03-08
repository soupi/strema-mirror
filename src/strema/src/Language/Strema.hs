{- | Exporting the important stuff from the strema frontend
-}

module Language.Strema
  ( -- * Strema frontend
    module Language.Strema
    -- * Strema language definition
  , module Language.Strema.Syntax.Ast
    -- * Strema parser extra
  , module Text.Megaparsec
    -- * Strema Type Inference
  , module Language.Strema.Types.Infer
    -- * Strema builtin functions and types
  , module Language.Strema.Builtins
    -- * Strema Rewrites
  , module Language.Strema.Rewrites
  , module Language.Strema.Rewrites.RemoveAnn
  )
where

import qualified Data.Text.IO as T

import Utils

import Language.Strema.Syntax.Ast
import Language.Strema.Syntax.Parser (runParser, parseFile)
import Language.Strema.Types.Infer (infer, Ann(..), TypeError(..), TypeErrorA)
import Language.Strema.Builtins
import Language.Strema.Rewrites
import Language.Strema.Rewrites.RemoveAnn (removeAnn, removeAnn')
import Text.Megaparsec (SourcePos)

-- * Strema Parser

-- | Parse a Strema source file from text
parse :: FilePath -> Text -> Either Text (File SourcePos)
parse = runParser parseFile

-- * Strema Type Inference

-- | Parse and infer a Strema source file from text
inferPipeline :: FilePath -> Text -> Either Text (File Ann)
inferPipeline path src = do
  parsed <- parse path src
  inferred <- first pShow $ infer parsed
  pure inferred

-- | Parse and infer a Strema sourcefile and output the result to stdout
testInfer :: Text -> IO ()
testInfer = either T.putStrLn (T.putStrLn . pShow) . inferPipeline "test"

