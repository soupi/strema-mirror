{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, OverloadedStrings, StandaloneDeriving, TypeOperators #-}

module Language.Strema.Compiler.Run where

import Language.Strema.Compiler.Compile
import Utils
import qualified Language.Strema.Syntax.Ast as Strema
import qualified Language.Strema.Syntax.Parser as Parser
import qualified Language.Strema.Types.Infer as Infer

import Options.Generic
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit
import System.IO

data Command w
  = Compile
    { input :: w ::: FilePath <?> "input file"
    , output :: w ::: Maybe FilePath <?> "output file"
    }
  | Parse
    { input :: w ::: FilePath <?> "input file"
    , output :: w ::: Maybe FilePath <?> "output file"
    }
  | Infer
    { input :: w ::: FilePath <?> "input file"
    , output :: w ::: Maybe FilePath <?> "output file"
    }
  deriving Generic

instance ParseRecord (Command Wrapped)
deriving instance Show (Command Unwrapped)

run :: IO ()
run = do
  opts <- unwrapRecord "stremac"
  case opts of
    Compile inputFile outputFile -> do
      process compile inputFile outputFile

    Parse inputFile outputFile -> do
      process
        (fmap (fmap pShow) . Parser.runParser Parser.parseFile)
        inputFile
        outputFile

    Infer inputFile outputFile -> do
      process
        (fmap (fmap pShow) . inferPipeline)
        inputFile
        outputFile

testInfer :: T.Text -> IO ()
testInfer = either T.putStrLn (T.putStrLn . pShow) . inferPipeline "test"

inferPipeline :: FilePath -> T.Text -> Either T.Text (Strema.File Infer.Ann)
inferPipeline path src = do
  parsed <- first pShow $ Parser.runParser Parser.parseFile path src
  inferred <- first pShow $ Infer.infer parsed
  pure inferred

process
  :: (FilePath -> T.Text -> Either T.Text T.Text)
  -> FilePath
  -> Maybe FilePath
  -> IO ()
process func inputFile outputFile = do
  file <- T.readFile inputFile
  let
    result = func inputFile file
  case result of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right prog -> do
      maybe T.putStrLn T.writeFile outputFile prog