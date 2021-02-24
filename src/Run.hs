{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, OverloadedStrings, StandaloneDeriving, TypeOperators #-}

module Run where

import Compile
import Utils
import qualified Strema.Parser as Parser

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
  deriving Generic

instance ParseRecord (Command Wrapped)
deriving instance Show (Command Unwrapped)

run :: IO ()
run = do
  opts <- unwrapRecord "stremac"
  case opts of
    Compile inputFile outputFile -> do
      process compileText inputFile outputFile

    Parse inputFile outputFile -> do
      process
        (fmap (fmap pShow) . Parser.runParser Parser.parseFile)
        inputFile
        outputFile

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
