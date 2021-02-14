{-# language OverloadedStrings #-}

module Tests.TranslateSpec where

import Compile
import Strema.Ast

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M

import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  runIO $ createDirectoryIfMissing False testsdir
  describe "translation" $ do
    simples
    patmatch

-- expressions --

simples :: Spec
simples = do
  describe "Simple" $ do
    it "lit int" $ do
      check "litint"
        ( boilerplate $
          ELit $ LInt 7
        )
        "7"

    it "variant" $ do
      check "variant"
        ( boilerplate $
          EVariant "Nil" (ERecord mempty)
        )
        "{ _field: {}, _tag: 'Nil' }"

patmatch :: Spec
patmatch = do
  describe "Pattern matching" $ do
    it "wildcard" $ do
      check "wildcard"
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PWildcard, ELit $ LInt 1)
          ]
        )
        "1"

    it "case int" $ do
      check "case_int"
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PLit (LInt 1), ELit $ LInt 1)
          , (PLit (LInt 0), ELit $ LInt 0)
          ]
        )
        "0"

    it "case var" $ do
      check "case_var"
        ( boilerplate $
          ECase (ELit $ LInt 17)
          [ (PLit (LInt 1), ELit $ LInt 1)
          , (PLit (LInt 0), ELit $ LInt 0)
          , (PVar "v", EVar "v")
          ]
        )
        "17"

    it "case_variant_label" $ do
      check "case_variant_label"
        ( boilerplate $
          ECase
          ( EVariant "Nil" $
            ERecord $ M.fromList
            [ ("head", ELit $ LInt 0)
            , ("tail", ERecord mempty)
            ]
          )
          [ (PVariant "Nil" (PVar "obj"), ERecordAccess (EVar "obj") "head")
          ]
        )
        "0"

    it "case variant record" $ do
      check "case_variant_record"
        ( boilerplate $
          ECase
          ( EVariant "Nil" $
            ERecord $ M.fromList
            [ ("head", ELit $ LInt 0)
            , ("tail", ERecord mempty)
            ]
          )
          [ ( PVariant "Nil"
              ( PRecord $ M.fromList
                [ ("head", PVar "head")
                , ("tail", PRecord mempty)
                ]
              )
            , EVar "head"
            )
          ]
        )
        "0"


--------------------------------------------

check :: FilePath -> File -> String -> IO ()
check name file expected = do
  let
    fname = testsdir <> name <> ".strm"
  T.writeFile fname $ compile file
  result <- readProcess "nodejs" [fname] ""
  shouldBe result (expected <> "\n")

testsdir = "/tmp/strema-tests/"

boilerplate :: Expr -> File
boilerplate e = File
  [ Function "main" []
    [ SExpr $ EFfi "console.log" [ e ]
    ]
  ]
