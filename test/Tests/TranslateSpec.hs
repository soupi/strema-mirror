{-# language OverloadedStrings #-}

module Tests.TranslateSpec where

import Compile
import Strema.Ast

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map as M

import System.Process (readProcess)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "translation" $ do
    simples
    patmatch

-- expressions --

simples :: Spec
simples = do
  describe "Simple" $ do
    it "lit int" $ do
      check
        ( boilerplate $
          ELit $ LInt 7
        )
        "7"

    it "variant" $ do
      check
        ( boilerplate $
          EVariant "Nil" (ERecord mempty)
        )
        "{ _field: {}, _tag: 'Nil' }"

patmatch :: Spec
patmatch = do
  describe "Pattern matching" $ do
    it "wildcard" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PWildcard, ELit $ LInt 1)
          ]
        )
        "1"

    it "case int" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PLit (LInt 1), ELit $ LInt 1)
          , (PLit (LInt 0), ELit $ LInt 0)
          ]
        )
        "0"

    it "case var" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 17)
          [ (PLit (LInt 1), ELit $ LInt 1)
          , (PLit (LInt 0), ELit $ LInt 0)
          , (PVar "v", EVar "v")
          ]
        )
        "17"

    it "case_variant_label" $ do
      check
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
      check
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

check :: File -> String -> IO ()
check file expected = do
  result <- readProcess "nodejs" [] (T.unpack $ compile file)
  shouldBe result (expected <> "\n")

boilerplate :: Expr -> File
boilerplate e = File
  [ Function "main" []
    [ SExpr $ EFfi "console.log" [ e ]
    ]
  ]
