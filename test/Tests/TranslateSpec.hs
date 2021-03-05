{-# language OverloadedStrings #-}

module Tests.TranslateSpec where

import Compile
import Strema.Ast
import Strema.Builtins

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
spec = parallel $ do
  describe "translation" $ do
    simples
    records
    patmatch
    builtin

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
          EVariant $ Variant "Nil" (ERecord mempty)
        )
        "{ _constr: 'Nil', _field: {} }"

    it "function application" $ do
      check
        ( boilerplate $
          EFunCall (EFun ["a", "b"] [SExpr $ EVar "a"])
            [ ELit $ LInt 7, ELit $ LString "hello" ]
        )
        "7"

records :: Spec
records = do
  describe "Records" $ do
    it "record access" $ do
      check
        ( boilerplate $
          ERecordAccess
            ( ERecord $ M.fromList [("x", ELit $ LInt 9)] )
            "x"
        )
        "9"

    it "record extension" $ do
      check
        ( boilerplate $
          ERecordAccess
            ( ERecordExtension
              ( M.fromList
                [ ("x", ELit $ LInt 7)
                ]
              )
              (ERecord mempty)
            )
            "x"
        )
        "7"

    it "record extension preserves info" $ do
      check
        ( boilerplate $
          ERecordAccess
            ( ERecordExtension
              ( M.fromList
                [ ("x", ELit $ LInt 7)
                ]
              )
              ( ERecord $ M.fromList
                [ ("y", ELit $ LString "hello")
                ]
              )
            )
            "y"
        )
        "hello"

    it "record extension overrides fields" $ do
      check
        ( boilerplate $
          ERecordAccess
            ( ERecordExtension
              ( M.fromList
                [ ("x", ELit $ LInt 7)
                ]
              )
              ( ERecord $ M.fromList
                [ ("x", ELit $ LString "hello")
                ]
              )
            )
            "x"
        )
        "7"

patmatch :: Spec
patmatch = do
  describe "Pattern matching" $ do
    it "wildcard" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PWildcard, pure . SExpr $ ELit $ LInt 1)
          ]
        )
        "1"

    it "case int" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PLit (LInt 1), pure . SExpr $ ELit $ LInt 1)
          , (PLit (LInt 0), pure . SExpr $ ELit $ LInt 0)
          ]
        )
        "0"

    it "case var" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 17)
          [ (PLit (LInt 1), pure . SExpr $ ELit $ LInt 1)
          , (PLit (LInt 0), pure . SExpr $ ELit $ LInt 0)
          , (PVar "v", pure . SExpr $ EVar "v")
          ]
        )
        "17"

    it "case_variant_label" $ do
      check
        ( boilerplate $
          ECase
          ( EVariant $ Variant "Nil" $
            ERecord $ M.fromList
            [ ("head", ELit $ LInt 0)
            , ("tail", ERecord mempty)
            ]
          )
          [ (PVariant $ Variant "Nil" (PVar "obj"), pure . SExpr $ ERecordAccess (EVar "obj") "head")
          ]
        )
        "0"

    it "case variant record" $ do
      check
        ( boilerplate $
          ECase
          ( EVariant $ Variant "Nil" $
            ERecord $ M.fromList
            [ ("head", ELit $ LInt 0)
            , ("tail", ERecord mempty)
            ]
          )
          [ ( PVariant $ Variant "Nil"
              ( PRecord $ M.fromList
                [ ("head", PVar "head")
                , ("tail", PRecord mempty)
                ]
              )
            , pure . SExpr $ EVar "head"
            )
          ]
        )
        "0"

    it "nested case" $ do
      check
        ( boilerplate $
          ECase (ELit $ LInt 0)
          [ (PLit (LInt 1), pure . SExpr $ ELit $ LInt 1)
          , ( PLit (LInt 0)
            , pure . SExpr $ ECase (ELit $ LInt 99)
              [ (PLit (LInt 1), pure . SExpr $ ELit $ LInt 1)
              , (PVar "n", pure . SExpr $ EVar "n")
              ]
            )
          ]
        )
        "99"

builtin :: Spec
builtin = do
  describe "Builtins" $ do
    ints
    bools
    strings

ints :: Spec
ints = do
  describe "ints" $ do
    it "addition" $ do
      check
        ( boilerplate $
          EFunCall (EVar "add") [ ELit $ LInt 1, ELit $ LInt 1 ]
        )
        "2"

    it "negate" $ do
      check
        ( boilerplate $
          EFunCall (EVar "negate") [ ELit $ LInt 2 ]
        )
        "-2"

strings :: Spec
strings = do
  describe "strings" $ do
    it "concat" $ do
      check
        ( boilerplate $
          EFunCall (EVar "concat")
          [ EFunCall (EVar "concat") [ ELit $ LString "hello", ELit $ LString " " ]
          , ELit $ LString "world"
          ]
        )
        "hello world"

bools :: Spec
bools = do
  describe "bools" $ do
    it "not" $ do
      check
        ( boilerplate $
          EFunCall (EVar "not") [ false ]
        )
        "true"

    describe "and" $ do
      it "true true" $ do
        check
          ( boilerplate $
            EFunCall (EVar "and") [ true, true ]
          )
          "true"

      it "true false" $ do
        check
          ( boilerplate $
            EFunCall (EVar "and") [ true, false ]
          )
          "false"

    describe "or" $ do
      it "false false" $ do
        check
          ( boilerplate $
            EFunCall (EVar "or") [ false, false ]
          )
          "false"

      it "true false" $ do
        check
          ( boilerplate $
            EFunCall (EVar "or") [ true, false ]
          )
          "true"

--------------------------------------------

check :: File () -> String -> IO ()
check file expected = do
  result <- readProcess "nodejs" [] (T.unpack $ translate' file)
  shouldBe result (expected <> "\n")

boilerplate :: Expr () -> File ()
boilerplate e = File
  [ TermDef () $ Function "main" []
    [ SExpr $ EFfi "console.log" [ e ]
    ]
  ]
