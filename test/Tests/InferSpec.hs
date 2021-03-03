
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Tests.InferSpec where

import Run
import Strema.Ast
import Strema.Builtins
import Strema.Types.Infer

import Data.Data (Data)
import Test.Hspec
import Text.RawString.QQ
import qualified Data.Text as T
import qualified Data.Map as M

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "infer" $ do
    lits
    lambdas
    funcalls

lits :: Spec
lits = do
  describe "literals" $ do
    it "assign lit" $
      shouldBe
        (testinfer "def x := 1")
        (pure $ boilerplate "x" tInt $ ELit $ LInt 1)

    it "assign string" $
      shouldBe
        (testinfer [r|def x := "hello"|])
        (pure $ boilerplate "x" tString $ ELit $ LString "hello")

    it "assign float" $
      shouldBe
        (testinfer "def x := 1.0")
        (pure $ boilerplate "x" tFloat $ ELit $ LFloat 1)

lambdas :: Spec
lambdas = do
  describe "lambdas" $ do
    it "assign id" $
      shouldBe
        (testinfer "def id := fun(x) -> x")
        ( pure $ boilerplate "id"
          ( TypeFun [TypeVar "t3"] (TypeVar "t3") )
          ( EFun ["x"] [ SExpr ( EAnnotated (TypeVar "t3") $ EVar "x" ) ] )
        )

funcalls :: Spec
funcalls = do
  describe "funcalls" $ do
    it "id 1" $
      shouldBe
        (testinfer "def one := (fun(x) -> x)(1)")
        ( pure $ boilerplate "one" tInt $
          EFunCall
            ( EAnnotated ( TypeFun [tInt] tInt ) $
              EFun ["x"] [ SExpr ( EAnnotated tInt $ EVar "x" ) ]
            )
            [ EAnnotated tInt $
              ELit $ LInt 1
            ]
        )

-------------------------

boilerplate :: Var -> Type -> Expr Type -> File Type
boilerplate name typ expr =
  File [TermDef typ $ Variable name $ EAnnotated typ expr]

testinfer :: T.Text -> Either T.Text (File Type)
testinfer = inferPipeline "test"
