
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

-------------------------

boilerplate :: Var -> Type -> Expr Type -> File Type
boilerplate name typ expr =
  File [TermDef typ $ Variable name $ EAnnotated typ expr]

testinfer :: T.Text -> Either T.Text (File Type)
testinfer = inferPipeline "test"
