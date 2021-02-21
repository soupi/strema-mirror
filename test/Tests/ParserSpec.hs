
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Tests.ParserSpec where

import Strema.Ast
import Strema.Parser

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
  describe "parser" $ do
    lits
    records

lits :: Spec
lits = do
  describe "literals" $ do
    numbers
    strings

numbers :: Spec
numbers = do
  describe "literals" $ do
    it "0" $
      shouldBe
        (testParser parseLit "0")
        (pure $ LInt 0)

    it "11717" $
      shouldBe
        (testParser parseLit "11717")
        (pure $ LInt 11717)

    it "-11" $
      shouldBe
        (testParser parseLit "-11")
        (pure $ LInt (-11))

    it "0.0" $
      shouldBe
        (testParser parseLit "0.0")
        (pure $ LFloat 0)

    it "117.17" $
      shouldBe
        (testParser parseLit "117.17")
        (pure $ LFloat 117.17)

    it "-1.1" $
      shouldBe
        (testParser parseLit "-1.1")
        (pure $ LFloat (-1.1))

strings :: Spec
strings = do
  describe "strings" $ do
    it "empty string" $
      shouldBe
        (testParser parseLit [r|""|])
        (pure $ LString "")

    it "hello world" $
      shouldBe
        (testParser parseLit [r|"hello world"|])
        (pure $ LString "hello world")

    it "quotes" $
      shouldBe
        (testParser parseLit [r|"\"hello world\""|])
        (pure $ LString "\"hello world\"")

    it "newlines" $
      shouldBe
        (testParser parseLit [r|"hello\nworld"|])
        (pure $ LString "hello\nworld")

records :: Spec
records = do
  describe "records" $ do
    it "{}" $
      shouldBe
        (testParser (parseRecord parseExpr parseExpr) "{}")
        (pure (mempty, Nothing))

    it "{} with spaces" $
      shouldBe
        (testParser (parseRecord parseExpr parseExpr) "{    \n }")
        (pure (mempty, Nothing))

    it "x, y" $
      shouldBe
        ( testParser
          (parseRecord parseLit parseLit)
          [r|{ x = "hello", y = -1.1 } |]
        )
        ( pure
          ( M.fromList
            [ ("x", LString "hello")
            , ("y", LFloat (-1.1))
            ]
          , Nothing
          )
        )

    it "duplicate labels" $
      shouldBe
        ( testParser
          (parseRecord parseLit parseLit)
          [r|{ a = "hello", a = 2, a = -1.1 } |]
        )
        ( pure
          ( M.fromList
            [ ("a", LString "hello")
            ]
          , Nothing
          )
        )

testParser :: Parser a -> T.Text -> Either T.Text a
testParser p src =
  runParser p "test" src
