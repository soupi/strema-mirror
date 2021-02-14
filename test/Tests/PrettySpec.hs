{-# language OverloadedStrings #-}

module Tests.PrettySpec where

import JS.Ast
import JS.Pretty

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map as M

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pretty" $ do
    lits
    records
    exprs

-- expressions --

exprs :: Spec
exprs = do
  describe "Expressions" $ do
    it "var" $ do
      shouldBe
        (ppExpr' $ EVar "x")
        "x"

    it "function" $ do
      shouldBe
        (ppExpr' $ EFun ["x","y"] [SRet (EVar "x")])
        "function(x, y) {\n    return x;\n}"

    it "function call" $ do
      shouldBe
        (ppExpr' $ EFunCall (EVar "f") [])
        "f()"

    it "lambda function call" $ do
      shouldBe
        (ppExpr' $ EFunCall (EFun ["x","y"] [SRet (EVar "x")]) [ELit $ LInt 1, ELit $ LInt 2])
        "(function(x, y) {     return x; })(1, 2)"


-- literals --

lits :: Spec
lits = do
  describe "Literals" $ do
    ints
    floats
    strings
    bools

ints :: Spec
ints = do
  describe "int" $ do
    it "int simple" $
      shouldBe
        (ppLit' $ LInt 7)
        "7"

    it "int longer" $
      shouldBe
        (ppLit' $ LInt 1234754)
        "1234754"

    it "int negative" $
      shouldBe
        (ppLit' $ LInt (-1234754))
        "-1234754"

floats :: Spec
floats = do
  describe "Float" $ do
    it "Float simple" $
      shouldBe
        (ppLit' $ LFloat 7.0)
        "7.0"

    it "Float longer" $
      shouldBe
        (ppLit' $ LFloat 1234.754)
        "1234.754"

    it "Float negative" $
      shouldBe
        (ppLit' $ LFloat (-123.4754))
        "-123.4754"

strings :: Spec
strings = do
  describe "strings" $ do
    it "string simple" $
      shouldBe
        (ppLit' $ LString "hello")
        "\"hello\""

    it "string spaces" $
      shouldBe
        (ppLit' $ LString "hello   world")
        "\"hello   world\""

    it "string \"\"" $
      shouldBe
        (ppLit' $ LString "\"hello world\"")
        "\"\\\"hello world\\\"\""

bools :: Spec
bools = do
  describe "Boolean" $ do
    it "true" $
      shouldBe
        (ppLit' $ LBool True)
        "true"

    it "false" $
      shouldBe
        (ppLit' $ LBool False)
        "false"

records :: Spec
records = do
  describe "Records" $ do
    it "empty" $
      shouldBe
        (ppRecord' $ rec_ [])
        "{}"

    it "1 element" $
      shouldBe
        (ppRecord' $ rec_ [ "bool" .= ELit (LBool False) ])
        "{\"bool\" : false}"

    it "2 elements" $
      shouldBe
        ( ppRecord' $ rec_
          [ "bool" .= ELit (LBool False)
          , "int" .= ELit (LInt 7)
          ]
        )
        "{\"bool\" : false, \"int\" : 7}"

    it "nested" $
      shouldBe
        ( ppRecord' $ rec_
          [ "record" .= ERecord (rec_ [ "var" .= EVar "x" ])
          , "int" .= ELit (LInt 7)
          ]
        )
        "{\"int\" : 7, \"record\" : {\"var\" : x}}"

rec_ :: [(Var, a)] -> Record a
rec_ = M.fromList

(.=) :: Var -> a -> (Var, a)
(.=) = (,)

ppRecord' :: Record Expr -> T.Text
ppRecord' = T.init . pp (ppRecord ppExpr)

ppLit' :: Lit -> T.Text
ppLit' = T.init . pp ppLit

ppExpr' :: Expr -> T.Text
ppExpr' = T.init . pp ppExpr
