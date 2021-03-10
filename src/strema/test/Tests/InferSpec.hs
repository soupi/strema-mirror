
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Tests.InferSpec where

import Language.Strema

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
    functions
    variants
    let_polymorphism

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
          ( TypeFun [TypeVar "t1"] (TypeVar "t1") )
          ( EFun ["x"] [ SExpr ( EAnnotated (TypeVar "t1") $ EVar "x" ) ] )
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

functions :: Spec
functions = do
  describe "functions" $ do
    it "builtin add" $
      shouldBe
        (testinfer "def increment(n) := add(n, 1)")
        ( pure $ boilerplateFun "increment" (TypeFun [tInt] tInt) ["n"]
          [ SExpr $
            EAnnotated tInt $ EFunCall
              (EAnnotated (TypeFun [tInt, tInt] tInt) (EVar "add"))
              [EAnnotated tInt $ EVar "n", EAnnotated tInt $ ELit $ LInt 1]
          ]
        )

    it "builtin bool" $
      shouldBe
        (testinfer "def nand(x, y) := not(and(x, y))")
        ( pure $ boilerplateFun "nand" (TypeFun [tBool, tBool] tBool) ["x", "y"]
          [ SExpr $
            EAnnotated tBool $ EFunCall
              (EAnnotated (TypeFun [tBool] tBool) (EVar "not"))
              [ EAnnotated tBool $ EFunCall
                  (EAnnotated (TypeFun [tBool, tBool] tBool) (EVar "and"))
                  [EAnnotated tBool $ EVar "x", EAnnotated tBool $ EVar "y"]
              ]
          ]
        )

    it "id" $
      shouldBe
        (testinfer "def id(x) := x")
        ( pure $ boilerplateFun "id" (TypeFun [TypeVar "targ2"] (TypeVar "targ2")) ["x"]
          [ SExpr $ EAnnotated (TypeVar "targ2") $ EVar "x"
          ]
        )

    it "const" $
      shouldBe
        (testinfer "def const(x, y) := x")
        ( pure $ boilerplateFun "const" (TypeFun [TypeVar "targ2", TypeVar "targ3"] (TypeVar "targ2")) ["x", "y"]
          [ SExpr $ EAnnotated (TypeVar "targ2") $ EVar "x"
          ]
        )

variants :: Spec
variants = do
  describe "variants" $ do
    it "Id" $
      shouldBe
        ( testinfer [r|
type Id a =
    | Id a
end

def oneId := Id 1
|])
        ( pure $ File
          [ TypeDef tUnit $ Datatype "Id" ["a"]
            [Variant "Id" (TypeVar "a")]
          , TermDef (TypeApp (TypeCon "Id") tInt) $
            Variable "oneId" $
              EAnnotated (TypeApp (TypeCon "Id") tInt) $
                EVariant $ Variant "Id" $ EAnnotated tInt $ ELit $ LInt 1
          ]
        )

let_polymorphism :: Spec
let_polymorphism = do
  describe "let polymorphism" $ do
    it "id" $
      shouldBe
        ( testinfer [r|
def id(x) := x
def intId := id(1)
def strId := id("hello")
|])
        ( pure $ File
          [ TermDef (TypeFun [TypeVar "targ4"] (TypeVar "targ4")) $
            Function "id" ["x"]
              [ SExpr $ EAnnotated (TypeVar "targ4") $ EVar "x"
              ]

          , TermDef tInt $
            Variable "intId" $
              EAnnotated tInt $
                EFunCall
                  (EAnnotated (TypeFun [tInt] tInt) (EVar "id"))
                  [EAnnotated tInt $ ELit $ LInt 1]

          , TermDef tString $
            Variable "strId" $
              EAnnotated tString $
                EFunCall
                  (EAnnotated (TypeFun [tString] tString) (EVar "id"))
                  [EAnnotated tString $ ELit $ LString "hello"]
          ]
        )

-------------------------

boilerplate :: Var -> Type -> Expr Type -> File Type
boilerplate name typ expr =
  File [TermDef typ $ Variable name $ EAnnotated typ expr]

boilerplateFun :: Var -> Type -> [Var] -> Block Type -> File Type
boilerplateFun name typ args body =
  File [TermDef typ $ Function name  args body]

testinfer :: T.Text -> Either T.Text (File Type)
testinfer = fmap (fmap annType) . inferPipeline "test"
