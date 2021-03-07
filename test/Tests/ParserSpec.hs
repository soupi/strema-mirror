
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Tests.ParserSpec where

import Strema.Ast
import Strema.Parser
import Strema.Rewrites.RemoveAnn

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
  describe "parser" $ do
    lits
    records
    expressions
    programs

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
        (testParser (parseRecord equals parseExpr $ Just parseExpr) "{}")
        (pure (mempty, Nothing))

    it "{} with spaces" $
      shouldBe
        (testParser (parseRecord equals parseExpr $ Just parseExpr) "{    \n }")
        (pure (mempty, Nothing))

    it "x, y" $
      shouldBe
        ( testParser
          parseLitRecord
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
          parseLitRecord
          [r|{ a = "hello", a = 2, a = -1.1 } |]
        )
        ( pure
          ( M.fromList
            [ ("a", LString "hello")
            ]
          , Nothing
          )
        )

expressions :: Spec
expressions = do
  describe "expressions" $ do
    vars
    recordsExpr
    functions
    cases

vars :: Spec
vars = do
  describe "variables" $ do
    it "single letter" $
      shouldBe
        (testParserNoAnn parseExpr "v")
        (pure $ EVar "v")

    it "multiple mixed letters" $
      shouldBe
        (testParserNoAnn parseExpr "vAcBkFx")
        (pure $ EVar "vAcBkFx")

    it "with numbers" $
      shouldBe
        (testParserNoAnn parseExpr "v123")
        (pure $ EVar "v123")

    it "with symbols" $
      shouldBe
        (testParserNoAnn parseExpr "v_123?")
        (pure $ EVar "v_123?")

recordsExpr :: Spec
recordsExpr = do
  describe "records" $ do
    it "one item" $
      shouldBe
        (testParserNoAnn parseExpr "{ a = f(x) }")
        ( pure $ mkERec
          [("a", EFunCall (EVar "f") [EVar "x"])]
          Nothing
        )

    it "two items" $
      shouldBe
        (testParserNoAnn parseExpr [r|{ a = f(x), b = "hello" }|])
        ( pure $ mkERec
          [ ("a", EFunCall (EVar "f") [EVar "x"])
          , ("b", ELit $ LString "hello")
          ]
          Nothing
        )

    it "extension" $
      shouldBe
        (testParserNoAnn parseExpr [r|{ a = f(x), b = "hello" | r }|])
        ( pure $ mkERec
          [ ("a", EFunCall (EVar "f") [EVar "x"])
          , ("b", ELit $ LString "hello")
          ]
          (Just $ EVar "r")
        )

    it "access" $
      shouldBe
        (testParserNoAnn parseExpr [r|record.x.y.z|])
        ( pure $
          ERecordAccess
            ( ERecordAccess
              (ERecordAccess (EVar "record") "x")
              "y"
            )
            "z"
        )


functions :: Spec
functions = do
  describe "functions" $ do
    it "f(x)" $
      shouldBe
        (testParserNoAnn parseExpr "f(x)")
        ( pure $ EFunCall (EVar "f") [EVar "x"]
        )

    it "f(x,  y,z)" $
      shouldBe
        (testParserNoAnn parseExpr "f(x,  y,z)")
        ( pure $ EFunCall (EVar "f") [EVar "x", EVar "y", EVar "z"]
        )

    it "(fun (x,y) -> add(x , y))(1, 2)" $
      shouldBe
        (testParserNoAnn parseExpr "(fun (x,y) -> add(x , y))(1, 2)")
        ( pure $
          EFunCall
            ( EFun ["x","y"]
              [SExpr $ EFunCall (EVar "add") [EVar "x", EVar "y"]]
            )
            [ELit $ LInt 1, ELit $ LInt 2]
        )


cases :: Spec
cases = do
  describe "cases of" $ do
    it "simple" $
      shouldBe
        (testParserNoAnn parseExpr "case x of | 1 -> 1 end")
        ( pure $
          ECase (EVar "x")
          [(PLit $ LInt 1, pure . SExpr $ ELit $ LInt 1)]
        )

    it "mutlicase" $
      shouldBe
        ( testParserNoAnn parseExpr
          [r|case x of
            | v -> v
            | _ -> x.y
            | Constr {} -> {}
            | Constr { a = 1 } -> 1
            end
          |]
        )
        ( pure $
          ECase (EVar "x")
          [ (PVar "v", pure . SExpr $ EVar "v")
          , (PWildcard, pure . SExpr $ ERecordAccess (EVar "x") "y")
          , ( PVariant (Variant "Constr" $ PRecord mempty)
            , pure . SExpr $ ERecord mempty
            )
          , ( PVariant
              ( Variant "Constr" $
                PRecord $ M.fromList [("a", PLit $ LInt 1)]
              )
            , pure . SExpr $ ELit (LInt 1)
            )
          ]
        )

programs :: Spec
programs = do
  describe "programs" $ do
    it "length" $
      shouldBe
        (testParserNoAnn parseFile [r|
type List a =
    | Nil {}
    | Cons { head : a, tail : List a }
end

def length(xs) := do
    case xs of
        | Nil {} -> 0
        | Cons { head = _, tail = rest } -> do
            def res := add(1, length(rest))
            res
        end
    end
end

def main() := do
    ffi("console.log", length(Cons { head = 1, tail = Cons { head = 2, tail = Nil {} } }))
end
|]
        )
        ( pure $ File
          [ TypeDef ()
              ( Datatype "List" [ "a" ]
                  [ Variant "Nil" (TypeRec [])
                  , Variant "Cons" $ TypeRec
                    [ ("head", TypeVar "a")
                    , ("tail", TypeApp (TypeCon "List") (TypeVar "a"))
                    ]
                  ]
              )
          , TermDef ()
            ( Function "length" [ "xs" ]
              [ SExpr
                ( ECase ( EVar "xs" )
                  [ ( PVariant (Variant "Nil" (PRecord mempty))
                    , [ SExpr (ELit (LInt 0)) ]
                    )
                  , ( PVariant $ Variant "Cons"
                      ( PRecord $ M.fromList
                        [ ("head", PWildcard)
                        , ("tail", PVar "rest")
                        ]
                      )
                    , [ SDef () $ Variable "res"
                        ( EFunCall ( EVar "add" )
                          [ ELit ( LInt 1 )
                          , EFunCall ( EVar "length" ) [ EVar "rest" ]
                          ]
                         )
                       , SExpr ( EVar "res" )
                       ]
                    )
                  ]
                )
              ]
            )
          , TermDef () $ Function "main" []
            [ SExpr
              ( EFfi "console.log"
                [ EFunCall ( EVar "length" )
                  [ EVariant $ Variant "Cons"
                    ( ERecord $ M.fromList
                      [ ("head", ELit (LInt 1))
                      , ("tail", EVariant $ Variant "Cons"
                          ( ERecord $ M.fromList
                            [ ("head", ELit (LInt 2))
                            , ("tail", EVariant $ Variant "Nil" (ERecord mempty))
                            ]
                          )
                        )
                      ]
                    )
                  ]
                ]
              )
            ]
          ]
        )


----------------------

testParser :: Parser a -> T.Text -> Either T.Text a
testParser p src =
  runParser p "test" src

testParserNoAnn
  :: Functor f => Data a => Data (f ())
  => Parser (f a) -> T.Text -> Either T.Text (f ())
testParserNoAnn p src =
  removeAnn' <$> testParser p src

parseLitRecord :: Parser (Record Lit, Maybe ())
parseLitRecord = parseRecord equals parseLit Nothing

