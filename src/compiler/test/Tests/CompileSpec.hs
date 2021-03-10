{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Tests.CompileSpec where

import Language.Strema.Compiler.Compile

import Test.Hspec
import Text.RawString.QQ
import qualified Data.Text as T

import System.Process (readProcess)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = parallel $ do
  describe "compilation" $ do
    programs


programs :: Spec
programs = do
  describe "programs" $ do
    it "length" $
      check $ Check
        { program = [r|
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
        , expected = "2"
        }

    it "const" $
      check $ Check
        { program = [r|
def const(a, b) := a

def main() := do
    ffi("console.log", const(1, 2))
end
|]
        , expected = "1"
        }

    it "bools" $
      check $ Check
        { program = [r|
def hello(n) :=
	case int_equals(n, 0) of
		 | True {} -> "Hi!"
		 | False {} -> "Hello!"
	end

def main() := ffi("console.log", hello(1))

|]
        , expected = "Hello!"
        }


data Check
  = Check
  { program :: T.Text
  , expected :: String
  }
        
check :: Check -> IO ()
check (Check prog expect) = do
  prog' <- either (error . T.unpack) pure $ compile "test" prog
  result <- readProcess "nodejs" [] (T.unpack prog')
  shouldBe result (expect <> "\n")
