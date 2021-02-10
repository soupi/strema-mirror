module Tests.Unit (main, spec) where

import Test.Hspec
import Test.QuickCheck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "example" $ do
    it "test" $ do
      shouldBe () ()
    it "property" $ property $
      \() -> () === ()
