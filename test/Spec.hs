-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Tests.Pretty as Pretty

main = do
  hspec Pretty.spec
