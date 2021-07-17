module Monadoc.Utility.GhcSpec where

import qualified Data.Either as Either
import qualified Monadoc.Utility.Ghc as Ghc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

    Hspec.describe "parseModule" $ do

        Hspec.it "parses a valid module" $ do
            Ghc.parseModule "M.hs" "module M where" `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "fails to parse an invalid module" $ do
            Ghc.parseModule "M.hs" "module invalid" `Hspec.shouldSatisfy` Either.isLeft
