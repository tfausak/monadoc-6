module Monadoc.Utility.GhcSpec where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified GHC.LanguageExtensions.Type as X
import qualified Monadoc.Utility.Ghc as Ghc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

    Hspec.describe "parseModule" $ do

        Hspec.it "parses a valid module" $ do
            result <- Ghc.parseModule [] "M.hs" "module M where"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "fails to parse an invalid module" $ do
            result <- Ghc.parseModule [] "M.hs" "module invalid"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "fails without language extension set" $ do
            result <- Ghc.parseModule [] "M.hs" "x# = 1"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "respects language extension pragmas" $ do
            result <- Ghc.parseModule [] "M.hs" "{-# language MagicHash #-} x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "accepts language extensions" $ do
            result <- Ghc.parseModule [(True, X.MagicHash)] "M.hs" "x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "handles extensions being toggled" $ do
            result <- Ghc.parseModule [(False, X.MagicHash), (True, X.MagicHash)] "M.hs" "x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "lets pragmas override extensions" $ do
            result <- Ghc.parseModule [(False, X.MagicHash)] "M.hs" "{-# language MagicHash #-} x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "does not run CPP by default" $ do
            result <- Ghc.parseModule [] "M.hs" "#define x x"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "runs CPP when requested" $ do
            result <- Ghc.parseModule [] "M.hs" "{-# language CPP #-}\n#define x x"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "keeps documentation comments" $ do
            result <- Ghc.parseModule [] "M.hs" "-- | x"
            result `Hspec.shouldSatisfy` Either.isRight
            show result `Hspec.shouldSatisfy` List.isInfixOf "<document comment>"

        Hspec.it "does not unlit by default" $ do
            result <- Ghc.parseModule [] "M.hs" "> module M where"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "handles literate Haskell" $ do
            result <- Ghc.parseModule [] "M.lhs" "> module M where"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "handles implied extensions" $ do
            -- I haven't found an implied extension that causes parsing to
            -- fail. For example the @TemplateHaskell@ extension implies the
            -- @TemplateHaskellQuotes@ extension, but @x = '()@ successfully
            -- parses with no extensions enabled even though it requires
            -- @TemplateHaskellQuotes@.
            Hspec.pending
