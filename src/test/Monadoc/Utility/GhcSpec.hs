module Monadoc.Utility.GhcSpec where

import qualified Control.Monad.Catch as Exception
import qualified Data.Either as Either
import qualified Data.List as List
import qualified GHC.LanguageExtensions.Type as X
import qualified Monadoc.Utility.Ghc as Ghc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

    Hspec.describe "parseModule" $ do

        Hspec.it "parses a valid module" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "module M where"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "fails to parse an invalid module" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "module invalid"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "fails without language extension set" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "x# = 1"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "respects language extension pragmas" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "{-# language MagicHash #-} x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "accepts language extensions" $ do
            result <- Ghc.parseModule Nothing [(True, X.MagicHash)] "M.hs" "x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "handles extensions being toggled" $ do
            result <- Ghc.parseModule Nothing [(False, X.MagicHash), (True, X.MagicHash)] "M.hs" "x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "lets pragmas override extensions" $ do
            result <- Ghc.parseModule Nothing [(False, X.MagicHash)] "M.hs" "{-# language MagicHash #-} x# = 1"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "does not run CPP by default" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "#define x x"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "runs CPP when requested" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "{-# language CPP #-}\n#define x x"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "keeps documentation comments" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "-- | x"
            result `Hspec.shouldSatisfy` Either.isRight
            show result `Hspec.shouldSatisfy` List.isInfixOf "<document comment>"

        Hspec.it "does not unlit by default" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "> module M where"
            result `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "handles literate Haskell" $ do
            result <- Ghc.parseModule Nothing [] "M.lhs" "> module M where"
            result `Hspec.shouldSatisfy` Either.isRight

        Hspec.it "handles implied extensions" $ do
            -- I haven't found an implied extension that causes parsing to
            -- fail. For example the @TemplateHaskell@ extension implies the
            -- @TemplateHaskellQuotes@ extension, but @x = '()@ successfully
            -- parses with no extensions enabled even though it requires
            -- @TemplateHaskellQuotes@.
            Hspec.pending

        Hspec.it "uses the requested language" $ do
            -- I haven't found anything that causes parsing to fail based on
            -- the language. I expected something like @data X@ to fail with
            -- Haskell98, but it succeeds even though empty data declarations
            -- aren't part of that language.
            Hspec.pending

    Hspec.describe "extract" $ do

        Hspec.it "handles a simple binding" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "x = ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["x"]

        Hspec.it "handles a simple type signature" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "x :: ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["x"]

        Hspec.it "handles a complex type signature" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "x, y :: ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["x", "y"]

        Hspec.it "handles multiple declarations" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "x :: ()\ny = ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["x", "y"]

        Hspec.it "ignores documentation comments" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "-- | x"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just []

        Hspec.it "handles a fixity declaration" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "infix 5 %"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["%"]

        Hspec.it "ignores duplicates" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "x = ()\nx = ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["x"]

        Hspec.it "handles an inline pragma" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "{-# inline x #-}"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["x"]

        Hspec.it "ignores default declarations" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "default ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just []

        Hspec.it "handles a data declaration" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "data X"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["data X"]

        Hspec.it "handles an instance declaration" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "instance X Y"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["instance X Y"]

        Hspec.it "handles a type class declaration" $ do
            result <- Ghc.parseModule Nothing [] "M.hs" "class X"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["class X"]

        Hspec.it "handles a pattern signature" $ do
            result <- Ghc.parseModule Nothing [(True, X.PatternSynonyms)] "M.hs" "pattern X :: ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["pattern X"]

        Hspec.it "handles a unidirectional pattern synonym" $ do
            result <- Ghc.parseModule Nothing [(True, X.PatternSynonyms)] "M.hs" "pattern X <- ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["pattern X"]

        Hspec.it "handles a bidirectional pattern synonym" $ do
            result <- Ghc.parseModule Nothing [(True, X.PatternSynonyms)] "M.hs" "pattern X = ()"
            hsModule <- either Exception.throwM pure result
            Ghc.extract hsModule `Hspec.shouldBe` Just ["pattern X"]
