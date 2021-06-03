module Monadoc.Type.ConfigSpec where

import Monadoc.Prelude

import qualified Data.String as String
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Warning as Warning
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

    Hspec.describe "fromArguments" $ do

        Hspec.it "defaults to the initial config" $ do
            Config.fromArguments [] `Hspec.shouldBe` Just ([], Config.initial)

        Hspec.it "warns about an unexpected argument" $ do
            fmap fst (Config.fromArguments ["a"]) `Hspec.shouldBe` Just [Warning.UnexpectedArgument "a"]

        Hspec.it "warns about an unrecognized option" $ do
            fmap fst (Config.fromArguments ["-a"]) `Hspec.shouldBe` Just [Warning.UnrecognizedOption "-a"]
            fmap fst (Config.fromArguments ["--a"]) `Hspec.shouldBe` Just [Warning.UnrecognizedOption "--a"]
            fmap fst (Config.fromArguments ["--a=0"]) `Hspec.shouldBe` Just [Warning.UnrecognizedOption "--a=0"]

        Hspec.it "sets the help option" $ do
            fmap (Config.help . snd) (Config.fromArguments ["-h"]) `Hspec.shouldBe` Just True
            fmap (Config.help . snd) (Config.fromArguments ["-?"]) `Hspec.shouldBe` Just True
            fmap (Config.help . snd) (Config.fromArguments ["--help"]) `Hspec.shouldBe` Just True

        Hspec.it "rejects passing an argument to an option that doesn't accept one" $ do
            Config.fromArguments ["--help=0"] `Hspec.shouldBe` Nothing

        Hspec.it "sets the version option" $ do
            fmap (Config.version . snd) (Config.fromArguments ["--version"]) `Hspec.shouldBe` Just True

        Hspec.it "sets the host option" $ do
            fmap (Config.host . snd) (Config.fromArguments ["--host=0"]) `Hspec.shouldBe` Just (String.fromString "0")

        Hspec.it "rejects not passing an argument to an option that requires one" $ do
            Config.fromArguments ["--host"] `Hspec.shouldBe` Nothing

        Hspec.it "sets the port option" $ do
            fmap (Config.port . snd) (Config.fromArguments ["--port=0"]) `Hspec.shouldBe` Just (from @Int 0)

        Hspec.it "rejects an invalid port" $ do
            Config.fromArguments ["--port=x"] `Hspec.shouldBe` Nothing

        Hspec.it "sets the database option" $ do
            fmap (Config.database . snd) (Config.fromArguments ["--database=0"]) `Hspec.shouldBe` Just "0"
