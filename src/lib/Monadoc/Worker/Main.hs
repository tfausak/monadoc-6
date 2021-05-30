module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Pool as Pool
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Monadoc.Exception.BadHackageIndexSize as BadHackageIndexSize
import qualified Monadoc.Exception.InvalidPackageDescription as InvalidPackageDescription
import qualified Monadoc.Exception.InvalidPackageVersionConstraint as InvalidPackageVersionConstraint
import qualified Monadoc.Exception.UnexpectedTarEntry as UnexpectedTarEntry
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Read as Read

run :: Context.Context -> IO ()
run context = do
    Log.info "starting worker"
    Monad.forever <| do
        Log.info "beginning worker loop"
        upsertHackageIndex context
        processHackageIndex context
        Log.info "finished worker loop"
        Concurrent.threadDelay 60000000

upsertHackageIndex :: Context.Context -> IO ()
upsertHackageIndex context = do
    Log.info "refreshing Hackage index"
    maybeHackageIndex <- Pool.withResource (Context.pool context) HackageIndex.select
    case maybeHackageIndex of
        Nothing -> insertHackageIndex context
        Just hackageIndex -> updateHackageIndex context hackageIndex

insertHackageIndex :: Context.Context -> IO ()
insertHackageIndex context = do
    Log.info "requesting initial Hackage index"
    request <- Client.parseUrlThrow <| Config.hackageUrl (Context.config context) <> "/01-index.tar.gz"
    response <- Client.httpLbs request <| Context.manager context
    let
        contents = Client.responseBody response
            |> Gzip.decompress
            |> into @ByteString
        size = ByteString.length contents
        hackageIndex = HackageIndex.HackageIndex { HackageIndex.contents, HackageIndex.size }
    Log.info <| "got initial Hackage index (size: " <> pluralize "byte" size <> ")"
    Pool.withResource (Context.pool context) <| \ connection ->
        HackageIndex.insert connection hackageIndex

updateHackageIndex :: Context.Context -> HackageIndex.HackageIndex -> IO ()
updateHackageIndex context oldHackageIndex = do
    let oldSize = HackageIndex.size oldHackageIndex
    Log.info <| "requesting new Hackage index size (old size: " <> pluralize "byte" oldSize <> ")"
    request <- Client.parseUrlThrow <| Config.hackageUrl (Context.config context) <> "/01-index.tar"
    headResponse <- Client.httpNoBody
        request { Client.method = into @ByteString "HEAD" }
        <| Context.manager context
    let
        maybeNewSize = do
            x <- lookup Http.hContentLength <| Client.responseHeaders headResponse
            y <- hush <| tryInto @String x
            Read.readMaybe @Int y
    case maybeNewSize of
        Nothing -> throwM <| BadHackageIndexSize.new oldSize maybeNewSize
        Just newSize
            | newSize < oldSize -> throwM <| BadHackageIndexSize.new oldSize maybeNewSize
            | newSize == oldSize -> Log.info "Hackage index has not changed"
            | otherwise -> do
                Log.info <| "got new Hackage index size: " <> pluralize "byte" newSize
                let
                    delta = newSize - oldSize
                    start = oldSize - HackageIndex.offset
                    end = newSize - 1
                    range = into @ByteString <| "bytes=" <> show start <> "-" <> show end
                Log.info <| "requesting " <> pluralize "byte" delta <> " of new Hackage index"
                rangeResponse <- Client.httpLbs
                    request { Client.requestHeaders = (Http.hRange, range) : Client.requestHeaders request }
                    <| Context.manager context
                Log.info "got new Hackage index"
                let
                    before = ByteString.take start <| HackageIndex.contents oldHackageIndex
                    after = into @ByteString <| Client.responseBody rangeResponse
                    contents = before <> after
                    newHackageIndex = HackageIndex.fromByteString contents
                Pool.withResource (Context.pool context) <| \ connection ->
                    HackageIndex.update connection newHackageIndex

processHackageIndex :: Context.Context -> IO ()
processHackageIndex context = do
    Log.info "processing Hackage index"
    maybeHackageIndex <- Pool.withResource (Context.pool context) HackageIndex.select
    case maybeHackageIndex of
        Nothing -> Log.warn "missing Hackage index"
        Just hackageIndex -> hackageIndex
            |> HackageIndex.contents
            |> into @LazyByteString
            |> Tar.read
            |> Tar.foldEntries (:) [] (Unsafe.unsafePerformIO <. throwM)
            |> traverse_ (processTarEntry context)

-- Possible Hackage index tar entry paths:
--
-- - PKG_NAME/PKG_VERSION/PKG_NAME.cabal
-- - PKG_NAME/preferred-versions
-- - PKG_NAME/PKD_VERSION/package.json
processTarEntry :: Context.Context -> Tar.Entry -> IO ()
processTarEntry _context entry = case Tar.entryContent entry of
    Tar.NormalFile contents _ | isValidTarEntry entry -> do
        IO.appendFile "tar-entries.txt" <| Tar.entryPath entry <> "\n"
        case FilePath.takeExtension <| Tar.entryPath entry of
            "" ->
                if LazyByteString.null contents
                    then pure () -- TODO: any version
                    else case Cabal.simpleParsecBS <| into @ByteString contents of
                        Nothing -> throwM <| InvalidPackageVersionConstraint.new contents
                        Just (Cabal.PackageVersionConstraint _ _) -> pure () -- TODO: do something with parsed version constraint
            ".cabal" ->
                -- TODO: don't re-parse unchanged package descriptions
                case Cabal.parseGenericPackageDescriptionMaybe <| into @ByteString contents of
                    Nothing -> throwM <| InvalidPackageDescription.new contents
                    Just _ -> pure () -- TODO: do something with the parsed package description
            ".json" -> pure ()
            _ -> throwM <| UnexpectedTarEntry.new entry
    _ -> throwM <| UnexpectedTarEntry.new entry

isValidTarEntry :: Tar.Entry -> Bool
isValidTarEntry entry = Tar.entryPermissions entry == 420
    && Tar.groupName (Tar.entryOwnership entry) == "Hackage"
    && Tar.groupId (Tar.entryOwnership entry) == 0
    && Tar.entryFormat entry == Tar.UstarFormat

pluralize :: String -> Int -> String
pluralize word count = show count <> " " <> word <> if count == 1 then "" else "s"
