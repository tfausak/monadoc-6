module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Monadoc.Exception.BadHackageIndexSize as BadHackageIndexSize
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.UnexpectedTarEntry as UnexpectedTarEntry
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified System.FilePath as FilePath
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Read as Read

run :: Context.Context -> IO ()
run context = do
    Log.info "starting worker"
    Monad.forever $ do
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
    request <- Client.parseUrlThrow $ Config.hackageUrl (Context.config context) <> "/01-index.tar.gz"
    response <- Client.httpLbs request $ Context.manager context
    let
        contents = Client.responseBody response
            & Gzip.decompress
            & into @ByteString
        size = ByteString.length contents
        hackageIndex = HackageIndex.HackageIndex { HackageIndex.contents, HackageIndex.size }
    Log.info $ "got initial Hackage index (size: " <> pluralize "byte" size <> ")"
    Pool.withResource (Context.pool context) $ \ connection ->
        HackageIndex.insert connection hackageIndex

updateHackageIndex :: Context.Context -> HackageIndex.HackageIndex -> IO ()
updateHackageIndex context oldHackageIndex = do
    let oldSize = HackageIndex.size oldHackageIndex
    Log.info $ "requesting new Hackage index size (old size: " <> pluralize "byte" oldSize <> ")"
    request <- Client.parseUrlThrow $ Config.hackageUrl (Context.config context) <> "/01-index.tar"
    headResponse <- Client.httpNoBody
        request { Client.method = into @ByteString "HEAD" }
        $ Context.manager context
    let
        maybeNewSize = do
            x <- lookup Http.hContentLength $ Client.responseHeaders headResponse
            y <- hush $ tryInto @String x
            Read.readMaybe @Int y
    case maybeNewSize of
        Nothing -> throwM $ BadHackageIndexSize.new oldSize maybeNewSize
        Just newSize
            | newSize < oldSize -> throwM $ BadHackageIndexSize.new oldSize maybeNewSize
            | newSize == oldSize -> Log.info "Hackage index has not changed"
            | otherwise -> do
                Log.info $ "got new Hackage index size: " <> pluralize "byte" newSize
                let
                    delta = newSize - oldSize
                    start = oldSize - HackageIndex.offset
                    end = newSize - 1
                    range = into @ByteString $ "bytes=" <> show start <> "-" <> show end
                Log.info $ "requesting " <> pluralize "byte" delta <> " of new Hackage index"
                rangeResponse <- Client.httpLbs
                    request { Client.requestHeaders = (Http.hRange, range) : Client.requestHeaders request }
                    $ Context.manager context
                Log.info "got new Hackage index"
                let
                    before = ByteString.take start $ HackageIndex.contents oldHackageIndex
                    after = into @ByteString $ Client.responseBody rangeResponse
                    contents = before <> after
                    newHackageIndex = HackageIndex.fromByteString contents
                Pool.withResource (Context.pool context) $ \ connection ->
                    HackageIndex.update connection newHackageIndex

processHackageIndex :: Context.Context -> IO ()
processHackageIndex context = do
    Log.info "processing Hackage index"
    maybeHackageIndex <- Pool.withResource (Context.pool context) HackageIndex.select
    case maybeHackageIndex of
        Nothing -> Log.warn "missing Hackage index"
        Just hackageIndex -> do
            preferredVersionsVar <- Stm.newTVarIO Map.empty
            hackageIndex
                & HackageIndex.contents
                & into @LazyByteString
                & Tar.read
                & Tar.foldEntries (:) [] (Unsafe.unsafePerformIO . throwM)
                & traverse_ (processTarEntry context preferredVersionsVar)
            preferredVersions <- Stm.atomically $ Stm.readTVar preferredVersionsVar
            preferredVersions
                & Map.toAscList
                & fmap (uncurry PreferredVersions.new)
                & traverse_ (\ pv -> Pool.withResource (Context.pool context) $ \ connection ->
                    PreferredVersions.upsert connection pv)

-- Possible Hackage index tar entry paths:
--
-- - PKG_NAME/PKG_VERSION/PKG_NAME.cabal
-- - PKG_NAME/preferred-versions
-- - PKG_NAME/PKD_VERSION/package.json
--
-- Note that on Windows, tar entries have Windows-style paths:
--
-- - Windows: base\4.15.0.0\base.cabal
-- - Unix: base/4.15.0.0/base.cabal
processTarEntry :: Context.Context -> Stm.TVar (Map PackageName.PackageName VersionRange.VersionRange) -> Tar.Entry -> IO ()
processTarEntry _context preferredVersionsVar entry = do
    when (not $ isValidTarEntry entry) . throwM $ UnexpectedTarEntry.new entry
    contents <- case Tar.entryContent entry of
        Tar.NormalFile x _ -> pure x
        _ -> throwM $ UnexpectedTarEntry.new entry
    case getTarEntryPath entry of
        ([rawPackageName, "preferred-versions"], "") -> do
            packageName <- either throwM pure $ tryInto @PackageName.PackageName rawPackageName
            versionRange <- if LazyByteString.null contents
                then pure VersionRange.any
                else case Cabal.simpleParsecBS $ into @ByteString contents of
                    Nothing -> throwM $ TryFromException @_ @Cabal.PackageVersionConstraint contents Nothing
                    Just (Cabal.PackageVersionConstraint otherPackageName versionRange) -> do
                        when (otherPackageName /= into @Cabal.PackageName packageName)
                            . throwM
                            . Mismatch.new packageName
                            $ into @PackageName.PackageName otherPackageName
                        pure $ into @VersionRange.VersionRange versionRange
            Stm.atomically
                . Stm.modifyTVar preferredVersionsVar
                $ Map.insert packageName versionRange
        ([rawPackageName, rawVersion, _], ".cabal") -> do
            packageName <- either throwM pure $ tryInto @PackageName.PackageName rawPackageName
            version <- either throwM pure $ tryInto @Version.Version rawVersion
            -- TODO: don't re-parse unchanged package descriptions
            case Cabal.parseGenericPackageDescriptionMaybe $ into @ByteString contents of
                Nothing -> throwM $ TryFromException @_ @Cabal.GenericPackageDescription contents Nothing
                Just gpd -> do
                    let
                        otherPackageName = gpd
                            & Cabal.packageDescription
                            & Cabal.package
                            & Cabal.pkgName
                            & into @PackageName.PackageName
                        otherVersion = gpd
                            & Cabal.packageDescription
                            & Cabal.package
                            & Cabal.pkgVersion
                            & into @Version.Version
                    when (otherPackageName /= packageName)
                        . throwM
                        $ Mismatch.new packageName otherPackageName
                    when (otherVersion /= version)
                        . throwM
                        $ Mismatch.new version otherVersion
                    pure () -- TODO: do something with the parsed package description
        ([_, _, "package"], ".json") -> pure ()
        _ -> throwM $ UnexpectedTarEntry.new entry

isValidTarEntry :: Tar.Entry -> Bool
isValidTarEntry entry = Tar.entryPermissions entry == 420
    && Tar.groupName (Tar.entryOwnership entry) == "Hackage"
    && Tar.groupId (Tar.entryOwnership entry) == 0
    && Tar.entryFormat entry == Tar.UstarFormat

pluralize :: String -> Int -> String
pluralize word count = show count <> " " <> word <> if count == 1 then "" else "s"

getTarEntryPath :: Tar.Entry -> ([FilePath], String)
getTarEntryPath entry =
    let (prefix, suffix) = FilePath.splitExtensions $ Tar.entryPath entry
    in (FilePath.splitDirectories prefix, suffix)
