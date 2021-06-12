module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Fixed as Fixed
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Distribution.Compiler as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Types.Benchmark as Cabal
import qualified Distribution.Types.Component as Cabal
import qualified Distribution.Types.ComponentName as Cabal
import qualified Distribution.Types.ComponentRequestedSpec as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.Executable as Cabal
import qualified Distribution.Types.Flag as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Types.LibraryName as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Types.TestSuite as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Exception.BadHackageIndexSize as BadHackageIndexSize
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.MissingHackageIndex as MissingHackageIndex
import qualified Monadoc.Exception.UnexpectedTarEntry as UnexpectedTarEntry
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.CabalVersion as CabalVersion
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.License as License
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Sha256 as Sha256
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
        void $ HackageIndex.insert connection hackageIndex

updateHackageIndex :: Context.Context -> Model.Model HackageIndex.HackageIndex -> IO ()
updateHackageIndex context model = do
    let
        oldHackageIndex = Model.value model
        oldSize = HackageIndex.size oldHackageIndex
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
                    HackageIndex.update connection (Model.key model) newHackageIndex

processHackageIndex :: Context.Context -> IO ()
processHackageIndex context = do
    Log.info "processing Hackage index"
    maybeHackageIndex <- Pool.withResource (Context.pool context) HackageIndex.select
    hackageIndex <- maybe (throwM MissingHackageIndex.new) (pure . Model.value) maybeHackageIndex
    revisionsVar <- Stm.newTVarIO Map.empty
    preferredVersionsVar <- Stm.newTVarIO Map.empty
    hackageIndex
        & HackageIndex.contents
        & into @LazyByteString
        & Tar.read
        & Tar.foldEntries (:) [] (Unsafe.unsafePerformIO . throwM)
        & traverse_ (processTarEntry context revisionsVar preferredVersionsVar)
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
processTarEntry
    :: Context.Context
    -> Stm.TVar (Map (PackageName.PackageName, Version.Version) Revision.Revision)
    -> Stm.TVar (Map PackageName.PackageName VersionRange.VersionRange)
    -> Tar.Entry
    -> IO ()
processTarEntry context revisionsVar preferredVersionsVar entry = do
    unless (isValidTarEntry entry) . throwM $ UnexpectedTarEntry.new entry
    contents <- case Tar.entryContent entry of
        Tar.NormalFile x _ -> pure $ into @ByteString x
        _ -> throwM $ UnexpectedTarEntry.new entry
    case getTarEntryPath entry of
        ([rawPackageName, "preferred-versions"], "") ->
            processPreferredVersions preferredVersionsVar rawPackageName contents
        ([rawPackageName, rawVersion, otherRawPackageName], ".cabal") ->
            processPackageDescription context revisionsVar entry rawPackageName rawVersion otherRawPackageName contents
        ([_, _, "package"], ".json") -> pure ()
        _ -> throwM $ UnexpectedTarEntry.new entry

processPreferredVersions
    :: Stm.TVar (Map PackageName.PackageName VersionRange.VersionRange)
    -> String
    -> ByteString
    -> IO ()
processPreferredVersions preferredVersionsVar rawPackageName contents = do
    packageName <- either throwM pure $ tryInto @PackageName.PackageName rawPackageName
    versionRange <- if ByteString.null contents
        then pure VersionRange.any
        else case Cabal.simpleParsecBS contents of
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

processPackageDescription
    :: Context.Context
    -> Stm.TVar (Map (PackageName.PackageName, Version.Version) Revision.Revision)
    -> Tar.Entry
    -> String
    -> String
    -> String
    -> ByteString
    -> IO ()
processPackageDescription context revisionsVar entry rawPackageName rawVersion otherRawPackageName contents = do
    when (otherRawPackageName /= rawPackageName)
        . throwM
        $ Mismatch.new rawPackageName otherRawPackageName
    packageName <- either throwM pure $ tryInto @PackageName.PackageName rawPackageName
    version <- either throwM pure $ tryInto @Version.Version rawVersion
    revision <- Stm.atomically . Stm.stateTVar revisionsVar $ \ revisions ->
        let
            key = (packageName, version)
            revision = Map.findWithDefault Revision.zero key revisions
            newRevisions = Map.insert key (Revision.increment revision) revisions
        in (revision, newRevisions)
    maybePackage <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.select connection packageName version revision
    let hash = Sha256.hash contents
    when (fmap (Package.hash . Model.value) maybePackage /= Just hash) $ do
        Log.info
            $ into @String hash
            <> " "
            <> into @String packageName
            <> " "
            <> into @String version
            <> " "
            <> show (into @Word revision)
        case Cabal.parseGenericPackageDescriptionMaybe contents of
            Nothing -> throwM $ TryFromException @_ @Cabal.GenericPackageDescription contents Nothing
            Just gpd -> do
                pd <- case toPackageDescription gpd of
                    Left dependencies -> throwM $ userError $ "invalid package description: " <> show dependencies
                    Right (pd, _) -> pure pd
                let
                    otherPackageName = pd
                        & Cabal.package
                        & Cabal.pkgName
                        & into @PackageName.PackageName
                    otherVersion = pd
                        & Cabal.package
                        & Cabal.pkgVersion
                        & into @Version.Version
                when (otherPackageName /= packageName)
                    . throwM
                    $ Mismatch.new packageName otherPackageName
                when (otherVersion /= version)
                    . throwM
                    $ Mismatch.new version otherVersion
                hackageUser <- do
                    let
                        ownership = Tar.entryOwnership entry
                        name = Tar.ownerName ownership
                        id = Tar.ownerId ownership
                        value = HackageUser.HackageUser { HackageUser.id, HackageUser.name }
                    maybeModel <- Pool.withResource (Context.pool context) $ \ connection ->
                        HackageUser.selectByName connection name
                    case maybeModel of
                        Just model -> pure model
                        Nothing -> do
                            key <- Pool.withResource (Context.pool context) $ \ connection ->
                                HackageUser.insert connection value
                            pure Model.Model { Model.key, Model.value }
                let
                    package = Package.Package
                        { Package.author = into @Text $ Cabal.author pd
                        , Package.bugReports = into @Text $ Cabal.bugReports pd
                        , Package.buildType = into @BuildType.BuildType $ Cabal.buildType pd
                        , Package.cabalVersion = into @CabalVersion.CabalVersion $ Cabal.specVersion pd
                        , Package.category = into @Text $ Cabal.category pd
                        , Package.contents
                        , Package.copyright = into @Text $ Cabal.copyright pd
                        , Package.description = into @Text $ Cabal.description pd
                        , Package.hash
                        , Package.homepage = into @Text $ Cabal.homepage pd
                        , Package.license = into @License.License $ Cabal.license pd
                        , Package.maintainer = into @Text $ Cabal.maintainer pd
                        , Package.name = packageName
                        , Package.pkgUrl = into @Text $ Cabal.pkgUrl pd
                        , Package.revision
                        , Package.stability = into @Text $ Cabal.stability pd
                        , Package.synopsis = into @Text $ Cabal.synopsis pd
                        , Package.uploadedAt = epochTimeToUtcTime $ Tar.entryTime entry
                        , Package.uploadedBy = Model.key hackageUser
                        , Package.version
                        }
                key <- Pool.withResource (Context.pool context) $ \ connection ->
                    Package.insertOrUpdate connection package
                pd -- TODO
                    & Cabal.pkgComponents
                    & traverse_ (\ component -> do
                        let
                            tag = case component of
                                Cabal.CLib _ -> ComponentTag.Library
                                Cabal.CFLib _ -> ComponentTag.ForeignLibrary
                                Cabal.CExe _ -> ComponentTag.Executable
                                Cabal.CTest _ -> ComponentTag.TestSuite
                                Cabal.CBench _ -> ComponentTag.Benchmark
                            name = maybe (into @String packageName) Cabal.unUnqualComponentName
                                . Cabal.componentNameString
                                $ Cabal.componentName component
                        maybeComponent <- Pool.withResource (Context.pool context) $ \ connection ->
                            Component.select connection key tag name
                        case maybeComponent of
                            Just _ -> pure ()
                            Nothing -> Pool.withResource (Context.pool context) $ \ connection ->
                                void $ Component.insert connection Component.Component
                                    { Component.name
                                    , Component.package = key
                                    , Component.tag
                                    })

epochTimeToUtcTime :: Tar.EpochTime -> Time.UTCTime
epochTimeToUtcTime = into @Time.UTCTime
    . into @Time.POSIXTime
    . into @Fixed.Pico
    . (* 1000000000000)
    . into @Integer

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

-- | Although the generic package description type does have a package
-- description in it, that nested PD isn't actually usable. This function is
-- necessary in order to choose the platform, compiler, flags, and other stuff.
toPackageDescription
    :: Cabal.GenericPackageDescription
    -> Either [Cabal.Dependency] (Cabal.PackageDescription, Cabal.FlagAssignment)
toPackageDescription = Cabal.finalizePD
    (Cabal.mkFlagAssignment [])
    Cabal.defaultComponentRequestedSpec
    (always True)
    (Cabal.Platform Cabal.X86_64 Cabal.Linux)
    (Cabal.unknownCompilerInfo (Cabal.CompilerId Cabal.GHC (Cabal.mkVersion [9, 0, 1])) Cabal.NoAbiTag)
    []

componentName :: Cabal.Component -> String
componentName component = case component of
    Cabal.CLib library -> case Cabal.libName library of
        Cabal.LMainLibName -> "lib"
        Cabal.LSubLibName unqualComponentName -> "lib:" <> Cabal.unUnqualComponentName unqualComponentName
    -- Only a handful of libraries have foreign libraries: hexchat, HABQT,
    -- perceptual-hash, and toysolver.
    Cabal.CFLib foreignLib -> "flib:" <> Cabal.unUnqualComponentName (Cabal.foreignLibName foreignLib)
    Cabal.CExe executable -> "exe:" <> Cabal.unUnqualComponentName (Cabal.exeName executable)
    Cabal.CTest testSuite -> "test:" <> Cabal.unUnqualComponentName (Cabal.testName testSuite)
    Cabal.CBench benchmark -> "bench:" <> Cabal.unUnqualComponentName (Cabal.benchmarkName benchmark)
