module Monadoc.Job.ProcessHackageIndex where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Control.Concurrent.STM as Stm
import qualified Data.ByteString as ByteString
import qualified Data.Fixed as Fixed
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Compiler as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Types.BuildInfo.Lens as Cabal
import qualified Distribution.Types.Component as Cabal
import qualified Distribution.Types.ComponentName as Cabal
import qualified Distribution.Types.ComponentRequestedSpec as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.Flag as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.UnexpectedTarEntry as UnexpectedTarEntry
import qualified Monadoc.Job.UpdateLatestVersions as UpdateLatestVersions
import qualified Monadoc.Job.UpdatePreferredVersions as UpdatePreferredVersions
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.SourceRepository as SourceRepository
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.CabalVersion as CabalVersion
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.HackageId as HackageId
import qualified Monadoc.Type.HackageName as HackageName
import qualified Monadoc.Type.License as License
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Utility.Foldable as Foldable
import qualified Monadoc.Utility.Log as Log
import qualified System.FilePath as FilePath

run :: Context.Context -> HackageIndex.HackageIndex -> IO ()
run context hackageIndex = do
    Log.info "[worker] processing hackage index"
    revisionsVar <- Stm.newTVarIO Map.empty
    preferredVersionsVar <- Stm.newTVarIO Map.empty
    hashes <- Context.withConnection context Package.selectHashes
    hackageIndex
        & HackageIndex.contents
        & into @LazyByteString
        & Tar.read
        & Tar.foldEntries ((:) . Right) [] (pure . Left)
        & traverse_ (processTarItem context revisionsVar preferredVersionsVar hashes)
    UpdatePreferredVersions.run context preferredVersionsVar
    UpdateLatestVersions.run context revisionsVar preferredVersionsVar

processTarItem
    :: Context.Context
    -> Stm.TVar (Map (PackageName.PackageName, Version.Version) Revision.Revision)
    -> Stm.TVar (Map PackageName.PackageName VersionRange.VersionRange)
    -> Map (PackageName.PackageName, Version.Version, Revision.Revision) Sha256.Sha256
    -> Either Tar.FormatError Tar.Entry
    -> IO ()
processTarItem context revisionsVar preferredVersionsVar hashes item =
    case item of
        Left formatError -> throwM formatError
        Right entry -> processTarEntry context revisionsVar preferredVersionsVar hashes entry

-- Possible Hackage index tar entry paths:
--
-- - PKG_NAME/PKG_VERSION/PKG_NAME.cabal
-- - PKG_NAME/preferred-versions
-- - PKG_NAME/PKD_VERSION/package.json
processTarEntry
    :: Context.Context
    -> Stm.TVar (Map (PackageName.PackageName, Version.Version) Revision.Revision)
    -> Stm.TVar (Map PackageName.PackageName VersionRange.VersionRange)
    -> Map (PackageName.PackageName, Version.Version, Revision.Revision) Sha256.Sha256
    -> Tar.Entry
    -> IO ()
processTarEntry context revisionsVar preferredVersionsVar hashes entry = do
    unless (isValidTarEntry entry) . throwM $ UnexpectedTarEntry.new entry
    contents <- case Tar.entryContent entry of
        Tar.NormalFile x _ -> pure $ into @ByteString x
        _ -> throwM $ UnexpectedTarEntry.new entry
    case getTarEntryPath entry of
        ([rawPackageName, "preferred-versions"], "") ->
            processPreferredVersions preferredVersionsVar rawPackageName contents
        ([rawPackageName, rawVersion, otherRawPackageName], ".cabal") ->
            processPackageDescription context revisionsVar hashes entry rawPackageName rawVersion otherRawPackageName contents
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
    -> Map (PackageName.PackageName, Version.Version, Revision.Revision) Sha256.Sha256
    -> Tar.Entry
    -> String
    -> String
    -> String
    -> ByteString
    -> IO ()
processPackageDescription context revisionsVar hashes entry rawPackageName rawVersion otherRawPackageName contents = do
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
    let maybeHash = Map.lookup (packageName, version, revision) hashes
    let hash = Sha256.hash contents
    -- TODO: Is it possible to delay processing package descriptions until
    -- after distributions have been fetched? That way they only need to be
    -- walked over once.
    when (maybeHash /= Just hash) $ do
        Log.info
            $ "[worker] "
            <> into @String packageName
            <> " "
            <> into @String version
            <> " "
            <> show (into @Word revision)
            <> " "
            <> into @String hash
        pd <- either throwM pure $ parsePackageDescription contents
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
                name = into @HackageName.HackageName $ Tar.ownerName ownership
                id = into @HackageId.HackageId $ Tar.ownerId ownership
                value = HackageUser.HackageUser { HackageUser.id, HackageUser.name }
            maybeModel <- Context.withConnection context $ \ connection ->
                HackageUser.selectByName connection name
            case maybeModel of
                Just model -> pure model
                Nothing -> do
                    key <- Context.withConnection context $ \ connection ->
                        HackageUser.insert connection value
                    pure Model.Model { Model.key, Model.value }
        let
            package = Package.Package
                { Package.author = into @Text $ Cabal.author pd
                , Package.bugReports = into @Text $ Cabal.bugReports pd
                , Package.buildType = into @BuildType.BuildType $ Cabal.buildType pd
                , Package.cabalVersion = into @CabalVersion.CabalVersion $ Cabal.specVersion pd
                , Package.category = into @Text $ Cabal.category pd
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
        key <- Context.withConnection context $ \ connection -> do
            Blob.upsert connection $ Blob.fromByteString contents
            Package.insertOrUpdate connection package
        syncSourceRepositories context key $ Cabal.sourceRepos pd
        pd
            & Cabal.pkgComponents
            & traverse_ (\ component -> do
                let
                    tag = case component of
                        Cabal.CLib _ -> ComponentTag.Library
                        Cabal.CFLib _ -> ComponentTag.ForeignLibrary
                        Cabal.CExe _ -> ComponentTag.Executable
                        Cabal.CTest _ -> ComponentTag.TestSuite
                        Cabal.CBench _ -> ComponentTag.Benchmark
                    name = maybe (into @ComponentName.ComponentName packageName) (into @ComponentName.ComponentName)
                        . Cabal.componentNameString
                        $ Cabal.componentName component
                maybeComponent <- Context.withConnection context $ \ connection ->
                    Component.select connection key tag name
                componentKey <- case maybeComponent of
                    Just model -> pure $ Model.key model
                    Nothing -> Context.withConnection context $ \ connection ->
                        Component.insert connection Component.Component
                            { Component.name
                            , Component.package = key
                            , Component.tag
                            }
                syncModules context componentKey component
                syncDependencies context componentKey component)

syncModules :: Context.Context -> Component.Key -> Cabal.Component -> IO ()
syncModules context componentKey component = case component of
    Cabal.CLib library -> Context.withConnection context $ \ connection -> do
        oldModules <- Module.selectByComponent connection componentKey
        let
            newModuleNames = library
                & Cabal.exposedModules
                & fmap (into @ModuleName.ModuleName)
                & Set.fromList
            oldModuleNames = oldModules
                & fmap (Module.name . Model.value)
                & Set.fromList
            shouldDelete x = Set.notMember (Module.name $ Model.value x) newModuleNames
            shouldUpsert x = Set.notMember x oldModuleNames
        oldModules
            & filter shouldDelete
            & traverse_ (Module.delete connection . Model.key)
        newModuleNames
            & Set.filter shouldUpsert
            & traverse_ (\ moduleName -> Module.upsert connection Module.Module
                { Module.component = componentKey
                , Module.name = moduleName
                , Module.file = Nothing
                })
    _ -> pure ()

syncDependencies :: Context.Context -> Component.Key -> Cabal.Component -> IO ()
syncDependencies context componentKey component = Context.withConnection context $ \ connection -> do
    old <- Dependency.selectByComponent connection componentKey
    let
        toKey x = (Dependency.packageName x, Dependency.libraryName x)
        toMap :: [Dependency.Dependency] -> Map (PackageName.PackageName, ComponentName.ComponentName) VersionRange.VersionRange
        toMap = fmap (VersionRange.unions . NonEmpty.toList . fmap Dependency.versionRange) . Foldable.groupBy toKey
        oldMap = toMap $ fmap Model.value old
        new = foldMap (Dependency.fromDependency componentKey) $ Lens.view Cabal.targetBuildDepends component
        newMap = toMap new
        shouldDelete x = Map.notMember (toKey $ Model.value x) newMap
        toDelete = fmap Model.key $ filter shouldDelete old
        shouldInsert x = Map.notMember (toKey x) oldMap
        toInsert = filter shouldInsert new
    traverse_ (Dependency.delete connection) toDelete
    traverse_ (Dependency.insert connection) toInsert

syncSourceRepositories
    :: Context.Context
    -> Package.Key
    -> [Cabal.SourceRepo]
    -> IO ()
syncSourceRepositories context packageKey sourceRepos = Context.withConnection context $ \ connection -> do
    old <- SourceRepository.selectByPackage connection packageKey
    let
        oldSet = Set.fromList $ fmap Model.value old
        new = fmap (SourceRepository.fromSourceRepo packageKey) sourceRepos
        newSet = Set.fromList new
        shouldDelete x = Set.notMember (Model.value x) newSet
        toDelete = fmap Model.key $ filter shouldDelete old
        shouldInsert x = Set.notMember x oldSet
        toInsert = filter shouldInsert new
    traverse_ (SourceRepository.delete connection) toDelete
    traverse_ (SourceRepository.insert connection) toInsert

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

getTarEntryPath :: Tar.Entry -> ([FilePath], String)
getTarEntryPath entry =
    let (prefix, suffix) = FilePath.splitExtensions $ Tar.entryPath entry
    in (FilePath.splitDirectories prefix, suffix)

parsePackageDescription
    :: ByteString
    -> Either
        (TryFromException ByteString Cabal.PackageDescription)
        Cabal.PackageDescription
parsePackageDescription = maybeTryFrom $ \ bs -> do
    gpd <- hush $ parseGenericPackageDescription bs
    hush $ toPackageDescription gpd

parseGenericPackageDescription
    :: ByteString
    -> Either
        (TryFromException ByteString Cabal.GenericPackageDescription)
        Cabal.GenericPackageDescription
parseGenericPackageDescription = maybeTryFrom Cabal.parseGenericPackageDescriptionMaybe

-- | Although the generic package description type does have a package
-- description in it, that nested PD isn't actually usable. This function is
-- necessary in order to choose the platform, compiler, flags, and other stuff.
toPackageDescription
    :: Cabal.GenericPackageDescription
    -> Either
        (TryFromException Cabal.GenericPackageDescription Cabal.PackageDescription)
        Cabal.PackageDescription
toPackageDescription =
    let
        flags = Cabal.mkFlagAssignment []
        components = Cabal.ComponentRequestedSpec
            { Cabal.benchmarksRequested = True
            , Cabal.testsRequested = True
            }
        satisfiable = always True :: Cabal.Dependency -> Bool
        platform = Cabal.Platform Cabal.X86_64 Cabal.Linux
        compiler = Cabal.unknownCompilerInfo
            (Cabal.CompilerId Cabal.GHC (Cabal.mkVersion [9, 0, 1]))
            Cabal.NoAbiTag
        constraints = [] :: [Cabal.PackageVersionConstraint]
        finalize = Cabal.finalizePD
            flags
            components
            satisfiable
            platform
            compiler
            constraints
    in maybeTryFrom $ fmap fst . hush . finalize
