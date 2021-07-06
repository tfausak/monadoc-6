module Monadoc.Job.ProcessDistributions where

import Monadoc.Prelude

import qualified Data.Set as Set
import qualified Database.SQLite.Simple as Sqlite
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Types.Component as Cabal
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Job.ProcessHackageIndex as ProcessHackageIndex
import qualified Monadoc.Job.UnpackDistribution as UnpackDistribution
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Utility.Log as Log
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix

run :: Context.Context -> IO ()
run context = Context.withConnection context $ \ connection -> Sqlite.fold
    connection
    (Sqlite.Query $ into @Text
        "select key, author, bugReports, buildType, cabalVersion, category, \
        \copyright, description, hash, homepage, license, maintainer, name, \
        \pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, \
        \version from package order by name asc, version asc, revision asc")
    ()
    () $ \ () package -> do
        let packageName = Package.name $ Model.value package
        maybeDistribution <- Distribution.selectByPackageAndVersion connection
            packageName
            (Package.version $ Model.value package)
        distribution <- maybe (throwM NotFound.new) pure maybeDistribution
        files <- File.selectByDistribution connection $ Model.key distribution
        maybePackageBlob <- Blob.selectByHash connection
            . Package.hash
            $ Model.value package
        packageBlob <- maybe (throwM NotFound.new) pure maybePackageBlob
        packageDescription <- either throwM pure
            . ProcessHackageIndex.parsePackageDescription
            . Blob.contents
            $ Model.value packageBlob
        components <- Component.selectByPackage connection $ Model.key package
        for_ components $ \ component -> do
            componentName <- either throwM pure
                . componentIdToComponentName packageName
                . Component.id
                $ Model.value component
            cabalComponent <- maybe (throwM NotFound.new) pure
                $ Cabal.lookupComponent packageDescription componentName
            let buildInfo = componentToBuildInfo cabalComponent
            modules <- Module.selectByComponent connection $ Model.key component
            for_ modules $ \ module_ ->
                case Module.file $ Model.value module_ of
                    Just _ -> pure () -- TODO: Parse module.
                    Nothing ->
                        case findFiles package files buildInfo module_ of
                            [file] -> Module.upsert connection
                                (Model.value module_)
                                { Module.file = Just $ Model.key file }
                            -- haste-compiler appears to be the only package
                            -- that currently returns multiple files for any
                            -- modules. Check version 0.5.2 revision 2, but
                            -- many other releases are affected.
                            xs -> Log.warn -- TODO
                                $ "[worker] wrong file count: "
                                <> show (length xs)
                                <> " "
                                <> (into @String . Package.name $ Model.value package)
                                <> " "
                                <> (into @String . Package.version $ Model.value package)
                                <> " "
                                <> (into @String . Package.revision $ Model.value package)
                                <> " "
                                <> (into @String . Component.tag $ Model.value component)
                                <> " "
                                <> (into @String . Component.name $ Model.value component)
                                <> " "
                                <> (into @String . Module.name $ Model.value module_)

componentIdToComponentName
    :: PackageName.PackageName
    -> ComponentId.ComponentId
    -> Either
        (TryFromException ComponentId.ComponentId Cabal.ComponentName)
        Cabal.ComponentName
componentIdToComponentName package = maybeTryFrom $ \ id ->
    case (ComponentId.tag id, ComponentId.name id) of
        (ComponentTag.Library, Nothing) -> Just $ Cabal.CLibName Cabal.LMainLibName
        (ComponentTag.Library, Just name)
            | name == from package -> Just $ Cabal.CLibName Cabal.LMainLibName
            | otherwise -> Just . Cabal.CLibName . Cabal.LSubLibName $ from name
        (ComponentTag.ForeignLibrary, Just name) -> Just $ Cabal.CFLibName $ from name
        (ComponentTag.Executable, Just name) -> Just $ Cabal.CExeName $ from name
        (ComponentTag.TestSuite, Just name) -> Just $ Cabal.CTestName $ from name
        (ComponentTag.Benchmark, Just name) -> Just $ Cabal.CBenchName $ from name
        (_, Nothing) -> Nothing

componentToBuildInfo :: Cabal.Component -> Cabal.BuildInfo
componentToBuildInfo c = case c of
    Cabal.CBench b -> Cabal.benchmarkBuildInfo b
    Cabal.CExe e -> Cabal.buildInfo e
    Cabal.CFLib f -> Cabal.foreignLibBuildInfo f
    Cabal.CLib l -> Cabal.libBuildInfo l
    Cabal.CTest t -> Cabal.testBuildInfo t

findFiles
    :: Package.Model
    -> [File.Model]
    -> Cabal.BuildInfo
    -> Module.Model
    -> [File.Model]
findFiles package files buildInfo module_ =
    let
        partialPath = module_
            & Model.value
            & Module.name
            & into @Cabal.ModuleName
            & Cabal.components
            & FilePath.Posix.joinPath
        -- PKG-VER/DIR/MOD.hs
        toPath directory = UnpackDistribution.normalizeFilePath $ mconcat
            [ into @String . Package.name $ Model.value package
            , "-"
            , into @String . Package.version $ Model.value package
            , [FilePath.Posix.pathSeparator]
            , directory
            , [FilePath.Posix.pathSeparator]
            , partialPath
            , [FilePath.extSeparator]
            , "hs"
            ]
        paths = Set.fromList . fmap toPath $ Cabal.hsSourceDirs buildInfo
        isMatch file = Set.member (File.path $ Model.value file) paths
    in filter isMatch files
