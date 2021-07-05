module Monadoc.Job.ProcessDistributions where

import Monadoc.Prelude

import qualified Database.SQLite.Simple as Sqlite
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Types.Component as Cabal
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Job.ProcessHackageIndex as ProcessHackageIndex
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
        _files <- File.selectByDistribution connection $ Model.key distribution
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
            let _buildInfo = componentToBuildInfo cabalComponent
            modules <- Module.selectByComponent connection $ Model.key component
            for_ modules $ \ _module_ -> do
                -- TODO: Find module's file and parse it.
                pure ()

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
