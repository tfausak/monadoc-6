{-# LANGUAGE TypeApplications #-}

module Monadoc.Job.ProcessDistributions where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Set as Set
import qualified Data.Text as Text
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
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Log as Log
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import qualified Witch

run :: Context.Context -> IO ()
run context = Context.withConnection context $ \ connection -> Sqlite.fold
    connection
    (Sqlite.Query $ Witch.into @Text.Text
        "select key, author, bugReports, buildType, cabalVersion, category, \
        \copyright, description, hash, homepage, license, maintainer, name, \
        \pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, \
        \version from package order by name asc, version asc, revision asc")
    ()
    () $ \ () package -> do
        -- TODO: Figure out some way to avoid even looking at packages that
        -- haven't changed.
        let packageName = Package.name $ Model.value package
        maybeDistribution <- Distribution.selectByPackageAndVersion connection
            packageName
            (Package.version $ Model.value package)
        distribution <- maybe (Exception.throwM NotFound.new) pure maybeDistribution
        files <- File.selectByDistribution connection $ Model.key distribution
        maybePackageBlob <- Blob.selectByHash connection
            . Package.hash
            $ Model.value package
        packageBlob <- maybe (Exception.throwM NotFound.new) pure maybePackageBlob
        packageDescription <- either Exception.throwM pure
            . ProcessHackageIndex.parsePackageDescription
            . Blob.contents
            $ Model.value packageBlob
        components <- Component.selectByPackage connection $ Model.key package
        Monad.forM_ components $ \ component -> do
            componentName <- either Exception.throwM pure
                . componentIdToComponentName packageName
                . Component.id
                $ Model.value component
            cabalComponent <- maybe (Exception.throwM NotFound.new) pure
                $ Cabal.lookupComponent packageDescription componentName
            let buildInfo = componentToBuildInfo cabalComponent
            modules <- Module.selectByComponent connection $ Model.key component
            Monad.forM_ modules $ \ module_ ->
                case Module.file $ Model.value module_ of
                    Just _ -> pure () -- TODO: Parse module.
                    Nothing ->
                        case findFiles package files buildInfo module_ of
                            file : _ -> Module.upsert connection
                                (Model.value module_)
                                { Module.file = Just $ Model.key file }
                            -- haste-compiler appears to be the only package
                            -- that currently returns multiple files for any
                            -- modules. Check version 0.5.2 revision 2, but
                            -- many other releases are affected.
                            --
                            -- Actually if multiple files match, I think it's
                            -- reasonable to take the first one. The source
                            -- directories are ordered after all.
                            [] -> Log.warn -- TODO
                                $ "[worker] no files "
                                <> (Witch.into @String . Package.name $ Model.value package)
                                <> " "
                                <> (Witch.into @String . Package.version $ Model.value package)
                                <> " "
                                <> (Witch.into @String . Package.revision $ Model.value package)
                                <> " "
                                <> (Witch.into @String . Component.tag $ Model.value component)
                                <> " "
                                <> (Witch.into @String . Component.name $ Model.value component)
                                <> " "
                                <> (Witch.into @String . Module.name $ Model.value module_)

componentIdToComponentName
    :: PackageName.PackageName
    -> ComponentId.ComponentId
    -> Either
        (Witch.TryFromException ComponentId.ComponentId Cabal.ComponentName)
        Cabal.ComponentName
componentIdToComponentName package = Witch.maybeTryFrom $ \ ci ->
    case (ComponentId.tag ci, ComponentId.name ci) of
        (ComponentTag.Library, Nothing) -> Just $ Cabal.CLibName Cabal.LMainLibName
        (ComponentTag.Library, Just name)
            | name == Witch.from package -> Just $ Cabal.CLibName Cabal.LMainLibName
            | otherwise -> Just . Cabal.CLibName . Cabal.LSubLibName $ Witch.from name
        (ComponentTag.ForeignLibrary, Just name) -> Just $ Cabal.CFLibName $ Witch.from name
        (ComponentTag.Executable, Just name) -> Just $ Cabal.CExeName $ Witch.from name
        (ComponentTag.TestSuite, Just name) -> Just $ Cabal.CTestName $ Witch.from name
        (ComponentTag.Benchmark, Just name) -> Just $ Cabal.CBenchName $ Witch.from name
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
        extensions = ["hs", "hs-boot", "chs", "hsc", "lhs", "cpphs"]
        directories = case Cabal.hsSourceDirs buildInfo of
            [] -> ["."]
            ds -> ds
        toPaths d = fmap (toPath
            (Package.name $ Model.value package)
            (Package.version $ Model.value package)
            d
            (Module.name $ Model.value module_)) extensions
        paths = Set.fromList $ foldMap toPaths directories
        isMatch file = Set.member (File.path $ Model.value file) paths
    in filter isMatch files

-- PKG-VER/DIR/MOD.EXT
toPath
    :: PackageName.PackageName
    -> Version.Version
    -> FilePath
    -> ModuleName.ModuleName
    -> String
    -> FilePath
toPath p v d m x =
    FilePath.normalise $ FilePath.addExtension
        (FilePath.Posix.joinPath
            [ Witch.into @String p <> "-" <> Witch.into @String v
            , UnpackDistribution.normalizeFilePath d
            , FilePath.Posix.joinPath . Cabal.components $ Witch.into @Cabal.ModuleName m
            ])
        x
