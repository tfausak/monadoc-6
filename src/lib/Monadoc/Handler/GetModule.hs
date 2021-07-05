module Monadoc.Handler.GetModule where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Distribution.ModuleName as Cabal
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml
import qualified System.FilePath.Posix as FilePath.Posix

handler
    :: PackageName.PackageName
    -> Release.Release
    -> ComponentId.ComponentId
    -> ModuleName.ModuleName
    -> Handler.Handler
handler packageName release componentId moduleName context request = do
    let version = Release.version release
    revision <- maybe (throwM NotFound.new) pure $ Release.revision release
    maybeUser <- Common.getUser context request
    let route = Route.Module packageName release componentId moduleName
    package <- do
        maybePackage <- Context.withConnection context $ \ connection ->
            Package.select connection packageName version revision
        maybe (throwM NotFound.new) pure maybePackage
    component <- do
        -- TODO: More accurately handle component IDs.
        maybeComponent <- Context.withConnection context $ \ connection ->
            Component.select connection
                (Model.key package)
                (ComponentId.tag componentId)
                (maybe (from packageName) identity $ ComponentId.name componentId)
        maybe (throwM NotFound.new) pure maybeComponent
    _module <- do
        maybeModule <- Context.withConnection context $ \ connection ->
            Module.select connection (Model.key component) moduleName
        maybe (throwM NotFound.new) pure maybeModule

    -- TODO: This method of locating the module's file is brittle and janky. It
    -- should be done ahead of time using the `hs-source-dirs` information on
    -- the component.
    distribution <- do
        maybeDistribution <- Context.withConnection context $ \ connection ->
            Distribution.selectByPackageAndVersion connection packageName version
        maybe (throwM NotFound.new) pure maybeDistribution
    files <- Context.withConnection context $ \ connection ->
        File.selectByDistribution connection $ Model.key distribution
    let
        needle = moduleName
            & into @Cabal.ModuleName
            & Cabal.components
            & FilePath.Posix.joinPath
            & (FilePath.Posix.pathSeparator :)
            & (<> [FilePath.Posix.extSeparator])
        matches = files
            & filter (List.isInfixOf needle . File.path . Model.value)
        maybeFile = case matches of
            [match] -> Just match
            _ -> Nothing

    maybeBlob <- case maybeFile of
        Nothing -> pure Nothing
        Just file -> Context.withConnection context $ \ connection ->
            Blob.selectByHash connection . File.hash $ Model.value file
    -- TODO: Parse module! And then move this into a worker job.

    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs =
                [ Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Home"
                    , Breadcrumb.route = Just Route.Index
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String packageName
                    , Breadcrumb.route = Just $ Route.Package packageName
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String release
                    , Breadcrumb.route = Just $ Route.Release packageName release
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String componentId
                    , Breadcrumb.route = Just $ Route.Component packageName release componentId
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String moduleName
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Meta.title = List.intercalate " - " ["Monadoc", into @String packageName, into @String release, into @String componentId, into @String moduleName]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = Xml.node "module" []
            [ Xml.node "package" [] [ToXml.toXml packageName]
            , Xml.node "release" [] [ToXml.toXml release]
            , Xml.node "component" [] [ToXml.toXml $ into @String componentId]
            , Xml.node "module" [] [ToXml.toXml moduleName]
            , Xml.node "file" []
                [ Xml.node "contents" [] [ToXml.toXml $ case maybeBlob of
                    Nothing -> Nothing
                    Just blob -> hush . tryInto @Text . Blob.contents $ Model.value blob]
                , Xml.node "path" [] [ToXml.toXml $ fmap (File.path . Model.value) maybeFile]
                , Xml.node "route" [] [ToXml.toXml $ fmap (Route.File packageName release . File.path . Model.value) maybeFile]
                ]
            ]
        }
