{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.GetModule where

import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
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
import qualified Monadoc.Utility.Ghc as Ghc
import qualified Monadoc.Utility.Xml as Xml
import qualified Witch

handler
    :: PackageName.PackageName
    -> Release.Release
    -> ComponentId.ComponentId
    -> ModuleName.ModuleName
    -> Handler.Handler
handler packageName release componentId moduleName context request = do
    let version = Release.version release
    revision <- maybe (Exception.throwM NotFound.new) pure $ Release.revision release
    maybeUser <- Common.getUser context request
    let route = Route.Module packageName release componentId moduleName
    package <- do
        maybePackage <- Context.withConnection context $ \ connection ->
            Package.select connection packageName version revision
        maybe (Exception.throwM NotFound.new) pure maybePackage
    component <- do
        -- TODO: More accurately handle component IDs.
        maybeComponent <- Context.withConnection context $ \ connection ->
            Component.select connection
                (Model.key package)
                (ComponentId.tag componentId)
                (Maybe.fromMaybe (Witch.from packageName) $ ComponentId.name componentId)
        maybe (Exception.throwM NotFound.new) pure maybeComponent
    module_ <- do
        maybeModule <- Context.withConnection context $ \ connection ->
            Module.select connection (Model.key component) moduleName
        maybe (Exception.throwM NotFound.new) pure maybeModule
    maybeFile <- case Module.file $ Model.value module_ of
        Nothing -> pure Nothing
        Just key -> Context.withConnection context $ \ connection ->
            File.select connection key
    maybeBlob <- case maybeFile of
        Nothing -> pure Nothing
        Just file -> Context.withConnection context $ \ connection ->
            Blob.selectByHash connection . File.hash $ Model.value file
    maybeHsModule <- case (maybeFile, maybeBlob) of
        (Just file, Just blob) -> do
            contents <- either Exception.throwM pure
                . Witch.tryInto @String
                . Blob.contents
                $ Model.value blob
            let filePath = File.path $ Model.value file
            result <- Ghc.parseModule filePath contents
            case result of
                Left errors -> Exception.throwM errors
                Right hsModule -> pure $ Just hsModule
        _ -> pure Nothing

    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs =
                [ Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Home"
                    , Breadcrumb.route = Just Route.Index
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = Witch.into @String packageName
                    , Breadcrumb.route = Just $ Route.Package packageName
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = Witch.into @String release
                    , Breadcrumb.route = Just $ Route.Release packageName release
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = Witch.into @String componentId
                    , Breadcrumb.route = Just $ Route.Component packageName release componentId
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = Witch.into @String moduleName
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Meta.title = List.intercalate " - " ["Monadoc", Witch.into @String packageName, Witch.into @String release, Witch.into @String componentId, Witch.into @String moduleName]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = Xml.node "module" []
            [ Xml.node "package" [] [ToXml.toXml packageName]
            , Xml.node "release" [] [ToXml.toXml release]
            , Xml.node "component" [] [ToXml.toXml $ Witch.into @String componentId]
            , Xml.node "module" [] [ToXml.toXml moduleName]
            , Xml.node "file" []
                [ Xml.node "contents" [] [ToXml.toXml $ fmap (Witch.into @String) maybeHsModule]
                , Xml.node "path" [] [ToXml.toXml $ fmap (File.path . Model.value) maybeFile]
                , Xml.node "route" [] [ToXml.toXml $ fmap (Route.File packageName release . File.path . Model.value) maybeFile]
                ]
            ]
        }
