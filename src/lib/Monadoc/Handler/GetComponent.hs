{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.GetComponent where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Foldable as Foldable
import qualified Monadoc.Utility.Xml as Xml
import qualified Monadoc.Vendor.Sql as Sql

handler
    :: PackageName.PackageName
    -> Release.Release
    -> ComponentId.ComponentId
    -> Handler.Handler
handler packageName release componentId context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        componentTag = ComponentId.tag componentId
        isLibrary = componentTag == ComponentTag.Library
        maybeComponentName = ComponentId.name componentId
        namesMatch = maybeComponentName == Just (into @ComponentName.ComponentName packageName)
        version = Release.version release

    -- TODO: Redirect to the latest release?
    revision <- maybe (throwM NotFound.new) pure $ Release.revision release

    -- Redirect foo:lib:foo to foo:lib.
    when (isLibrary && namesMatch) . throwM . Found.new $ baseUrl <> Route.toString
        (Route.Component packageName release $ ComponentId.ComponentId componentTag Nothing)

    maybeUser <- Common.getUser context request
    maybePackage <- Context.withConnection context $ \ connection ->
        Package.select connection packageName version revision
    package <- maybe (throwM NotFound.new) pure maybePackage
    allComponents <- Context.withConnection context $ \ connection ->
        Component.selectByPackage connection $ Model.key package
    let componentsByTag = Foldable.groupBy (Component.tag . Model.value) allComponents
    components <- case Map.lookup (ComponentId.tag componentId) componentsByTag of
        Nothing -> throwM NotFound.new
        Just components -> pure components

    when (not isLibrary && Maybe.isNothing maybeComponentName)
        $ case NonEmpty.toList components of
            -- Redirect foo:exe to foo:exe:bar when there is only one
            -- executable. Same for all component types except library.
            [component] -> throwM . Found.new $ baseUrl <> Route.toString
                ( Route.Component packageName release
                . ComponentId.ComponentId componentTag
                . Just
                . Component.name
                $ Model.value component
                )
            -- Otherwise this route is ambiguous.
            _ -> throwM NotFound.new

    let componentName = Maybe.fromMaybe (into @ComponentName.ComponentName packageName) maybeComponentName
    component <- components
        & List.find ((== componentName) . Component.name . Model.value)
        & maybe (throwM NotFound.new) pure
    dependencies <- Context.withConnection context $ \ connection ->
        Dependency.selectByComponent connection $ Model.key component

    -- TODO: Chase down transitive dependencies?
    -- <https://sqlite.org/lang_with.html>

    -- TODO: Mark dependencies that do not allow latest?

    -- TODO: Display licenses of dependencies.

    reverseDependencies <- Context.withConnection context $ \ connection -> Sql.query
        connection
        "select package.name \
        \from dependency \
        \inner join component \
        \on component.key = dependency.component \
        \inner join package \
        \on package.key = component.package \
        \where dependency.packageName = ? \
        \and dependency.libraryName = ? \
        \and package.name != dependency.packageName \
        \group by package.name \
        \limit 16"
        (packageName, componentName)

    modules <- if isLibrary
        then Context.withConnection context $ \ connection ->
            Module.selectByComponent connection $ Model.key component
        else pure []

    let
        route = Route.Component packageName release componentId
        breadcrumbs =
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
                , Breadcrumb.route = Nothing
                }
            ]
        page = Xml.node "component" []
            [ Xml.node "package" [] [ToXml.toXml packageName]
            , Xml.node "release" [] [ToXml.toXml release]
            , Xml.node "tag" [] [ToXml.toXml . Component.tag $ Model.value component]
            , Xml.node "name" [] [ToXml.toXml . Component.name $ Model.value component]
            , Xml.node "dependencies" []
            . fmap (\ x -> Xml.node "dependency" []
                [ Xml.node "packageName" [] [ToXml.toXml $ Dependency.packageName x]
                , Xml.node "libraryName" [] [ToXml.toXml $ Dependency.libraryName x]
                , Xml.node "route" [] [ToXml.toXml . Route.Package $ Dependency.packageName x]
                , Xml.node "versionRange" [] [ToXml.toXml $ Dependency.versionRange x]
                ])
            . List.sortOn (CI.mk . into @String . Dependency.packageName)
            $ fmap Model.value dependencies
            , Xml.node "reverseDependencies" []
            . fmap (\ x -> Xml.node "reverseDependency" []
                [ Xml.node "packageName" [] [ToXml.toXml x]
                , Xml.node "route" [] [ToXml.toXml $ Route.Package x]
                ])
            . List.sortOn (CI.mk . into @String)
            $ fmap Sql.fromOnly reverseDependencies
            , Xml.node "modules" []
            . fmap (\ x -> Xml.node "module" []
                [ Xml.node "name" [] [ToXml.toXml $ Module.name x]
                , Xml.node "route" [] [ToXml.toXml $ Route.Module packageName release componentId (Module.name x)]
                ])
            . List.sortOn (CI.mk . into @String . Module.name)
            $ fmap Model.value modules
            ]
    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs = breadcrumbs
            , Meta.title = List.intercalate " - " ["Monadoc", into @String packageName, into @String release, into @String componentId]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = page
        }
