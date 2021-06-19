module Monadoc.Handler.GetComponent where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Foldable as Foldable
import qualified Monadoc.Utility.Xml as Xml

handler
    :: PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> ComponentId.ComponentId
    -> Handler.Handler
handler packageName version revision componentId context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        componentTag = ComponentId.tag componentId
        isLibrary = componentTag == ComponentTag.Library
        maybeComponentName = ComponentId.name componentId
        namesMatch = maybeComponentName == Just (into @ComponentName.ComponentName packageName)

    -- Redirect foo:lib:foo to foo:lib.
    when (isLibrary && namesMatch) . throwM . Found.new $ baseUrl <> Route.toString
        (Route.Component packageName version revision $ ComponentId.ComponentId componentTag Nothing)

    let route = Route.Component packageName version revision componentId
    maybeUser <- Common.getUser context request
    maybePackage <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.select connection packageName version revision
    package <- maybe (throwM NotFound.new) pure maybePackage
    allComponents <- Pool.withResource (Context.pool context) $ \ connection ->
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
                ( Route.Component packageName version revision
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
    dependencies <- Pool.withResource (Context.pool context) $ \ connection ->
        Dependency.selectByComponent connection $ Model.key component

    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context route)
            { Common.config_breadcrumbs =
                [ Common.Breadcrumb
                    { Common.breadcrumb_name = "Home"
                    , Common.breadcrumb_route = Just Route.Index
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String packageName
                    , Common.breadcrumb_route = Just $ Route.Package packageName
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String version <> if revision == Revision.zero then "" else "-" <> into @String revision
                    , Common.breadcrumb_route = Just $ Route.Revision packageName version revision
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String componentId
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.monadoc_page = Component
            { component_package = Model.value package
            , component_component = Model.value component
            , component_dependencies = fmap Model.value dependencies
            }
        }

data Component = Component
    { component_package :: Package.Package
    , component_component :: Component.Component
    , component_dependencies :: [Dependency.Dependency]
    } deriving (Eq, Show)

instance ToXml.ToXml Component where
    toXml component = Xml.node "component" []
        [ Xml.node "package" [] [ToXml.toXml . Package.name $ component_package component]
        , Xml.node "version" [] [ToXml.toXml . Package.version $ component_package component]
        , Xml.node "revision" [] [ToXml.toXml . Package.revision $ component_package component]
        , Xml.node "tag" [] [ToXml.toXml . Component.tag $ component_component component]
        , Xml.node "name" [] [ToXml.toXml . Component.name $ component_component component]
        , Xml.node "dependencies" [] . fmap (\ dependency -> Xml.node "dependency" []
            [ Xml.node "packageName" [] [ToXml.toXml $ Dependency.packageName dependency]
            , Xml.node "libraryName" [] [ToXml.toXml $ Dependency.libraryName dependency]
            , Xml.node "versionRange" [] [ToXml.toXml $ Dependency.versionRange dependency]
            ]) . List.sortOn (CI.mk . into @String . Dependency.packageName) $ component_dependencies component
        ]
