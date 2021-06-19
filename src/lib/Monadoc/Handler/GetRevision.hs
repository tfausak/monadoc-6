module Monadoc.Handler.GetRevision where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.SourceRepository as SourceRepository
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Utility.Xml as Xml

handler
    :: PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> Handler.Handler
handler packageName version revision context request = do
    let route = Route.Revision packageName version revision
    maybeUser <- Common.getUser context request
    maybePackage <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.select connection packageName version revision
    package <- maybe (throwM NotFound.new) pure maybePackage
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByName connection packageName
    maybePreferredVersions <- Pool.withResource (Context.pool context) $ \ connection ->
        PreferredVersions.selectByPackageName connection packageName
    let versionRange = maybe VersionRange.any (PreferredVersions.versionRange . Model.value) maybePreferredVersions
    components <- Pool.withResource (Context.pool context) $ \ connection ->
        Component.selectByPackage connection $ Model.key package
    sourceRepositories <- Pool.withResource (Context.pool context) $ \ connection ->
        SourceRepository.selectByPackage connection $ Model.key package
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
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.monadoc_page = Revision
            { revision_package = Package (Model.value package) (VersionRange.contains (Package.version $ Model.value package) versionRange)
            , revision_versions = packages
                & fmap Model.value
                & List.sortOn (\ p -> Ord.Down (Package.version p, Package.revision p))
                & fmap (\ p -> Version p (VersionRange.contains (Package.version p) versionRange))
            , revision_components = components
                & fmap Model.value
                & List.sortOn (\ c -> (Component.tag c, Component.name c))
                & fmap (toComponent $ Model.value package)
            , revision_sourceRepositories = sourceRepositories
                & fmap Model.value
            }
        }

data Revision = Revision
    { revision_package :: Package
    , revision_versions :: [Version]
    , revision_components :: [Component]
    , revision_sourceRepositories :: [SourceRepository.SourceRepository]
    } deriving (Eq, Show)

instance ToXml.ToXml Revision where
    toXml revision = Xml.node "revision" []
        [ ToXml.toXml $ revision_package revision
        , Xml.node "versions" [] . fmap ToXml.toXml $ revision_versions revision
        , Xml.node "components" [] . fmap ToXml.toXml $ revision_components revision
        , Xml.node "sourceRepositories" [] . fmap (\ sourceRepository -> Xml.node "sourceRepository" []
            [ Xml.node "branch" [] [ToXml.toXml $ SourceRepository.branch sourceRepository]
            , Xml.node "kind" [] [ToXml.toXml $ SourceRepository.kind sourceRepository]
            , Xml.node "location" [] [ToXml.toXml $ SourceRepository.location sourceRepository]
            , Xml.node "module" [] [ToXml.toXml $ SourceRepository.module_ sourceRepository]
            , Xml.node "subdir" [] [ToXml.toXml $ SourceRepository.subdir sourceRepository]
            , Xml.node "tag" [] [ToXml.toXml $ SourceRepository.tag sourceRepository]
            , Xml.node "type" [] [ToXml.toXml $ SourceRepository.type_ sourceRepository]
            ]) $ revision_sourceRepositories revision
        ]

data Package = Package
    { package_package :: Package.Package
    , package_preferred :: Bool
    } deriving (Eq, Show)

instance ToXml.ToXml Package where
    toXml (Package package preferred) = Xml.node "package" []
        [ Xml.node "author" [] [ToXml.toXml $ Package.author package]
        , Xml.node "bugReports" [] [ToXml.toXml $ Package.bugReports package]
        , Xml.node "buildType" [] [ToXml.toXml $ Package.buildType package]
        , Xml.node "cabalVersion" [] [ToXml.toXml $ Package.cabalVersion package]
        , Xml.node "category" [] [ToXml.toXml $ Package.category package]
        , Xml.node "copyright" [] [ToXml.toXml $ Package.copyright package]
        , Xml.node "description" []
            [ ToXml.toXml
            . Haddock.toRegular @Void
            . Haddock._doc
            . Haddock.parseParas Nothing
            . into @String
            $ Package.description package
            ]
        , Xml.node "homepage" [] [ToXml.toXml $ Package.homepage package]
        , Xml.node "license" [] [ToXml.toXml $ Package.license package]
        , Xml.node "maintainer" [] [ToXml.toXml $ Package.maintainer package]
        , Xml.node "name" [] [ToXml.toXml $ Package.name package]
        , Xml.node "pkgUrl" [] [ToXml.toXml $ Package.pkgUrl package]
        , Xml.node "preferred" [] [ToXml.toXml preferred]
        , Xml.node "revision" [] [ToXml.toXml $ Package.revision package]
        , Xml.node "stability" [] [ToXml.toXml $ Package.stability package]
        , Xml.node "synopsis" [] [ToXml.toXml $ Package.synopsis package]
        , Xml.node "uploadedAt" [] [ToXml.toXml $ Package.uploadedAt package]
        , Xml.node "uploadedBy" [] [ToXml.toXml $ Package.uploadedBy package]
        , Xml.node "version" [] [ToXml.toXml $ Package.version package]
        ]

data Version = Version
    { version_package :: Package.Package
    , version_preferred :: Bool
    } deriving (Eq, Show)

instance ToXml.ToXml Version where
    toXml (Version package preferred) = Xml.node "version" []
        [ Xml.node "number" [] [ToXml.toXml $ Package.version package]
        , Xml.node "preferred" [] [ToXml.toXml preferred]
        , Xml.node "revision" [] [ToXml.toXml $ Package.revision package]
        , Xml.node "route" [] [ToXml.toXml $ Route.Revision (Package.name package) (Package.version package) (Package.revision package)]
        , Xml.node "uploadedAt" [] [ToXml.toXml $ Package.uploadedAt package]
        ]

data Component = Component
    { component_tag :: ComponentTag.ComponentTag
    , component_name :: Maybe ComponentName.ComponentName
    , component_route :: Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Component where
    toXml component = Xml.node "component" []
        [ Xml.node "tag" [] [ToXml.toXml $ component_tag component]
        , Xml.node "name" [] [ToXml.toXml $ component_name component]
        , Xml.node "route" [] [ToXml.toXml $ component_route component]
        ]

toComponent :: Package.Package -> Component.Component -> Component
toComponent package component =
    let
        tag = Component.tag component
        name = Component.name component
        isLibrary = tag == ComponentTag.Library
        namesMatch = into @PackageName.PackageName name == Package.name package
        maybeName = if isLibrary && namesMatch then Nothing else Just name
        route = Route.Component
            (Package.name package)
            (Package.version package)
            (Package.revision package)
            (ComponentId.ComponentId tag maybeName)
    in Component tag maybeName route
