module Monadoc.Handler.GetRevision where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Ord as Ord
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
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Root as Root
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
    maybePackage <- Context.withConnection context $ \ connection ->
        Package.select connection packageName version revision
    package <- maybe (throwM NotFound.new) pure maybePackage
    packages <- Context.withConnection context $ \ connection ->
        Package.selectByName connection packageName
    let
        sortedPackages = packages
            & fmap Model.value
            & List.sortOn (\ p -> Ord.Down (Package.version p, Package.revision p))
    maybePreferredVersions <- Context.withConnection context $ \ connection ->
        PreferredVersions.selectByPackageName connection packageName
    let versionRange = maybe VersionRange.any (PreferredVersions.versionRange . Model.value) maybePreferredVersions
    components <- Context.withConnection context $ \ connection ->
        Component.selectByPackage connection $ Model.key package
    sourceRepositories <- Context.withConnection context $ \ connection ->
        SourceRepository.selectByPackage connection $ Model.key package
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
                    { Breadcrumb.name = into @String version <> if revision == Revision.zero then "" else "-" <> into @String revision
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = Revision
            { revision_package = Package
                { package_package = Model.value package
                , package_preferred = VersionRange.contains (Package.version $ Model.value package) versionRange
                , package_latest = case sortedPackages of
                    [] -> True
                    p : _ -> Package.version p == Package.version (Model.value package) && Package.revision p == Package.revision (Model.value package)
                }
            , revision_versions = sortedPackages
                & fmap (\ p -> Version
                    { version_package = p
                    , version_preferred = VersionRange.contains (Package.version p) versionRange
                    })
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
    , package_latest :: Bool
    } deriving (Eq, Show)

instance ToXml.ToXml Package where
    toXml (Package package preferred latest) = Xml.node "package" []
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
        , Xml.node "latest" [] [ToXml.toXml latest]
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
    in Component { component_tag = tag, component_name = maybeName, component_route = route }
