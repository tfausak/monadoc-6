module Monadoc.Handler.GetRevision where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
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
    let versionRange = maybe VersionRange.any PreferredVersions.versionRange maybePreferredVersions
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
                    { Common.breadcrumb_name = into @String version
                    , Common.breadcrumb_route = Just $ Route.Version packageName version
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String revision
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap User.githubLogin maybeUser
            }
        , Common.monadoc_page = Revision
            { revision_package = Package package (VersionRange.contains (Package.version package) versionRange)
            , revision_versions = packages
                & List.sortOn (\ p -> Ord.Down (Package.version p, Package.revision p))
                & fmap (\ p -> Version p (VersionRange.contains (Package.version p) versionRange))
            }
        }

data Revision = Revision
    { revision_package :: Package
    , revision_versions :: [Version]
    } deriving (Eq, Show)

instance ToXml.ToXml Revision where
    toXml revision = Xml.node "revision" []
        [ ToXml.toXml $ revision_package revision
        , Xml.node "versions" [] . fmap ToXml.toXml $ revision_versions revision
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
        , Xml.node "description" [] [ToXml.toXml $ Package.description package]
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
