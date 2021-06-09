module Monadoc.Handler.GetRevision where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
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
            { revision_package = package
            }
        }

newtype Revision = Revision
    { revision_package :: Package.Package
    } deriving (Eq, Show)

instance ToXml.ToXml Revision where
    toXml revision = Xml.node "revision" []
        [ Xml.node "author" [] [ToXml.toXml . Package.author $ revision_package revision]
        , Xml.node "bugReports" [] [ToXml.toXml . Package.bugReports $ revision_package revision]
        , Xml.node "buildType" [] [ToXml.toXml . Package.buildType $ revision_package revision]
        , Xml.node "cabalVersion" [] [ToXml.toXml . Package.cabalVersion $ revision_package revision]
        , Xml.node "category" [] [ToXml.toXml . Package.category $ revision_package revision]
        , Xml.node "copyright" [] [ToXml.toXml . Package.copyright $ revision_package revision]
        , Xml.node "description" [] [ToXml.toXml . Package.description $ revision_package revision]
        , Xml.node "homepage" [] [ToXml.toXml . Package.homepage $ revision_package revision]
        , Xml.node "license" [] [ToXml.toXml . Package.license $ revision_package revision]
        , Xml.node "maintainer" [] [ToXml.toXml . Package.maintainer $ revision_package revision]
        , Xml.node "name" [] [ToXml.toXml . Package.name $ revision_package revision]
        , Xml.node "pkgUrl" [] [ToXml.toXml . Package.pkgUrl $ revision_package revision]
        , Xml.node "revision" [] [ToXml.toXml . Package.revision $ revision_package revision]
        , Xml.node "stability" [] [ToXml.toXml . Package.stability $ revision_package revision]
        , Xml.node "synopsis" [] [ToXml.toXml . Package.synopsis $ revision_package revision]
        , Xml.node "uploadedAt" [] [ToXml.toXml . Package.uploadedAt $ revision_package revision]
        , Xml.node "version" [] [ToXml.toXml . Package.version $ revision_package revision]
        ]
