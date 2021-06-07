module Monadoc.Handler.GetVersion where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Ord as Ord
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

handler :: PackageName.PackageName -> Version.Version -> Handler.Handler
handler packageName version context request = do
    let route = Route.Version packageName version
    maybeUser <- Common.getUser context request
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByNameAndVersion connection packageName version
    when (null packages) $ throwM NotFound.new
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
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap User.githubLogin maybeUser
            }
        , Common.monadoc_page = Version
            { version_name = packageName
            , version_version = version
            , version_revisions =
                fmap
                    (\ package -> Revision
                        { revision_number = Package.revision package
                        , revision_route = Route.Revision (Package.name package) (Package.version package) (Package.revision package)
                        })
                $ List.sortOn (Ord.Down . Package.revision) packages
            }
        }

data Version = Version
    { version_name :: PackageName.PackageName
    , version_version :: Version.Version
    , version_revisions :: [Revision]
    } deriving (Eq, Show)

instance ToXml.ToXml Version where
    toXml version = Xml.node "version" []
        [ Xml.node "name" [] [ToXml.toXml $ version_name version]
        , Xml.node "version" [] [ToXml.toXml $ version_version version]
        , Xml.node "revisions" [] . fmap ToXml.toXml $ version_revisions version
        ]

data Revision = Revision
    { revision_number :: Revision.Revision
    , revision_route :: Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Revision where
    toXml revision = Xml.node "revision" []
        [ Xml.node "number" [] [ToXml.toXml $ revision_number revision]
        , Xml.node "route" [] [ToXml.toXml $ revision_route revision]
        ]
