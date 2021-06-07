module Monadoc.Handler.GetRevision where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
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
    when (Maybe.isNothing maybePackage) $ throwM NotFound.new
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
            { revision_name = packageName
            , revision_version = version
            , revision_revision = revision
            }
        }

data Revision = Revision
    { revision_name :: PackageName.PackageName
    , revision_version :: Version.Version
    , revision_revision :: Revision.Revision
    } deriving (Eq, Show)

instance ToXml.ToXml Revision where
    toXml revision = Xml.node "revision" []
        [ Xml.node "name" [] [ToXml.toXml $ revision_name revision]
        , Xml.node "version" [] [ToXml.toXml $ revision_version revision]
        , Xml.node "revision" [] [ToXml.toXml $ revision_revision revision]
        ]
