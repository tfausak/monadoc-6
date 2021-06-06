module Monadoc.Handler.GetRevision where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Paths_monadoc as This

handler
    :: PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> Handler.Handler
handler packageName version revision context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- Common.getUser context request
    maybePackage <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.select connection packageName version revision
    case maybePackage of
        Nothing -> pure $ Response.status Http.notFound404 []
        Just _ -> pure $ Common.makeResponse Common.Monadoc
            { Common.monadoc_config = Common.Config
                { Common.config_baseUrl = baseUrl
                , Common.config_breadcrumbs =
                    [ Common.Breadcrumb
                        { Common.breadcrumb_name = "Home"
                        , Common.breadcrumb_link = Just $ baseUrl <> Route.toString Route.Index
                        }
                    , Common.Breadcrumb
                        { Common.breadcrumb_name = into @String packageName
                        , Common.breadcrumb_link = Just $ baseUrl <> Route.toString (Route.Package packageName)
                        }
                    , Common.Breadcrumb
                        { Common.breadcrumb_name = into @String version
                        , Common.breadcrumb_link = Just $ baseUrl <> Route.toString (Route.Version packageName version)
                        }
                    , Common.Breadcrumb
                        { Common.breadcrumb_name = into @String revision
                        , Common.breadcrumb_link = Nothing
                        }
                    ]
                , Common.config_clientId = clientId
                , Common.config_user = fmap User.githubLogin maybeUser
                , Common.config_version = into @Version.Version This.version
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
