module Monadoc.Handler.GetVersion where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Data.Set as Set
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

handler :: PackageName.PackageName -> Version.Version -> Handler.Handler
handler packageName version context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- Common.getUser context request
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByNameAndVersion connection packageName version
    case packages of
        [] -> pure $ Response.status Http.notFound404 []
        _ : _ -> pure $ Common.makeResponse Common.Monadoc
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
                        , Common.breadcrumb_link = Nothing
                        }
                    ]
                , Common.config_clientId = clientId
                , Common.config_user = fmap User.githubLogin maybeUser
                , Common.config_version = into @Version.Version This.version
                }
            , Common.monadoc_page = Version
                { version_name = packageName
                , version_version = version
                , version_revisions =
                    fmap
                        (\ revision -> Revision
                            { revision_number = revision
                            , revision_link = baseUrl <> Route.toString (Route.Revision packageName version revision)
                            })
                    . Set.toDescList
                    . Set.fromList
                    $ fmap Package.revision packages
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
    , revision_link :: String
    } deriving (Eq, Show)

instance ToXml.ToXml Revision where
    toXml revision = Xml.node "revision" []
        [ Xml.node "number" [] [ToXml.toXml $ revision_number revision]
        , Xml.node "link" [] [ToXml.toXml $ revision_link revision]
        ]
