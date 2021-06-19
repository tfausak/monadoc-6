module Monadoc.Type.Meta where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Routes as Routes
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Paths_monadoc as This

data Meta = Meta
    { baseUrl :: String
    , breadcrumbs :: [Breadcrumb.Breadcrumb]
    , clientId :: String
    , routes :: Routes.Routes
    , user :: Maybe String
    , version :: Version.Version
    } deriving (Eq, Show)

instance ToXml.ToXml Meta where
    toXml config = Xml.node "meta" []
        [ Xml.node "baseUrl" [] [ToXml.toXml $ baseUrl config]
        , Xml.node "breadcrumbs" [] . fmap ToXml.toXml $ breadcrumbs config
        , Xml.node "clientId" [] [ToXml.toXml $ clientId config]
        , ToXml.toXml $ routes config
        , Xml.node "user" [] [ToXml.toXml $ user config]
        , Xml.node "version" [] [ToXml.toXml $ version config]
        ]

fromContext :: Context.Context -> Route.Route -> Meta
fromContext context self = Meta
    { baseUrl = Config.baseUrl $ Context.config context
    , breadcrumbs = []
    , clientId = Config.clientId $ Context.config context
    , routes = Routes.Routes
        { Routes.account = Route.Account
        , Routes.bootstrap = Route.Bootstrap
        , Routes.callback = Route.Callback
        , Routes.favicon = Route.Favicon
        , Routes.logOut = Route.LogOut
        , Routes.revoke = Route.Revoke
        , Routes.search = Route.Search Nothing
        , Routes.self
        }
    , user = Nothing
    , version = into @Version.Version This.version
    }
