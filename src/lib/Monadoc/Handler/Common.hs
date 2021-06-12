module Monadoc.Handler.Common where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.Pool as Pool
import qualified Data.UUID as Uuid
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as This
import qualified Text.XML as Xml
import qualified Web.Cookie as Cookie

getUser :: Context.Context -> Wai.Request -> IO (Maybe User.User)
getUser context request = do
    maybeSession <- getSession context request
    case maybeSession of
        Nothing -> pure Nothing
        Just session -> getUserWith context session

getUserWith :: Context.Context -> Model.Model Session.Session -> IO (Maybe User.User)
getUserWith context session =
    Pool.withResource (Context.pool context) $ \ connection ->
        User.selectByGithubId connection . Session.userGithubId $ Model.value session

getSession :: Context.Context -> Wai.Request -> IO (Maybe (Model.Model Session.Session))
getSession context request =
    case getSessionGuid request of
        Nothing -> pure Nothing
        Just guid -> getSessionWith context guid

getSessionGuid :: Wai.Request -> Maybe Guid.Guid
getSessionGuid request = do
    cookies <- lookup Http.hCookie $ Wai.requestHeaders request
    byteString <- lookup (into @ByteString "guid") $ Cookie.parseCookies cookies
    fmap (into @Guid.Guid) $ Uuid.fromASCIIBytes byteString

getSessionWith :: Context.Context -> Guid.Guid -> IO (Maybe (Model.Model Session.Session))
getSessionWith context guid =
    Pool.withResource (Context.pool context) $ \ connection ->
        Session.selectByGuid connection guid

makeResponse :: ToXml.ToXml page => Monadoc page -> Wai.Response
makeResponse monadoc =
    let
        baseUrl = config_baseUrl $ monadoc_config monadoc
        status = Http.ok200
        hLink = CI.mk $ into @ByteString "Link"
        link = into @ByteString $ "<" <> baseUrl <> Route.toString Route.Bootstrap <> ">; rel=preload; as=style"
        headers = [(hLink, link)]
        instruction = Xml.Instruction
            (into @Text "xml-stylesheet")
            (into @Text $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape (baseUrl <> Route.toString Route.Template) <> "\"")
        prologue = Xml.Prologue [Xml.MiscInstruction instruction] Nothing []
        root = Xml.element "monadoc" []
            [ ToXml.toXml $ monadoc_config monadoc
            , Xml.node "page" [] [ToXml.toXml $ monadoc_page monadoc]
            ]
        epilogue = [] :: [Xml.Miscellaneous]
        document = Xml.Document prologue root epilogue
    in Response.xml status headers document

data Monadoc page = Monadoc
    { monadoc_config :: Config
    , monadoc_page :: page
    } deriving (Eq, Show)

data Config = Config
    { config_baseUrl :: String
    , config_breadcrumbs :: [Breadcrumb]
    , config_clientId :: String
    , config_routes :: Routes
    , config_user :: Maybe String
    , config_version :: Version.Version
    } deriving (Eq, Show)

instance ToXml.ToXml Config where
    toXml config = Xml.node "config" []
        [ Xml.node "baseUrl" [] [ToXml.toXml $ config_baseUrl config]
        , Xml.node "breadcrumbs" [] . fmap ToXml.toXml $ config_breadcrumbs config
        , Xml.node "clientId" [] [ToXml.toXml $ config_clientId config]
        , ToXml.toXml $ config_routes config
        , Xml.node "user" [] [ToXml.toXml $ config_user config]
        , Xml.node "version" [] [ToXml.toXml $ config_version config]
        ]

config_fromContext :: Context.Context -> Route.Route -> Config
config_fromContext context self = Config
    { config_baseUrl = Config.baseUrl $ Context.config context
    , config_breadcrumbs = []
    , config_clientId = Config.clientId $ Context.config context
    , config_routes = Routes
        { routes_account = Route.Account
        , routes_bootstrap = Route.Bootstrap
        , routes_callback = Route.Callback
        , routes_favicon = Route.Favicon
        , routes_logOut = Route.LogOut
        , routes_revoke = Route.Revoke
        , routes_search = Route.Search
        , routes_self = self
        }
    , config_user = Nothing
    , config_version = into @Version.Version This.version
    }

data Breadcrumb = Breadcrumb
    { breadcrumb_name :: String
    , breadcrumb_route :: Maybe Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Breadcrumb where
    toXml breadcrumb = Xml.node "breadcrumb" []
        [ Xml.node "name" [] [ToXml.toXml $ breadcrumb_name breadcrumb]
        , Xml.node "route" [] [ToXml.toXml $ breadcrumb_route breadcrumb]
        ]

data Routes = Routes
    { routes_account :: Route.Route
    , routes_bootstrap :: Route.Route
    , routes_callback :: Route.Route
    , routes_favicon :: Route.Route
    , routes_logOut :: Route.Route
    , routes_revoke :: Route.Route
    , routes_search :: Route.Route
    , routes_self :: Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Routes where
    toXml routes = Xml.node "routes" []
        [ Xml.node "account" [] [ToXml.toXml $ routes_account routes]
        , Xml.node "bootstrap" [] [ToXml.toXml $ routes_bootstrap routes]
        , Xml.node "callback" [] [ToXml.toXml $ routes_callback routes]
        , Xml.node "favicon" [] [ToXml.toXml $ routes_favicon routes]
        , Xml.node "logOut" [] [ToXml.toXml $ routes_logOut routes]
        , Xml.node "revoke" [] [ToXml.toXml $ routes_revoke routes]
        , Xml.node "search" [] [ToXml.toXml $ routes_search routes]
        , Xml.node "self" [] [ToXml.toXml $ routes_self routes]
        ]
