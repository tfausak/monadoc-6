module Monadoc.Handler.Common where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.UUID as Uuid
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Routes as Routes
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as This
import qualified Text.XML as Xml
import qualified Web.Cookie as Cookie

getUser :: Context.Context -> Wai.Request -> IO (Maybe (Model.Model User.User))
getUser context request = do
    maybeSession <- getSession context request
    case maybeSession of
        Nothing -> pure Nothing
        Just session -> getUserWith context session

getUserWith :: Context.Context -> Model.Model Session.Session -> IO (Maybe (Model.Model User.User))
getUserWith context session =
    Context.withConnection context $ \ connection ->
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
    Context.withConnection context $ \ connection ->
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
    , config_breadcrumbs :: [Breadcrumb.Breadcrumb]
    , config_clientId :: String
    , config_routes :: Routes.Routes
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
    , config_routes = Routes.Routes
        { Routes.account = Route.Account
        , Routes.bootstrap = Route.Bootstrap
        , Routes.callback = Route.Callback
        , Routes.favicon = Route.Favicon
        , Routes.logOut = Route.LogOut
        , Routes.revoke = Route.Revoke
        , Routes.search = Route.Search Nothing
        , Routes.self = self
        }
    , config_user = Nothing
    , config_version = into @Version.Version This.version
    }
